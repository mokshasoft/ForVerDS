{-
 Copyright 2019, Mokshasoft AB (mokshasoft.com)

 This software may be distributed and modified according to the terms of
 the GNU General Public License version 3. Note that NO WARRANTY is provided.
 See "LICENSE.txt" for details.
 -}
module QuickCheck
  ( runQuickCheckTests
  ) where

import Control.Monad
import Data.Int
import qualified Data.Map as Map
import Foreign.C.Types
import Foreign.Storable
import Test.QuickCheck
import Test.QuickCheck.Monadic

import FFI.Mempool

-- |Top-level function that runs all mempool QuickCheck tests.
runQuickCheckTests :: IO ()
runQuickCheckTests = quickCheck (withMaxSuccess 10000 prop_ReadSameValues)

-- |Index is only needed for the test-case to keep track of memory pointers.
type Index = Int

-- |The memory returned by the mempool is of type void* and is represented as a list of 8-bit ints.
type Data = Int8

-- |The actions a client can do on a mempool and its returned memory.
data Action
  -- |Get a pointer from the mempool and use the index to create a map to the pointers.
  = Get Index
  -- |Return a pointer to the mempool using the map.
  | Return Index
  -- |Write 'Data' to all memory addresses in the memory block returned by the mempool.
  | Write Index
          Data
  -- |Read all memory addresses in the memory block and compare with the expected value in 'Data'.
  | Read Index
         Data
  deriving (Show)

-- |Run a list of well-formed actions on the model.
performOnModel :: [Action] -> [Data]
performOnModel [] = []
performOnModel (a:as) =
  case a of
    Read _ d -> d : performOnModel as
    _ -> performOnModel as

-- |Run a list of well-formed action on the mempool to test.
performOnLibrary :: CInt -> MempoolPtr -> [Action] -> IO [Data]
performOnLibrary block_size pool as =
  performOnLibrary' block_size pool as Map.empty

-- |Helper for 'performOnLibrary'.
-- Runs a list of well-formed actions on the mempool to test. Uses an index-to-pointer map to keep
-- track of the memory addresses returned by Get.
performOnLibrary' ::
     CInt -> MempoolPtr -> [Action] -> Map.Map Index BlockPtr -> IO [Data]
performOnLibrary' _ _ [] _ = return []
performOnLibrary' block_size pool (a:as) m =
  case a of
    Get idx -> do
      blPtr <- mempool_get pool
      performOnLibrary' block_size pool as $ Map.insert idx blPtr m
    Return idx -> do
      mempool_return pool $ m Map.! idx
      performOnLibrary' block_size pool as $ Map.delete idx m
    Write idx d -> do
      let ptr = blockPtr $ m Map.! idx
      mapM_ (\off -> pokeByteOff ptr off d) [0 .. fromIntegral (block_size - 1)]
      performOnLibrary' block_size pool as m
    Read idx d -> do
      let ptr = blockPtr $ m Map.! idx
      res <-
        mapM (peekByteOff ptr) [0 .. fromIntegral (block_size - 1)] :: IO [Data]
      if all (== d) res
        then fmap (d :) (performOnLibrary' block_size pool as m)
        else performOnLibrary' block_size pool as m

-- |Get a minimal well-formed sequence of actions.
getSequence :: Index -> Data -> [Action]
getSequence idx value = [Get idx, Write idx value, Read idx value, Return idx]

-- |Generate a well-formed and random sequence of actions from a number of minimal well-formed
-- actions.
actions :: Gen [Action]
actions = do
  nbr <- arbitrary
  let idx = [1 .. nbr]
  values <- arbitrary :: Gen [Data]
  let cmds = zipWith getSequence idx values
  actions' cmds

-- |Randomly merge together a list of action sequences.
actions' :: [[Action]] -> Gen [Action]
actions' = foldM mix []

-- |Randomly merge two action sequences.
mix :: [Action] -> [Action] -> Gen [Action]
mix [] [] = return []
mix as [] = return as
mix [] bs = return bs
mix (a:as) (b:bs) = do
  rnd <- arbitrary
  if rnd
    then fmap (a :) (mix as (b : bs))
    else fmap (b :) (mix (a : as) bs)

-- |Generate valid mempool creation parameters.
configs :: Gen (CInt, CInt)
configs = do
  block_size <- choose (1, 10)
  nbr_blocks <- choose (1, 10)
  return (block_size, nbr_blocks)

-- The implementation is considered correct if the values returned by the reads are the same in
-- the model and the mempool implementation.
prop_ReadSameValues :: Property
prop_ReadSameValues =
  forAll actions $ \a ->
    forAll configs $ \(block_size, nbr_blocks) -> do
      let golden = performOnModel a
      monadicIO $ do
        dd <-
          run $ do
            pool <- mempool_create block_size nbr_blocks
            d <- performOnLibrary block_size pool a
            mempool_destroy pool
            return d
        assert (golden == dd)

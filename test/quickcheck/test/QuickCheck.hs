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
import qualified Data.Map as Map
import Data.Int
import Data.List
import Data.Ord

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import FFI.Mempool
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

runQuickCheckTests :: IO ()
runQuickCheckTests =
    quickCheck prop_ReadSameValues

type Index = Int
type Data = Int8

data Action =
    Get Index
  | Return Index
  | Write Index Data
  | Read Index Data
  deriving (Show)

performOnModel :: [Action] -> [Data]
performOnModel [] = []
performOnModel (a : as) =
    case a of
        Read _ d -> d : performOnModel as
        otherwise -> performOnModel as

performOnLibrary :: CInt -> MempoolPtr -> [Action] -> IO [Data]
performOnLibrary block_size pool as =
    performOnLibrary' block_size pool as Map.empty

performOnLibrary' :: CInt -> MempoolPtr -> [Action] -> Map.Map Index BlockPtr -> IO [Data]
performOnLibrary' _ _ [] _ = return []
performOnLibrary' block_size pool (a : as) m =
    case a of
        Get idx -> do
            blPtr <- mempool_get pool
            performOnLibrary' block_size pool as $ Map.insert idx blPtr m
        Return idx -> do
            mempool_return pool $ m Map.! idx
            performOnLibrary' block_size pool as $ Map.delete idx m
        Write idx d -> do
            let ptr = blockPtr $ m Map.! idx
            mapM_ (\off -> pokeByteOff ptr off d) [0..fromIntegral (block_size - 1)]
            performOnLibrary' block_size pool as m
        Read idx d -> do
            let ptr = blockPtr $ m Map.! idx
            res <- mapM (\off -> peekByteOff ptr off) [0..fromIntegral (block_size - 1)] :: IO [Data]
            if all (== d) res
                then liftM (d :) $ performOnLibrary' block_size pool as m
                else performOnLibrary' block_size pool as m

getSequence :: Index -> Data -> [Action]
getSequence idx value =
    [ Get idx
    , Write idx value
    , Read idx value
    , Return idx
    ]

actions :: Gen [Action]
actions = do
    nbr <- arbitrary
    let idx = [1..nbr]
    values <- arbitrary :: Gen [Data]
    let cmds = map (\(i, v) -> getSequence i v) $ zip idx values
    actions' cmds

actions' :: [[Action]] -> Gen [Action]
actions' a = return $ concat a

configs :: Gen (CInt, CInt)
configs = do
    block_size <- choose (1, 10)
    nbr_blocks <- choose (1, 10)
    return (block_size, nbr_blocks)

prop_ReadSameValues :: Property
prop_ReadSameValues =
    forAll actions $ \a ->
    forAll configs $ \(block_size, nbr_blocks) -> do
        let golden = performOnModel a
        monadicIO $ do
            dd <- run $ do
                pool <- mempool_create block_size nbr_blocks
                d <- performOnLibrary block_size pool a
                mempool_destroy pool
                return d
            assert (golden == dd)

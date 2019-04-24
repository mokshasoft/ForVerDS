{-
 Copyright 2019, Mokshasoft AB (mokshasoft.com)

 This software may be distributed and modified according to the terms of
 the GNU General Public License version 3. Note that NO WARRANTY is provided.
 See "LICENSE.txt" for details.
 -}
{-
 Modelling the mempool
 -}
module Model.Mempool
  ( createS
  , getS
  , returnS
  ) where

import Data.Array.IO
import Data.Char
import Data.List

type MemoryBlock = IO (IOArray Int Char)

mkMemoryBlock :: Int -> MemoryBlock
mkMemoryBlock size = newArray (1, size) $ chr 0

newtype MempoolState = MempoolState
  { blockSize :: Int
  }

createS :: Int -> Int -> MempoolState
createS block_size _ = MempoolState {blockSize = block_size}

getS :: MempoolState -> (MempoolState, MemoryBlock)
getS state = (state, mkMemoryBlock $ blockSize state)

returnS :: MempoolState -> MemoryBlock -> MempoolState
returnS state _ = state

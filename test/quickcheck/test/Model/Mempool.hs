{-
 Copyright 2019, Mokshasoft AB (mokshasoft.com)

 This software may be distributed and modified according to the terms of
 the GNU General Public License version 3. Note that NO WARRANTY is provided.
 See "LICENSE.txt" for details.
 -}

module Model.Mempool
    ( createS
    , getS
    , returnS
    ) where

{- Modelling this C interface
    typedef struct mempool_t mempool;
    mempool* mempool_create(size_t item_size, size_t nbr_items);
    void mempool_destroy(mempool* pool);
    void* mempool_get(mempool* pool);
    void mempool_return(mempool* pool, void* ptr);
 -}

import Data.Char
import Data.List
import Data.Array.IO

type MemoryBlock = IO (IOArray Int Char)

mkMemoryBlock :: Int -> MemoryBlock
mkMemoryBlock size =
    newArray (1, size) $ chr 0

newtype MempoolState = MempoolState
    { blockSize :: Int
    }

createS :: Int -> Int -> MempoolState
createS block_size _ = MempoolState
    { blockSize = block_size
    }

getS :: MempoolState -> (MempoolState, MemoryBlock)
getS state =
    (state, mkMemoryBlock $ blockSize state)

returnS :: MempoolState -> MemoryBlock -> MempoolState
returnS state _ =
    state

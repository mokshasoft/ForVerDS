{-
 Copyright 2019, Mokshasoft AB (mokshasoft.com)

 This software may be distributed and modified according to the terms of
 the GNU General Public License version 3. Note that NO WARRANTY is provided.
 See "LICENSE.txt" for details.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module FFI.Mempool where

import Data.Int
import Foreign.C
import Foreign.Ptr

{- FFI for this C interface from mempool.h
    typedef struct mempool_t mempool;
    mempool* mempool_create(size_t item_size, size_t nbr_items);
    void mempool_destroy(mempool* pool);
    void* mempool_get(mempool* pool);
    void mempool_return(mempool* pool, void* ptr);
 -}

newtype BlockPtr = BlockPtr
    { blockPtr :: Ptr Int8
    }

newtype MempoolPtr = MempoolPtr
    { mempoolPtr :: Ptr Int8
    }

foreign import ccall "mempool_create" mempool_create :: CInt -> CInt -> IO MempoolPtr
foreign import ccall "mempool_destroy" mempool_destroy :: MempoolPtr -> IO ()
foreign import ccall "mempool_get" mempool_get :: MempoolPtr -> IO BlockPtr
foreign import ccall "mempool_return" mempool_return :: MempoolPtr -> BlockPtr -> IO ()

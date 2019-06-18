{-
 Copyright 2019, Mokshasoft AB (mokshasoft.com)

 This software may be distributed and modified according to the terms of
 the GNU General Public License version 3. Note that NO WARRANTY is provided.
 See "LICENSE.txt" for details.
 -}
import Foreign.Ptr
import Test.HUnit

import FFI.Mempool as FFI
import Model.Mempool as Model
import QuickCheck

main :: IO ()
main = do
  runHUnitTests
  runQuickCheckTests

-- HUnit
runHUnitTests :: IO ()
runHUnitTests = do
  res <- runTestTT hTests
  putStrLn $ "Results HUnit tests: " ++ show res

hTest1 =
  TestCase
    (do let mempool = Model.createS 10 100
        let mem = Model.getS mempool
        assertBool "This can never fail" True)

hTest2 =
  TestCase
    (do let pool1 = Model.createS 10 1
        let (pool2, mem1) = Model.getS pool1
        let (_, mem2) = Model.getS pool2
        assertBool "This can never fail" True)

hTest3 =
  TestCase
    (do mempool <- FFI.mempool_create 10 1
        FFI.mempool_destroy mempool)

hTest4 =
  TestCase
    (do mempool <- FFI.mempool_create 10 1
        ptr <- FFI.mempool_get mempool
        FFI.mempool_destroy mempool
        assertBool "No memory returned" $ blockPtr ptr /= nullPtr)

hTest5 =
  TestCase
    (do mempool <- FFI.mempool_create 10 1
        ptr1 <- FFI.mempool_get mempool
        ptr2 <- FFI.mempool_get mempool
        FFI.mempool_destroy mempool
        assertBool "Memory not returned from empty mempool" $
          blockPtr ptr2 /= nullPtr)

hTests =
  TestList
    [ TestLabel "Model: mempool get" hTest1
    , TestLabel "Model: mempool get from empty" hTest2
    , TestLabel "FFI: mempool create/destroy" hTest3
    , TestLabel "FFI: mempool get" hTest4
    , TestLabel "FFI: mempool get from empty" hTest5
    ]

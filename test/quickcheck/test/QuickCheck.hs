{-
 Copyright 2019, Mokshasoft AB (mokshasoft.com)

 This software may be distributed and modified according to the terms of
 the GNU General Public License version 3. Note that NO WARRANTY is provided.
 See "LICENSE.txt" for details.
 -}
module QuickCheck
  ( runQuickCheckTests
  ) where

import MempoolQC

-- |Top-level function that runs all QuickCheck tests
runQuickCheckTests :: IO ()
runQuickCheckTests = runMempoolQC

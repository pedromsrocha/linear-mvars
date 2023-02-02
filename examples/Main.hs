module Main where

import Test.Counter qualified as Counter 
import Test.LList qualified as LList 
import Test.LChan qualified as LChan
import Test.HUnit
import qualified GHC.Base as List
import qualified GHC.Base as List
{-
import Test.OneShot qualified as OneShot
import Test.Session qualified as Session
import Test.Session.DF qualified as SessionDF
import Prelude
-}

main :: IO Counts
main = runTestTT tests
  where
    tests :: Test
    tests =
      TestList
        [ 
            Counter.test1, 
            Counter.test2,
            Counter.test3,
            LList.test1, 
            LList.test2, 
            LList.test3,
            LChan.test1
        ]
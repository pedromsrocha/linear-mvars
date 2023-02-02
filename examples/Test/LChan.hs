{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.LChan where

import Test.HUnit 
import LChan
import LVars
import Control.Functor.Linear
import System.IO.Linear qualified as Linear
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe (toLinear) 


allGEQ1 :: [Maybe Int] -> Bool 
allGEQ1 = all (>= Just 1) . filter (/= Nothing)

allLEQ1000 :: [Maybe Int] -> Bool 
allLEQ1000 = all (<= Just 1000) . filter (/= Nothing)

test1 :: Test
test1 = TestCase main
  where
    main :: IO ()
    main = Linear.withLinearIO $
            do  (ch::ChanR Int) <- newChan  (\ch -> do ch <- writeList [1..1000] ch; dispose ch) 
                (xs, ch) <- readN 1000 ch 
                dispose ch 
                (Unsafe.toLinear $ (\xs -> Linear.fromSystemIO $ assertBool (show xs) (allGEQ1 xs && allLEQ1000 xs))) xs
                return (Ur ())



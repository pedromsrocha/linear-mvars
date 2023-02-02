{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Test.Counter where

import Test.HUnit
import Counter
import LVars
import Control.Functor.Linear
import System.IO.Linear qualified as Linear
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe (toLinear) 

test1 :: Test
test1 = TestCase main
  where
    main :: IO ()
    main = Linear.withLinearIO $
           do c <- newFull 0
              c <- incC c
              c <- incC c
              c <- incC c
              (n,c) <- getC c
              dispose c 
              (Unsafe.toLinear $ (\n -> Linear.fromSystemIO $ assertEqual (show n) 3 n)) n
              return (Ur ())



test2 :: Test
test2 = TestCase main
  where
    main :: IO ()
    main = Linear.withLinearIO $
           do c <- newFull 0
              c <- incC c
              c <- incC c
              c <- incC c
              c <- resetC c
              (n,c) <- getC c
              dispose c 
              (Unsafe.toLinear $ (\n -> Linear.fromSystemIO $ assertEqual (show n) 0 n)) n
              return (Ur ())

              

test3 :: Test
test3 = TestCase main
  where
    main :: IO ()
    main = Linear.withLinearIO $
           do c <- newFull 0
              (c, Ur _) <- share c (\c -> do c <- incC c
                                             c <- incC c
                                             dispose c)
              c <- incC c
              c <- incC c
              c <- incC c
              (n,c) <- getC c
              dispose c 
              (Unsafe.toLinear $ (\n -> Linear.fromSystemIO $ assertBool (show n) (n >= 3 && n <= 5))) n
              return (Ur ())
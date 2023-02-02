{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.LList where

import Test.HUnit 
import LList
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
           do (l :: LL Int) <- empty
              (xs,l) <- toList l
              dispose l
              (Unsafe.toLinear $ (\xs -> Linear.fromSystemIO $ assertEqual "" xs [] )) xs
              return (Ur ()) 


test2 :: Test
test2 = TestCase main
  where
    main :: IO ()
    main = Linear.withLinearIO $
           do (l::LL Int) <- empty
              l <- fromListP [1..10] l
              (xs,l) <- toList l
              dispose l
              (Unsafe.toLinear $ (\xs -> Linear.fromSystemIO $ assertEqual "" xs (reverse [1..10]) )) xs
              return (Ur ()) 
              
test3 :: Test
test3 = TestCase main
  where
    main :: IO ()
    main = Linear.withLinearIO $
           do (l::LL Int) <- empty
              l <- fromListA [1..10] l
              (xs, l) <- toList l
              dispose l
              (Unsafe.toLinear $ (\xs -> Linear.fromSystemIO $ assertEqual "" xs [1..10])) xs
              return (Ur ())
           
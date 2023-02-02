{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module Counter where 

import LVars
import Control.Functor.Linear  
import System.IO.Linear qualified as Linear
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe (toLinear, toLinear2)

type Counter = Full Int

incC :: Counter %1 ->  Linear.IO Counter
incC c = do (n,c) <- takeL c
            c <- putL c (n + 1)
            return c

getC :: Counter %1 -> Linear.IO (Int, Counter)
getC c = do (n,c) <- takeL c
            (n1, n2) <- return (dup n)
            c <- putL c n1
            return (n2, c)  

resetC :: Counter %1 -> Linear.IO Counter
resetC c = do (n,c) <- takeL c
              return (consume n)
              c <- putL c 0 
              return c 


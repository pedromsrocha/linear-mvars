{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module LVars where 

import System.IO.Linear qualified as Linear
import Control.Monad
import Control.Applicative
import Control.Functor.Linear qualified as Linear
import Prelude.Linear qualified as Linear ((.), ($))
import Control.Concurrent.MVar
import Control.Concurrent (ThreadId, forkIO)
import Unsafe.Linear qualified as Unsafe (toLinear, toLinear2, coerce)
import Data.Unrestricted.Linear 


newtype Full a  = Full (MVar (Int, a)) 
newtype Empty a = Empty (Int, MVar (Int, a)) 


unFull :: Full a %1 -> MVar (Int, a)
unFull (Full x) = x

unEmpty :: Empty a %1 -> (Int, MVar (Int, a))
unEmpty (Empty x) = x 

newFull :: a %1 -> Linear.IO (Full a)
newFull = Unsafe.toLinear Linear.$
  \x -> Linear.fromSystemIO Linear.$
        do c <- newMVar (1,x)
           return (Full c)
           
newEmpty :: Linear.IO (Empty a)
newEmpty = Linear.fromSystemIO Linear.$
           do c <- newEmptyMVar
              return (Empty (1,c))

takeL :: Full a %1 -> Linear.IO (a, Empty a)
takeL = Unsafe.toLinear Linear.$
  \c -> Linear.fromSystemIO Linear.$
        do (n,x) <- takeMVar (unFull c)
           return (x, Empty (n, (unFull c)))


putL :: Empty a %1-> a %1 -> Linear.IO (Full a)
putL = Unsafe.toLinear2 Linear.$
  \c x -> Linear.fromSystemIO Linear.$
          let (n,cc) = unEmpty c in
            do putMVar cc (n,x)
               return (Full cc)

class Disposable a where
  dispose :: a %1 -> Linear.IO ()


instance Consumable a => Disposable a where
  dispose x = Linear.return (consume x) 


instance {-# OVERLAPPING #-} Disposable a => Disposable (Full a) where
  dispose =  Unsafe.toLinear Linear.$
    \c -> Linear.fromSystemIO Linear.$
          let cc = unFull c in
            do (n,x) <- takeMVar cc
               if n == 1
                 then Unsafe.coerce (dispose x)
                 else do putMVar cc (n-1,x)
                         return ()

class Shareable m1 m2 m3 | m1 m2 -> m3, m1 m3 -> m2, m2 m3 -> m1 where
  share :: m1 a %1 -> (m2 a %1 -> Linear.IO()) %1 -> Linear.IO (m3 a, Ur ThreadId)

instance Shareable Full Full Full where
  -- share :: Full a % 1 -> (Full a %1 -> Linear.IO()) %1 -> Linear.IO (Full a, Ur ThreadId)
  share = Unsafe.toLinear2 Linear.$
    \c action -> Linear.fromSystemIO Linear.$
                 let cc = unFull c in
                   do (n, x) <- takeMVar cc
                      putMVar cc (n+1,x)
                      tid <- forkIO(Linear.withLinearIO ((Unsafe.toLinear Ur Linear.<$> (action c))))
                      return (c, Ur tid)


instance Shareable Empty Full Empty  where
  -- share :: Empty a % 1 -> (Full a %1 -> Linear.IO()) %1 -> Linear.IO (Empty a, Ur ThreadId)
  share = Unsafe.toLinear2 Linear.$
    \c action -> Linear.fromSystemIO Linear.$
                 let (n, cc) = unEmpty c in 
                   do tid <- forkIO(Linear.withLinearIO ((Unsafe.toLinear Ur) Linear.<$> (action (Full cc) )))
                      return (Empty (n+1,cc), Ur tid)

instance Shareable Empty Empty Full where
  -- share :: Empty a %1 -> (Empty a %1 -> Linear.IO()) %1 -> Linear.IO (Full a, Ur ThreadId)
  share = Unsafe.toLinear2 Linear.$
    \c action -> Linear.fromSystemIO Linear.$
                 let (n, cc) = unEmpty c in 
                   do tid <- forkIO(Linear.withLinearIO ((Unsafe.toLinear Ur) Linear.<$> (action (Empty (n+1,cc)) )))
                      return (Full cc, Ur tid)  
                      

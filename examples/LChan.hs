{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LChan where 

import LVars
import LList
import Control.Functor.Linear  
import Data.Num.Linear 
import Data.Unrestricted.Linear
import System.IO.Linear qualified as Linear
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe (toLinear, toLinear2, coerce)
import Control.Concurrent (threadDelay)

newtype ChanW a = ChanW (Full (LL a))
newtype ChanR a = ChanR (Full (LL a))

newChan :: (ChanW a %1 -> Linear.IO ()) %1 -> Linear.IO (ChanR a)
newChan wAction = do l <- empty
                     (l, Ur _) <- share l (\l -> do c <- newFull l; wAction (ChanW c))
                     c <- newFull l
                     return (ChanR c)

writeChan :: Disposable a => ChanW a %1 -> a %1 -> Linear.IO (ChanW a)
writeChan (ChanW wPtr) v = do (LL l, wPtr) <- takeL wPtr
                              (n, l) <- takeL l
                              newl <- newFull n
                              (newl, Ur _) <- share newl (\x -> do l <- putL l (Node (Just (v, LL x))); dispose l) 
                              wPtr <- putL wPtr (LL newl)
                              return (ChanW wPtr)


readChan :: Disposable a => ChanR a %1 -> Linear.IO (Maybe a, ChanR a)
readChan (ChanR rPtr) = do (LL l, rPtr) <- takeL rPtr
                           (Node n, l) <- takeL l
                           case n of
                             Nothing -> do l <- putL l (Node Nothing)
                                           rPtr <- putL rPtr (LL l)
                                           return (Nothing, ChanR rPtr)
                             Just (v, l2) -> do l <- putL l (Node Nothing)
                                                dispose l
                                                rPtr <- putL rPtr l2
                                                return (Just v, ChanR rPtr) 
                                

instance {-# OVERLAPPING #-} Disposable a => Disposable (ChanW a ) where
  --dispose :: ChanW a %1 -> Linear.IO () 
  dispose (ChanW wPtr) = dispose wPtr 

instance {-# OVERLAPPING #-} Disposable a => Disposable (ChanR a ) where
  --dispose :: ChanW a %1 -> Linear.IO () 
  dispose (ChanR rPtr) = dispose rPtr 


instance Shareable ChanW ChanW ChanW where
  -- share :: ChanW a %1 -> (ChanW a %1 -> Linear.IO()) %1 -> Linear.IO (ChanW, Ur ThreadId)
  share (ChanW wPtr) thread  = do (wPtr, tid) <- share wPtr (\x -> thread (ChanW x))
                                  return (ChanW wPtr, tid)

instance Shareable ChanR ChanR ChanR where
  -- share :: ChanR a %1 -> (ChanR a %1 -> Linear.IO()) %1 -> Linear.IO (ChanR, Ur ThreadId)
  share (ChanR rPtr) thread = do  (rPtr, tid) <- share rPtr (\x -> thread (ChanR x))
                                  return (ChanR rPtr, tid) 


writeList :: Disposable a => [a] %1 -> ChanW a %1 -> Linear.IO(ChanW a) 
writeList [] ch = return ch 
writeList (x:xs) ch = do  ch <- writeChan ch x 
                          ch <- writeList xs ch 
                          return ch


readN :: Disposable a => Int -> ChanR a %1 -> Linear.IO ([Maybe a], ChanR a)
readN 0 ch = return ([], ch)
readN n ch = do (x, ch) <- readChan ch 
                (xs,ch) <- readN (n-1) ch
                return (x:xs, ch)
                  

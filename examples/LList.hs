{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LList where 

import LVars
import Control.Functor.Linear  
import Data.Num.Linear 
import Data.Unrestricted.Linear
import System.IO.Linear qualified as Linear
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe (toLinear, toLinear2, coerce)
import Control.Concurrent (threadDelay)

newtype  Node a = Node (Maybe (a, LL a)) 
newtype  LL a = LL (Full (Node a)) 


instance {-# OVERLAPPING #-} Disposable a => Disposable (Node a) where
  --dispose :: Node a %1 - Linear.IO ()
  dispose (Node x) = case x of
                       Nothing -> return () 
                       Just (v, LL l) -> do dispose v
                                            dispose l 

instance {-# OVERLAPPING #-} Disposable a => Disposable (LL a) where
  --dispose :: LL a %1 -> Linear.IO ()
  dispose (LL c) = dispose c

instance Shareable LL LL LL where
  --share :: LL a %1 -> (LL a %1 -> Linear.IO ()) %1 -> Linear.IO (LL a, Ur ThreadId)
  share (LL l) thread = do  (l, tid) <- share l (\l -> thread (LL l))
                            return (LL l, tid)  

empty :: Linear.IO (LL a)
empty = do l <- newFull $ Node Nothing  
           return (LL l)

singleNode :: a %1 -> Linear.IO (Node a)
singleNode v = do l <- empty
                  return $ Node $ Just (v, l)

prepend :: a %1 -> LL a %1 -> Linear.IO (LL a)
prepend v l = do nl <- newFull $ Node $ Just (v,l)
                 return (LL nl) 

append :: a %1 -> LL a %1 -> Linear.IO (LL a)
append v (LL l) = do (Node current,l) <- takeL l
                     case current of
                       Nothing -> do new <- singleNode v
                                     l <- putL l new
                                     return (LL l) 
                       Just (hd,nxt) -> do nxt <- append v nxt
                                           l <- putL l $ Node $ Just (hd,nxt)
                                           return (LL l)



toList :: (Dupable a) => LL a %1 -> Linear.IO ([a], LL a)
toList (LL l) = do (Node current, l) <- takeL l
                   case current of
                     Nothing -> do l <- putL l (Node Nothing)
                                   return ([], LL l)
                     Just (hd, nxt) -> do (xs, nxt) <- toList nxt
                                          (hd1, hd2) <- return (dup hd)
                                          l <- putL l $ Node $ Just  (hd1, nxt)
                                          return (hd2:xs, LL l)


fromListP :: [a] %1 -> LL a %1 -> Linear.IO (LL a)
fromListP [] l = return l
fromListP (x:xs) l = do l <- prepend x l
                        l <- fromListP xs l
                        return l

fromListA :: [a] %1 -> LL a %1 -> Linear.IO (LL a)
fromListA [] l = return l
fromListA (x:xs) l = do l <- append x l
                        l <- fromListA xs l
                        return l 

{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Bracket
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bar splitting and beam grouping.
--
--------------------------------------------------------------------------------



module Mullein.Bracket where

import Mullein.Core
import Mullein.Duration
import Mullein.Utils

import MonadLib.Monads
import qualified Data.DList as D

import Data.Ratio

newtype SnocWriterT e m a = SnocWriterT { 
          getSnocWriterT :: D.DList e -> m (a, D.DList e) }

instance Monad m => Monad (SnocWriterT e m) where
  return a = SnocWriterT $ \dl -> return (a,dl)
  mf >>= k = SnocWriterT $ \dl -> getSnocWriterT mf dl >>= \(a,dl') ->
                                  (getSnocWriterT . k) a dl'

instance MonadT (SnocWriterT e) where
  lift mf = SnocWriterT $ \dl -> mf >>= \a -> return (a,dl)

runSnocWriterT :: Monad m => SnocWriterT e m a -> m (a, D.DList e)
runSnocWriterT mf = getSnocWriterT mf $ D.empty

-- type family Elem c :: *


class Monad m => SnocWriterM m where
  type DiffElem m :: *
  snoc :: DiffElem m -> m () 



instance (Monad m) => SnocWriterM (SnocWriterT e m) where
  type DiffElem (SnocWriterT e m) = e
  snoc e = SnocWriterT $ \dl -> return ((), dl `D.snoc` e)  

{-
-- cannot make a WriterM instance as the parameter 
instance Monad m => WriterM (SnocWriterT e m) (D.DList e) where
  put e = SnocWriterT $ \dl -> return ((), dl `D.snoc` e)
-}               


data Step a s = Done | Skip s | Yield a s
  deriving (Eq)

data Binary a = One a | Two a a
  deriving (Eq,Show)


ratDuration :: HasDuration e => e -> Rational
ratDuration = extent . getDuration


apoSkipList :: (st -> a -> Step b st) -> (st -> [a] -> [b]) -> st -> [a] -> [b]
apoSkipList f g st0 xs0 = step st0 xs0 where
  step s []     = g s []
  step s (x:xs) = case f s x of
                    Done       -> g s (x:xs)     -- x must be re-queued
                    Skip s'    -> step s' xs
                    Yield a s' -> a : step s' xs
           

-- | Skipping apomorphism that is unfolded /along/ a list
-- Unfortunately, a single beam step can produce two results:
--   1. the current accumulation 
--   2. a single note too long for a beam group
-- So we have to pollute what could be a general combinator @apoSkipList@ 
-- with the result wrapped in the Binary type.

apoSkipListB :: (st -> a -> Step (Binary b) st) -> (st -> [a] -> [b]) -> st -> [a] -> [b]
apoSkipListB f g st0 xs0 = step st0 xs0 where
  step s []     = g s []
  step s (x:xs) = case f s x of
                    Done               -> g s (x:xs)     -- x must be re-queued
                    Skip s'            -> step s' xs
                    Yield (One a) s'   -> a : step s' xs
                    Yield (Two a b) s' -> a : b : step s' xs



--------------------------------------------------------------------------------
-- bar & beam

-- bracket :: (HasDuration e, Tied e) => MeterPattern -> [e] -> [Bar e]
-- bracket mp = map (Bar . beam mp) . bar (sum mp)


bracket :: (HasDuration e, Tied e) => MeterPattern -> [e] -> [Bar e]
bracket mp notes = runId $ 
    barM (sum mp) notes >>= mapM (\es -> beamM mp es >>= return . Bar)

--------------------------------------------------------------------------------
-- bar

-- In LilyPond, if a series of notes does not divide properly 
-- into bars then the bar on the left gets the 'bad' note making 
-- it too long, the spill duration of ther long note is then 
-- subtracted from the bar on the right.

bar :: (HasDuration e, Tied e) => Rational -> [e] -> [[e]]
bar bar_len ns = apoSkipList (barStep bar_len) barFlush ([],0) ns


barStep :: (HasDuration e, Tied e) 
        => Rational -> ([e],Rational) -> e -> Step [e] ([e],Rational)
barStep bar_len (ca,i) note = 
    case compare (i+note_len) bar_len of

      -- fits with room to spare (add to accumulator)
      LT -> Skip (note:ca,i+note_len)
    
      -- exact fit ("close" the bar)
      EQ -> Yield (reverse $ note:ca) ([],0)

      -- too big (split if possible)
      GT -> case split i note of
              Just (l,r) -> Yield (reverse $ tied $ l:ca) (pushback r)

              -- The note doesn't fit but it won't split either
              -- so push it back...
              Nothing    -> Yield (reverse $ ca) (pushback note)
  where
    pushback a = ([a], ratDuration a)
    note_len   = ratDuration note


    tied xs   = mkTie : xs

split :: HasDuration e => Rational -> e -> Maybe (e,e)
split i note = fn $ splitDuration i $ getDuration note
  where
    fn (Just left, right) = Just (setDuration left note, setDuration right note)
    fn (Nothing,_)        = Nothing


barM :: (Monad m, HasDuration e) => Rational -> [e] -> m [[e]]
barM barlen ns = runSnocWriterT (consumeSt barStepM (0,[]) ns) >>= fn
  where
    fn ((_,[]),dlist)  = return $ D.toList dlist
    fn ((_,cca),dlist) = return $ D.toList $ dlist `D.snoc` (reverse cca)
 

    barStepM e (n,cca) = let ed = ratDuration e 
                         in case (n+ed) `compare` barlen of
                           GT -> snoc (reverse $ e:cca) >>
                                 emptySpan barlen ed    >>= \x ->
                                 return (x,[])
                           EQ -> snoc (reverse $ e:cca) >> return (0,[])
                           LT -> return (n+ed,e:cca)

emptySpan :: (SnocWriterM m, DiffElem m ~ [a]) => Rational -> Rational -> m Rational
emptySpan barlen n = let (blank_count,x) = n `divModR` barlen in 
    replicateM_ (fromIntegral blank_count) (snoc []) >> return x


-- 
barFlush :: ([e],Rational) -> [e] -> [[e]]
barFlush ([],_) _ = []
barFlush (ca,_) _ = [reverse ca]



--------------------------------------------------------------------------------
-- beam


beam :: HasDuration e => MeterPattern -> [e] ->  [Pulse e]
beam mp notes = apoSkipListB beamStep beamFlush ([],mp) notes


beamStep :: HasDuration e => ([e],[Rational]) 
         -> e
         -> Step (Binary (Pulse e)) ([e],[Rational])
beamStep (ca,stk) e = maybe Done (sk (ratDuration e)) (top stk) where
  sk n d | n >= 1%4 || n > d  = next ca     (Just e) (consume n stk)
         | n == d             = next (e:ca) Nothing  (consume n stk)
         | otherwise          = skip (e:ca) (consume n stk)

  next []  Nothing  ss = Skip ([],ss)
  next []  (Just a) ss = Yield (One (Pulse a)) ([],ss)
  next cca (Just a) ss = Yield (Two (toPulse $ reverse cca) (Pulse a)) ([],ss)
  next cca _        ss = Yield (One (toPulse $ reverse cca)) ([],ss)

  skip cca ss          = Skip (cca,ss) 


beamFlush :: HasDuration e => ([e],z) -> [e] -> [Pulse e]
beamFlush ([],_)  rs = map Pulse rs
beamFlush (cca,_) rs = toPulse (reverse cca) : map Pulse rs




beamM :: (HasDuration e, Monad m) => MeterPattern -> [e] -> m [Pulse e]
beamM mp notes = runSnocWriterT (consumeSt beamStepM (mp,[]) notes) >>= fn
  where
    fn ((_,[]),dlist)  = return $ D.toList dlist
    fn ((_,cca),dlist) = return $ D.toList $ dlist `D.snoc` (toPulse $ reverse cca)

beamStepM :: (HasDuration e, SnocWriterM m, DiffElem m ~ Pulse e)
          => e -> (MeterPattern,[e]) -> m (MeterPattern,[e])
beamStepM e ([],cca)          = putPulsation cca >> putPulse1 e >> return ([],[])
beamStepM e (stk@(a:rs),cca)  = fn (ratDuration e) 
  where
    fn n | n >= 1%4 || n > a  = putPulsation cca >> putPulse1 e 
                                                 >> return (consume n stk,[])
         | n == a             = putPulsation (e:cca) >> return (rs,[])
         | otherwise          = return (consume n stk, e:cca)

                                            
-- Run a stateful computation over a list, return the final state
consumeSt :: Monad m => (a -> st -> m st) -> st -> [a] -> m st
consumeSt _  st []     = return st
consumeSt mf st (a:as) = mf a st >>= \st' -> consumeSt mf st' as  

putPulse1 :: (SnocWriterM m, DiffElem m ~ Pulse e) => e -> m ()
putPulse1 a = snoc $ Pulse a

putPulsation :: (SnocWriterM m, DiffElem m ~ Pulse e) => [e] -> m ()
putPulsation []  = return ()
putPulsation [a] = snoc $ Pulse a
putPulsation cca = snoc $ BeamedL $ reverse cca

toPulse :: [a] -> Pulse a
toPulse []  = error "Bracket.toPulse: cannot build Pulse from empty list"
toPulse [x] = Pulse x
toPulse xs  = BeamedL xs



-- Maybe the top of the stack.
top :: [a] -> Maybe a
top (x:_) = Just x
top []    = Nothing


-- reduce the MeterPattern stack by the duration
consume :: (Num a, Ord a) => a -> [a] -> [a]
consume _ []                  = []
consume n (x:xs) | n < x      = (x-n):xs
                 | n == x     = xs
                 | otherwise  = consume (n-x) xs

                      

  
---------------------------------------------------------------------------------
-- overlay


zipOverlays :: [Bar e] -> [Bar e] -> [Bar e]
zipOverlays = endoLongZip gappend


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



--------------------------------------------------------------------------------
-- bar & beam


phrase :: (HasDuration t) => MeterPattern -> [t Duration] -> Phrase (t Duration)
phrase mp notes = runId $ 
    barM (sum mp) notes >>= mapM (\es -> beamM mp es >>= return . Bar)

--------------------------------------------------------------------------------
-- bar

-- In LilyPond, if a series of notes does not divide properly 
-- into bars then the bar on the left gets the 'bad' note making 
-- it too long, the spill duration of ther long note is then 
-- subtracted from the bar on the right.


barM :: (Monad m, HasDuration t) => Rational -> [t Duration] -> m [[t Duration]]
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



--------------------------------------------------------------------------------
-- beam




beamM :: (HasDuration t, Monad m) 
      => MeterPattern -> [t Duration] -> m [Pulse (t Duration)]
beamM mp notes = runSnocWriterT (consumeSt beamStepM (mp,[]) notes) >>= fn
  where
    fn ((_,[]),dlist)  = return $ D.toList dlist
    fn ((_,cca),dlist) = return $ D.toList $ dlist `D.snoc` (toPulse $ reverse cca)

beamStepM :: (HasDuration t, SnocWriterM m, DiffElem m ~ Pulse (t Duration))
          => t Duration 
          -> (MeterPattern,[t Duration]) 
          -> m (MeterPattern,[t Duration])
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


ratDuration :: HasDuration t => t Duration -> Rational
ratDuration = extent . getDuration                      



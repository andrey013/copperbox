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



module Mullein.Bracket 
  ( 
  -- * Partition into bars and pulsations
    phrase
  , freePhrase
  , overlayPhrases

  ) where

import Mullein.Core
import Mullein.Duration
import Mullein.Utils

import MonadLib.Monads
import qualified Data.DList as D

import Data.Ratio


--------------------------------------------------------------------------------

-- /Extremity Beam/ certain gylphs (rests) should not be the 
-- start or end of a beam group.

class ExtBeam a where
  extBeam :: a -> Bool

instance ExtBeam (Glyph pch dur) where
  extBeam (Note _ _)     = True
  extBeam (Rest _)       = False
  extBeam (Spacer _)     = True     -- Note - is this correct? 
  extBeam (Chord _ _)    = True
  extBeam (GraceNotes _) = False
  extBeam Tie            = False

--------------------------------------------------------------------------------
-- Writer-like Snoc monad machinary

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



--------------------------------------------------------------------------------
-- bar & beam

-- | Partition into bars and pulsations. A pulsation is either 
-- a single notes or a group of notes joined with a beam. 
-- Pulsations illustrate the division of the bar into beats.
-- The length of the bars will be the sum of the meter pattern.
phrase :: (HasDuration t, ExtBeam (t Duration))
       => MeterPattern -> [t Duration] -> Phrase (t Duration)
phrase mp notes = runId $ 
  barM (sum mp) notes >>= mapM (\es -> beamM mp es >>= return . Bar)

-- | Partition into pulsations and return one long bar. Use this 
-- function for free metered music, the MeterPattern should be
-- infinite.
freePhrase :: (HasDuration t, ExtBeam (t Duration))
           => MeterPattern -> [t Duration] -> Phrase (t Duration)
freePhrase mp notes = runId $ do 
  xs <- beamM mp notes
  return $ [Bar xs]


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




beamM :: (HasDuration t, Monad m, ExtBeam (t Duration)) 
      => MeterPattern -> [t Duration] -> m [Pulse (t Duration)]
beamM mp notes = runSnocWriterT (consumeSt beamStepM (mp,[]) notes) >>= fn
  where
    fn ((_,[]),dlist)  = return $ D.toList dlist
    fn ((_,cca),dlist) = return $ D.toList $ dlist `D.snoc` (toPulse $ reverse cca)

    -- Problem - the /cca/ in the flush doesn't respect extBeam-ing, 
    -- i.e. it might start or finish with a rest. This needs more 
    -- thought.


beamStepM :: (HasDuration t, SnocWriterM m, ExtBeam (t Duration), 
              DiffElem m ~ Pulse (t Duration))
          => t Duration 
          -> (MeterPattern,[t Duration]) 
          -> m (MeterPattern,[t Duration])
beamStepM e ([],cca)          = putPulsation (reverse cca) >> putPulse1 e 
                                                           >> return ([],[])
beamStepM e (stk@(a:rs),cca)  = fn (ratDuration e) 
  where
    fn n | n >= 1%4 || n > a  = putPulsation (reverse cca) >> putPulse1 e 
                                  >> return (consume n stk,[])
         | n == a             = putPulsation (reverse $ e:cca) >> return (rs,[])
         | otherwise          = return (consume n stk, e:cca)

                                            
-- Run a stateful computation over a list, return the final state
consumeSt :: Monad m => (a -> st -> m st) -> st -> [a] -> m st
consumeSt _  st []     = return st
consumeSt mf st (a:as) = mf a st >>= \st' -> consumeSt mf st' as  

putPulse1 :: (SnocWriterM m, DiffElem m ~ Pulse e) => e -> m ()
putPulse1 a = snoc $ Pulse a


putPulsation :: (SnocWriterM m, ExtBeam e, DiffElem m ~ Pulse e) 
             => [e] -> m ()
putPulsation xs = let (headsingles, body, tailsingles) = dblbreak extBeam xs 
                  in do { mapM_ putPulse1 headsingles
                        ; step body
                        ; mapM_ putPulse1 tailsingles }
  where
    step []               = return ()
    step [a]              = snoc $ Pulse a
    step as               = snoc $ BeamedL as


toPulse :: [a] -> Pulse a
toPulse []  = error "Bracket.toPulse: cannot build Pulse from empty list"
toPulse [x] = Pulse x
toPulse xs  = BeamedL xs

dblbreak :: (a -> Bool) -> [a] -> ([a],[a],[a])
dblbreak p xs = (as,reverse sb,reverse sc) where
  (as,bs)           = break p xs
  (sc,sb)           = break p (reverse bs)




-- reduce the MeterPattern stack by the duration
consume :: (Num a, Ord a) => a -> [a] -> [a]
consume _ []                  = []
consume n (x:xs) | n < x      = (x-n):xs
                 | n == x     = xs
                 | otherwise  = consume (n-x) xs


ratDuration :: HasDuration t => t Duration -> Rational
ratDuration = extent . getDuration                      


--------------------------------------------------------------------------------
-- overlay

overlayPhrases :: HasDuration t 
        => Phrase (t Duration) -> Phrase (t Duration) -> Phrase (t Duration)
overlayPhrases = longZip

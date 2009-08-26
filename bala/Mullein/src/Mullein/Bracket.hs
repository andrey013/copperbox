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

import Data.List ( foldl' )
import Data.Ratio


--------------------------------------------------------------------------------

-- /Extremity Beam/ certain gylphs (rests) should not be the 
-- start or end of a beam group.

class ExtBeam a where
  outerElement :: a -> Bool

instance ExtBeam (Glyph pch dur) where
  outerElement (Note _ _)     = True
  outerElement (Rest _)       = False
  outerElement (Spacer _)     = True     -- Note - is this correct? 
  outerElement (Chord _ _)    = True
  outerElement (GraceNotes _) = False
  outerElement Tie            = False

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

data Status e = Fits e          -- Fits in the current beam group
              | Completes e     -- Completes the current beam group
              | Breaks e        -- Spans current and next beam group



twinStatus :: HasDuration t => MeterPattern -> [t Duration] -> [Status (t Duration)]
twinStatus = fst `oo` anaMap step where
  step e []         = Just (Breaks e, [])
  step e stk@(a:as) = fn (ratDuration e) where
    fn n | n >= 1%4   = Just (Breaks e, consume n stk)
         | n >  a     = Just (Breaks e, consume n stk) 
         | n == a     = Just (Completes e, as)
         | otherwise  = Just (Fits e, (a-n):as)


-- The current state of beaming is represented by a DList of 
-- processed pulses (DList provides snoc-ing), and a reversed 
-- list of the current beam group.
type BeamAcc t = (D.DList (Pulse (t Duration)), [t Duration])

beamM :: (HasDuration t, ExtBeam (t Duration), Monad m)
      => MeterPattern -> [t Duration] -> m [Pulse (t Duration)]
beamM mp notes = return (beam2 mp notes)

beam2 :: (HasDuration t, ExtBeam (t Duration))
      => MeterPattern -> [t Duration] -> [Pulse (t Duration)]
beam2 mp notes = (D.toList . fst . popBeam) $ 
    foldl' beamStep (D.empty,[]) $ twinStatus mp notes
 
beamStep :: ExtBeam (t Duration) 
         => BeamAcc t -> Status (t Duration) -> BeamAcc t
beamStep acc (Fits e)
    | withinGroup acc         = pushIntoBeam e acc 
    | outerElement e          = pushIntoBeam e acc
    | otherwise               = snocPulse1 e acc
beamStep acc (Completes e)    = popBeam $ pushIntoBeam e acc
beamStep acc (Breaks e)       = snocPulse1 e acc 

withinGroup :: BeamAcc t -> Bool
withinGroup (_,cca) = null cca

pushIntoBeam :: t Duration -> BeamAcc t -> BeamAcc t
pushIntoBeam e (dl,cca) = (dl,e:cca)

popBeam :: ExtBeam (t Duration) => BeamAcc t -> BeamAcc t
popBeam (dl,[])  = (dl,[])
popBeam (dl,cca) = ((dl `addBeam` body) `addSingles` tailsingles, [])
  where
    addBeam dlist []  = dlist
    addBeam dlist [a] = dlist `D.snoc` (Pulse a)
    addBeam dlist xs  = dlist `D.snoc` (BeamedL xs)

    addSingles = foldl' (\a e -> a `D.snoc` (Pulse e)) 
    
    (tailsingles, body) = prod reverse reverse $ break outerElement cca

snocPulse1 :: ExtBeam (t Duration) => t Duration -> BeamAcc t -> BeamAcc t
snocPulse1 e acc = (dl `D.snoc` (Pulse e), [])
  where (dl,_) = popBeam acc    -- popBeam always leaves righthand stack empty! 


{-

beamM' :: (HasDuration t, Monad m, ExtBeam (t Duration)) 
      => MeterPattern -> [t Duration] -> m [Pulse (t Duration)]
beamM' mp notes = runSnocWriterT (consumeSt beamStepM [] $ twinStatus mp notes) >>= fn
  where
    fn ([],dlist)  = return $ D.toList dlist
    fn (cca,dlist) = return $ D.toList $ dlist `D.snoc` (toPulse $ reverse cca)

    -- Problem - the /cca/ in the flush doesn't respect outerElement-ing, 
    -- i.e. it might start or finish with a rest. This needs more 
    -- thought.


beamStepM :: (HasDuration t, SnocWriterM m, ExtBeam (t Duration), 
              DiffElem m ~ Pulse (t Duration))
          => (Status (t Duration))
          -> [t Duration]
          -> m [t Duration]
beamStepM (Fits e)      cca = return (e:cca) 
beamStepM (Completes e) cca = putPulsation (reverse $ e:cca) >> return []
beamStepM (Breaks e)    cca = putPulsation (reverse cca)     >> putPulse1 e >> return []


putPulse1 :: (SnocWriterM m, DiffElem m ~ Pulse e) => e -> m ()
putPulse1 a = snoc $ Pulse a


putPulsation :: (SnocWriterM m, ExtBeam e, DiffElem m ~ Pulse e) 
             => [e] -> m ()
putPulsation xs = let (headsingles, body, tailsingles) = dblbreak outerElement xs 
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

-}


-- reduce the MeterPattern stack by the duration
consume :: (Num a, Ord a) => a -> [a] -> [a]
consume _ []                  = []
consume n (x:xs) | n < x      = (x-n):xs
                 | n == x     = xs
                 | otherwise  = consume (n-x) xs

                                            
-- Run a stateful computation over a list, return the final state
consumeSt :: Monad m => (a -> st -> m st) -> st -> [a] -> m st
consumeSt _  st []     = return st
consumeSt mf st (a:as) = mf a st >>= \st' -> consumeSt mf st' as  


ratDuration :: HasDuration t => t Duration -> Rational
ratDuration = extent . getDuration                      


--------------------------------------------------------------------------------
-- overlay

overlayPhrases :: HasDuration t 
        => Phrase (t Duration) -> Phrase (t Duration) -> Phrase (t Duration)
overlayPhrases = longZip

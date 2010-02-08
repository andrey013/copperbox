{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  M2.Bracket
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bar splitting and beam grouping.
--
--------------------------------------------------------------------------------

-- ARGH - I don't like this code any more.... (and I wrote it!)

module M2.Bracket 
  ( 
    NumMeasured(..)
  , BeamExtremity(..)
  , beamSegment

  ) where

import M2.Datatypes
import M2.Duration
import M2.OneList

import Data.Ratio


-- This is the Measured class from the FingerTree paper and 
-- library but with Num for the superclass rather than Monoid

class Num (Measurement a) => NumMeasured a where
  type Measurement a
  nmeasure :: a -> Measurement a

class BeamExtremity a where 
   outerElement :: a -> Bool
   innerOnly    :: a -> Bool
   
   innerOnly    = not . outerElement



-- cexpression :: BeamExtremity a => [OneMany a] -> CExpr a

beamSegment :: (Measurement a ~ DurationMeasure, NumMeasured a) 
            => [DurationMeasure] -> [a] -> ([OneMany a],[a])
beamSegment meter_pats xs = 
    beamingAUnfold bufferReduce carryBorrow beamSegStep1 meter_pats 0 xs


bufferReduce :: [a] -> Maybe (OneMany a)
bufferReduce [] = Nothing
bufferReduce xs = Just $ fromList xs


carryBorrow :: MeterPattern
            -> DurationMeasure 
            -> Maybe (MeterPattern,DurationMeasure)
carryBorrow mp dx | dx == 0   = post mp
                  | carry dx  = post $ dx:mp
                  | otherwise = post $ step (abs dx) mp 
   where
     post []                     = Nothing 
     post (x:xs)                 = Just (xs,x)
     
     step _   []                 = []
     step bor (x:xs) | x >  bor  = (x-bor):xs
                     | x == bor  = xs
                     | otherwise = step (bor-x) xs
 

carry :: DurationMeasure -> Bool
carry = (>=0)

-- borrow :: DurationMeasure -> Bool
-- borrow = (<0)


beamSegStep1 :: (Measurement a ~ DurationMeasure, NumMeasured a) 
             => a -> DurationMeasure -> BStep a (OneMany a) DurationMeasure
beamSegStep1 _ v | v <= 0 = BDone v
beamSegStep1 x v          = step $ nmeasure x where
  step dx | dx >= 1%4 = BYield (one x) (v-dx)
          | otherwise = BPush  x       (v-dx)




--------------------------------------------------------------------------------


data BStep interim ans st = BYield ans      !st
                          | BPush  interim  !st 
                          | BDone           !st
  deriving (Eq,Show)



-- FIFO - Hughes representation of stacks/lists for efficient 
-- snoc-ing

type FIFO a = [a] -> [a]       

beamingAUnfold :: forall outer_state inner_state a interim ans.
                  ([interim] -> Maybe ans) 
               -> (outer_state -> inner_state -> Maybe (outer_state,inner_state))
               -> (a -> inner_state -> BStep interim ans inner_state) 
               -> outer_state -> inner_state -> [a] -> ([ans],[a])
beamingAUnfold buf_reduce next_state phi outer_st inner_st xs0 = 
    outer outer_st inner_st xs0
  where
    outer :: outer_state -> inner_state -> [a] -> ([ans],[a])
    outer _   _   []  = ([],[])
    outer out inn xs  = case next_state out inn of
        Nothing          -> ([],xs)
        Just (out',inn') -> inner out' inn' fifoEmpty xs

    inner :: outer_state -> inner_state -> FIFO interim -> [a] -> ([ans],[a])
    inner _   _   stk []     = (terminate stk,[])
    inner out inn stk (x:xs) = case phi x inn of
        BYield a st' -> (mbCons pref (a:as'), xs') 
                        where pref      = buf_reduce $ unFIFO stk
                              (as',xs') = inner out st' fifoEmpty xs
        BPush  b st' -> inner out st' (fifoSnoc stk b) xs
        BDone    st' -> (mbCons pref as', xs') 
                        where pref      = buf_reduce $ unFIFO stk
                              (as',xs') = outer out st' (x:xs)

    terminate :: FIFO interim -> [ans]
    terminate stk       = maybe [] return $ buf_reduce (unFIFO stk)


fifoEmpty           :: FIFO a
fifoEmpty           = id 

fifoSnoc            :: FIFO a -> a -> FIFO a
fifoSnoc f a        = f . (a:)           


unFIFO              :: FIFO a -> [a]
unFIFO              = ($ [])

mbCons :: Maybe a -> [a] -> [a]
mbCons Nothing  = id
mbCons (Just a) = (a:)



{-
--------------------------------------------------------------------------------

-- | /Extremity Beam/ - certain gylphs (rests) should not be the 
-- start or end of a beam group.

class ExtBeam a where
  outerElement :: a -> Bool

instance ExtBeam (Glyph anno pch dur) where
  outerElement (Note _ _ _ _) = True
  outerElement (Rest _)       = False
  outerElement (Spacer _)     = True     -- Note - is this correct? 
  outerElement (Chord _ _ _)  = True
  outerElement (GraceNotes _) = False

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

-- | Partition a notelist into bars and pulsations. A pulsation 
-- is either a single notes or a group of notes joined with a 
-- beam. Pulsations illustrate the division of the bar into beats.
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


-- | Partition into bars only (no grouping into pulsations). Each 
-- note or rest will effectively be a single pulse regardless of 
-- its duration.
phraseNoPulses :: HasDuration t => Rational -> [t Duration] -> Phrase (t Duration)
phraseNoPulses barlen notes = runId $ 
  barM barlen notes >>= mapM (\es -> mapM (return . Pulse) es >>= return . Bar)



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


type BeamAcc t = [t Duration]

beamM :: (HasDuration t, ExtBeam (t Duration), Monad m)
      => MeterPattern -> [t Duration] -> m [Pulse (t Duration)]
beamM mp notes = (return . D.toList . snd) =<< runSnocWriterT mf where
  xs = twinStatus mp notes
  mf = foldlM beamStep [] xs >>= popBeam
 
beamStep :: (ExtBeam (t Duration), SnocWriterM m, DiffElem m ~ Pulse (t Duration))
         => BeamAcc t -> Status (t Duration) -> m (BeamAcc t)
beamStep acc (Fits e)
    | withinGroup acc         = pushIntoBeam e acc 
    | outerElement e          = pushIntoBeam e acc
    | otherwise               = snocPulse1 e acc
beamStep acc (Completes e)    = pushIntoBeam e acc >>= popBeam
beamStep acc (Breaks e)       = snocPulse1 e acc 

withinGroup :: BeamAcc t -> Bool
withinGroup cca = not $ null cca

pushIntoBeam :: Monad m => t Duration -> BeamAcc t -> m (BeamAcc t)
pushIntoBeam e cca = return $ e:cca

popBeam :: (ExtBeam (t Duration), SnocWriterM m, 
            DiffElem m ~ Pulse (t Duration)) 
        => BeamAcc t -> m (BeamAcc t)
popBeam []  = return []
popBeam cca = addBeam body >> addSingles tailsingles >> return []
  where
    addBeam []  = return ()
    addBeam [a] = snoc $ Pulse a
    addBeam xs  = snoc $ BeamedL xs

    addSingles = mapM_ (snoc . Pulse) 
    
    (tailsingles, body) = prod reverse reverse $ break outerElement cca

snocPulse1 :: (ExtBeam (t Duration), SnocWriterM m, DiffElem m ~ Pulse (t Duration)) 
           => t Duration -> BeamAcc t -> m (BeamAcc t)
snocPulse1 e acc = popBeam acc >> snoc (Pulse e) >> return []



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


-- | Overlay a list of note-lists after phrasing them with the 
-- same meter pattern.
overlayNoteLists :: (HasDuration t, ExtBeam (t Duration) ) 
         => MeterPattern -> [[t Duration]] -> Phrase (t Duration)
overlayNoteLists mp = foldl1' overlayPhrases . map (phrase mp) 


-}
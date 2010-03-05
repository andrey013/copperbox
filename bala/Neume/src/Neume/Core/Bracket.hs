{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Bracket
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

module Neume.Core.Bracket 
  ( 
    phrase

  ) where

import Neume.Core.BeamExtremity
import Neume.Core.Datatypes
import Neume.Core.Duration
import Neume.Core.SyntaxStaff
import Neume.Core.TreeOps
import Neume.Core.Utils.Common

import Text.PrettyPrint.Leijen hiding ( empty )   -- package: wl-pprint

import Data.Foldable ( toList )
import Data.Ratio
import Data.Sequence hiding ( splitAt, length )
import Prelude hiding ( null )


   


-- | A Metric unit represents a division of a NoteList according
-- to the sizes of a 'MeterPattern'.
--
-- Where there is not an exact fit, the splitting algorithm will
-- borrow from the input list an extra note. This potentially 
-- means the next metrical unit might have been 'all borrowed', 
-- example a whole note in 4/4 time with meter pattern [2,2] 
-- gives:
-- 
-- > [ MUnit "wholenote", BZero ]
--
--
data MetricUnit a = MUnit (NoteList a)
                  | BZero                  -- Borrowed zero
 deriving (Eq,Show)




--------------------------------------------------------------------------------
-- 

phrase :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
       => MeterPattern -> NoteList a -> StaffPhrase a
phrase mp notes = StaffPhrase $ barsFromDivisions (length mp) $ divisions 0 mp notes


barsFromDivisions :: ( Measurement a ~ DurationMeasure
                     , NumMeasured a, BeamExtremity a) 
                  => Int -> [MetricUnit a] -> [StaffBar a]
barsFromDivisions i = step . splitAt i
  where
    step (xs,[]) = [unitsToBar xs]
    step (xs,ys) = (unitsToBar xs) : step (splitAt i ys)

    fromUnit BZero         = CExprList []
    fromUnit (MUnit notes) = beam notes

    unitsToBar = StaffBar . catExprLists . map fromUnit

catExprLists :: [CExprList a] -> CExprList a
catExprLists = CExprList . step where
  step []     = []
  step (x:xs) = case getCExprList x of 
                  [] -> step xs 
                  e  -> e ++ step xs

--------------------------------------------------------------------------------
-- Cycle the MetricalSpec / MeterPattern

-- | 'cyclePattern' : anacrusis * meter_pattern -> meter_pattern
--
-- Cycle the MeterPattern producing an infinite list.
-- 
-- The anacrusis is the length of the prefix - negative values 
-- are ignored.
--
cyclePattern :: Rational -> MeterPattern -> MeterPattern
cyclePattern ana mp | ana <= 0  = cycle mp
                    | otherwise = let len = sum mp
                                      a2  = len - (ana `modR` len)
                                  in sminus a2 (cycle mp)








--------------------------------------------------------------------------------
-- Stack (meter pattern) addition and subtraction

-- | sminus subtracts from the top of stack.
--
-- If the number to be subtracted is greater than the top of the
-- stack the top of the stack is popped and the remainder is 
-- subtracted from the new stack top (the stack will never 
-- contain negative numbers).
-- 
--

sminus :: (Num a, Ord a) => a -> [a] -> [a]
sminus a       xs     | a < 0   = splus (abs a) xs
sminus _       []               = []
sminus a       (x:xs)           = step (x-a) xs where
  step r ys      | r > 0     = r:ys
  step r (y:ys)  | r < 0     = step (y - abs r) ys
  step _ ys                  = ys                   -- empty stack or r==0
  

-- | splus always conses the scalar (unless it is negative, which 
-- is treated as stack-minus).
--    
splus :: (Num a, Ord a) => a -> [a] -> [a]
splus a xs | a > 0     = a:xs
           | a < 0     = sminus (abs a) xs
           | otherwise = xs


--------------------------------------------------------------------------------
-- Divide the NoteList

divisions :: (Measurement a ~ DurationMeasure, NumMeasured a) 
          => Rational -> MeterPattern -> NoteList a -> [MetricUnit a]
divisions ana mp notes = step 0 (cyclePattern ana mp) (getNoteList notes) where
  
  -- borrow greater than next mertical unit => produce a BZero
  step borrow (d:ds) xs   | borrow >= d = BZero : step (borrow-d) ds xs
 
   -- input exhausted => produce empty list
  step _      _      []   = []
 
  -- 'normal' operation => use division1 to produce a MUnit
  step borrow (d:ds) xs   = let (one,borrow',rest) = division1 (d-borrow) xs
                            in one : step borrow' ds rest

  -- unreachable as meter patterns have been cycled
  step _       []     _    = error "Bracket.divisions - unreachable"

                            
-- | Extract one MeticUnit from the input list, return the unit, 
-- the carry, and the rest-of-input.
--
division1 :: (Measurement a ~ DurationMeasure, NumMeasured a)  
          => DurationMeasure
          -> [PletTree a] 
          -> (MetricUnit a, DurationMeasure, [PletTree a])
division1 runit = post . unwind phi runit where
  phi r a | r > 0       = Yield a (r - pletMeasure a)
          | otherwise   = Done    

  post (xs,borrow,rest) = (MUnit $ NoteList xs, abs borrow, rest)


-- | unfold against a list, presenting the rest-of-list and the 
-- state at the end

-- Strict version of Maybe for unfolding...

data Step a st = Yield a  !st
               | Done     
  deriving (Eq,Show)

unwind :: (st -> a -> Step b st) -> st -> [a] -> ([b],st,[a])
unwind phi s0 = step id s0 where
  step k st (x:xs) = case phi st x of
                       Yield a st' -> step (k . (a:)) st' xs
                       Done        -> (k [],st,x:xs)
  step k st []     = (k [],st,[])  


--------------------------------------------------------------------------------


beam :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
     => NoteList a -> CExprList a
beam = CExprList . beamNotes . getNoteList 

beamNotes :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
     => [PletTree a] -> [CExpr a]
beamNotes = alternateUnwind out inn unbuffer
  where
    out a@(S e) 
       | pletMeasure a < quarter_note && rendersToNote e  = Nothing
    out a@(Plet _ _ _) 
       | pletAll ((<quarter_note) . nmeasure) a           = Nothing
    out pt                                                = Just (conv pt)
    
       
    inn a | pletAll ((<quarter_note) . nmeasure) a        = Just (conv a)
          | otherwise                                     = Nothing




conv :: PletTree a -> CExpr a
conv (S a)            = Atom a
conv (Plet p q notes) = N_Plet desc (CExprList $ map conv $ getNoteList notes)
  where desc = N_PletDescr p q



unbuffer :: BeamExtremity a => Seq (CExpr a) -> [CExpr a]
unbuffer = step [] . viewr 
  where
    step acc EmptyR     = acc
    step acc (se :> Atom a)  
        | rendersToNote a && null se = Atom a : acc
        | rendersToNote a            = (mkbeam $ toList $ se |> Atom a) : acc
        | otherwise                  = step (Atom a : acc) (viewr se)  

    -- if the right edge is a n_plet always beam
    step acc (se :> bt)              = (mkbeam $ toList $ se |> bt) : acc

    mkbeam = Beamed . CExprList


alternateUnwind :: (a -> Maybe b) 
                -> (a -> Maybe interim) 
                -> (Seq interim -> [b]) 
                -> [a] -> [b]
alternateUnwind outside inside flush_buffer inp = outstep inp 
  where
    outstep []        = []
    outstep (x:xs)    = case outside x of
                         Just a  -> a : outstep xs
                         Nothing -> instep empty (x:xs)
    
    instep buf []     = flush_buffer buf
    instep buf (x:xs) = case inside x of
                          Just a  -> instep (buf |> a) xs
                          Nothing -> flush_buffer buf ++ outstep (x:xs)



--------------------------------------------------------------------------------
-- Pretty instances

instance Pretty a => Pretty (MetricUnit a) where
  pretty (MUnit notes) = angles $ pretty notes
  pretty BZero         = text "___" 




--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.MusicRep.Motivic
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Motivic development
--
--------------------------------------------------------------------------------


module Bala.MusicRep.Motivic where

import Bala.Base.BaseExtra
import Bala.Base.Duration
import Bala.Base.Pitch
import Bala.Base.FocusedShapeContents
import Bala.Base.Structural

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import qualified Data.Traversable as T


  
-- double the duration of each element in a sequence
augmentation :: (T.Traversable t, RhythmicValue a) => t a -> t a 
augmentation = fmap fn where
    fn :: RhythmicValue a => a -> a
    fn a = modifyDuration a (2 * rhythmicValue a)

-- halve the duration of each element in a sequence
diminution :: (T.Traversable t, RhythmicValue a) => t a -> t a 
diminution = fmap fn where
    fn :: RhythmicValue a => a -> a
    fn a = modifyDuration a ((rhythmicValue a) / 2)
  
  
pitchFocus :: Focus Elt Pitch
pitchFocus = FN { focus = pitched, extract = pitchof, putback = modifyPitch}
  where
    pitched = isJust . pitchValue
    pitchof = fromJust . pitchValue

-- For the moment, the motivic functions have very general types, although
-- I've only thought about how the work on motifs. I must look at how they 
-- work on sections and phrases, and potentially like them to just motifs.
  
                                 
-- reverse the pitch values of a sequence, rests etc stay in the same place
retrograde :: T.Traversable t => t Elt -> t Elt 
retrograde = rejoin pitchFocus . second reverse . separate pitchFocus


-- This gives a 'arithmetic' answer easily produces pitches outside the scale.

arithmeticInversion :: T.Traversable t => t Elt -> t Elt 
arithmeticInversion = rejoin pitchFocus . second invert . separate pitchFocus where
    invert :: [Pitch] -> [Pitch]
    invert xs = let maxmin = maximum xs + minimum xs in fmap (maxmin -) xs
  
--- This one makes a map of pitches instead...
inversion :: T.Traversable t => t Elt -> t Elt 
inversion = rejoin pitchFocus . second invertByRemap . separate pitchFocus
  where
    invertByRemap :: [Pitch] -> [Pitch]
    invertByRemap xs = fmap (findinMap cm) xs where
        cm = crossMap xs
        findinMap m p = case Map.lookup p m of
                          Just pch -> pch
                          Nothing -> error $ "bad map ... " ++ show m
    
    crossMap :: Ord a => [a] -> Map.Map a a
    crossMap zs = let xs = nub $ sort zs in addto Map.empty xs (reverse xs)
      where 
        addto m (x:xs) (y:ys) = addto (Map.insert x y m) xs ys
        addto m []     []     = m
        addto _ _      _      = error "unreachable - crossMap!"
    
     

  

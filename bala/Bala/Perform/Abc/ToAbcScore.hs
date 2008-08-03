
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Abc.ToAbcScore
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Render an (general) Score to an Abc-tailored representation, 
-- that is easier to emit Abc from.
--
--------------------------------------------------------------------------------


module Bala.Perform.Abc.ToAbcScore where

import Bala.Format.Score
import Bala.Perform.Base.OnsetQueue
import Bala.Perform.Abc.AbcScoreDatatypes
import Bala.Perform.Score.MeasureOnsets
import Bala.Perform.Score.Utils

import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Sequence

abcscore :: ScScore pch dur -> AbcScTuneBook pch dur
abcscore (ScScore se) = AbcScTuneBook $ F.foldl fn mempty se 
  where fn acc e = acc |> tune e
  
tune :: ScPart pch dur -> AbcScTune pch dur
tune p@(ScPart i _ _) = AbcScTune i (polycat $ deriveQueue p)


-- Folding takes us too far into the OnsetQueue evt rather than [evt]
-- So we have to use direct recursion
-- TODO - build bigger segments if the pattern of the voices stays the same 
polycat :: OnsetQueue (OnsetMeasure pch dur) -> AbcScLine pch dur
polycat = rec mempty . viewH
  where
    rec acc EmptyQ          = acc
    rec acc ((_,es) :>> q)  = rec (acc |*> cnstr es) (viewH q) 
    
    
    
    cnstr :: [OnsetMeasure pch dur] -> Maybe (AbcScPolyPhrase pch dur)
    cnstr = polyPhrase . map measure
    
    
polyPhrase :: [AbcScMeasure pch dur] -> Maybe (AbcScPolyPhrase pch dur)
polyPhrase []   = Nothing
polyPhrase [x]  = Just $ AbcScSingletonPhrase x
polyPhrase xs   = Just $ AbcScPolyPhrase xs



measure :: OnsetMeasure pch dur -> AbcScMeasure pch dur
measure (OnsetMeasure i voice se) = 
  AbcScMeasure i voice (fmap glyph (normalizeGroupedElements se))
  
  
glyph :: ScGlyph pch dur -> AbcScGlyph pch dur
glyph (ScNote scp dur)          = AbcScNote (pitch scp) dur
glyph (ScRest dur)              = AbcScRest dur
glyph (ScSpacer dur)            = AbcScSpacer dur
glyph (ScGroup ScChord xs)      = AbcScChord (catMaybes $ map justNote xs)
glyph (ScGroup ScGraceNotes xs) = AbcScGraceNotes (catMaybes $ map justNote xs)



justNote :: ScGlyph pch dur -> Maybe (AbcScGlyph pch dur)                     
justNote (ScNote scp dur)   = Just $ AbcScNote (pitch scp) dur                   
justNote _                  = Nothing   

              
pitch :: ScPitch pch -> pch
pitch (ScPitch pch) = pch


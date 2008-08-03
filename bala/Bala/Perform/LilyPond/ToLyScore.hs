{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.LilyPond.ToLyScore
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Render an (general) Score to a LilyPond-tailored representation, 
-- that is easier to emit LilyPond from.
--
--------------------------------------------------------------------------------

module Bala.Perform.LilyPond.ToLyScore where

import Bala.Format.Score
import Bala.Perform.Base.OnsetQueue
import Bala.Perform.LilyPond.LyScoreDatatypes
import Bala.Perform.Score.MeasureOnsets
import Bala.Perform.Score.Utils

import qualified Data.Foldable as F 
import qualified Data.IntMap as IM
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Sequence
import Prelude hiding (null)



instance OnsetEvent (LyScMeasure pch dur) (LyScMeasure pch dur) where
  onset m@(LyScMeasure i _ _) = (i, m)

lyscore :: ScScore pch dur -> LyScScore pch dur
lyscore (ScScore se) = LyScScore $ F.foldl fn mempty se 
  where fn acc e = acc |> part e
    
part :: ScPart pch dur -> LyScPart pch dur
part p@(ScPart i _ _) = LyScPart i (polycat $ deriveQueue p)


-- Folding takes us too far into the OnsetQueue to 'evt' rather than '[evt]'.
-- So we have to use direct recursion
-- TODO - build bigger segments if the pattern of the voices stays the same 
polycat :: OnsetQueue (OnsetMeasure pch dur) -> LyScLine pch dur
polycat = rec mempty . viewH
  where
    rec acc EmptyQ          = acc
    rec acc ((_,es) :>> q)  = rec (acc |*> trans es) (viewH q) 
        
    trans :: [OnsetMeasure pch dur] -> Maybe (LyScPolyPhrase pch dur)
    trans = polyPhrase . map (LyScSegment . singleton . measure)
    
    
polyPhrase :: [LyScSegment pch dur] -> Maybe (LyScPolyPhrase pch dur)
polyPhrase []   = Nothing
polyPhrase [x]  = Just $ LyScSingletonPhrase x
polyPhrase xs   = Just $ LyScPolyPhrase xs
    

measure :: OnsetMeasure pch dur -> LyScMeasure pch dur
measure (OnsetMeasure i voice se) = 
  LyScMeasure i voice (fmap glyph (normalizeGroupedElements se))


glyph :: ScGlyph pch dur -> LyScGlyph pch dur
glyph (ScNote scp dur)          = LyScNote (pitch scp) dur
glyph (ScRest dur)              = LyScRest dur
glyph (ScSpacer dur)            = LyScSpacer dur
glyph (ScGroup ScChord xs)      = LyScChord (catMaybes $ map justNote xs)
glyph (ScGroup ScGraceNotes xs) = LyScGraceNotes (catMaybes $ map justNote xs)



justNote :: ScGlyph pch dur -> Maybe (LyScGlyph pch dur)                     
justNote (ScNote scp dur)   = Just $ LyScNote (pitch scp) dur                   
justNote _                  = Nothing   

              
pitch :: ScPitch pch -> pch
pitch (ScPitch pch) = pch


 
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Score.ToPolyScore
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Render an Score (which has polyphony as references) to a PolyScore 
-- which has linear polyphony.
--
--------------------------------------------------------------------------------

module Bala.Perform.Score.ToPolyScore where

import Bala.Format.Score
import Bala.Format.Score.PolyDatatypes
import Bala.Perform.Base.OnsetQueue

import qualified Data.Foldable as F 
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Sequence



instance OnsetEvent (PScMeasure pch dur) (PScMeasure pch dur) where
  onset m@(PScMeasure i _ _) = (i, m)


deriveQueue :: ScPart pch dur -> OnsetQueue (PScMeasure pch dur)
deriveQueue p = 
  (buildQueue $ primaryLine p) `mappend` (buildQueue $ linearizedRefs p)
  

primaryLine :: ScPart pch dur -> Seq (PScMeasure pch dur)
primaryLine = line 0 . sc_part_primary_line 

    
linearizedRefs ::  ScPart pch dur -> Seq (PScMeasure pch dur)  
linearizedRefs = F.foldl fn mempty . extract 
  where
    extract = IM.toAscList . getPolyRefs . sc_part_poly_refs 
    fn acc (v,ln) = acc >< line v ln
    
line :: Int -> ScLine pch dur -> Seq (PScMeasure pch dur) 
line voice = F.foldl fn mempty
  where
    fn acc e = acc |> measure voice e  

----


measure :: Int -> ScMeasure pch dur -> PScMeasure pch dur
measure voice (ScMeasure i _ sg) = PScMeasure i voice (fmap glyph sg)

groupType :: ScGroupType -> PScGroupType
groupType ScBeam        = PScBeam
groupType ScChord       = PScChord
groupType ScGraceNotes  = PScGraceNotes


glyph :: ScGlyph pch dur -> PScGlyph pch dur 
glyph (ScNote scp dur)          = PScNote (pitch scp) dur
glyph (ScRest dur)              = PScRest dur
glyph (ScSpacer dur)            = PScSpacer dur
glyph (ScGroup ty xs)           = PScGroup (groupType ty) (map glyph xs)

                     
pitch :: ScPitch pch -> PScPitch pch
pitch (ScPitch pch) = PScPitch pch


 
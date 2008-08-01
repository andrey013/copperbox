{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Score.ToLyScore
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

module Bala.Perform.LilyPond.ToLyScore where

import Bala.Format.Score
import Bala.Perform.Base.OnsetQueue
import Bala.Perform.LilyPond.LyScoreDatatypes
import Bala.Perform.Score.MeasureOnsets

import qualified Data.Foldable as F 
import qualified Data.IntMap as IM
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Sequence
import Prelude hiding (null)

infixl 5 |*>
(|*>) se Nothing  = se
(|*>) se (Just e) = se |> e

instance OnsetEvent (LyScMeasure pch dur) (LyScMeasure pch dur) where
  onset m@(LyScMeasure i _ _) = (i, m)

lyscore :: ScScore pch dur -> LyScScore pch dur
lyscore (ScScore se) = LyScScore $ F.foldl fn mempty se 
  where fn acc e = acc |> part e
    
part :: ScPart pch dur -> LyScPart pch dur
part p@(ScPart i _ _) = LyScPart i (polycat $ deriveQueue p)


-- Folding takes us too far into the OnsetQueue evt rather than [evt]
-- So we have to use direct recursion
-- TODO - build bigger segments if the pattern of the voices stays the same 
polycat :: OnsetQueue (OnsetMeasure pch dur) -> LyScLine pch dur
polycat = rec mempty . viewH
  where
    rec acc EmptyQ          = acc
    rec acc ((_,es) :>> q)  = rec (acc |*> cnstr es) (viewH q) 
    
    
    
    cnstr :: [OnsetMeasure pch dur] -> Maybe (LyScPolyPhrase pch dur)
    cnstr = polyPhrase . map (LyScSegment . singleton . measure)
    
    
polyPhrase :: [LyScSegment pch dur] -> Maybe (LyScPolyPhrase pch dur)
polyPhrase []   = Nothing
polyPhrase [x]  = Just $ LyScSingletonPhrase x
polyPhrase xs   = Just $ LyScPolyPhrase xs
    
{-
nubEmpty :: [LyScSegment pch dur] -> [LyScSegment pch dur]
nubEmpty = filter (not . null . getLyScSegment)

emptySegment :: LyScSegment pch dur -> Bool
emptySegment (LyScSegment se) 
    | null se       = True
    | otherwise     = F.foldl fn False se
  where
    fn b (LyScMeasure _ _ se) = b && (not $ null se) 
-}




 

----


measure :: OnsetMeasure pch dur -> LyScMeasure pch dur
measure (OnsetMeasure i voice se) = 
  LyScMeasure i voice (fmap glyph (removeBeams se))



-- Concat toplevel beams, drop nested beams.
-- Sematically nested beams are meaningless (but the datatypes allow them).
removeBeams :: Seq (ScGlyph pch dur) -> Seq (ScGlyph pch dur)
removeBeams = F.foldl fn mempty 
  where
    fn se (ScGroup ScBeam xs) = se >< beamElts xs
    fn se e                   = se |> e
    
    -- don't allow nested beams - drop them
    beamElts = fromList . filter notBeam
    notBeam (ScGroup ScBeam _)  = False
    notBeam _                   = True

glyph :: ScGlyph pch dur -> LyScGlyph pch dur
glyph (ScNote scp dur)          = LyScNote (pitch scp) dur
glyph (ScRest dur)              = LyScRest dur
glyph (ScSpacer dur)            = LyScSpacer dur
glyph (ScGroup ScChord xs)      = LyScChord (catMaybes $ map justNote xs)
glyph (ScGroup ScGraceNotes xs) = LyScGraceNotes (catMaybes $ map justNote xs)
glyph (ScGroup ScBeam _)        = error "glyph - beam where it shouldn't be" 


justNote :: ScGlyph pch dur -> Maybe (LyScGlyph pch dur)                     
justNote (ScNote scp dur)   = Just $ LyScNote (pitch scp) dur                   
justNote _                  = Nothing   

              
pitch :: ScPitch pch -> pch
pitch (ScPitch pch) = pch


 
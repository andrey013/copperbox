--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Score.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utility functions to work on Score rep.
--
--------------------------------------------------------------------------------

module Bala.Perform.Score.Utils (
  (|*>),
  removeBeams,
  normalizeGroupedElements
  ) where

import Bala.Format.Score

import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence


infixl 5 |*>
(|*>) se Nothing  = se
(|*>) se (Just e) = se |> e



-- Concat toplevel beams, drop nested beams.
-- Sematically nested beams are meaningless (but the datatypes allow them).
removeBeams :: Seq (ScGlyph pch dur) -> Seq (ScGlyph pch dur)
removeBeams = F.foldl fn mempty 
  where
    fn se (ScGroup ScBeam xs) = se >< beamElts xs
    fn se e                   = se |> e
    
    -- don't allow nested beams - drop them
    beamElts = fromList . filter (not . isBeam)


normalizeGroupedElements :: Seq (ScGlyph pch dur) -> Seq (ScGlyph pch dur)
normalizeGroupedElements = F.foldl fn mempty 
  where
    fn se (ScGroup ScChord xs)        = se |> ScGroup ScChord (notes xs)
    fn se (ScGroup ScBeam xs)         = se |> ScGroup ScBeam  (notes xs)
    fn se (ScGroup ScGraceNotes xs)   = se |> ScGroup ScGraceNotes (singles xs)
    fn se e                           = se |> e
    
    
    notes = filter isNote
    
    singles = filter (not . isGroup) 
     
isNote :: ScGlyph pch dur -> Bool
isNote (ScNote _ _) = True
isNote _            = False   

isGroup :: ScGlyph pch dur -> Bool
isGroup (ScGroup _ _) = True
isGroup _             = False

isBeam :: ScGlyph pch dur -> Bool
isBeam (ScGroup ScBeam _)  = True
isBeam _                   = False
    
    
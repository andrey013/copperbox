{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  BackendAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit Abc from Score representation.
--
--------------------------------------------------------------------------------

{-
module BackendAbc (
    Notate_Abc_Env(..), 
    default_abc_env,
    translateAbc
  ) where
-}

module BackendAbc where 

import CommonUtils
import Duration
import OutputUtils hiding (abcPitchLetter)
import Pitch
import ScoreRepresentation
import TextAbc
import Traversals

import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence
import qualified Text.PrettyPrint.Leijen as PP

newtype AbcExprs = AbcExprs { 
    getAbcExprs :: Seq AbcMusicLine
  }
  
type AbcMusicLine = AbcCxt_Body

-- type AbcGlyph     = Glyph AbcNote (Maybe AbcDuration)
type AbcGlyph     = Glyph Pitch (Maybe AbcDuration)

type AbcSystem    = ScSystem   AbcGlyph
type AbcStrata    = ScStrata   AbcGlyph
type AbcBlock     = ScBlock    AbcGlyph
type AbcMeasure   = ScMeasure  AbcGlyph

translateAbc :: ScoreSystem -> Duration -> AbcExprs
translateAbc sys default_duration =
    let (ScSystem se) = abcForm sys default_duration
    in AbcExprs $ F.foldl fn mempty se
  where
    fn se e = se |> outputStrata e 
    
abcForm :: ScoreSystem -> Duration -> AbcSystem
abcForm (ScSystem se) default_duration =
    ScSystem $ F.foldl fn mempty se  
  where
    fn se e = se |> translateDuration e default_duration

translateDuration :: ScStrata (Glyph pch Duration)
                      -> Duration
                      -> ScStrata (Glyph pch (Maybe AbcDuration))
translateDuration strata default_duration = 
    traversalEnv default_encode_Abc strata default_duration 

default_encode_Abc :: Glyph p Duration 
                   -> Duration 
                   -> Glyph p (Maybe AbcDuration)
default_encode_Abc e = \env -> 
    let drn = glyphDuration e
        od  = abcRelativeDuration env drn
    in changeDuration e od
    
    


outputStrata :: AbcStrata -> AbcMusicLine
outputStrata (ScStrata i se) = F.foldl outputBlock body se

outputBlock :: AbcMusicLine -> AbcBlock -> AbcMusicLine
outputBlock cxt (ScSingleBlock i se) = 
    let cxt' = outputMeasure cxt se in cxt' +++ barline

outputBlock cxt (ScPolyBlock i se) = 
    let voices = F.foldr (\e a -> outputMeasure body e : a) [] se
    in voiceOverlay cxt voices 

outputMeasure :: AbcMusicLine -> AbcMeasure -> AbcMusicLine
outputMeasure cxt (ScMeasure se) = F.foldl outputGlyph cxt se

outputGlyph :: AbcMusicLine -> AbcGlyph -> AbcMusicLine
outputGlyph cxt (GlyNote p od) = 
    cxt +++ (abcNote p *! od)
    
outputGlyph cxt (GlyRest od) = 
    cxt +++ (rest *! od) 
    
outputGlyph cxt (GlySpacer od) = 
    cxt +++ (spacer *! od)
    
outputGlyph cxt (GlyChord se od) = 
    cxt
    
outputGlyph cxt (GlyGraceNotes se) = 
    cxt

voiceOverlay :: AbcCxt_Body -> [AbcCxt_Body] -> AbcCxt_Body
voiceOverlay cxt []     = cxt
voiceOverlay cxt [v]    = cxt `mappend` v +++ barline
voiceOverlay cxt (v:vs) = voiceOverlay (cxt &\ v) vs 


abcPitchLetter   :: PitchLetter -> AbcPitchLetter
abcPitchLetter = toEnum . fromEnum

oabcAccidental :: Accidental -> Maybe AbcAccidental
oabcAccidental Nat            = Nothing
oabcAccidental Sharp          = Just sharp
oabcAccidental Flat           = Just flat
oabcAccidental DoubleSharp    = Just doubleSharp
oabcAccidental DoubleFlat     = Just doubleFlat


instance PP.Pretty AbcExprs where
  pretty (AbcExprs se) = F.foldl fn PP.empty se
    where fn a e = a PP.<$> PP.text "____" PP.<$> getAbc e
    
    
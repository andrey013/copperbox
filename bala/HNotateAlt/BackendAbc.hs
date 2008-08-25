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
    Notate_Abc_Env(..), default_abc_env,
    {- generateAbc -}
    translateAbc
  ) where
-}

module BackendAbc where 

import CommonUtils
import Duration
import NotateMonad
import TextAbc
import Pitch
import ScoreRepresentation



import Control.Applicative
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)
import qualified Data.Foldable as F
import Data.Monoid
import Data.Ratio
import Data.Sequence
import Data.Traversable
import Prelude hiding (mapM)
import qualified Text.PrettyPrint.Leijen as PP

newtype AbcExprs = AbcExprs { 
    getAbcExprs :: Seq AbcMusicLine
  }
  
type AbcMusicLine = AbcCxt_Body

instance PP.Pretty AbcExprs where
  pretty (AbcExprs se) = F.foldl fn PP.empty se
    where fn a e = a PP.<$> PP.text "____" PP.<$> getAbc e
    
    

type ProcessAbc a = NotateM Notate_Abc_State Notate_Abc_Env a

data Abc_PitchValue = Abc_PitchValue {
    abc_pitchletter :: AbcPitchLetter,
    abc_accidental   :: Maybe AbcAccidental,
    abc_octave       :: Maybe AbcOctave
  }
  
data Abc_DurationValue = Abc_DurationValue {
    abc_duration      :: Maybe AbcDuration
  }
  
type AbcGlyph = Glyph Abc_PitchValue Abc_DurationValue

                 
                 
type AbcSystem    = ScSystem  AbcGlyph
type AbcStrata    = ScStrata  AbcGlyph
type AbcBlock     = ScBlock   AbcGlyph
type AbcMeasure   = ScMeasure AbcGlyph



data Notate_Abc_State = Notate_Abc_State {
    abc_unknown_st      :: ()
  }
  deriving (Show)

data Notate_Abc_Env = Notate_Abc_Env {
    default_note_length      :: Duration
  }

state0 = Notate_Abc_State ()

default_abc_env :: Notate_Abc_Env
default_abc_env = Notate_Abc_Env {
    default_note_length        = semiquaver
  }

changeDuration :: Duration -> ProcessAbc Abc_DurationValue
changeDuration d = Abc_DurationValue <$> do 
    dnl   <- asks default_note_length
    if (d == dnl) 
      then return Nothing 
      else let scale = denominator (toRatio dnl)
               r     = toRatio d
               (nm,dm) = (numerator r, denominator r)
           in return $ Just $ dur ( nm*scale, dm)


translateAbc :: ScoreSystem -> Notate_Abc_Env -> AbcSystem
translateAbc (ScSystem se) env =
    ScSystem $ F.foldl fn mempty se
  where
    fn se e = se |> transStrata e env 

transStrata :: ScoreStrata -> Notate_Abc_Env -> AbcStrata
transStrata s env = evalNotate (unwrapMonad $ changeRep s) state0 env 

changeRep :: ScoreStrata
          -> WrappedMonad (NotateM Notate_Abc_State Notate_Abc_Env) AbcStrata
changeRep = traverse changeRepBody

changeRepBody :: ScoreGlyph
              -> WrappedMonad (NotateM Notate_Abc_State Notate_Abc_Env) AbcGlyph
changeRepBody g = WrapMonad $ changeGlyph g

changeGlyph :: ScoreGlyph -> ProcessAbc AbcGlyph 
changeGlyph (GlyNote p d)       = 
    GlyNote   <$> pure (changePitch p) <*> changeDuration d
    
changeGlyph (GlyRest d)         = GlyRest   <$> changeDuration d

changeGlyph (GlySpacer d)       = GlySpacer <$> changeDuration d

changeGlyph (GlyChord se d)     = 
    GlyChord <$> pure (fmap changePitch se) <*> changeDuration d

changeGlyph (GlyGraceNotes se)  = GlyGraceNotes <$> mapM fn se
  where fn (p,d) = (,) <$> pure (changePitch p) <*> changeDuration d
  
changePitch (Pitch l a o)     = 
    Abc_PitchValue (abcPitchLetter l) (oabcAccidental a) Nothing




abcPitchLetter   :: PitchLetter -> AbcPitchLetter
abcPitchLetter = toEnum . fromEnum

oabcAccidental :: Accidental -> Maybe AbcAccidental
oabcAccidental Nat            = Nothing
oabcAccidental Sharp          = Just sharp
oabcAccidental Flat           = Just flat
oabcAccidental DoubleSharp    = Just doubleSharp
oabcAccidental DoubleFlat     = Just doubleFlat
  
--------------------------------------------------------------------------------
-- pretty printing

abcppopt :: Maybe (Abc a) -> PP.Doc
abcppopt (Just a) = unwrap a
abcppopt Nothing  = PP.empty

instance PP.Pretty Abc_PitchValue where
  pretty (Abc_PitchValue l oa os) = 
      abcppopt oa PP.<> PP.pretty l PP.<> abcppopt os
  
instance PP.Pretty Abc_DurationValue where
  pretty (Abc_DurationValue od)   = abcppopt od

  
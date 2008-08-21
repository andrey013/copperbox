
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ScoreRepresentation2
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for Score format
--
-- Polyphony is indicated at the start of a measure by pointers to a
-- dictionary of lines (a sequence of measures). These lines may themselves
-- have measures that point to further polyphonic lines.
--
--------------------------------------------------------------------------------

module ScoreRepresentation (
    -- * Datatypes
    ScSystem(..),
    ScStrata(..),
    ScBlock(..),
    ScMeasure(..),
    ScGlyph(..),
    CommonGlyph(..),
    
    duration, dzero, dplus,
    
    -- * Aliases
    DSystem,
    DStrata,
    DBlock,
    DMeasure,
    DGlyph,
    
    dsystem,
    dstrata,
    dsingleBlock,
    dpolyBlock,
    dmeasure,
    dglyph    

  ) where

import CommonUtils (sepSeq)
import Duration
import Pitch

import qualified Control.Applicative as A
import qualified Data.Foldable as F
import Data.Sequence (Seq, ViewL(..), viewl)
import Data.Traversable
import Text.PrettyPrint.Leijen




newtype ScSystem e = ScSystem { getSystem :: Seq (ScStrata e) }


-- A 'horizontal line' of music - of course it maybe printed on 
-- more than one line and might contain polyphony, but it will be played 
-- by a single instrument.
data ScStrata e = ScStrata {
    _strata_number    :: Int,
    _strata_chunks    :: Seq (ScBlock e)
  }




-- Follow the Abc style when voice overlays are grouped measure-wise.
-- The Int holds the measure number
data ScBlock e = ScSingleBlock Int (ScMeasure e)
               | ScPolyBlock Int (Seq (ScMeasure e))


newtype ScMeasure e = ScMeasure { getMeasure :: Seq (ScGlyph e) }


-- have glyph open so we can have say, lyrics...
data ScGlyph e = ScGlyph e

--------------------------------------------------------------------------------
-- Functor instances

instance Functor ScSystem where
  fmap f (ScSystem se)        = ScSystem (fmap (fmap f) se)
  
instance Functor ScStrata where
  fmap f (ScStrata i se)      = ScStrata i (fmap (fmap f) se)
  
instance Functor ScBlock where
  fmap f (ScSingleBlock i e)  = ScSingleBlock i (fmap f e)
  fmap f (ScPolyBlock i se)   = ScPolyBlock i (fmap (fmap f) se)
  
instance Functor ScMeasure where
  fmap f (ScMeasure se)       = ScMeasure (fmap (fmap f) se)

instance Functor ScGlyph where
  fmap f (ScGlyph e)          = ScGlyph (f e)


--------------------------------------------------------------------------------
-- Foldable instances

instance F.Foldable ScSystem where
  foldMap f (ScSystem se)         = F.foldMap (F.foldMap f) se
  
instance F.Foldable ScStrata where
  foldMap f (ScStrata i se)       = F.foldMap (F.foldMap f) se
  
instance F.Foldable ScBlock where
  foldMap f (ScSingleBlock i e)   = F.foldMap f e
  foldMap f (ScPolyBlock i se)    = F.foldMap (F.foldMap f) se
  
instance F.Foldable ScMeasure where
  foldMap f (ScMeasure se)        = F.foldMap (F.foldMap f) se
  
instance F.Foldable ScGlyph where
  foldMap f (ScGlyph e)           = f e

--------------------------------------------------------------------------------
-- Traversable instances


instance Traversable ScSystem where
  traverse f (ScSystem se)        = ScSystem A.<$> traverse (traverse f) se
  
instance Traversable ScStrata where
  traverse f (ScStrata i se)      = (ScStrata i) A.<$> traverse (traverse f) se

instance Traversable ScBlock where
  traverse f (ScSingleBlock i e)  = (ScSingleBlock i) A.<$> traverse f e
  traverse f (ScPolyBlock i se)   = 
      (ScPolyBlock i) A.<$> traverse (traverse f) se
 
instance Traversable ScMeasure where
  traverse f (ScMeasure se)       = ScMeasure A.<$> traverse (traverse f) se
 
instance Traversable ScGlyph where
  traverse f (ScGlyph e)          = ScGlyph A.<$> f e


data CommonGlyph = CmnNote Pitch Duration
                 | CmnRest Duration
                 | CmnSpacer Duration -- non-printed rest
                 | CmnChord (Seq Pitch) Duration
                 | CmnGraceNotes (Seq (Pitch,Duration))

duration :: CommonGlyph -> Duration 
duration (CmnNote p d)      = d
duration (CmnRest d)        = d
duration (CmnSpacer d)      = d
duration (CmnChord se d)    = d
duration (CmnGraceNotes se) = F.foldr f dzero se where f (_,d) a = a `dplus` d

-- At some point define an instance of Num on duration,
-- until then graces notes won't work. 
dzero = demisemiquaver

dplus :: Duration -> Duration -> Duration
dplus a b = b



type DSystem    = ScSystem CommonGlyph
type DStrata    = ScStrata CommonGlyph
type DBlock     = ScBlock CommonGlyph
type DMeasure   = ScMeasure CommonGlyph
type DGlyph     = ScGlyph CommonGlyph


dsystem :: Seq DStrata -> DSystem
dsystem se = ScSystem se

dstrata :: Int -> Seq DBlock -> DStrata
dstrata i se = ScStrata i se

dsingleBlock :: Int -> DMeasure ->  DBlock
dsingleBlock i e = ScSingleBlock i e

dpolyBlock :: Int -> Seq DMeasure ->  DBlock
dpolyBlock i se = ScPolyBlock i se

dmeasure :: Seq DGlyph -> DMeasure
dmeasure se = ScMeasure se

-- seperate constructors for each sort in CommonGlyph might be more useful 
dglyph :: CommonGlyph -> DGlyph
dglyph g = ScGlyph g

instance (Pretty e) => Pretty (ScSystem e) where
  pretty (ScSystem se) = (sepSeq (<$>) se) <> line

instance (Pretty e) => Pretty (ScStrata e) where
  pretty (ScStrata i se) = prefix i <$> text ":line " <> int i
                                    <$> sepSeq (<$>) se
    where
      prefix i  = let l = snd $ intPlex i in text $ replicate (l+6) '-'


instance (Pretty e) => Pretty (ScBlock e) where
  pretty (ScSingleBlock i e) = measureNumber i
                                         <$> indent 4 (pretty e)
  pretty (ScPolyBlock i se)  = 
      measureNumber i <$> indent 4 (encloseSep (text "<<") 
                                               (text ">>") 
                                               (text " // ")
                                               (map pretty $ F.toList se))

measureNumber :: Int -> Doc
measureNumber i = text "|:" <>  int i


instance (Pretty e) => Pretty (ScMeasure e) where
  pretty (ScMeasure se) = sepSeq (</>) se

instance (Pretty e) => Pretty (ScGlyph e) where 
  pretty (ScGlyph e) = pretty e

instance Pretty CommonGlyph where
  pretty (CmnNote pch dur)       = pretty pch <> durationSuffix dur
  pretty (CmnRest dur)           = char 'r' <> durationSuffix dur
  pretty (CmnSpacer dur)         = char 's' <> durationSuffix dur
  pretty (CmnChord ps dur)       = (brackets $ sepSeq (<>) ps) 
                                      <> durationSuffix dur
  pretty (CmnGraceNotes es)      = text "grace..." -- braces $ sepSeq (<>) ps

durationSuffix :: Duration -> Doc
durationSuffix d = char '/' <> pretty d 


intPlex i = let s = show i in (s,length s)

tagint i = let (s,l) = intPlex i in
  if l < 5 then text (replicate (5-l) '0' ++ s) else text s


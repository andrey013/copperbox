
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ScoreRepresentation
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

module HNotate.ScoreRepresentation (
    -- * Datatypes
    ScSystem(..),
    ScNoteList(..),
    ScBlock(..),
    ScMeasure(..),
    
    Glyph(..),
    
    -- aliases
    ScoreGlyph,
    ScoreSystem,
    ScoreNoteList,
    ScoreBlock,
    ScoreMeasure,
    
    glyphDuration,
    
    onNoteList, onNoteListM
    

  ) where

import HNotate.Bifunctor
import HNotate.CommonUtils (sepSeq)
import HNotate.Duration
import HNotate.Pitch

import qualified Control.Applicative as A
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence (Seq, ViewL(..), viewl)
import Data.Traversable
import Text.PrettyPrint.Leijen




newtype ScSystem e = ScSystem { getSystem :: Map.Map String (ScNoteList e) }

onNoteList (ScSystem mp) k f = case Map.lookup k mp of
    Just v -> Just $ f v
    _ -> Nothing

onNoteListM (ScSystem mp) k mf = case Map.lookup k mp of
    Just v -> mf v >>= return . Just
    _ -> return Nothing
    
    
-- A 'horizontal line' of music - of course it maybe printed on 
-- more than one line and might contain polyphony, but it will be played 
-- by a single instrument.
newtype ScNoteList e = ScNoteList { getNoteList :: Seq (ScBlock e) }




-- Follow the Abc style when voice overlays are grouped measure-wise.
-- The Int holds the measure number
data ScBlock e = ScSingleBlock Int (ScMeasure e)
               | ScPolyBlock Int (Seq (ScMeasure e))


newtype ScMeasure e = ScMeasure { getMeasure :: Seq e }


data Glyph pch drn = GlyNote pch drn
                   | GlyRest drn
                   | GlySpacer drn -- non-printed rest
                   | GlyChord (Seq pch) drn
                   | GlyGraceNotes (Seq (pch,drn))

type ScoreGlyph = Glyph Pitch Duration

type ScoreSystem    = ScSystem ScoreGlyph
type ScoreNoteList  = ScNoteList ScoreGlyph
type ScoreBlock     = ScBlock ScoreGlyph
type ScoreMeasure   = ScMeasure ScoreGlyph

                 

--------------------------------------------------------------------------------
-- Functor instances

instance Functor ScSystem where
  fmap f (ScSystem se)        = ScSystem (fmap (fmap f) se)
  
instance Functor ScNoteList where
  fmap f (ScNoteList se)      = ScNoteList (fmap (fmap f) se)
  
instance Functor ScBlock where
  fmap f (ScSingleBlock i e)  = ScSingleBlock i (fmap f e)
  fmap f (ScPolyBlock i se)   = ScPolyBlock i (fmap (fmap f) se)
  
instance Functor ScMeasure where
  fmap f (ScMeasure se)       = ScMeasure (fmap f se)



--------------------------------------------------------------------------------
-- Foldable instances

instance F.Foldable ScSystem where
  foldMap f (ScSystem se)         = F.foldMap (F.foldMap f) se
  
instance F.Foldable ScNoteList where
  foldMap f (ScNoteList se)       = F.foldMap (F.foldMap f) se
  
instance F.Foldable ScBlock where
  foldMap f (ScSingleBlock i e)   = F.foldMap f e
  foldMap f (ScPolyBlock i se)    = F.foldMap (F.foldMap f) se
  
instance F.Foldable ScMeasure where
  foldMap f (ScMeasure se)        = F.foldMap f se
  

--------------------------------------------------------------------------------
-- Traversable instances


instance Traversable ScSystem where
  traverse f (ScSystem se)        = ScSystem A.<$> traverse (traverse f) se
  
instance Traversable ScNoteList where
  traverse f (ScNoteList se)      = ScNoteList A.<$> traverse (traverse f) se

instance Traversable ScBlock where
  traverse f (ScSingleBlock i e)  = (ScSingleBlock i) A.<$> traverse f e
  traverse f (ScPolyBlock i se)   = 
      (ScPolyBlock i) A.<$> traverse (traverse f) se
 
instance Traversable ScMeasure where
  traverse f (ScMeasure se)       = ScMeasure A.<$> traverse f se
 

--------------------------------------------------------------------------------
-- Bifunctor instance for Glyphs


instance Bifunctor Glyph where
  bimap f g (GlyNote p d)      = GlyNote (f p) (g d)
  bimap f g (GlyRest d)        = GlyRest (g d)
  bimap f g (GlySpacer d)      = GlySpacer (g d)
  bimap f g (GlyChord se d)    = GlyChord (fmap f se) (g d)
  bimap f g (GlyGraceNotes se) = GlyGraceNotes (fmap (onPair f g) se)
    where onPair f g (a,b)     = (f a, g b)

    

instance Bicollect Glyph where
  bicollect f g (GlyNote p d)      = (f p) `mappend` (g d)
  bicollect f g (GlyRest d)        = g d
  bicollect f g (GlySpacer d)      = g d
  bicollect f g (GlyChord se d)    = F.foldl fn mempty se
    where fn a e = a `mappend` f e
  bicollect f g (GlyGraceNotes se) = F.foldl fn mempty se
    where fn a (x,y) = a `mappend` f x `mappend` g y

  
instance Biproject Glyph where
  biprojectL (GlyNote p d)      = [p]
  biprojectL (GlyRest d)        = []
  biprojectL (GlySpacer d)      = []
  biprojectL (GlyChord se d)    = F.foldr (:) [] se  
  biprojectL (GlyGraceNotes se) = F.foldr fn mempty se
    where fn (p,_) a = p:a

  biprojectR (GlyNote p d)      = [d]
  biprojectR (GlyRest d)        = [d]
  biprojectR (GlySpacer d)      = [d]
  biprojectR (GlyChord se d)    = [d]
  biprojectR (GlyGraceNotes se) = F.foldr fn mempty se
    where fn (_,d) a = d:a


glyphDuration :: Monoid d => Glyph p d -> d
glyphDuration (GlyNote p d)      = d
glyphDuration (GlyRest d)        = d
glyphDuration (GlySpacer d)      = d
glyphDuration (GlyChord se d)    = d
glyphDuration (GlyGraceNotes _)  = mempty 


    
--------------------------------------------------------------------------------
-- pretty printing

instance (Pretty e) => Pretty (ScSystem e) where
  pretty (ScSystem mp) = Map.foldWithKey fn empty mp
    where fn k v acc = acc <$> text k <+> equals <+> braces (pretty v)

instance (Pretty e) => Pretty (ScNoteList e) where
  pretty (ScNoteList se) = sepSeq (<$>) se



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


instance (Pretty pch, Pretty drn) => Pretty (Glyph pch drn) where
  pretty (GlyNote pch dur)       = pretty pch <> durationSuffix dur
  pretty (GlyRest dur)           = char 'r' <> durationSuffix dur
  pretty (GlySpacer dur)         = char 's' <> durationSuffix dur
  pretty (GlyChord ps dur)       = (brackets $ sepSeq (<>) ps) 
                                      <> durationSuffix dur
  pretty (GlyGraceNotes es)      = text "grace..." -- braces $ sepSeq (<>) ps

durationSuffix :: Pretty drn => drn -> Doc
durationSuffix d = char '/' <> pretty d 


intPlex i = let s = show i in (s,length s)

tagint i = let (s,l) = intPlex i in
  if l < 5 then text (replicate (5-l) '0' ++ s) else text s


{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.SyntaxInterim
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Images - scores that have been partially rendered and are 
-- composed as Docs.
--
--
--------------------------------------------------------------------------------

module Neume.Core.SyntaxInterim
  (


  -- * Phrases and bars
    Phrase(..)
  , Bar 
  , PhraseName
  , BarNum


  -- * Staff expressions
  , CExpr(..)
  , CPhrase
  , CBar 

  , mapCPhrase
  , mapCBar
  , mapCExpr
  , stmapCPhrase
  , stmapBarInitialGlyph

  , Image
  , PhraseImage(..)
  , BarImage

  , PhraseOverlayImage(..)
  , BarOverlayImage


  , ExtractBarImages(..)

  ) where

import Neume.Core.Metrical
import Neume.Core.Utils

import Text.PrettyPrint.Leijen          -- package: wl-print

--------------------------------------------------------------------------------
-- Phrases and bars 

-- Note - phrases are polymorphic on bar so they can handle 
-- images (all notes concatenated into a Doc) or interim note 
-- lists of (CExpr gly).


data Phrase bar = Phrase 
      { phrase_name   :: PhraseName
      , pahrase_bars  :: [bar]
      }
  deriving (Show)

type Bar elt = [elt]

type PhraseName     = String
type BarNum         = Int

--------------------------------------------------------------------------------
-- Staff \Expressions\



-- | Contextual expression. This is a sequence of one or more 
-- notes together with some context to be communicated to the 
-- pretty printer - the context being either that the notes 
-- should be beamed or that they are n-plets (duplets, triplets, 
-- ...). 
--
-- Note this formulation permits beam groups within beam groups.
-- Ideally this would be disallowed, but beam groups may contain
-- n-plets (and n-plets must be recursive).
--

data CExpr gly = Atom               gly 
               | N_Plet  PletMult   [CExpr gly]
               | Beamed             [CExpr gly]
  deriving (Eq,Show)


type CPhrase gly = Phrase (CBar gly)
type CBar    gly = Bar    (CExpr gly)

mapCPhrase :: (gly -> gly') -> CPhrase gly -> CPhrase gly'
mapCPhrase f = fmap (mapCBar f) 

mapCBar :: (gly -> gly') -> CBar gly -> CBar gly'
mapCBar f = map (mapCExpr f) 

mapCExpr :: (gly -> gly') -> CExpr gly -> CExpr  gly'
mapCExpr = fmap


stmapCPhrase :: (st -> gly -> (gly',st)) 
             -> st 
             -> CPhrase gly 
             -> (CPhrase gly', st)
stmapCPhrase f = stmap (stmap (stmap f))


-- This one models the traversal for LilyPond duration changes.
-- 
-- Note - it doesn't keep the final state as the same initial 
-- state is seeded at each bar.
--

stmapBarInitialGlyph :: (st -> gly -> (gly',st)) 
                     -> (st -> gly -> (gly',st)) 
                     -> st 
                     -> CPhrase gly 
                     -> CPhrase gly'
stmapBarInitialGlyph f g st0 = fmap barfun where
  barfun xs = fst $ stmapIx cexprIx st0 xs

  cexprIx i st x  | i == 0    = stmapIx glyphIx st x
                  | otherwise = stmap g st x

  glyphIx i st x  | i == 0    = f st x
                  | otherwise = g st x
  


--------------------------------------------------------------------------------
-- Phrases and bars 

-- Phrases and bars are composable with pretty-print operations...
--
-- All notes, chords etc. within a a bar have already been 
-- rendered to a Doc.
--

type Image          = Doc

data PhraseImage = PhraseImage 
      { phrase_image_name   :: PhraseName
      , phrase_image_bars   :: [BarImage]
      }
  deriving (Show)

type BarImage           = Image


--------------------------------------------------------------------------------
-- 

-- This is formed from merging 2 or more PhraseImages so it has
-- a list of phrase names
--
data PhraseOverlayImage = PhraseOverlayImage
      { phrase_overlay_img_names :: [PhraseName]
      , phrase_overlay_img_bars  :: [BarOverlayImage] 
      }
  deriving (Show)

type BarOverlayImage    = Image



class ExtractBarImages a where
  extractBarImages :: a -> [Image]

instance ExtractBarImages PhraseImage where
  extractBarImages = phrase_image_bars

instance ExtractBarImages PhraseOverlayImage where
  extractBarImages = phrase_overlay_img_bars



--------------------------------------------------------------------------------
-- Instances

instance Functor Phrase where
  fmap f (Phrase name bars) = Phrase name $ map f bars


instance Functor CExpr where
  fmap f (Atom e)         = Atom $ f e
  fmap f (N_Plet d cexpr) = N_Plet d $ map (fmap f) cexpr
  fmap f (Beamed cexpr)   = Beamed $ map (fmap f) cexpr



-- StateMap

instance StateMap Phrase where
  stmap f st (Phrase name xs) = fmap2a (Phrase name) $ stmap f st xs

instance StateMap CExpr where
  stmap f st (Atom  e)     = fmap2a Atom $ f st e  
  stmap f st (N_Plet d ce) = fmap2a (N_Plet d) $ stmap (stmap f) st ce
  stmap f st (Beamed ce)   = fmap2a Beamed $ stmap (stmap f) st ce



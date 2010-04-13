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



--------------------------------------------------------------------------------
-- Phrases and bars 

-- Phrases and bars are composable with pretty-print operations...


type Image          = Doc

data PhraseImage = PhraseImage 
      { phrase_image_name   :: PhraseName
      , phrase_image_bars   :: [BarImage]
      }
  deriving (Show)

type BarImage           = Image


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
-- 

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
  stmap f st (Phrase name xs) = (Phrase name xs',st') 
    where (xs',st') = stmap f st xs

instance StateMap CExpr where
  stmap f st (Atom  e)     = (Atom e',st')      where (e',st') = f st e
  stmap f st (N_Plet d ce) = (N_Plet d ce',st') 
                             where (ce',st') = stmap (stmap f) st ce

  stmap f st (Beamed ce)   = (Beamed ce',st')   
                             where (ce',st') = stmap (stmap f) st ce


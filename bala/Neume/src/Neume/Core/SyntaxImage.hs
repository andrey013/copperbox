{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.SyntaxImage
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

module Neume.Core.SyntaxImage
  (


    Image
  , PhraseImage(..)
  , BarImage

  , PhraseOverlayImage(..)
  , BarOverlayImage


  , BarNum
  , PhraseName
  , ExtractBarImages(..)

  ) where



import Text.PrettyPrint.Leijen          -- package: wl-print


--------------------------------------------------------------------------------
-- Phrases and bars 

-- Phrases and bars are composable with pretty-print operations...

type PhraseName     = String
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

type BarNum             = Int


class ExtractBarImages a where
  extractBarImages :: a -> [Image]

instance ExtractBarImages PhraseImage where
  extractBarImages = phrase_image_bars

instance ExtractBarImages PhraseOverlayImage where
  extractBarImages = phrase_overlay_img_bars

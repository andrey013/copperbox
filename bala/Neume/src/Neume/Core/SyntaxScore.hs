{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.SyntaxScore
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Minimal syntax types for bars and phrases after rendering to 
-- ABC or Lilypond. 
--
-- Operations - e.g. interspersing with bar lines, adding repeat
-- marks - are simpler and more general when there is almost no 
-- syntax to get in the way. 
--
--------------------------------------------------------------------------------

module Neume.Core.SyntaxScore
  (
  -- * Score ( assembled from repeats and /straights/ )
    Section(..) -- to become Score once the typeclass Score has gone...
  , Score


  , Image
  , PhraseImage(..)
  , BarImage

  , PhraseOverlayImage(..)
  , BarOverlayImage


  , BarNum
  , PhraseName
  , ExtractBarImages(..)

  ) where


import Neume.Core.Utils

import Text.PrettyPrint.Leijen          -- package: wl-print


type Score a = [Section a]

data Section a = Straight a 
               | Repeated a
               | AltRepeat a [a]
  deriving (Eq,Show)


instance Functor Section where
  fmap f (Straight a)       = Straight $ f a
  fmap f (Repeated a)       = Repeated $ f a
  fmap f (AltRepeat a alts) = AltRepeat (f a) (map f alts)

instance StateMap Section where
  stmap f st (Straight a)       = (Straight a',st') where (a',st') = f st a
  stmap f st (Repeated a)       = (Repeated a',st') where (a',st') = f st a
  stmap f st (AltRepeat a alts) = (AltRepeat a' alts',st'') 
    where 
      (a',st')     = f st a
      (alts',st'') = stmap f st' alts


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

{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.SyntaxDoc
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

module Neume.Core.SyntaxDoc
  (
  -- * Score ( assembled from repeats and /straights/ )
    Score(..)

  , BarNum
  , BarImage

  , PhraseImage(..)
  , OverlayBar(..)

  , renderToBars
  , renderToBars_st

  ) where

import Neume.Core.Utils

import Text.PrettyPrint.Leijen          -- package: wl-print

-- Scores are implemented in the TypeCase / Tagless / EMGM style.
--

class Score repr a where
  straight  :: a -> repr a
  repeated  :: a -> repr a
  altRepeat :: a -> [a] -> repr a  
  caten     :: repr a -> repr a -> repr a


--------------------------------------------------------------------------------
-- Phrases and bars 

-- Phrases and bars are composable with pretty-print operations...



type BarImage = Doc

type BarNum   = Int




newtype PhraseImage = PhraseImage  { getPhraseImage  :: [BarImage] }
  deriving Show


newtype OverlayBar = OverlayBar { getOverlayBar :: BarImage }   deriving Show


renderToBars :: (a -> PhraseImage) -> a -> [BarImage]
renderToBars f = getPhraseImage . f

renderToBars_st :: (a -> st -> (PhraseImage,st)) -> a -> st -> ([BarImage],st)
renderToBars_st f = fmap2a getPhraseImage `oo` f

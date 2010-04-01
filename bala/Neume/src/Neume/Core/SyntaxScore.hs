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
    Score(..)

  , BarNum
  , ScoreImage
  , BarImage

  , Overlay(..)

  , PhraseImage
  , OverlayImage(..)


  ) where


import Text.PrettyPrint.Leijen          -- package: wl-print

-- Scores are implemented in a TypeCase / Tagless / EMGM style.
--

class Score repr where
  type ScoreBase repr :: *
  straight  :: (a ~ ScoreBase repr) => a -> repr
  repeated  :: (a ~ ScoreBase repr) => a -> repr
  altRepeat :: (a ~ ScoreBase repr) => a -> [a] -> repr  
  caten     :: repr -> repr -> repr


--------------------------------------------------------------------------------
-- Phrases and bars 

-- Phrases and bars are composable with pretty-print operations...


type ScoreImage = Doc
type BarImage   = Doc

type BarNum   = Int


newtype Overlay a = Overlay { getOverlays :: [[a]] } deriving Show


type PhraseImage = [BarImage]


newtype OverlayImage = OverlayImage { getOverlayImage :: BarImage }   
  deriving Show


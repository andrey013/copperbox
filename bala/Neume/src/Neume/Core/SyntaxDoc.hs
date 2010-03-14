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
  -- * Score ( assembled from phrases / overlays )
    BarImage
  , Score(..)
  , Section(..)
  , Phrase(..)
  , OverlayBar(..)

  , ABC
  , LY
  , ABC_overlay
  , LY_overlay

  ) where



import Text.PrettyPrint.Leijen          -- package : wl-print


--------------------------------------------------------------------------------
-- Phrases and bars 

-- Phrases and bars are composable with pretty-print operations...
-- No type-change operation, so not functors... 

type BarImage = Doc

newtype Score a   = Score { getSections :: [Section a] }        deriving Show

data Section a = Straight  (Phrase a)
               | Repeated  (Phrase a)
               | AltRepeat (Phrase a) [Phrase a]
  deriving Show

newtype Phrase a  = Phrase  { getPhrase  :: [BarImage] }   deriving Show


newtype OverlayBar = OverlayBar { getOverlayBar :: BarImage }   deriving Show

data ABC
data LY

data ABC_overlay
data LY_overlay
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
  -- * Phrases and bars
    LyPhrase(..)
  , LyBar(..)
  , AbcPhrase(..)
  , AbcBar(..)

  -- * Score ( assembled from phrases / overlays )
  , Score(..)
  , Section(..)
  , OverlayBar(..)

  ) where



import Text.PrettyPrint.Leijen          -- package : wl-print


--------------------------------------------------------------------------------
-- Phrases and bars 

-- Phrases and bars are composable with pretty-print operations...
-- No type-change operation, so not functors... 

newtype LyPhrase   = LyPhrase   { getLyPhrase  :: [LyBar] }     deriving Show
newtype LyBar      = LyBar      { getLyBar     :: Doc }         deriving Show

newtype AbcPhrase   = AbcPhrase { getAbcPhrase :: [AbcBar] }    deriving Show
newtype AbcBar      = AbcBar    { getAbcBar    :: Doc }         deriving Show


newtype Score bar   = Score { getBars :: [Section bar] }        deriving Show

data Section bar = Straight      [bar]
                 | Repeated      [bar]
                 | RepeatedVolta [bar] [[bar]]
  deriving Show

newtype OverlayBar = OverlayBar { getOverlayBar :: Doc }        deriving Show


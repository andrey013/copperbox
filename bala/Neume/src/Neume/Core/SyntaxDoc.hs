{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.SyntaxDoc
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

module Neume.SyntaxDoc
  (
  -- * Phrases and bars
    LyPhrase(..)
  , LyBar(..)
  , AbcPhrase(..)
  , AbcBar(..)


  ) where



import Text.PrettyPrint.Leijen          -- package : wl-print


--------------------------------------------------------------------------------
-- Phrases and bars 

-- Phrases and bars are composable with pretty-print operations...
-- No type-change operation, so not functors... 

newtype LyPhrase   = LyPhrase   { getLyPhrase  :: [LyBar] }
newtype LyBar      = LyBar      { getLyBar     :: Doc }

newtype AbcPhrase   = AbcPhrase { getAbcPhrase :: [AbcBar] }
newtype AbcBar      = AbcBar    { getAbcBar    :: Doc }




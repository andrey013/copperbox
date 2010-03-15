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
  -- * Score ( assembled from phrases / overlays )
    BarImage
  , Score(..)
  , TermScore(..)
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

class Score repr a where
  straight  :: a -> repr a
  repeated  :: a -> repr a
  altRepeat :: a -> [a] -> repr a  
  caten     :: repr a -> repr a -> repr a

instance Score TermScore Phrase where
  straight a       = Score [Straight a]
  repeated a       = Score [Repeated a]
  altRepeat a alts = Score [AltRepeat a alts]
  caten a b        = Score (a'++b') 
                     where 
                       a' = getSections a
                       b' = getSections b

-- only the top of the syntax tree needs a type parameter...

newtype TermScore a = Score { getSections :: [Section] }        deriving Show

data Section = Straight  Phrase
             | Repeated  Phrase
             | AltRepeat Phrase [Phrase]
  deriving Show

newtype Phrase = Phrase  { getPhrase  :: [BarImage] }   deriving Show


newtype OverlayBar = OverlayBar { getOverlayBar :: BarImage }   deriving Show

data ABC
data LY

data ABC_overlay
data LY_overlay


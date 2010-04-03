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


  , ScoreImage
  , PhraseImage
  , BarImage

  , PhraseOverlayImage
  , BarOverlayImage


  , BarNum

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


type ScoreImage         = Doc
type PhraseImage        = [BarImage]
type BarImage           = Doc

type PhraseOverlayImage = [BarOverlayImage]
type BarOverlayImage    = Doc


type BarNum   = Int




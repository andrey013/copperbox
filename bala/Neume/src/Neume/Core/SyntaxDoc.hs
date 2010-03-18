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
  , ScoreImage(..)
  , SectionImage(..)
  , PhraseImage(..)
  , OverlayBar(..)


  ) where


import Data.JoinList                    -- package: joinlist

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
-- No type-change operation, so not functors... 



type BarImage = Doc

type BarNum  = Int



instance Score ScoreImage PhraseImage where
  straight a       = ScoreImage $ singleton $ Straight a
  repeated a       = ScoreImage $ singleton $ Repeated a
  altRepeat a alts = ScoreImage $ singleton $ AltRepeat a alts
  caten a b        = ScoreImage $ getScoreImage a `join` getScoreImage b


-- only the top of the syntax tree needs a type parameter...

newtype ScoreImage a = ScoreImage { getScoreImage :: JoinList SectionImage }
  deriving Show

data SectionImage = Straight  PhraseImage
                  | Repeated  PhraseImage
                  | AltRepeat PhraseImage [PhraseImage]
  deriving Show

newtype PhraseImage = PhraseImage  { getPhraseImage  :: [BarImage] }
  deriving Show


newtype OverlayBar = OverlayBar { getOverlayBar :: BarImage }   deriving Show


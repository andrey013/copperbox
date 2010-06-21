{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.Tab
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Glyphs annotated with /string number/ for printing tabs.
-- 
--------------------------------------------------------------------------------

module Neume.Extra.Tab
  (

    TabGlyph
  , tabAlg

  , StringNumber
  , stringNumber

  , ppStringNumber
  

  ) where

import Neume.Core.Duration
import Neume.Core.LilyPondOutput
import Neume.Core.LilyPondPretty
import Neume.Core.LilyPondTrafo
import Neume.Core.Pitch
import Neume.Core.Syntax
import Neume.Core.Utils.Pretty

import Neume.Extra.LilyPondScoreOutput

import Text.PrettyPrint.Leijen          -- package: wl-pprint



type TabGlyph dur = Glyph StringNumber Pitch dur


tabAlg :: (LyAbsPitchTrafo repr, LyRelDurTrafo repr)
       => LilyPondImageAlg repr (TabGlyph Duration) (TabGlyph (Maybe Duration))
tabAlg = LilyPondImageAlg
    { glyph_printer     = renderGlyph pitch ppStringNumber
    , duration_trafo    = fmap runRelDurTrafo
    , pitch_trafo       = fmap (runAbsPitchTrafo octaveTabF)
    }

-- for neume-core
octaveTrebleF :: Octave -> Octave
octaveTrebleF o = o-3

octaveTabF :: Octave -> Octave
octaveTabF o = o-4





--------------------------------------------------------------------------------
-- Tab Glyphs with string number


-- $tab_string_anno
-- Note form is \<pitch\>\<duration\>/\/\<finger_number\> 
-- e.g. @c4\\2@
--
-- Chord form is /</\<pitch\>/\/\<finger_number> .../>/\<duration\> 
-- e.g. @\<c\\1 e\\2 g\\4>2@

-- Notes are annotated with string number

newtype StringNumber = StringNumber Int
  deriving (Eq,Ord)

stringNumber :: Int -> StringNumber
stringNumber = StringNumber . abs

instance Show StringNumber where
  showsPrec p (StringNumber i) = showsPrec p i


ppStringNumber :: StringNumber -> DocS
ppStringNumber (StringNumber i) = \d -> d <> char '\\' <> int i

 


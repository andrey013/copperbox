{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.ABC
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Collective import module for ABC
--
--------------------------------------------------------------------------------


module Mullein.Abc
  ( 
    module Mullein.AbcDoc
  , module Mullein.AbcOutput
  , module Mullein.Bracket
  , module Mullein.Core
  , module Mullein.Duration
  , module Mullein.Extended
  , module Mullein.NamedElements
  , module Mullein.Pitch
  , renderDocEighty
  , writeDoc


  -- * Simple free meter template
  , abcSimple

  ) where
 
import Mullein.AbcDoc
import Mullein.AbcOutput
import Mullein.Bracket
import Mullein.Core
import Mullein.Duration
import Mullein.Extended
import Mullein.NamedElements
import Mullein.Pitch
import Mullein.Utils ( renderDocEighty, writeDoc )

import Text.PrettyPrint.Leijen

import Data.Ratio

--------------------------------------------------------------------------------

-- Print a list of /notes/ in free meter, notionally in C major 
-- - i.e. all sharps and flats will be printed explicitly.
abcSimple :: ( HasPitch pch, ChangePitchAbc t
             , HasDuration (t pch), ChangeDurationAbc (t pch)
             , AbcGlyph (t pch AbcMultiplier)
             , ExtBeam (t pch Duration) )
          => [t pch Duration] -> Doc
abcSimple xs = tunenum   1 
      <$> title     ""
      <$> key       "Cmaj"
      <$> meter     "none"
      <$> tune
  where
    tune = simpleOutput $ renderPhrase 
                        $ rewritePitch cmaj
                        $ rewriteDuration (1%8)
                        $ phrase (makeMeterPattern 4 4) xs

    cmaj = makeSpellingMap 0


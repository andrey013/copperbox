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
-- Note - @Mullein.NamedElements@ is not exported, it exports many 
-- short names (e.g. c,d,...) which are likely to cause name 
-- clashes so it would usually be imported @qualified@.
-- 
-- Similarly not all the functions exported by @Mullein.AbcDoc@ 
-- are re-exported by this module. If the /missing/ functions are 
-- needed then the module should be imported qualified.
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
 
import Mullein.AbcDoc hiding ( note, pitch, pitchLabel, spacer, rest )
import Mullein.AbcOutput
import Mullein.Bracket
import Mullein.Core
import Mullein.Duration
import Mullein.Extended
import Mullein.NamedElements
import Mullein.Pitch hiding ( C,D,E,F,G,A,B )
import Mullein.Utils ( renderDocEighty, writeDoc )

import Text.PrettyPrint.Leijen

import Data.Ratio

--------------------------------------------------------------------------------


{-
-- Ideally we should have a type more like this that allows any
-- glyph with pitch and duration to be printed rather than just a 
-- PDGlyph

abcSimple :: ( HasPitch pch, ChangePitchAbc t
             , HasDuration (t pch), ChangeDurationAbc (t pch)
             , AbcGlyph (t pch AbcMultiplier)
             , ExtBeam (t pch Duration) )
          => [t pch Duration] -> Doc
-}

-- Print a list of /notes/ in free meter, notionally in C major 
-- - i.e. all sharps and flats will be printed explicitly.
abcSimple :: [PDGlyph] -> Doc
abcSimple xs = tunenum   1 
      <$> title     ""
      <$> key       "Cmaj"
      <$> meter     "none"
      <$> tune
  where
    tune = simpleOutput $ renderPhrase abcGlyph
                        $ rewritePitch cmaj
                        $ rewriteDuration (1%8)
                        $ freePhrase (makeMeterPattern 4 4) xs

    cmaj = makeSpellingMap 0


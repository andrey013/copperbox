{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPond
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Collective import module for LilyPond
--
-- Note - @Mullein.NamedElements@ is not exported, it exports many 
-- short names (e.g. c,d,...) which are likely to cause name 
-- clashes so it would usually be imported @qualified@.
-- 
-- Similarly not all the functions exported by 
-- @Mullein.LilyPondDoc@ are re-exported by this module. If the 
-- /missing/ functions are needed then the module should be 
-- imported qualified.
-- 
--------------------------------------------------------------------------------


module Mullein.LilyPond 
  ( 
    module Mullein.Bracket
  , module Mullein.Core
  , module Mullein.Duration
  , module Mullein.Extended
  , module Mullein.LilyPondDoc
  , module Mullein.LilyPondOutput
  , module Mullein.Pitch
  , renderDocEighty
  , writeDoc

  -- * Simple free meter template
  , lilyPondSimple

  ) where
 
import Mullein.Bracket
import Mullein.Core
import Mullein.Duration
import Mullein.Extended
import Mullein.LilyPondDoc hiding ( note, pitch, pitchLabel, duration, 
                                    spacer , tie )
import Mullein.LilyPondOutput
import Mullein.NamedElements   -- not re-exported!
import Mullein.Pitch
import Mullein.Utils ( renderDocEighty, writeDoc )


import Text.PrettyPrint.Leijen


--------------------------------------------------------------------------------

-- Print a list of /notes/ in free meter, notionally in C major 
-- - i.e. all sharps and flats will be printed explicitly.
lilyPondSimple :: ( HasDuration (t pch), ChangeDurationLR (t pch)
                  , HasPitch pch, ChangePitchLR t
                  , LilyPondGlyph (t pch (Maybe Duration))
                  , ExtBeam (t pch Duration) )
               => [t pch Duration] -> Doc
lilyPondSimple xs =  version "2.12.2" 
     <$> score (relative middle_c $ key c_nat "major" 
                                    <$> cadenzaOn <$> tune <$> cadenzaOff) 
  where
    tune = simpleOutput $ renderPhrase 
                        $ rewritePitch middle_c 
                        $ rewriteDuration 
                        $ freePhrase (makeMeterPattern 4 4) xs



--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Backend.LilyPond.LilyPondVersion
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Version of LilyPond targetted.
--
--------------------------------------------------------------------------------

module HNotate.Backend.LilyPond.LilyPondVersion (
    __lilypond_version,
  ) where


import Data.Version


__lilypond_version :: Version
__lilypond_version = Version [2,10,33] [] 

    

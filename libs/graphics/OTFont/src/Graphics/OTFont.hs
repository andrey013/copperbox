{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- OpenType fonts
--
--------------------------------------------------------------------------------

module Graphics.OTFont ( 
    module Graphics.OTFont.Datatypes,
    module Graphics.OTFont.Parse,
    module Graphics.OTFont.Pretty,
    module Graphics.OTFont.Utils,
    module Graphics.OTFont.Table.Head,
    module Graphics.OTFont.Table.Name,
  ) where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Parse
import Graphics.OTFont.Pretty
import Graphics.OTFont.Utils
import Graphics.OTFont.Table.Name
import Graphics.OTFont.Table.Head



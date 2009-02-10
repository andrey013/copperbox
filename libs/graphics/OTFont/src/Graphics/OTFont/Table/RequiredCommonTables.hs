{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.RequiredCommonTables
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Required common tables [cmap, head, hhea, hmtx, maxp, name,os/2, post]
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.RequiredCommonTables (
    module Graphics.OTFont.Table.Cmap,
    module Graphics.OTFont.Table.CommonDatatypes,
    module Graphics.OTFont.Table.Head,
    module Graphics.OTFont.Table.Hhea,
    module Graphics.OTFont.Table.Hmtx,
    module Graphics.OTFont.Table.Maxp,
    module Graphics.OTFont.Table.Name,
    module Graphics.OTFont.Table.OS2,
    module Graphics.OTFont.Table.Post
 ) where

import Graphics.OTFont.Table.Cmap
import Graphics.OTFont.Table.CommonDatatypes
import Graphics.OTFont.Table.Head
import Graphics.OTFont.Table.Hhea
import Graphics.OTFont.Table.Hmtx
import Graphics.OTFont.Table.Maxp
import Graphics.OTFont.Table.Name
import Graphics.OTFont.Table.OS2
import Graphics.OTFont.Table.Post

                     
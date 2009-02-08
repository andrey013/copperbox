--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utils
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Utils where

import Graphics.OTFont.Datatypes

import qualified Data.ByteString as BS
import Data.List ( find )
import qualified Data.Map as Map
import Data.Word

findTable :: String -> LaxFont -> Maybe (BS.ByteString)
findTable name (LaxFont _ _ fm) = Map.lookup name fm

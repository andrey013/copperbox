{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ToyFontMetrics.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM concrete syntax.
-- 
--------------------------------------------------------------------------------

module Graphics.ToyFontMetrics.Datatypes
  ( 
    AfmFile(..)

  ) where

data AfmFile = AfmFile
      { afm_file_metrics_version :: String
      }
  deriving (Eq,Show)

data Header = Header
      { name_data   :: NameData
      }
  deriving (Eq,Show)

data NameData = NameData 
      { font_name   :: String -- This is used by PostScript\'s @findfont@
      , full_name   :: String
      , family_name :: String
      }
  derving (Eq,Show)
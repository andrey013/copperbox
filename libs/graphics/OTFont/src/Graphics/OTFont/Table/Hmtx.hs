{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Hmtx
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Hmtx - horizontal metrics
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Hmtx where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.ParseMonad
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty

import Control.Applicative
import Data.Array.IArray hiding ( array )
import Data.Typeable

import Text.PrettyPrint.Leijen ( Pretty(..), tupled )


  
data HmtxTable = HmtxTable { 
      h_metrics           :: Array Int LongHorMetric,
      left_side_bearing   :: Array Int Short
    }
  deriving (Eq,Show,Typeable)

data LongHorMetric = LongHorMetric { 
      lhm_advance_width   :: UShort,
      lhm_lsb             :: Short
    } 
  deriving (Eq,Show)

-- hmetric_count is @number_of_h_metrics@ from @hhea@
-- glyph_count @num_glyphs@ from @maxp@
readHmtxTable :: Monad m => UShort -> UShort -> ParserT r m HmtxTable
readHmtxTable hmetric_count glyf_count = HmtxTable <$>
        bxsequence msize readLongHorMetric
    <*> bxsequence gsize short
  where
    msize = fromIntegral hmetric_count
    gsize = fromIntegral $ glyf_count - hmetric_count
    
readLongHorMetric :: Monad m => ParserT r m LongHorMetric
readLongHorMetric = LongHorMetric <$>
    ushort <*> short
        
instance Pretty HmtxTable where
  pretty t = ppTable "hmtx Table" 
      [ field "h_metrics"           24 (ppArray pretty $ h_metrics t )
      , field "left_side_bearing"   24 (ppArray integral $ left_side_bearing t)
      ]

instance Pretty LongHorMetric where
  pretty (LongHorMetric aw lsb) = tupled [integral aw, integral lsb]       
                          
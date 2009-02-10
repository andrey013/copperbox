{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.OS2
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- OS/2 - Global font information table
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.OS2 where


import Graphics.OTFont.Parse
import Graphics.OTFont.Pretty

-- import Text.ZParse hiding ( text )

import Control.Applicative
import Data.Array.Unboxed
import Data.Int 
import Data.Word

import Text.PrettyPrint.Leijen ( Pretty(..), text )

data OS2Table = OS2Table { 
      os2_version             :: Word16,
      x_avg_char_width        :: Int16,
      us_weight_class         :: Word16,
      us_width_class          :: Word16,
      fs_type                 :: Word16,
      y_subscript_x_size      :: Int16,
      y_subscript_y_size      :: Int16,
      y_subscript_x_offset    :: Int16,
      y_subscript_y_offset    :: Int16,
      y_superscript_x_size    :: Int16,
      y_superscript_y_size    :: Int16,
      y_superscript_x_offset  :: Int16,
      y_superscript_y_offset  :: Int16,
      y_strikeout_size        :: Int16,
      y_strikeout_position    :: Int16,
      s_family_class          :: Int16,
      panose                  :: UArray Int Word8,  -- 10
      ul_unicode_range1       :: Word32,
      ul_unicode_range2       :: Word32,
      ul_unicode_range3       :: Word32,
      ul_unicode_range4       :: Word32,
      ach_vendor_id           :: UArray Int Char,   -- 4
      fs_selection            :: Word16,
      us_first_char_index     :: Word16,
      us_last_char_index      :: Word16,
      s_typo_ascender         :: Int16,
      s_typo_descender        :: Int16,
      s_typo_line_gap         :: Int16,
      us_win_ascent           :: Word16,
      us_win_descent          :: Word16,
      ul_code_page_range1     :: Word32,
      ul_code_page_range2     :: Word32,
      sx_height               :: Int16,
      s_cap_height            :: Int16,
      us_default_char         :: Word16,
      us_break_char           :: Word16,
      us_max_context          :: Word16
    }
  deriving (Eq,Show)

readOS2Table :: Monad m => ReadData m OS2Table
readOS2Table = OS2Table <$> 
        ushort          -- os2_version
    <*> short   
    <*> ushort  
    <*> ushort
    <*> ushort  
    <*> short   
    <*> short   
    <*> short
    <*> short   
    <*> short   
    <*> short   
    <*> short
    <*> short   
    <*> short   
    <*> short   
    <*> short
    <*> uarray 10 byte  -- panose
    <*> ulong  
    <*> ulong  
    <*> ulong  
    <*> ulong
    <*> uarray 4 char  -- ach_vendor_id
    <*> ushort  
    <*> ushort  
    <*> ushort
    <*> short   
    <*> short   
    <*> short
    <*> ushort  
    <*> ushort
    <*> ulong   
    <*> ulong
    <*> short   
    <*> short
    <*> ushort 
    <*> ushort  
    <*> ushort
    
instance Pretty OS2Table where
  pretty t = ppTable "OS/2 Table (version 0.5)"
      [ field "version"                 24 (pphex     $ os2_version t)
      , field "x_avg_char_width"        24 (integral  $ x_avg_char_width t)
      , field "us_weight_class"         24 (integral  $ us_weight_class t)
      , field "us_width_class"          24 (integral  $ x_avg_char_width t)
      , field "fs_type"                 24 (integral  $ fs_type t)
      , field "y_subscript_x_size"      24 (integral  $ y_subscript_x_size t)
      , field "y_subscript_y_size"      24 (integral  $ y_subscript_y_size t)
      , field "y_subscript_x_offset"    24 (integral  $ y_subscript_x_offset t)
      , field "y_subscript_y_offset"    24 (integral  $ y_subscript_y_offset t)
      , field "y_superscript_x_size"    24 (integral  $ y_superscript_x_size t)
      , field "y_superscript_y_size"    24 (integral  $ y_superscript_y_size t)
      , field "y_superscript_x_offset"  24 (integral  $ y_superscript_x_offset t)
      , field "y_superscript_y_offset"  24 (integral  $ y_superscript_y_offset t)
      , field "y_strikeout_size"        24 (integral  $ y_strikeout_size t)
      , field "y_strikeout_position"    24 (integral  $ y_strikeout_position t)
      , field "s_family_class"          24 (integral  $ s_family_class t)
      , field "panose"                  24 (text "Arr - todo")
      , field "ul_unicode_range1"       24 (integral  $ ul_unicode_range1 t)
      , field "ul_unicode_range2"       24 (integral  $ ul_unicode_range2 t)
      , field "ul_unicode_range3"       24 (integral  $ ul_unicode_range3 t)
      , field "ul_unicode_range4"       24 (integral  $ ul_unicode_range4 t)
      , field "ach_vendor_id"           24 (text "Arr - todo")
      , field "fs_selection"            24 (integral  $ fs_selection t)
      , field "first_char_index"        24 (integral  $ us_first_char_index t)
      , field "last_char_index"         24 (integral  $ us_last_char_index t)
      , field "s_typo_ascender"         24 (integral  $ s_typo_ascender t)
      , field "s_typo_descender"        24 (integral  $ s_typo_descender t)
      , field "s_typo_line_gap"         24 (integral  $ s_typo_line_gap t)
      , field "us_win_ascent"           24 (integral  $ us_win_ascent t)
      , field "us_win_descent"          24 (integral  $ us_win_descent t)
      , field "ul_code_page_range1"     24 (integral  $ ul_code_page_range1 t)
      , field "ul_code_page_range2"     24 (integral  $ ul_code_page_range2 t)
      , field "sx_height"               24 (integral  $ sx_height t)
      , field "s_cap_height"            24 (integral  $ s_cap_height t)
      , field "us_default_char"         24 (integral  $ us_default_char t)
      , field "us_break_char"           24 (integral  $ us_break_char t)
      , field "us_max_context"          24 (integral  $ us_max_context t)
      ]

                            
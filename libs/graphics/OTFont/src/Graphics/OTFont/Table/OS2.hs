{-# LANGUAGE DeriveDataTypeable #-}
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

import Graphics.OTFont.Datatypes
import Graphics.OTFont.ParseMonad
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty


import Control.Applicative
import Data.Array.Unboxed
import Data.Typeable

import Text.PrettyPrint.Leijen ( Pretty(..) )

data OS2Table = OS2Table { 
      os2_version             :: UShort,
      x_avg_char_width        :: Short,
      us_weight_class         :: UShort,
      us_width_class          :: UShort,
      fs_type                 :: UShort,
      y_subscript_x_size      :: Short,
      y_subscript_y_size      :: Short,
      y_subscript_x_offset    :: Short,
      y_subscript_y_offset    :: Short,
      y_superscript_x_size    :: Short,
      y_superscript_y_size    :: Short,
      y_superscript_x_offset  :: Short,
      y_superscript_y_offset  :: Short,
      y_strikeout_size        :: Short,
      y_strikeout_position    :: Short,
      s_family_class          :: Short,
      panose                  :: UArray Int Byte,  -- 10
      ul_unicode_range1       :: ULong,
      ul_unicode_range2       :: ULong,
      ul_unicode_range3       :: ULong,
      ul_unicode_range4       :: ULong,
      ach_vendor_id           :: UArray Int Char,   -- 4
      fs_selection            :: UShort,
      us_first_char_index     :: UShort,
      us_last_char_index      :: UShort,
      s_typo_ascender         :: Short,
      s_typo_descender        :: Short,
      s_typo_line_gap         :: Short,
      us_win_ascent           :: UShort,
      us_win_descent          :: UShort,
      ul_code_page_range1     :: ULong,
      ul_code_page_range2     :: ULong,
      sx_height               :: Short,
      s_cap_height            :: Short,
      us_default_char         :: UShort,
      us_break_char           :: UShort,
      us_max_context          :: UShort
    }
  deriving (Eq,Show,Typeable)

readOS2Table ::  Monad m => ParserT r m OS2Table
readOS2Table = OS2Table <$> 
        ushort          -- os2_version
    <*> short           -- x_avg_char_width   
    <*> ushort          -- us_weight_class  
    <*> ushort          -- us_width_class
    <*> ushort          -- fs_type
    <*> short           -- y_subscript_x_size   
    <*> short   
    <*> short
    <*> short   
    <*> short   
    <*> short   
    <*> short
    <*> short   
    <*> short   
    <*> short   
    <*> short           -- s_family_class
    <*> usequence 10 byte  -- panose
    <*> ulong           -- ul_unicode_range1  
    <*> ulong           -- ul_unicode_range2
    <*> ulong           -- ul_unicode_range3
    <*> ulong           -- ul_unicode_range4
    <*> usequence 4 char   -- ach_vendor_id
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
      , field "panose"                  24 (ppArray pphex2 $ panose t)
      , field "ul_unicode_range1"       24 (integral  $ ul_unicode_range1 t)
      , field "ul_unicode_range2"       24 (integral  $ ul_unicode_range2 t)
      , field "ul_unicode_range3"       24 (integral  $ ul_unicode_range3 t)
      , field "ul_unicode_range4"       24 (integral  $ ul_unicode_range4 t)
      , field "ach_vendor_id"           24 (ppArray pchar $ ach_vendor_id t)
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

                            
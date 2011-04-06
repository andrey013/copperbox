{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.StandardFontDefs
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Safe to use \"Core 13\" fonts that are expected to be present
-- for any PostScript interpreter.
--
-- Note - regrettably Symbol is not safe to use for SVG.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.StandardFontDefs
  ( 
  -- * Times Roman
    times_roman_family
  
  , times_roman
  , times_italic
  , times_bold
  , times_bold_italic

  -- * Helvetica
  , helvetica_family

  , helvetica
  , helvetica_oblique
  , helvetica_bold
  , helvetica_bold_oblique

  -- * Courier
  , courier_family

  , courier
  , courier_oblique
  , courier_bold
  , courier_bold_oblique

  -- * Symbol
  , symbol

  ) where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.StandardEncoding
import Wumpus.Core.Text.Symbol

-- Supported fonts are:
--
-- Times-Roman  Times-Italic       Times-Bold      Times-BoldItalic
-- Helvetica    Helvetica-Oblique  Helvetica-Bold  Helvetica-Bold-Oblique
-- Courier      Courier-Oblique    Courier-Bold    Courier-Bold-Oblique
-- Symbol

--------------------------------------------------------------------------------
-- 

-- | 'FontFamily' definition for Times-Roman.
--
times_roman_family :: FontFamily
times_roman_family = 
    FontFamily { ff_regular     = times_roman
               , ff_bold        = Just times_bold
               , ff_italic      = Just times_italic
               , ff_bold_italic = Just times_bold_italic
               }


-- | Times-Roman
-- 
times_roman :: FontDef
times_roman = 
    FontDef { font_def_face   = face
            , gs_file_name    = "n021003l.afm"          -- NimbusRomNo9L-Regu
            , afm_file_name   = "Times-Roman.afm"
            }
  where
    face = FontFace { ps_font_name      = "Times-Roman" 
                    , svg_font_family   = "Times New Roman" 
                    , svg_font_style    = SVG_REGULAR 
                    , font_enc_vector   = standard_encoding
                    }


-- | Times Italic
--
times_italic :: FontDef
times_italic =
    FontDef { font_def_face   = face
            , gs_file_name    = "n021023l.afm"        -- NimbusRomNo9L-ReguItal
            , afm_file_name   = "Times-Italic.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Times-Italic"
                     , svg_font_family  = "Times New Roman"
                     , svg_font_style   = SVG_ITALIC
                     , font_enc_vector  = standard_encoding
                     }


-- | Times Bold
--
times_bold :: FontDef
times_bold = 
    FontDef { font_def_face   = face
            , gs_file_name    = "n021004l.afm"          -- NimbusRomNo9L-Medi
            , afm_file_name   = "Times-Bold.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Times-Bold"
                     , svg_font_family  = "Times New Roman"
                     , svg_font_style   = SVG_BOLD
                     , font_enc_vector  = standard_encoding
                     }


-- | Times Bold Italic
--
times_bold_italic :: FontDef
times_bold_italic = 
    FontDef { font_def_face   = face
            , gs_file_name    = "n021024l.afm"          -- NimbusRomNo9L-MediItal
            , afm_file_name   = "Times-BoldItalic.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Times-BoldItalic"
                     , svg_font_family  = "Times New Roman"
                     , svg_font_style   = SVG_BOLD_ITALIC
                     , font_enc_vector  = standard_encoding
                     }


--------------------------------------------------------------------------------
-- Helvetica

-- | 'FontFamily' definition for Helvetica.
--
helvetica_family :: FontFamily
helvetica_family = 
    FontFamily { ff_regular     = helvetica
               , ff_bold        = Just helvetica_bold
               , ff_italic      = Just helvetica_oblique
               , ff_bold_italic = Just helvetica_bold_oblique
               }



-- | Helvetica regular weight.
--
helvetica :: FontDef
helvetica =
    FontDef { font_def_face   = face
            , gs_file_name    = "n019003l.afm"          -- NimbusSanL-Regu
            , afm_file_name   = "Helvetica.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Helvetica"
                     , svg_font_family  = "Helvetica"
                     , svg_font_style   = SVG_REGULAR
                     , font_enc_vector  = standard_encoding
                     }


-- | Helvetica Oblique
--
helvetica_oblique :: FontDef
helvetica_oblique = 
    FontDef { font_def_face   = face
            , gs_file_name    = "n019023l.afm"          -- NimbusSanL-ReguItal
            , afm_file_name   = "Helvetica-Oblique.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Helvetica-Oblique"
                     , svg_font_family  = "Helvetica"
                     , svg_font_style   = SVG_OBLIQUE
                     , font_enc_vector  = standard_encoding
                     }

-- | Helvetica Bold
-- 
helvetica_bold :: FontDef
helvetica_bold = 
    FontDef { font_def_face   = face
            , gs_file_name    = "n019004l.afm"          -- NimbusSanL-Bold
            , afm_file_name   = "Helvetica-Bold.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Helvetica-Bold"
                     , svg_font_family  = "Helvetica"
                     , svg_font_style   = SVG_BOLD
                     , font_enc_vector  = standard_encoding
                     }


-- | Helvetica Bold Oblique
--
helvetica_bold_oblique :: FontDef
helvetica_bold_oblique = 
    FontDef { font_def_face   = face
            , gs_file_name    = "n019024l.afm"          -- NimbusSanL-BoldItal
            , afm_file_name   = "Helvetica-BoldOblique.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Helvetica-Bold-Oblique"
                     , svg_font_family  = "Helvetica"
                     , svg_font_style   = SVG_BOLD_OBLIQUE
                     , font_enc_vector  = standard_encoding
                     }



--------------------------------------------------------------------------------

-- | 'FontFamily' definition for Courier.
--
courier_family :: FontFamily
courier_family = 
    FontFamily { ff_regular     = courier
               , ff_bold        = Just courier_bold
               , ff_italic      = Just courier_oblique
               , ff_bold_italic = Just courier_bold_oblique
               }



-- | Courier
-- 
courier :: FontDef
courier = 
    FontDef { font_def_face   = face
            , gs_file_name    = "n022003l.afm"          -- NimbusMonL-Regu
            , afm_file_name   = "Courier.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Courier"
                     , svg_font_family  = "Courier New"
                     , svg_font_style   = SVG_REGULAR
                     , font_enc_vector  = standard_encoding
                     }

-- | Courier Oblique
-- 
courier_oblique :: FontDef
courier_oblique = 
    FontDef { font_def_face   = face
            , gs_file_name    = "n022023l.afm"          -- NimbusMonL-ReguObli
            , afm_file_name   = "Courier-Oblique.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Courier-Oblique"
                     , svg_font_family  = "Courier New"
                     , svg_font_style   = SVG_OBLIQUE
                     , font_enc_vector  = standard_encoding
                     }


-- | Courier Bold
-- 
courier_bold :: FontDef
courier_bold =
    FontDef { font_def_face   = face
            , gs_file_name    = "n022004l.afm"          -- NimbusMonL-Bold
            , afm_file_name   = "Courier-Bold.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Courier-Bold"
                     , svg_font_family  = "Courier New"
                     , svg_font_style   = SVG_BOLD
                     , font_enc_vector  = standard_encoding
                     }


-- | Courier Bold Oblique
-- 
courier_bold_oblique :: FontDef
courier_bold_oblique = 
    FontDef { font_def_face   = face
            , gs_file_name    = "n022024l.afm"          -- NimbusMonL-BoldObli
            , afm_file_name   = "Courier-BoldOblique.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Courier-Bold-Oblique"
                     , svg_font_family  = "Courier New"
                     , svg_font_style   = SVG_BOLD_OBLIQUE
                     , font_enc_vector  = standard_encoding
                     }

--------------------------------------------------------------------------------
-- Symbol

-- | Symbol
--
-- Note - Symbol is intentionally not supported for SVG by some 
-- renderers (Firefox). Chrome is fine, but the use of symbol 
-- should be still be avoided for web graphics.
-- 
symbol :: FontDef
symbol = 
    FontDef { font_def_face   = face
            , gs_file_name    = "s050000l.afm"          -- StandardSymL
            , afm_file_name   = "Symbol.afm"
            }
  where
    face =  FontFace { ps_font_name     = "Symbol"
                     , svg_font_family  = "Symbol"
                     , svg_font_style   = SVG_REGULAR
                     , font_enc_vector  = symbol_encoding
                     }






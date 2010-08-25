{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.PostScriptDoc
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh PostScript.
--
--------------------------------------------------------------------------------

module Wumpus.Fresh.PostScriptDoc
  ( 
 
    escapeSpecial
    
  , psHeader
  , epsHeader
  , missingCharCode

  , command
  , ps_comment

  , ps_concat

  , ps_newpath
  , ps_moveto
  , ps_lineto
  , ps_arc
  , ps_curveto
  , ps_closepath

  , ps_fill 
  , ps_stroke
  
  , ps_showpage
  , ps_findfont
  , ps_scalefont
  , ps_setfont
  , ps_show
  , ps_glyphshow

  )  where

import Wumpus.Fresh.BoundingBox
import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.FreshIR
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.TextEncoder
import Wumpus.Fresh.Utils

import Data.Time




--------------------------------------------------------------------------------
-- Escape special chars

-- | Escape these characters:
--
-- > \\ - (, ), <, >, [, ], {, }, /, and %
--
escapeSpecial :: String -> String
escapeSpecial = foldr f "" 
  where
    f c ss | c `elem` ps_special = '\\' : c : ss
           | otherwise           = c : ss

ps_special :: [Char]
ps_special = "\\()<>[]{}/%"



psHeader  :: Int -> ZonedTime -> Doc
psHeader page_count tod = vcat $ 
    [ text "%!PS-Adobe-3.0"
    , text "%%Pages:"         <+> int page_count
    , text "%%CreationDate:"  <+> parens (showsDoc $ psTimeStamp tod)                
    , text "%%EndComments"                  
    ]


epsHeader :: PSUnit u => BoundingBox u -> ZonedTime -> Doc
epsHeader bb tod = vcat $ 
    [ text "%!PS-Adobe-3.0 EPSF-3.0"
    , text "%%BoundingBox:"   <+> upint llx <+> upint lly
                              <+> upint urx <+> upint ury
    , text "%%CreationDate:"  <+> parens (showsDoc $ psTimeStamp tod)                
    , text "%%EndComments"                  
    ]
  where
    upint             = text . roundup . toDouble
    (llx,lly,urx,ury) = destBoundingBox bb 


missingCharCode :: CharCode -> GlyphName -> Doc
missingCharCode i fallback = vcat $
    [ ps_comment $ "missing lookup for &#" ++ show i ++ ";" 
    , ps_glyphshow fallback
    ]

--------------------------------------------------------------------------------

formatArray :: (a -> Doc) -> [a] -> Doc
formatArray _  [] = brackets space
formatArray fn xs = brackets (separate space $ map fn xs)

command :: String -> [Doc] -> Doc
command cmd [] = text cmd
command cmd ds = hsep ds <+> text cmd 


-- | @ %% ... @
--
ps_comment :: String -> Doc
ps_comment ss = text "%%" <+> text ss

--------------------------------------------------------------------------------
-- coordinate system and matrix operators 


-- Note - Do not use @setmatrix@ for changing the CTM use 
-- @concat@ instead.
--
-- Using concat is harmonious with /nesting/.
-- 

-- | @ [... ... ... ... ... ...] concat @
--
ps_concat :: PSUnit u => Matrix3'3 u -> Doc
ps_concat mtrx = doc <+> text  "concat"
  where 
    (a,b,c,d,e,f) = deconsMatrix mtrx
    doc           = formatArray dtruncFmt [a,b,c,d,e,f]

--------------------------------------------------------------------------------
-- Path construction operators

-- | @ newpath @
--
ps_newpath :: Doc
ps_newpath = command "newpath" []

-- Note - it is apparently preferable to show doubles as 0.0 
-- rather than 0.
--
-- I have read that in PostScript the coercion from int to float
-- is apparently quite expensive (I ought to locate the reference 
-- for this...).

-- | @ ... ... moveto @
--
ps_moveto :: PSUnit u => Point2 u -> Doc
ps_moveto (P2 x y) = command "moveto" [dtruncFmt x, dtruncFmt y]


-- | @ ... ... lineto @
--
ps_lineto :: PSUnit u => Point2 u -> Doc
ps_lineto (P2 x y) = command "lineto" [dtruncFmt x, dtruncFmt y]


-- | @ ... ... ... ... ... arc @
--
ps_arc :: PSUnit u => Point2 u -> u -> Radian -> Radian -> Doc
ps_arc (P2 x y) radius ang1 ang2 = 
    command "arc" $ [ dtruncFmt x
                    , dtruncFmt y
                    , dtruncFmt radius
                    , dtruncFmt $ fromR ang1
                    , dtruncFmt $ fromR ang2
                    ]
  where
    fromR :: Radian -> Double
    fromR = fromRadian

-- | @ ... ... ... ... ... ... curveto @
--
ps_curveto :: PSUnit u => Point2 u -> Point2 u -> Point2 u -> Doc
ps_curveto (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) =
    command "curveto" $ map dtruncFmt [x1,y1, x2,y2, x3,y3]

-- | @ closepath @
--
ps_closepath :: Doc
ps_closepath = command "closepath" []

--------------------------------------------------------------------------------
--  painting operators

-- | @ fill @
--
ps_fill :: Doc
ps_fill = command "fill" []

-- | @ stroke @
--
ps_stroke :: Doc
ps_stroke = command "stroke" []

--------------------------------------------------------------------------------
-- Output operators

-- | @ showpage @
ps_showpage :: Doc
ps_showpage = command "showpage" []


--------------------------------------------------------------------------------
-- Character and font operators

-- | The following fonts are expected to exist on most platforms:
--
-- > Times-Roman  Times-Italic  Times-Bold  Times-BoldItalic
-- > Helvetica  Helvetica-Oblique  Helvetica-Bold  Helvetica-Bold-Oblique
-- > Courier  Courier-Oblique  Courier-Bold  Courier-Bold-Oblique
-- > Symbol
--
-- List taken from Bill Casselman \'Mathematical Illustrations\' p279.
--


-- | @ /... findfont @
--
ps_findfont :: String -> Doc
ps_findfont ss = command "findfont" [text $ '/':ss]

-- | @ ... scalefont @
--
ps_scalefont :: Int -> Doc
ps_scalefont sz = command "scalefont" [int sz]

-- | @ setfont @
--
ps_setfont :: Doc
ps_setfont = command "setfont" []

-- | @ (...) show  @
--
ps_show :: String -> Doc
ps_show ss = command "show" [text ss]

-- | @ (/...) show  @
--
ps_glyphshow :: String -> Doc
ps_glyphshow ss = command "glyphshow" [text $ '/':ss]


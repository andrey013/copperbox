{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.SVGDoc
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh SVG.
--
--------------------------------------------------------------------------------

module Wumpus.Fresh.SVGDoc
  (

  
    svg_elem_path

  , svg_path_m
  , svg_path_l
  , svg_path_c

  ) where

import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.Utils


-- Note - nesting is vital for XML output, if you\'re thinking 
-- of relacing xml-light...

type SvgElem = Doc

type SvgAttr = Doc

svgElem :: String -> [SvgAttr] -> SvgElem
svgElem name []    = angles (text name)
svgElem name attrs = angles (text name <+> hsep attrs <+> char '/')

svgElemB :: String -> [SvgAttr] -> [SvgElem] -> SvgElem
svgElemB name attrs elems = vcat [ open, indentLines 2 elems, close ]
  where
    open  = if null attrs then angles (text name)
                          else angles (text name <+> hsep attrs)
    close = angles (char '/' <> text name)


svg_elem_path :: [Doc] -> Doc -> SvgElem
svg_elem_path _attrs _path = undefined

--------------------------------------------------------------------------------
-- Path Segments, encoded as string values.


-- | @ M ... ... @
--
-- c.f. PostScript's @moveto@.
--
svg_path_m :: PSUnit u => Point2 u -> Doc
svg_path_m (P2 x y) = char 'M' <+> dtruncFmt x <+> dtruncFmt y

-- | @ L ... ... @
--
-- c.f. PostScript's @lineto@.
--
svg_path_l :: PSUnit u => Point2 u -> Doc
svg_path_l (P2 x y) = char 'L' <+> dtruncFmt x <+> dtruncFmt y

-- | @ S ... ... ... ... ... ... @
-- 
-- c.f. PostScript's @curveto@.
--
svg_path_c :: PSUnit u => Point2 u -> Point2 u -> Point2 u -> Doc
svg_path_c (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) =
    char 'C' <+> dtruncFmt x1 <+> dtruncFmt y1
             <+> dtruncFmt x2 <+> dtruncFmt y2
             <+> dtruncFmt x3 <+> dtruncFmt y3




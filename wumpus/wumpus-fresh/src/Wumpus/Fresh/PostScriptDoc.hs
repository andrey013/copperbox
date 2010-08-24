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
    psHeader
  , epsHeader

  , command

  , ps_newpath
  , ps_arc
  , ps_closepath

  , ps_fill 
  , ps_stroke

  )  where

import Wumpus.Fresh.BoundingBox
import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.Utils

import Data.Time



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






command :: String -> [String] -> Doc
command cmd [] = text cmd
command cmd xs = hsep (map text xs) <+> text cmd 


--------------------------------------------------------------------------------
-- Path construction operators

-- | @ newpath @
--
ps_newpath :: Doc
ps_newpath = command "newpath" []


-- | @ ... ... ... ... ... arc @
--
ps_arc :: PSUnit u => u -> u -> u -> Radian -> Radian -> Doc
ps_arc x y r ang1 ang2 = 
    command "arc" $ [ dtrunc x, dtrunc y, dtrunc r
                    , dtrunc $ fromR ang1
                    , dtrunc $ fromR ang2
                    ]
  where
    fromR :: Radian -> Double
    fromR = fromRadian


-- | @ closepath @
ps_closepath :: Doc
ps_closepath = command "closepath" []

--------------------------------------------------------------------------------
--  painting operators

-- | @ fill @
ps_fill :: Doc
ps_fill = command "fill" []

-- | @ stroke @
ps_stroke :: Doc
ps_stroke = command "stroke" []
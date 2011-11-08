{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.PDDoc
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Gen monad
--
--------------------------------------------------------------------------------


module PDSS.Core.PDDoc
  ( 

    rec_array
  , rec_array_data
  , rec_canvas0
  , rec_canvas
  , rec_connect
  , rec_coords
  , rec_floatatom0
  , rec_obj
  , rec_bang
  , rec_cnv
  , rec_text
  


  ) where 


import PDSS.Core.Colour
import PDSS.Core.InternalTypes
import PDSS.Core.Utils.Common
import PDSS.Core.Utils.FormatCombinators

record :: Char -> String -> [Doc] -> Doc
record ch ss params = 
    char '#' <> char ch <+> text ss <+> hsep params <> char ';'

recA :: [Doc] -> Doc
recA params = char '#' <> hsep params <> char ';'

recN :: String -> [Doc] -> Doc
recN = record 'N'

recX :: String -> [Doc] -> Doc
recX = record 'X'

intBool :: Bool -> Doc 
intBool False = int 0
intBool True  = int 1

mbEmpty :: Maybe String -> Doc
mbEmpty = maybe (text "empty") text

mbDash :: Maybe String -> Doc
mbDash = maybe (char '-') text

posn :: LabelPosition -> Doc
posn LEFT   = int 0
posn RIGHT  = int 1
posn TOP    = int 2
posn BOTTOM = int 3

font :: Font -> Doc
font COURIER   = int 0
font HELVETICA = int 1
font TIMES     = int 2

sendRecvLabelE :: SRL -> Doc
sendRecvLabelE srl = 
  hsep $ map mbEmpty $ [ srl_send srl, srl_receive srl, srl_label srl ]

sendRecvLabelD :: SRL -> Doc
sendRecvLabelD srl = 
  hsep $ map mbDash $ [ srl_send srl, srl_receive srl, srl_label srl ]


-- This is wrong - different ojects display different props.
--
displayProps :: DisplayProps -> Doc
displayProps props = 
   font (obj_font props) <+> int (obj_fontsize props) 
                         <+> int (rgbValue $ obj_bgcolour props)
                         <+> int (rgbValue $ obj_fgcolour props)
                         <+> int (rgbValue $ obj_lblcolour props)
  

--   int [font] [fontsize] [bg_color] [fg_color] [label_color] 

--------------------------------------------------------------------------------
-- Print records

rec_array :: String -> Int -> Bool -> Doc
rec_array ss sz save = 
    recX "array" [text ss, int sz, text "float", intBool save]

rec_array_data :: [Double] -> Doc
rec_array_data = recA . map dtruncFmt

-- | Top level canvas.
--
rec_canvas0 :: Int -> Int -> Int -> Int -> Int -> Doc
rec_canvas0  x y w h font_size = 
    recN "canvas" (map int [x,y,w,h,font_size])

-- | Nested canvas.
--
rec_canvas :: Int -> Int -> Int -> Int -> String -> Bool -> Doc
rec_canvas  x y w h name ool = 
    recN "canvas" [int x, int y, int w, int h, text name, intBool ool]

rec_connect :: Int -> Int-> Int-> Int -> Doc
rec_connect src out dst inn = 
    recN "canvas" (map int [src, out, dst, inn])

rec_coords :: Int -> Int -> Int -> Int -> Int -> Int -> Bool -> Doc
rec_coords xfrom yto xto yfrom w h gop = 
    recN "coords" [ int xfrom, int yto, int xto, int yfrom
                  , int w, int h, intBool gop ]

rec_floatatom0 :: Int -> Int -> Int -> Int -> Double -> Double -> Doc
rec_floatatom0 x y w h dmin dmax =
    recX "floatatom" [ int x, int y, int w, int h
                     , dtruncFmt dmin, dtruncFmt dmax
                     , posn LEFT
                     , mbDash Nothing, mbDash Nothing, mbDash Nothing ]


rec_obj :: Int -> Int -> String -> [Doc] -> Doc
rec_obj x y name params = 
    recX "obj" (int x : int y : text name : params)


rec_bang :: Int -> Int 
         -> Int -> Integer -> Int -> Int 
         -> SRL -> Int -> Int -> DisplayProps -> Doc
rec_bang x y sz hold interrupt dflt srl xoff yoff props = 
    rec_obj x y "bng" [ int sz, integer hold, int interrupt, int dflt
                      , sendRecvLabelE srl
                      , int xoff,   int yoff
                      , displayProps props ]

rec_cnv :: Int -> Int 
        -> Int -> Int -> Int  
        -> SRL -> Int -> Int -> DisplayProps -> Doc
rec_cnv x y sz w h srl xoff yoff props = 
    rec_obj x y "cnv" [ int sz,     int w,      int h
                      , sendRecvLabelE srl
                      , int xoff,   int yoff
                      , font $ obj_font props
                      , int $ obj_fontsize props
                      , int $ rgbValue $ obj_bgcolour props
                      , int $ rgbValue $ obj_lblcolour props
                      , int 0 ]



-- | Note - semicolon needs manually escaping with a backslash.
--
rec_text :: Int -> Int -> String -> Doc
rec_text x y ss = recX "text" [int x, int y, text ss]



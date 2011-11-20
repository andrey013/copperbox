{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.PdDoc
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Doc builders.
--
--------------------------------------------------------------------------------


module PDSS.Core.PdDoc
  ( 

    rec_array
  , rec_array_data
  , rec_canvas0
  , rec_canvas
  , rec_connect
  , rec_coords
  , rec_msg
  , rec_floatatom
  , rec_obj
  , rec_bang
  , rec_toggle
  , rec_vradio
  , rec_hradio 
  , rec_vu
  , rec_cnv
  , rec_restore0
  , rec_restore
  , rec_symbolatom
  , rec_text
  


  ) where 


import PDSS.Core.Colour
import PDSS.Core.InternalTypes
import PDSS.Core.Utils.Common
import PDSS.Core.Utils.FormatCombinators

record :: Char -> String -> [Doc] -> Doc
record ch ss params = 
    char '#' <> char ch <+> string ss <+> hsep params <> char ';'

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
mbEmpty = maybe (string "empty") string

mbDash :: Maybe String -> Doc
mbDash = maybe (char '-') string

posn :: LabelPosition -> Doc
posn LEFT   = int 0
posn RIGHT  = int 1
posn TOP    = int 2
posn BOTTOM = int 3

fontface :: FontFace -> Doc
fontface COURIER   = int 0
fontface HELVETICA = int 1
fontface TIMES     = int 2

sendD :: SRL -> Doc
sendD = mbDash . srl_send

recvD :: SRL -> Doc
recvD = mbDash . srl_recv

labelD :: SRL -> Doc
labelD = mbDash . srl_label

sendE :: SRL -> Doc
sendE = mbEmpty . srl_send

recvE :: SRL -> Doc
recvE = mbEmpty . srl_recv

labelE :: SRL -> Doc
labelE = mbEmpty . srl_label


rgbDoc :: RGBi -> Doc
rgbDoc = int . rgbValue



--------------------------------------------------------------------------------
-- Print records

-- | Array
--
-- > #X array [2 params] float [1 param]
-- 
-- Note - PD file format ref is out of date for the save flag.
-- 
rec_array :: String -> Int -> Int -> Doc
rec_array ss sz save = 
    recX "array" [string ss, int sz, string "float", int save]



-- | Array data
--
-- > #A [... params]
-- 
-- Encoding of Doubles needs looking at.
-- 
rec_array_data :: [Double] -> Doc
rec_array_data = recA . map dtruncFmt

-- | Canvas - top level canvas.
--
--
-- > #N canvas [5 params]
-- 
-- Double is probably encoded ans an Int... needs checking.
rec_canvas0 :: Int -> Int -> Int -> Int -> Int -> Doc
rec_canvas0 x y w h font_size = 
    recN "canvas" [ int x
                  , int y
                  , int w
                  , int h
                  , int font_size
                  ]

-- | Canvas - nested canvas
--
-- > #N canvas [6 params]
-- 
-- Double is probably encoded ans an Int... needs checking.
--
rec_canvas :: Int -> Int -> Int -> Int -> String -> Bool -> Doc
rec_canvas  x y w h name ool = 
    recN "canvas" [ int x
                  , int y
                  , int w
                  , int h
                  , string name
                  , intBool ool
                  ]
-- | Connect
-- 
-- > #X connect [4 params]
--
rec_connect :: Int -> Int-> Int-> Int -> Doc
rec_connect src out dst inn = 
    recX "connect" [ int src
                   , int out
                   , int dst
                   , int inn
                   ]
-- | Coords
--
-- > #X coords [7 params]
--
rec_coords :: Int -> Int -> Int -> Int -> Int -> Int -> Bool -> Doc
rec_coords xfrom yto xto yfrom w h gop = 
    recX "coords" [ int xfrom
                  , int yto
                  , int xto
                  , int yfrom
                  , int w
                  , int h
                  , intBool gop 
                  ]

-- | Floatatom
--
-- > #X floatatom [9 params]
-- 
-- Double is probably encoded ans an Int... needs checking.
--
rec_floatatom :: Int -> Int -> Int -> Double -> Double 
              -> LabelPosition -> SRL -> Doc
rec_floatatom x y w dmin dmax lpos srl =
    recX "floatatom" [ int x
                     , int y
                     , int w
                     , dtruncFmt dmin
                     , dtruncFmt dmax
                     , posn lpos
                     , labelD srl
                     , recvD srl
                     , sendD srl
                     ]


-- | Msg
-- 
-- #X msg [2 params] [... params]
-- 
-- Variable number of args - transmitted as list of Doc.
--
rec_msg :: Int -> Int -> [Doc] -> Doc
rec_msg x y args = recX "msg" (int x : int y : args)
               




-- | Bang
--
-- > #X obj [2 params] bng [14 params]
-- 
rec_bang :: Int -> Int -> Int -> Integer -> Int -> Int 
         -> SRL -> Int -> Int -> DisplayProps -> Doc
rec_bang x y sz hold interrupt dflt srl xoff yoff props = 
    rec_obj x y "bng" [ int sz
                      , integer hold
                      , int interrupt
                      , int dflt
                      , sendE srl
                      , recvE srl
                      , labelE srl
                      , int xoff
                      , int yoff
                      , fontface $ obj_fontface props
                      , int $ obj_fontsize props
                      , rgbDoc $ obj_bgcolour props
                      , rgbDoc $ obj_fgcolour props
                      , rgbDoc $ obj_lblcolour props
                      ]


-- | Toggle
-- 
-- > #X obj [2 params] tgl [14 params]
-- 
rec_toggle :: Int -> Int -> Int -> Int
           -> SRL -> Int -> Int -> DisplayProps -> Int -> Int -> Doc
rec_toggle x y sz init_load srl xoff yoff props init_val dflt_val= 
    rec_obj x y "tgl" [ int sz
                      , int init_load
                      , sendE srl
                      , recvE srl
                      , labelE srl
                      , int xoff
                      , int yoff
                      , fontface $ obj_fontface props
                      , int $ obj_fontsize props
                      , rgbDoc $ obj_bgcolour props
                      , rgbDoc $ obj_fgcolour props
                      , rgbDoc $ obj_lblcolour props
                      , int init_val
                      , int dflt_val
                      ]

-- | Vradio
-- 
-- > #X obj [2 params] tgl [15 params]
-- 
rec_vradio :: Int -> Int -> Int -> Bool -> Int -> Int
           -> SRL -> Int -> Int -> DisplayProps -> Int -> Doc
rec_vradio x y sz new_old init_load num srl xoff yoff props dflt_val = 
    rec_obj x y "vradio" [ int sz
                         , intBool new_old
                         , int init_load
                         , int num
                         , sendE srl
                         , recvE srl
                         , labelE srl
                         , int xoff
                         , int yoff
                         , fontface $ obj_fontface props
                         , int $ obj_fontsize props
                         , rgbDoc $ obj_bgcolour props
                         , rgbDoc $ obj_fgcolour props
                         , rgbDoc $ obj_lblcolour props
                         , int dflt_val
                         ]



-- | Hradio
-- 
-- > #X obj [2 params] tgl [15 params]
-- 
rec_hradio :: Int -> Int -> Int -> Bool -> Int -> Int
           -> SRL -> Int -> Int -> DisplayProps -> Int -> Doc
rec_hradio x y sz new_old init_load num srl xoff yoff props dflt_val = 
    rec_obj x y "hradio" [ int sz
                         , intBool new_old
                         , int init_load
                         , int num
                         , sendE srl
                         , recvE srl
                         , labelE srl
                         , int xoff
                         , int yoff
                         , fontface $ obj_fontface props
                         , int $ obj_fontsize props
                         , rgbDoc $ obj_bgcolour props
                         , rgbDoc $ obj_fgcolour props
                         , rgbDoc $ obj_lblcolour props
                         , int dflt_val
                         ]


-- | VU meter
-- 
-- #X obj [2 params] vu [12 params]
--
rec_vu :: Int -> Int 
       -> Int -> Int
       -> SRL -> Int -> Int -> DisplayProps -> Bool -> Doc
rec_vu x y w h srl xoff yoff props logbool = 
    rec_obj x y "vu" [ int w
                     , int h
                     , sendE srl
                     , recvE srl
                     , labelE srl
                     , int xoff
                     , int yoff
                     , fontface $ obj_fontface props
                     , int $ obj_fontsize props
                     , rgbDoc $ obj_bgcolour props
                     , rgbDoc $ obj_lblcolour props
                     , intBool logbool
                     ]



-- | Canvas
-- 
-- > #X obj [2 params] cnv [13 params]
-- 
rec_cnv :: Int -> Int -> Int -> Int -> Int  
        -> SRL -> Int -> Int -> DisplayProps -> Doc
rec_cnv x y sz w h srl xoff yoff props = 
    rec_obj x y "cnv" [ int sz
                      , int w
                      , int h
                      , sendE srl
                      , recvE srl
                      , labelE srl
                      , int xoff
                      , int yoff
                      , fontface $ obj_fontface props
                      , int $ obj_fontsize props
                      , rgbDoc $ obj_bgcolour props
                      , rgbDoc $ obj_lblcolour props
                      , int 0 
                      ]

-- | Obj
-- 
-- > #obj [3 params] [... params]
--
rec_obj :: Int -> Int -> String -> [Doc] -> Doc
rec_obj x y name params = 
    recX "obj" (int x : int y : string name : params)


-- | Restore
-- 
-- > #X restore [2 params] pd [1 param]
--
rec_restore0 :: Int -> Int -> String -> Doc
rec_restore0 x y ss = rec_restore x y "pd" ss


-- | Restore
-- 
-- > #X restore [4 params]
--
rec_restore :: Int -> Int -> String -> String -> Doc
rec_restore x y typ ss = 
    recX "restore" [ int x
                   , int y
                   , string typ
                   , string ss
                   ]


-- | Symbolatom
--
-- > #X symbolatom [9 params]
-- 
-- dmax and dmin might not be Doubles - needs checking.
--
rec_symbolatom :: Int -> Int -> Int -> Double -> Double 
               -> LabelPosition -> SRL -> Doc
rec_symbolatom x y w dmin dmax lpos srl =
    recX "symbolatom" [ int x
                      , int y
                      , int w
                      , dtruncFmt dmin
                      , dtruncFmt dmax
                      , posn lpos
                      , labelD srl
                      , recvD srl
                      , sendD srl
                      ]


-- | Text
-- 
-- > #X text [3 params]
--
-- Note - semicolon needs manually escaping with a backslash.
--
rec_text :: Int -> Int -> String -> Doc
rec_text x y ss = 
    recX "text" [ int x
                , int y
                , string ss
                ]



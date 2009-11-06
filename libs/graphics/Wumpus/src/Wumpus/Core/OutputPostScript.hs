{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.OutputPostScript
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.OutputPostScript 
  ( 
  -- * Font loading information
    FontSpec
  
  -- * Output PostScript
  , writePS
  , writeEPS
  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.Picture
import Wumpus.Core.PostScript
import Wumpus.Core.Utils

import Control.Monad ( mapM_, zipWithM_ )

-- | FontSpec = (font-name,font-size)
--
-- The following fonts are expected to exist on most platforms:
--
-- > Times-Roman  Times-Italic  Times-Bold  Times-Bolditalic
-- > Helvetica  Helvetica-Oblique  Helvetica-Bold  Helvetica-Bold-Oblique
-- > Courier  Courier-Oblique  Courier-Bold  Courier-Bold-Oblique
-- > Symbol
--
-- List from Bill Casselman \'Mathematical Illustrations\' p279.

type FontSpec = (String,Int)



--------------------------------------------------------------------------------
-- Render to PostScript

-- | Write a series of pictures to a Postscript file. Each 
-- picture will be printed on a separate page. 
--
-- If the picture contains text labels, you should provide a 
-- FontSpec to transmit @findfont@, @scalefont@ etc. commands 
-- to PostScript.    
writePS :: FilePath -> Maybe FontSpec -> [Picture Double] -> IO ()
writePS filepath mbFs pic = do 
    timestamp <- mkTimeStamp
    writeFile filepath $ psDraw timestamp mbFs pic

-- | Write a picture to an EPS (Encapsulated PostScript) file. 
-- The .eps file can then be imported or embedded in another 
-- document.
--
-- If the picture contains text labels, you should provide a 
-- FontSpec to transmit @findfont@, @scalefont@ etc. commands 
-- to PostScript.    
writeEPS :: FilePath -> Maybe FontSpec -> Picture Double -> IO ()
writeEPS filepath mbFs pic = do
    timestamp <- mkTimeStamp
    writeFile filepath $ epsDraw timestamp mbFs pic

-- | Draw a picture, generating PostScript output.
psDraw :: String -> Maybe FontSpec -> [Picture Double] -> PostScript
psDraw timestamp mbFs pics = runWumpus $ do
    psHeader 1 timestamp
    zipWithM_ (psDrawPage mbFs) pages pics
    psFooter
  where
    pages = map (\i -> (show i,i)) [1..]


psDrawPage :: Maybe FontSpec -> (String,Int) -> Picture Double -> WumpusM ()
psDrawPage mbFs (lbl,ordinal) pic = do
    dsc_Page lbl ordinal
    ps_gsave
    optFontSpec mbFs 
    cmdtrans
    outputPicture pic
    ps_grestore
    ps_showpage
  where
    bb0       = if nullPicture pic then BBox zeroPt zeroPt 
                                   else extractBounds pic
    (mbTx,_)  = translateBBox bb0
    cmdtrans  = maybe (return ()) (\(x,y) -> ps_translate x y) mbTx
  


-- Note the bounding box may have negative components - if it 
-- does it will need translating.

epsDraw :: String -> Maybe FontSpec -> Picture Double -> PostScript
epsDraw timestamp mbFs pic = runWumpus $ do 
    epsHeader bb timestamp      
    ps_gsave
    optFontSpec mbFs
    cmdtrans
    outputPicture pic
    ps_grestore
    epsFooter  
  where
    bb0       = if nullPicture pic then BBox zeroPt zeroPt 
                                   else extractBounds pic
    (mbTx,bb) = translateBBox bb0
    cmdtrans  = maybe (return ()) (\(x,y) -> ps_translate x y) mbTx
     
optFontSpec :: Maybe FontSpec -> WumpusM ()
optFontSpec Nothing          = return ()
optFontSpec (Just (name,sz)) = do 
    ps_findfont name
    ps_scalefont sz
    ps_setfont

psHeader :: Int -> String -> WumpusM ()
psHeader pagecount timestamp = do
    bang_PS
    dsc_Pages pagecount
    dsc_CreationDate $ bracketString timestamp
    dsc_EndComments


epsHeader :: BoundingBox Double -> String -> WumpusM ()
epsHeader bb timestamp = do
    bang_EPS
    dsc_BoundingBox llx lly urx ury
    dsc_CreationDate $ bracketString timestamp
    dsc_EndComments
  where
    (llx,lly,urx,ury) = getBounds bb

getBounds :: Num u => BoundingBox u -> (u,u,u,u)
getBounds ZeroBB                           = (0,0,0,0)
getBounds (BBox (P2 llx lly) (P2 urx ury)) = (llx,lly,urx,ury)

psFooter :: WumpusM ()
psFooter = dsc_EOF


epsFooter :: WumpusM ()
epsFooter = do
    ps_showpage
    dsc_EOF

-- Create margins at the left and bottom of 4 points...


-- | outputPicture 
-- Frame changes, representing scalings translation, rotations...
-- are drawn when they are encountered as a @concat@ statement in a 
-- block of @gsave ... grestore@.

outputPicture :: Picture Double -> WumpusM ()
outputPicture Empty                     = return ()
outputPicture (Single (fr,_) prim)      = 
    updateFrame fr $ outputPrimitive prim
outputPicture (Multi (fr,_) ps)         = 
    updateFrame fr $ mapM_ outputPrimitive ps
outputPicture (Picture (fr,_) a b)      = do
    updateFrame fr $ outputPicture  a
    updateFrame fr $ outputPicture  b
outputPicture (Clip (fr,_) cp p)        = 
    updateFrame fr $ do { clipPath cp ; outputPicture p }


updateFrame :: Frame2 Double -> WumpusM () -> WumpusM ()
updateFrame frm ma 
  | standardFrame frm = ma
  | otherwise         = do { ps_gsave
                           ; ps_concat $ toCTM frm
                           ; ma
                           ; ps_grestore 
                           }



outputPrimitive :: Primitive Double -> WumpusM ()
outputPrimitive (Path1 (c,dp) p)          = outputPath dp c p 
outputPrimitive (Label1 props l)          = updateFont props $ outputLabel l
outputPrimitive (Ellipse1 (c,dp) ct w h)  = outputEllipse dp c ct w h

updateFont :: LabelProps -> WumpusM () -> WumpusM ()
updateFont (c,fnt) ma = do 
    ps_gsave
    colourCommand c
    fontCommand fnt
    ma
    ps_grestore
    

-- TODO pass an enviroment and only write the colour change if it
-- is different to the current colour.
updateColour :: PSColour -> WumpusM () -> WumpusM ()
updateColour c ma = do 
    ps_gsave
    colourCommand c
    ma
    ps_grestore
    


fontCommand :: FontAttr -> WumpusM ()
fontCommand (FontAttr name _ sz) = do
    ps_findfont name
    ps_scalefont sz
    ps_setfont


colourCommand :: PSColour -> WumpusM ()
colourCommand (PSRgb r g b) = ps_setrgbcolor r g b
colourCommand (PSHsb h s v) = ps_sethsbcolor h s v
colourCommand (PSGray a)    = ps_setgray a

    
outputPath :: DrawProp -> PSColour -> Path Double -> WumpusM ()
outputPath CFill        c p = updateColour c $ do  
    startPath p
    ps_closepath
    ps_fill

outputPath (CStroke xs) c p = updatePen c xs $ do
    startPath p
    ps_closepath
    ps_stroke

outputPath (OStroke xs) c p = updatePen c xs $ do
    startPath p
    ps_stroke
  

startPath :: Path Double -> WumpusM ()
startPath (Path (P2 x y) xs) = do
    ps_newpath
    ps_moveto x y
    mapM_ outputPathSeg xs



clipPath :: Path Double -> WumpusM ()
clipPath p = do 
    startPath p
    ps_closepath
    ps_clip



updatePen :: PSColour -> [StrokeAttr] -> WumpusM () -> WumpusM ()
updatePen c xs ma = do 
    ps_gsave
    colourCommand c
    mapM_ penCommand xs
    ma
    ps_grestore

penCommand :: StrokeAttr -> WumpusM ()
penCommand (LineWidth d)    = ps_setlinewidth d
penCommand (MiterLimit d)   = ps_setmiterlimit d
penCommand (LineCap lc)     = ps_setlinecap lc
penCommand (LineJoin lj)    = ps_setlinejoin lj
penCommand (DashPattern dp) = ps_setdash dp


outputPathSeg :: PathSeg Double -> WumpusM ()
outputPathSeg (PLine (P2 x y))  = ps_lineto x y
outputPathSeg (PCurve p1 p2 p3) = ps_curveto x1 y1 x2 y2 x3 y3 
  where
    P2 x1 y1 = p1
    P2 x2 y2 = p2
    P2 x3 y3 = p3

-- | This is not very good as it uses a PostScript's
-- @scale@ operator - this will vary the line width during the
-- drawing of a stroked ellipse.
outputEllipse :: DrawProp 
              -> PSColour 
              -> Point2 Double 
              -> Double 
              -> Double 
              -> WumpusM ()
outputEllipse dp c (P2 x y) w h 
    | w==h      = outputArc dp c x y w
    | otherwise = do { ps_gsave
                     ; ps_scale 1 (h/w) -- Not so good -- changes stroke width
                     ; outputArc dp c x y w
                     ; ps_grestore
                     }

outputArc ::  DrawProp ->PSColour -> Double -> Double -> Double -> WumpusM ()
outputArc CFill        c x y r = updateColour c $ do 
    ps_arc x y r 0 360 
    ps_closepath
    ps_fill

outputArc (CStroke xs) c x y r = updatePen c xs $ do 
    ps_arc x y r 0 360 
    ps_closepath
    ps_stroke

outputArc (OStroke xs) c x y r = updatePen c xs $ do 
    ps_arc x y r 0 360 
    ps_stroke


outputLabel :: Label Double -> WumpusM ()
outputLabel (Label (P2 x y) str) = do
    ps_moveto x y
    ps_show str




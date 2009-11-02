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

module Wumpus.Core.OutputPostScript where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Geometry
import Wumpus.Core.Picture
import Wumpus.Core.PostScript
import Wumpus.Core.Utils

import Control.Monad ( zipWithM_ )
import qualified Data.Foldable  as F

type FontSpec = (String,Int)



--------------------------------------------------------------------------------
-- Render to PostScript

 
writePS :: FilePath -> Maybe FontSpec -> [Picture Double] -> IO ()
writePS filepath mbFs pic = do 
    timestamp <- mkTimeStamp
    writeFile filepath $ psDraw timestamp mbFs pic

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
    drawPicture pic
    ps_grestore
    ps_showpage
  where
    bb0       = if nullPicture pic then BBox zeroPt zeroPt 
                                   else extractBounds pic
    (mbTx,_)  = translateBBox bb0
    cmdtrans  = maybe (return ()) (\(x,y) -> ps_translate x y) mbTx
  


-- Note the bounding box may have negative components - if it does
-- it will need tranlating.

epsDraw :: String -> Maybe FontSpec -> Picture Double -> PostScript
epsDraw timestamp mbFs pic = runWumpus $ do 
    epsHeader bb timestamp      
    ps_gsave
    optFontSpec mbFs
    cmdtrans
    drawPicture pic
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
epsHeader (BBox (P2 llx lly) (P2 urx ury)) timestamp = do
    bang_EPS
    dsc_BoundingBox llx lly urx ury
    dsc_CreationDate $ bracketString timestamp
    dsc_EndComments


psFooter :: WumpusM ()
psFooter = dsc_EOF


epsFooter :: WumpusM ()
epsFooter = do
    ps_showpage
    dsc_EOF

-- Create margins at the left and bottom of 4 points...

translateBBox :: BoundingBox Double 
              -> (Maybe (Double,Double), BoundingBox Double)
translateBBox bb@(BBox (P2 llx lly) (P2 urx ury))
    | llx < 4 || lly < 4  = (Just (x,y), BBox ll ur)            
    | otherwise           = (Nothing, bb)
  where 
     x  = 4 - llx
     y  = 4 - lly
     ll = P2 (llx+x) (lly+x)
     ur = P2 (urx+x) (ury+y)  



-- | DrawPicture 
-- Frame changes, representing scalings translation, rotations...
-- are drawn when they are encountered as a @concat@ statement in a 
-- block of @gsave ... grestore@.

drawPicture :: Picture Double -> WumpusM ()
drawPicture Empty                     = return ()
drawPicture (Single (fr,_) prim)      = updateFrame fr $ drawPrimitive prim
drawPicture (Multi (fr,_) ps)         = updateFrame fr $ mapM_ drawPrimitive ps
drawPicture (Picture (fr,_) prop a b) = updatePen prop $ do
    updateFrame fr $ drawPicture  a
    updateFrame fr $ drawPicture  b

updateFrame :: MbFrame Double -> WumpusM () -> WumpusM ()
updateFrame Nothing    ma = ma
updateFrame (Just frm) ma = do
    ps_gsave
    ps_concat $ toCTM frm
    ma
    ps_grestore



bbComment :: BoundingBox Double -> WumpusM ()
bbComment (BBox (P2 x0 y0) (P2 x1 y1)) = 
    ps_comment $ "bounding-box " ++ show (x0,y0) ++ ".." ++ show (x1,y1)

drawPrimitive :: Primitive Double -> WumpusM ()
drawPrimitive (Path1 props p)           = updatePen props $ drawPath p
drawPrimitive (Label1 props l)          = updateFont props $ drawLabel l
drawPrimitive (Ellipse1 (mbc,dp) c w h) = updateColour mbc $ 
                                              drawEllipse dp c w h

updatePen :: PathProps -> WumpusM () -> WumpusM ()
updatePen prop@(mbc,se) ma
    | nullProps prop = ma
    | otherwise      = do { ps_gsave
                          ; optColourCommand mbc
                          ; F.mapM_ penCommand se
                          ; ma
                          ; ps_grestore
                          }

penCommand :: PenAttr -> WumpusM ()
penCommand (LineWidth d)    = ps_setlinewidth d
penCommand (MiterLimit d)   = ps_setmiterlimit d
penCommand (LineCap lc)     = ps_setlinecap lc
penCommand (LineJoin lj)    = ps_setlinejoin lj
penCommand (DashPattern dp) = ps_setdash dp

updateFont :: LabelProps -> WumpusM () -> WumpusM ()
updateFont prop@(mbc,mfnt) ma 
    | nullFontProps prop = ma
    | otherwise          = do { ps_gsave
                              ; optColourCommand mbc
                              ; optFontCommand mfnt
                              ; ma
                              ; ps_grestore
                              }

updateColour :: MbColour -> WumpusM () -> WumpusM ()
updateColour Nothing  ma = ma 
updateColour (Just c) ma = do { ps_gsave
                              ; colourCommand c
                              ; ma
                              ; ps_grestore
                              }



optFontCommand :: Maybe FontAttr -> WumpusM ()
optFontCommand = maybe (return ()) fontCommand

fontCommand :: FontAttr -> WumpusM ()
fontCommand (FontAttr name sz) = ps_findfont name >> ps_scalefont sz >> ps_setfont

optColourCommand :: Maybe PSColour -> WumpusM ()
optColourCommand = maybe (return ()) colourCommand

colourCommand :: PSColour -> WumpusM ()
colourCommand (PSRgb r g b) = ps_setrgbcolor r g b
colourCommand (PSHsb h s v) = ps_sethsbcolor h s v
colourCommand (PSGray a)    = ps_setgray a

    
drawPath :: Path Double -> WumpusM ()
drawPath (Path dp pt xs) = let P2 x y = pt in do  
    ps_newpath
    ps_moveto x y
    mapM_ drawPathSeg xs
    closePath dp   

drawPathSeg :: PathSeg Double -> WumpusM ()
drawPathSeg (PLine p)         = let P2 x y = p in ps_lineto x y
drawPathSeg (PCurve p1 p2 p3) = let P2 x1 y1 = p1
                                    P2 x2 y2 = p2
                                    P2 x3 y3 = p3
                                in ps_curveto x1 y1 x2 y2 x3 y3

-- | Currently this is not very good as it uses a PostScript's
-- @scale@ operator - this will vary the line width during the
-- drawing of a stroked ellipse.
drawEllipse :: DrawProp -> Point2 Double -> Double -> Double -> WumpusM ()
drawEllipse dp (P2 x y) w h 
    | w==h      = drawArc dp x y w
    | otherwise = do { ps_gsave
                     ; ps_scale 1 (h/w) -- Not so good -- changes stroke width
                     ; drawArc dp x y w
                     ; ps_grestore
                     }

drawArc :: DrawProp -> Double -> Double -> Double -> WumpusM ()
drawArc dp x y r = ps_arc x y r 0 360 >> closePath dp


closePath :: DrawProp -> WumpusM ()
closePath OStroke = ps_stroke
closePath CStroke = ps_closepath >> ps_stroke
closePath CFill   = ps_closepath >> ps_fill
closePath CCrop   = ps_closepath >> ps_clip


drawLabel :: Label Double -> WumpusM ()
drawLabel (Label pt str) = let P2 x y = pt in do
    ps_moveto x y
    ps_show str

makeCTM :: Num u => (Point2 u -> Point2 u) -> (u,u,u,u,u,u)
makeCTM f = (x0-o0, x1-o1, y0-o0, y1-o1, o0, o1) where
   P2 x0 x1 = f (P2 1 0)
   P2 y0 y1 = f (P2 0 1)
   P2 o0 o1 = f (P2 0 0)



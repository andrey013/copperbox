{-# OPTIONS -Wall #-}


module AlignPosObject where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red )

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx drawing01
    writeEPS "./out/align_pos_object01.eps" pic1
    writeSVG "./out/align_pos_object01.svg" pic1


std_ctx :: DrawingContext
std_ctx = standardContext 24


drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


mf :: TraceDrawing Double ()
mf = do
    drawl (P2 0   400) $ illustratePosObject rectCenter
    drawl (P2 100 400) $ illustratePosObject rectMinor
    drawl (P2 200 400) $ illustratePosObject rectBl

    drawl (P2   0 350) $ illustratePosObject $ vs1
    drawl (P2 150 350) $ illustratePosObject $ vs2
    drawl (P2 300 350) $ illustratePosObject $ vs3

    drawl (P2   0 200) $ illustratePosObject $ hs1
    drawl (P2 150 200) $ illustratePosObject $ hs2
    drawl (P2 300 200) $ illustratePosObject $ hs3

    drawl (P2   0 150)  $ illustratePosObject $ va1
    drawl (P2 150 150)  $ illustratePosObject $ va2
    drawl (P2 300 150)  $ illustratePosObject $ va3

    drawl (P2   0 0)   $ illustratePosObject $ ha1
    drawl (P2 150 0)   $ illustratePosObject $ ha2
    drawl (P2 300 0)   $ illustratePosObject $ ha3
    return ()
  where
    objs  = [ rectCenter, rectMinor, rectBl ]
    ha1   = alignRow HALIGN_TOP    objs
    ha2   = alignRow HALIGN_CENTER objs
    ha3   = alignRow HALIGN_BASE   objs
    va1   = alignColumn VALIGN_LEFT   objs
    va2   = alignColumn VALIGN_CENTER objs
    va3   = alignColumn VALIGN_RIGHT  objs


    hs1   = alignRowSep HALIGN_TOP    8 objs
    hs2   = alignRowSep HALIGN_CENTER 8 objs
    hs3   = alignRowSep HALIGN_BASE   8 objs 
    vs1   = alignColumnSep VALIGN_LEFT   8 objs
    vs2   = alignColumnSep VALIGN_CENTER 8 objs
    vs3   = alignColumnSep VALIGN_RIGHT  8 objs
 
---

rectCenter :: DPosGraphic
rectCenter = makePosObject (return opos) (mkRectCenter w h)
  where
    w    = 50
    h    = 35
    opos = Orientation { or_x_minor = 0.5 * w
                       , or_x_major = 0.5 * w
                       , or_y_minor = 0.5 * h
                       , or_y_major = 0.5 * h }
 

mkRectCenter :: Double -> Double -> DLocGraphic
mkRectCenter w h = moveStart v1 $ dcRectangle STROKE w h
  where
    v1 = V2 (negate $ 0.5 * w) (negate $ 0.5 * h)
    

rectBl :: DPosGraphic
rectBl = makePosObject (return opos) (mkRectBl w h)
  where
    w    = 40  
    h    = 20
    opos = Orientation { or_x_minor = 0
                       , or_x_major = w
                       , or_y_minor = 0
                       , or_y_major = h }
 

-- start-point - bottom left
mkRectBl :: InterpretUnit u => u -> u -> LocGraphic u
mkRectBl w h = dcRectangle STROKE w h



rectMinor :: DPosGraphic
rectMinor = makePosObject (return opos) (mkRectMinor w h)
  where
    w    = 30 
    h    = 25
    opos = Orientation { or_x_minor = 10
                       , or_x_major = (w-10)
                       , or_y_minor = 10
                       , or_y_major = (h-10) }
 

mkRectMinor :: Double -> Double -> DLocGraphic
mkRectMinor w h = moveStart v1 $ dcRectangle STROKE w h
  where
    v1 = V2 (-10) (-10)

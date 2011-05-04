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
    ha1   = alignRow HTop    objs
    ha2   = alignRow HCenter objs
    ha3   = alignRow HBottom objs
    va1   = alignColumn VLeft   objs
    va2   = alignColumn VCenter objs
    va3   = alignColumn VRight  objs


    hs1   = alignRowSep HTop    8 objs
    hs2   = alignRowSep HCenter 8 objs
    hs3   = alignRowSep HBottom 8 objs 
    vs1   = alignColumnSep VLeft   8 objs
    vs2   = alignColumnSep VCenter 8 objs
    vs3   = alignColumnSep VRight  8 objs
 
---

rectCenter :: (Fractional u, InterpretUnit u) => PosObject u 
rectCenter = makePosObject (return opos) (mkRectCenter w h)
  where
    w    = 50
    h    = 35
    opos = Orientation { or_x_minor = 0.5 * w
                       , or_x_major = 0.5 * w
                       , or_y_minor = 0.5 * h
                       , or_y_major = 0.5 * h }
 

mkRectCenter :: (Fractional u, InterpretUnit u) => u -> u -> LocGraphic u
mkRectCenter w h = moveStart (dispVec v1) $ dcRectangle STROKE w h
  where
    v1 = V2 (negate $ 0.5 * w) (negate $ 0.5 * h)
    

rectBl :: (Fractional u, InterpretUnit u) => PosObject u
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



rectMinor :: (Fractional u, InterpretUnit u) => PosObject u 
rectMinor = makePosObject (return opos) (mkRectMinor w h)
  where
    w    = 30 
    h    = 25
    opos = Orientation { or_x_minor = 10
                       , or_x_major = (w-10)
                       , or_y_minor = 10
                       , or_y_major = (h-10) }
 

mkRectMinor :: InterpretUnit u => u -> u -> LocGraphic u
mkRectMinor w h = moveStart (dispVec v1) $ dcRectangle STROKE w h
  where
    v1 = V2 (-10) (-10)

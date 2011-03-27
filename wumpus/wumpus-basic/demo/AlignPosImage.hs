{-# OPTIONS -Wall #-}


module AlignPosImage where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red )

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx drawing01
    writeEPS "./out/align_pos_image01.eps" pic1
    writeSVG "./out/align_pos_image01.svg" pic1


std_ctx :: DrawingContext
std_ctx = standardContext 24


drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


mf :: TraceDrawing Double ()
mf = do
    drawl (anchor $ P2 0   400) $ illustratePosImage rectCenter
    drawl (anchor $ P2 100 400) $ illustratePosImage rectMinor
    drawl (anchor $ P2 200 400) $ illustratePosImage rectBl

    drawl (anchor $ P2   0 275) $ illustratePosImage $ vs1
    drawl (anchor $ P2 150 275) $ illustratePosImage $ vs2
    drawl (anchor $ P2 300 275) $ illustratePosImage $ vs3

    drawl (anchor $ P2   0 200) $ illustratePosImage $ hs1
    drawl (anchor $ P2 150 200) $ illustratePosImage $ hs2
    drawl (anchor $ P2 300 200) $ illustratePosImage $ hs3

    drawl (anchor $ P2   0 75)  $ illustratePosImage $ va1
    drawl (anchor $ P2 150 75)  $ illustratePosImage $ va2
    drawl (anchor $ P2 300 75)  $ illustratePosImage $ va3

    drawl (anchor $ P2   0 0)   $ illustratePosImage $ ha1
    drawl (anchor $ P2 150 0)   $ illustratePosImage $ ha2
    drawl (anchor $ P2 300 0)   $ illustratePosImage $ ha3
    return ()
  where
    objs  = [ rectCenter, rectMinor, rectBl ]
    ha1   = halignPI emptyPosGraphic HTop    objs
    ha2   = halignPI emptyPosGraphic HCenter objs
    ha3   = halignPI emptyPosGraphic HBottom objs
    va1   = valignPI emptyPosGraphic VLeft   objs
    va2   = valignPI emptyPosGraphic VCenter objs
    va3   = valignPI emptyPosGraphic VRight  objs


    hs1   = halignSepPI emptyPosGraphic HTop    8 objs
    hs2   = halignSepPI emptyPosGraphic HCenter 8 objs
    hs3   = halignSepPI emptyPosGraphic HBottom 8 objs 
    vs1   = valignSepPI emptyPosGraphic VLeft   8 objs
    vs2   = valignSepPI emptyPosGraphic VCenter 8 objs
    vs3   = valignSepPI emptyPosGraphic VRight  8 objs
 
---

rectCenter :: (Fractional u, InterpretUnit u) => PosGraphic u 
rectCenter = makePosImage opos (mkRectCenter w h)
  where
    w    = 50
    h    = 35
    opos = ObjectPos { op_x_minor = 0.5 * w
                     , op_x_major = 0.5 * w
                     , op_y_minor = 0.5 * h
                     , op_y_major = 0.5 * h }
 

mkRectCenter :: (Fractional u, InterpretUnit u) => u -> u -> LocGraphic u
mkRectCenter w h = moveStart (displaceVec v1) $ strokedRectangle w h
  where
    v1 = V2 (negate $ 0.5 * w) (negate $ 0.5 * h)
    

rectBl :: (Fractional u, InterpretUnit u) => PosGraphic u
rectBl = makePosImage opos (mkRectBl w h)
  where
    w    = 40  
    h    = 20
    opos = ObjectPos { op_x_minor = 0
                     , op_x_major = w
                     , op_y_minor = 0
                     , op_y_major = h }
 

-- start-point - bottom left
mkRectBl :: InterpretUnit u => u -> u -> LocGraphic u
mkRectBl w h = strokedRectangle w h



rectMinor :: (Fractional u, InterpretUnit u) => PosGraphic u 
rectMinor = makePosImage opos (mkRectMinor w h)
  where
    w    = 30 
    h    = 25
    opos = ObjectPos { op_x_minor = 10
                     , op_x_major = (w-10)
                     , op_y_minor = 10
                     , op_y_major = (h-10) }
 

mkRectMinor :: InterpretUnit u => u -> u -> LocGraphic u
mkRectMinor w h = moveStart (displaceVec v1) $ strokedRectangle w h
  where
    v1 = V2 (-10) (-10)

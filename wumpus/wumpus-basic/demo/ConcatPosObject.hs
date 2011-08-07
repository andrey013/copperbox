{-# OPTIONS -Wall #-}


module ConcatPosObject where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red )

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx drawing01
    writeEPS "./out/concat_pos_object01.eps" pic1
    writeSVG "./out/concat_pos_object01.svg" pic1


std_ctx :: DrawingContext
std_ctx = standardContext 24


drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


mf :: TraceDrawing Double ()
mf = do
    drawl (P2 0   400) $ ipo rectCenter
    drawl (P2 100 400) $ ipo rectMinor

    drawl (P2   0 300) $ ipo $ valign VALIGN_CENTER rectCenter rectMinor
    drawl (P2 150 300) $ ipo $ valign VALIGN_CENTER rectMinor rectCenter

    drawl (P2 300 300) $ ipo $ valign VALIGN_RIGHT rectCenter rectMinor
    drawl (P2 450 300) $ ipo $ valign VALIGN_RIGHT rectMinor rectCenter


    drawl (P2   0 200) $ ipo $ halign HALIGN_CENTER rectCenter rectMinor
    drawl (P2 150 200) $ ipo $ halign HALIGN_CENTER rectMinor  rectCenter

    drawl (P2 300 200) $ ipo $ valign VALIGN_LEFT rectCenter rectMinor
    drawl (P2 450 200) $ ipo $ valign VALIGN_LEFT rectMinor rectCenter


    drawl (P2   0 100) $ ipo $ halign HALIGN_BASE rectCenter rectMinor
    drawl (P2 150 100) $ ipo $ halign HALIGN_BASE rectMinor rectCenter
    drawl (P2 300 100) $ ipo $ halign HALIGN_TOP  rectCenter rectMinor
    drawl (P2 450 100) $ ipo $ halign HALIGN_TOP  rectMinor  rectCenter


    drawl (P2   0 0) $ ipo $  rectCenter `hconcat` rectMinor
    drawl (P2 150 0) $ ipo $  rectCenter `vconcat` rectMinor
  where
    ipo = illustratePosObject

rectCenter :: PosGraphic Double 
rectCenter = makePosObject (return opos) (mkRectCenter w h)
  where
    w    = 50
    h    = 35
    opos = Orientation { or_x_minor = 0.5 * w
                       , or_x_major = 0.5 * w
                       , or_y_minor = 0.5 * h
                       , or_y_major = 0.5 * h }
 

mkRectCenter :: (Fractional u, InterpretUnit u) => u -> u -> LocGraphic u
mkRectCenter w h = moveStart v1 $ dcRectangle STROKE w h
  where
    v1 = V2 (negate $ 0.5 * w) (negate $ 0.5 * h)
    

rectBl :: PosGraphic Double
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



rectMinor :: PosGraphic Double
rectMinor = makePosObject (return opos) (mkRectMinor w h)
  where
    w    = 30 
    h    = 25
    opos = Orientation { or_x_minor = 10
                       , or_x_major = (w-10)
                       , or_y_minor = 10
                       , or_y_major = (h-10) }
 

mkRectMinor :: InterpretUnit u => u -> u -> LocGraphic u
mkRectMinor w h = moveStart v1 $ dcRectangle STROKE w h
  where
    v1 = V2 (-10) (-10)

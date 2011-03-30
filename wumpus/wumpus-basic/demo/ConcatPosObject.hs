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
    drawl (anchor $ P2 0   400) $ illustratePosObject rectCenter
    drawl (anchor $ P2 100 400) $ illustratePosObject rectMinor

    drawl (anchor $ P2   0 300) $ illustratePosObject $  
                  rectCenter `vcatCenterPO` rectMinor
    drawl (anchor $ P2 150 300) $ illustratePosObject $  
                  rectMinor `vcatCenterPO` rectCenter

    drawl (anchor $ P2 300 300) $ illustratePosObject $  
                  rectCenter `vcatRightPO` rectMinor
    drawl (anchor $ P2 450 300) $ illustratePosObject $  
                  rectMinor `vcatRightPO` rectCenter


    drawl (anchor $ P2   0 200) $ illustratePosObject $  
                  rectCenter `hcatCenterPO` rectMinor
    drawl (anchor $ P2 150 200) $ illustratePosObject $  
                  rectMinor `hcatCenterPO` rectCenter

    drawl (anchor $ P2 300 200) $ illustratePosObject $  
                  rectCenter `vcatLeftPO` rectMinor
    drawl (anchor $ P2 450 200) $ illustratePosObject $  
                  rectMinor `vcatLeftPO` rectCenter


    drawl (anchor $ P2   0 100) $ illustratePosObject $  
                  rectCenter `hcatBottomPO` rectMinor
    drawl (anchor $ P2 150 100) $ illustratePosObject $  
                  rectMinor `hcatBottomPO` rectCenter
    drawl (anchor $ P2 300 100) $ illustratePosObject $  
                  rectCenter `hcatTopPO` rectMinor
    drawl (anchor $ P2 450 100) $ illustratePosObject $  
                  rectMinor `hcatTopPO` rectCenter


    drawl (anchor $ P2   0 0) $ illustratePosObject $  rectCenter `hcatPO` rectMinor
    drawl (anchor $ P2 150 0) $ illustratePosObject $  rectCenter `vcatPO` rectMinor


rectCenter :: (Fractional u, InterpretUnit u) => PosGraphicObject u 
rectCenter = makePosObject (return opos) (mkRectCenter w h)
  where
    w    = 50
    h    = 35
    opos = Orientation { or_x_minor = 0.5 * w
                       , or_x_major = 0.5 * w
                       , or_y_minor = 0.5 * h
                       , or_y_major = 0.5 * h }
 

mkRectCenter :: (Fractional u, InterpretUnit u) => u -> u -> LocGraphic u
mkRectCenter w h = moveStart (displaceVec v1) $ strokedRectangle w h
  where
    v1 = V2 (negate $ 0.5 * w) (negate $ 0.5 * h)
    

rectBl :: (Fractional u, InterpretUnit u) => PosGraphicObject u
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
mkRectBl w h = strokedRectangle w h



rectMinor :: (Fractional u, InterpretUnit u) => PosGraphicObject u 
rectMinor = makePosObject (return opos) (mkRectMinor w h)
  where
    w    = 30 
    h    = 25
    opos = Orientation { or_x_minor = 10
                       , or_x_major = (w-10)
                       , or_y_minor = 10
                       , or_y_major = (h-10) }
 

mkRectMinor :: InterpretUnit u => u -> u -> LocGraphic u
mkRectMinor w h = moveStart (displaceVec v1) $ strokedRectangle w h
  where
    v1 = V2 (-10) (-10)

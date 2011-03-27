{-# OPTIONS -Wall #-}


module ConcatPosImage where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red )

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx drawing01
    writeEPS "./out/concat_pos_image01.eps" pic1
    writeSVG "./out/concat_pos_image01.svg" pic1


std_ctx :: DrawingContext
std_ctx = standardContext 24


drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


mf :: TraceDrawing Double ()
mf = do
    drawl (anchor $ P2 0   400) $ illustratePosImage rectCenter
    drawl (anchor $ P2 100 400) $ illustratePosImage rectMinor

    drawl (anchor $ P2   0 300) $ illustratePosImage $  
                  rectCenter `vcatCenterPI` rectMinor
    drawl (anchor $ P2 150 300) $ illustratePosImage $  
                  rectMinor `vcatCenterPI` rectCenter

    drawl (anchor $ P2 300 300) $ illustratePosImage $  
                  rectCenter `vcatRightPI` rectMinor
    drawl (anchor $ P2 450 300) $ illustratePosImage $  
                  rectMinor `vcatRightPI` rectCenter


    drawl (anchor $ P2   0 200) $ illustratePosImage $  
                  rectCenter `hcatCenterPI` rectMinor
    drawl (anchor $ P2 150 200) $ illustratePosImage $  
                  rectMinor `hcatCenterPI` rectCenter

    drawl (anchor $ P2 300 200) $ illustratePosImage $  
                  rectCenter `vcatLeftPI` rectMinor
    drawl (anchor $ P2 450 200) $ illustratePosImage $  
                  rectMinor `vcatLeftPI` rectCenter


    drawl (anchor $ P2   0 100) $ illustratePosImage $  
                  rectCenter `hcatBottomPI` rectMinor
    drawl (anchor $ P2 150 100) $ illustratePosImage $  
                  rectMinor `hcatBottomPI` rectCenter
    drawl (anchor $ P2 300 100) $ illustratePosImage $  
                  rectCenter `hcatTopPI` rectMinor
    drawl (anchor $ P2 450 100) $ illustratePosImage $  
                  rectMinor `hcatTopPI` rectCenter


    drawl (anchor $ P2   0 0) $ illustratePosImage $  rectCenter `hcatPI` rectMinor
    drawl (anchor $ P2 150 0) $ illustratePosImage $  rectCenter `vcatPI` rectMinor

unPosImage ::  Floating u 
           => Point2 u -> RectPosition -> PosImage2 r u -> Image r u
unPosImage pt rpos img = runPosImage2 rpos img  `at` pt

rectCenter :: (Fractional u, InterpretUnit u) => PosGraphic2 u 
rectCenter = makePosImage2 opos (mkRectCenter w h)
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
    

rectBl :: (Fractional u, InterpretUnit u) => PosGraphic2 u
rectBl = makePosImage2 opos (mkRectBl w h)
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





rectMinor :: (Fractional u, InterpretUnit u) => PosGraphic2 u 
rectMinor = makePosImage2 opos (mkRectMinor w h)
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

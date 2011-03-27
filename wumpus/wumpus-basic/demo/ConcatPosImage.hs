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
    drawl (anchor $ P2 0   500) $ illustratePosImage rectCenter
    drawl (anchor $ P2 100 500) $ illustratePosImage rectMinor

    drawl (anchor $ P2   0 300) $ illustratePosImage $  
                  rectCenter `hcatAlignCenter` rectMinor
    drawl (anchor $ P2 150 300) $ illustratePosImage $  
                  rectMinor `hcatAlignCenter` rectCenter

    drawl (anchor $ P2   0 150) $ illustratePosImage $  
                  rectCenter `hcatAlignBottom` rectMinor
    drawl (anchor $ P2 150 150) $ illustratePosImage $  
                  rectMinor `hcatAlignBottom` rectCenter
    drawl (anchor $ P2 300 150) $ illustratePosImage $  
                  rectCenter `hcatAlignTop` rectMinor
    drawl (anchor $ P2 450 150) $ illustratePosImage $  
                  rectMinor `hcatAlignTop` rectCenter


    drawl (anchor $ P2   0 0) $ illustratePosImage $  rectCenter `hcatPI` rectMinor
    drawl (anchor $ P2 150 0) $ illustratePosImage $  rectCenter `vcatPI` rectMinor

unPosImage ::  Floating u 
           => Point2 u -> RectPosition -> PosImage2 r u -> Image r u
unPosImage pt rpos img = runPosImage2 rpos img  `at` pt

rectCenter :: (Fractional u, InterpretUnit u) => PosGraphic2 u 
rectCenter = makePosImage2 opos (mkRectCenter w h)
  where
    w    = 60
    h    = 50
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
    w    = 50 
    h    = 30
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
    w    = 40 
    h    = 30
    opos = ObjectPos { op_x_minor = 10
                     , op_x_major = (w-10)
                     , op_y_minor = 10
                     , op_y_major = (h-10) }
 

mkRectMinor :: InterpretUnit u => u -> u -> LocGraphic u
mkRectMinor w h = moveStart (displaceVec v1) $ strokedRectangle w h
  where
    v1 = V2 (-10) (-10)

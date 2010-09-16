{-# OPTIONS -Wall #-}

module DotPic where


import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Dots
import Wumpus.Basic.Graphic
import Wumpus.Basic.PictureLanguage

import Wumpus.Core                      -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

pt2 :: Point2 Double
pt2 = P2 100 10


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/dots01.eps" pic
    writeSVG_latin1 "./out/dots01.svg" pic
  where 
    pic :: Picture Double
    pic = extendBoundary 10 10 $
          uniformScale 2       $ 
            vsepA VLeft 10 p1 [p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14
                              ,p15,p16,p17]
    
    p1  = makeDotPic dotHLine       points
    p2  = makeDotPic dotVLine       points
    p3  = makeDotPic dotX           points
    p4  = makeDotPic dotPlus        points   
    p5  = makeDotPic dotCross       points
    p6  = makeDotPic dotDiamond     points
    p7  = makeDotPic dotDisk        points
    p8  = makeDotPic dotSquare      points
    p9  = makeDotPic dotCircle      points
    p10 = makeDotPic dotPentagon    points
    p11 = makeDotPic dotStar        points
    p12 = makeDotPic dotAsterisk    points
    p13 = makeDotPic dotOPlus       points
    p14 = makeDotPic dotOCross      points
    p15 = makeDotPic dotFOCross     points
    p16 = makeDotPic dotFDiamond    points
    p17 = makeDotPic (dotText "AA") points
 
std_ctx :: DrawingContext
std_ctx = secondaryColour peru $ standardContext 12

points :: [Point2 Double]
points = [P2 0 0, P2 32 10, P2 64 0, P2 96 10]


-- Note - order of drawing is may need to change with future
-- revisions.
--
makeDotPic :: (Real u, Floating u, FromPtSize u) 
           => DotLocImage u -> [Point2 u] -> Picture u
makeDotPic dotImg xs = liftToPictureU $ execDrawing std_ctx $ do 
    dashline
    mapM_ (\pt -> drawi $ dotImg `ati` pt) xs
  where
    dashline = localCtx attrUpd (draw $ openStroke $ vertexPath xs)

    attrUpd  :: DrawingContext -> DrawingContext
    attrUpd  =  dashPattern (evenDashes 1) . primaryColour cadet_blue




-- Should these produce a DashPattern or a StrokeAttr?

evenDashes :: Int -> DashPattern 
evenDashes n = Dash 0 [(n,n)]

dashOffset :: Int -> DashPattern -> DashPattern
dashOffset _ Solid       = Solid
dashOffset n (Dash _ xs) = Dash n xs


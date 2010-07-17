{-# OPTIONS -Wall #-}

module DotPic where


import Wumpus.Basic.Dots
import Wumpus.Basic.Graphic
import Wumpus.Basic.SVGColours
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Extra.PictureLanguage

import Data.Maybe
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
            vsepA VLeft 10 p1 [p2,p3,p4,p5] 
    
    p1  = makeDotPic dotHLine   points
    p2  = makeDotPic dotVLine   points
    p3  = makeDotPic dotX       points
    p4  = makeDotPic dotPlus    points   
    p5  = makeDotPic dotCross   points

std_attr :: MarkAttr
std_attr = standardAttr 9

points :: [Point2 Double]
points = [P2 0 0, P2 32 10, P2 64 0, P2 96 10]

makeDotPic :: (Real u, Floating u) 
           => (MarkAttr -> GraphicF u) -> [Point2 u] -> Picture u
makeDotPic fn xs = fromMaybe errK $ drawGraphic $ veloH (fn std_attr) xs . dashline
  where
    dashline = wrapG $ ostroke attr $ vertexPath xs
    attr     = (cadetBlue, DashPattern $ evenDashes 1)


errK :: a
errK = error "no picture"



-- Should these produce a DashPattern or a StrokeAttr?

evenDashes :: Int -> DashPattern 
evenDashes n = Dash 0 [(n,n)]

dashOffset :: Int -> DashPattern -> DashPattern
dashOffset _ Solid       = Solid
dashOffset n (Dash _ xs) = Dash n xs


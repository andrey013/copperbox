{-# OPTIONS -Wall #-}

module ColourChart where

import ColourDefns

import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.Graphic
import Wumpus.Basic.Graphic.Primitive
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Data.List
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/SVGcolours.eps" svg
    writeSVG_latin1 "./out/SVGcolours.svg" svg
    writeEPS_latin1 "./out/X11colours.eps" $ uniformScale 0.75 x11_portrait
    writeSVG_latin1 "./out/X11colours.svg" x11_landscape

svg :: Picture Double
svg = mkPic all_svg_colours (ixDownLeftRight 4 60 (scalePt 160))

x11_landscape :: Picture Double
x11_landscape = mkPic all_x11_colours (ixDownLeftRight 6 60 (scalePt 140))

x11_portrait :: Picture Double
x11_portrait = mkPic all_x11_colours (ixDownLeftRight 5 72 (scalePt 140))
    

-- Note - this is code from an old project that needs tidying up...

mkPic :: [(String,RGBi)] -> [DPoint2] -> DPicture 
mkPic cs pts = drawImageU (standardContext 10) $ mkPrims cs pts
                 

mkPrims :: [(String,RGBi)] -> [DPoint2] -> Image Double 
mkPrims cs pts = concatImg $ zipWith fn cs pts
  where
    fn (name,rgb) pt = supplyPt pt $ colourSample name rgb


concatImg :: [Image u] -> Image u
concatImg []     = error "BAD!"
concatImg (z:zs) = step z zs
  where
    step ac []     = ac
    step ac (e:es) = step (ac `pend` e) es


colourSample :: (Fractional u, FromPtSize u) 
             => String -> RGBi -> CFImage u
colourSample name rgb = localDrawingContext (secondaryColour rgb) $ 
    disperse2 (.+^ vec 12 (-3)) (borderedRectangle 15 10) (textline RECT_LLC name)


disperse2 :: (Point2 u -> Point2 u) -> CFImage u -> CFImage u -> CFImage u
disperse2 upd img1 img2 = \pt ->  img1 pt `pend` img2 (upd pt)    

pend :: Image u -> Image u -> Image u
pend img1 img2 = 
    RImage $ \ctx -> (getRImage img1 ctx) `appendH` (getRImage img2 ctx)

-----------------------------------------------------
-- at some point this will be done by PointSupply...

scalePt :: Num u => u -> Point2 u -> Point2 u
scalePt w (P2 x y) = P2 (x*w) (y*12) 

-- | Generate points in a grid - move down a whole column, move 
-- right one, move down the next column.
-- 
-- Points are generated from count-1 to 0, but can be scaled 
-- or have the offest shifted with the point transformer function.
--
ixDownLeftRight :: (Num u)
                => Int -> Int -> (Point2 u -> Point2 u) -> [Point2 u]
ixDownLeftRight row_count col_count fn = 
    [fn $ P2 x y | x <- countup   (row_count - 1)
                 , y <- countdown (col_count - 1) ]


-- | Countdown from n to 0.
countdown :: Num u => Int -> [u]
countdown = unfoldr phi where
   phi i | i < 0 = Nothing
   phi i         = Just (fromIntegral i,i-1)

-- | Count up to n from 0. 
countup :: Num u => Int -> [u]
countup n = unfoldr phi 0 where
   phi i | i > n = Nothing
   phi i         = Just (fromIntegral i,i+1)

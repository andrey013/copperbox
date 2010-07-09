{-# OPTIONS -Wall #-}

module ColourCharts where

import ColourDefns

import Wumpus.Core
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Utils.HList

import Data.AffineSpace

import Data.List
import Data.Maybe

import System.Directory

main :: IO ()
main = do
   createDirectoryIfMissing True "./out/"
   test01
 
test01 :: IO ()
test01 = do 
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

mkPic :: [(String,DRGB)] -> [DPoint2] -> DPicture 
mkPic cs pts = fromMaybe errK $ 
                 drawGraphic $ concatH $ zipWith colourSample cs pts
  where
    errK = error "Empty Picture"

scalePt :: Num u => u -> Point2 u -> Point2 u
scalePt w (P2 x y) = P2 (x*w) (y*12) 

colourSample :: (Fractional u, Floating u, Ord u) 
             => (String,DRGB) -> GraphicF u
colourSample (name,rgb) = block `cc` lbl 
  where
    block = filledRectangle rgb  15 10
    lbl   = text (courier 10) name . (.+^ hvec 18)



---------------------------

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

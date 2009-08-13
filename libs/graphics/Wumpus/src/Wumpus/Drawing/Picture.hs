{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Picture
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Picture type
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.Picture where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.Curve
import Wumpus.Core.Geometric
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Polygon
import Wumpus.Core.Transform
import Wumpus.Core.Vector

import Wumpus.Drawing.GraphicsState
import Wumpus.Drawing.Label
import Wumpus.Drawing.Path
import Wumpus.Drawing.PostScript

import Data.AdditiveGroup
import Data.AffineSpace

import qualified Data.Foldable as F
import Data.List ( foldl' ) 
import Data.Monoid


-- THE HERE CODE IS HORRIBLE...
-- I've recently changed to make geometric objects parametric on
-- point type. There are still design issues to work out in the
-- lower layers, so Picture which depends on them has been changed
-- just enough to allow (some of)* the examples to compile. 
-- 
-- * Some of the examples wil be broken.

data Picture pt = Picture { 
      picBBox     :: BoundingBox pt, 
      picParts    :: [PictureElement pt]
    }

data PictureElement pt = PicPath  PathAttr (Path pt)
                       | PicLabel LabelAttr (Label pt)

data PathAttr = Stroke DRGB Pen
              | Fill   DRGB 
              | Clip
  deriving (Eq,Show) 


data LabelAttr = LabelAttr DRGB Font
  deriving (Eq,Show)


stroke :: PathAttr
stroke = Stroke wumpusBlack newPen

fill :: PathAttr
fill = Fill wumpusBlack

clip :: PathAttr
clip = Clip


timesRoman10 :: LabelAttr
timesRoman10 = LabelAttr wumpusBlack (timesRoman 10) 

changeColour :: DRGB -> PathAttr -> PathAttr  
changeColour c (Stroke _ pen)   = Stroke c pen
changeColour c (Fill   _)       = Fill c 
changeColour _ Clip             = Clip


-- instance Monoid (BoundingBox pt) =>  Monoid (Picture pt) where
instance Monoid (Picture (Point2 Double)) where
  mempty = Picture mempty []
  Picture bb es `mappend` Picture bb' es' = 
    Picture (bb `mappend` bb') (es ++ es')


extractCoordinate :: (DBoundingBox -> DPoint2) -> Picture (Point2 Double) -> DPoint2
extractCoordinate f = f . picBBox


infixr 7 <..>
infixr 6 <++>
infixr 5 <//>

picEmpty :: Picture (Point2 Double)
picEmpty = mempty

empty :: Picture (Point2 Double) -> Bool
empty (Picture bb []) | bb == mempty = True
empty _                              = False

(<..>) :: Picture (Point2 Double) -> Picture (Point2 Double) -> Picture (Point2 Double)
(<..>) = mappend

(<++>) :: Picture (Point2 Double) -> Picture (Point2 Double) -> Picture (Point2 Double) 
(<++>) p1 p2 | empty p1  = p2
             | empty p2  = p1
             | otherwise = p1 `mappend` (displacePicture v p2)  
   where 
     v = (east $ picBBox p1) .-. (west $ picBBox p2) 
        
(<//>) :: Picture (Point2 Double) -> Picture (Point2 Double) -> Picture (Point2 Double)
(<//>) p1 p2 | empty p1  = p2
             | empty p2  = p1
             | otherwise = p1 `mappend` (displacePicture v p2)  
   where  
     v = (south $ picBBox p1) .-. (north $ picBBox p2) 
      

centered :: Picture (Point2 Double) -> Picture (Point2 Double) -> Picture (Point2 Double)
centered p1 p2 | empty p1  = p2
               | empty p2  = p1
               | otherwise = p1 `mappend` (displacePicture v p2)  
  where 
    v = (center $ picBBox p1) .-. (center $ picBBox p2) 


displacePicture :: DVec2 -> Picture (Point2 Double) -> Picture (Point2 Double)
displacePicture v (Picture (BBox bl tr) es) = Picture bb' (map fn es)
  where
    bb' = BBox (bl .+^ v) (tr .+^ v)
    fn (PicPath a p) = PicPath a (displacePath v p)
    fn (PicLabel a l) = PicLabel a (displaceLabel v l)




at :: Picture (Point2 Double) -> (Double,Double) -> Picture (Point2 Double)
at p (x,y) = displacePicture (V2 x y) p

displace :: Double -> Double -> Picture (Point2 Double) -> Picture (Point2 Double)
displace x y = displacePicture (V2 x y) 


multiput :: Int -> DVec2 -> Picture (Point2 Double) -> Picture (Point2 Double)
multiput n disp pic = 
    foldl' (\p v -> p <..> (displacePicture v pic)) pic vecs
  where
    vecs  = scanl (^+^) disp (replicate (n-1) disp)


-- | Concatenate all the pictures with (<..>) preserving there 
-- original positions.
cat :: [Picture (Point2 Double)] -> Picture (Point2 Double)
cat = foldl' (<..>) picEmpty


-- | Concatenate all the pictures horizontally with (<++>).
hcat :: [Picture (Point2 Double)] -> Picture (Point2 Double)
hcat = foldl' (<++>) picEmpty

-- | Concatenate all the pictures vertically with (<//>).
vcat :: [Picture (Point2 Double)] -> Picture (Point2 Double)
vcat = foldl' (<//>) picEmpty

-- | Concatenate all the pictures horizontally separated by a 
-- space of @xsep@ units.
hcatSep :: Double -> [Picture (Point2 Double)] -> Picture (Point2 Double)
hcatSep _    []     = picEmpty
hcatSep xsep (x:xs) = foldl' fn x xs
  where
    fn acc a = acc <++> sep <++> a
    sep      = blankPicture xsep 0 


-- | Concatenate all the pictures vertically separated by a 
-- space of @ysep@ units.
vcatSep :: Double -> [Picture (Point2 Double)] -> Picture (Point2 Double)
vcatSep _    []      = picEmpty
vcatSep ysep (x:xs)  = foldl' fn x xs
  where
    fn acc a = acc <//> sep <//> a
    sep      = blankPicture 0 ysep



-- | Create a blank (empty) Picture of width @w@ and height @h@.
blankPicture :: Double -> Double -> Picture (Point2 Double)
blankPicture w h = Picture (BBox zeroPt (P2 w h)) []



picPolygon :: PathAttr -> Polygon (Point2 Double) -> Picture (Point2 Double)
picPolygon style pgon = Picture (getBoundingBox pgon) [path]
  where
    path = PicPath style . closePath . tracePoints . extractPoints $ pgon

picPath :: (Num a, Ord a) 
        => PathAttr -> Path (Point2 a) -> Picture (Point2 a)
picPath style p = Picture (bounds p)  [PicPath style p]


picLabel :: LabelAttr -> Label pt -> Picture pt
picLabel style lbl = Picture (labelRect lbl) [PicLabel style lbl]




-- TODO
-- The withRgbColour, withGray functions should work on Paths
-- not Pictures. For temporary compatibility they work Pictures.


withRgbColour :: DRGB -> Picture pt -> Picture pt 
withRgbColour c (Picture bb es) = Picture bb (map fn es) 
  where
   fn (PicPath attr path) = PicPath (changeColour c attr) path
   fn (PicLabel attr lbl) = PicLabel attr lbl -- TODO  

withGray :: Double -> Picture pt -> Picture pt
withGray n = withRgbColour (gray2rgb n)

withFont :: Font -> Picture pt -> Picture pt
withFont _ = id



--------------------------------------------------------------------------------
-- output
 
-- | Draw a picture, generating PostScript output.
psDraw :: Picture (Point2 Double) -> PostScript
psDraw pic = prologue ++ runWumpus env0 (drawPicture pic) ++ epilogue
  where
    prologue = unlines $ [ "%!PS-Adobe-2.0"
                         , "%%Pages: 1"
                         , "%%EndComments"
                         , "%%Page: 1 1"
                         , ""
                         ]

                   
    epilogue = unlines $ [ "showpage", "", "%%EOF", ""]
 
writePicture :: FilePath -> Picture (Point2 Double) -> IO ()
writePicture filepath pic = writeFile filepath $ psDraw pic



drawPicture :: Picture (Point2 Double) -> WumpusM ()
drawPicture (Picture _ es) = defaultFont >> mapM_ drawElt es
  where
    drawElt (PicPath attr p)  = drawPath attr p
    drawElt (PicLabel attr l) = drawLabel attr l

    defaultFont = do        
        command "findfont" ["/Times-Roman"]
        command "scalefont" [show (10::Int)]
        command "setfont" []


drawPath :: PathAttr -> Path (Point2 Double) -> WumpusM ()
drawPath attrs (Path p0@(P2 x0 y0) end sp) = withAttrs attrs $ do 
    ps_newpath
    ps_moveto x0 y0
    F.foldlM step p0 sp 
    optClose end
  where
    optClose (PathClosed _) = ps_closepath
    optClose _              = return ()
    
    step p (RMoveTo v)         = let p'@(P2 x y) = p .+^ v
                                 in ps_moveto x y >> return p'
    step p (RLineTo v)         = let p'@(P2 x y) = p .+^ v
                                 in ps_lineto x y >> return p'

    step p (RCurveTo v1 v2 v3) = let p1@(P2 x1 y1) = p  .+^ v1
                                     p2@(P2 x2 y2) = p1 .+^ v2
                                     p3@(P2 x3 y3) = p2 .+^ v3
                                 in ps_curveto x1 y1 x2 y2 x3 y3 >> return p3


withAttrs :: PathAttr -> WumpusM () -> WumpusM ()
withAttrs (Stroke c p) mf = localRgbColour c $ localPen p $ mf >> ps_stroke
withAttrs (Fill c)     mf = localRgbColour c $ mf >> ps_fill
withAttrs Clip         mf = mf >> ps_clip


-- labels must be drawn wrt a start point

drawLabel :: LabelAttr -> Label (Point2 Double) -> WumpusM ()
drawLabel attr (Label ss (P2 x y) (BBox _ (P2 x' y')) _lh) = 
  localFont (timesRoman 10) $ do
    ps_moveto     x  y
    ps_lineto     x  y'
    ps_lineto     x' y'
    ps_lineto     x' y
    ps_closepath
    ps_clip
    ps_moveto     x  y  -- this loses descenders
    ps_show ss



helvetica :: Int -> Font
helvetica us = Font "Helvetica" us

timesRoman :: Int -> Font
timesRoman us = Font "Times-Roman" us




--------------------------------------------------------------------------------
-- Dots etc.



polyDot :: Int -> Picture (Point2 Double)
polyDot n = picPolygon stroke $ regularPolygon n 2


dotTriangle :: Picture (Point2 Double)
dotTriangle = polyDot 3

dotDiamond :: Picture (Point2 Double)
dotDiamond = polyDot 4


dotPentagon :: Picture (Point2 Double)
dotPentagon = polyDot 5


dotX :: Picture (Point2 Double)
dotX = picPath stroke path
  where
    l1    = vline 2 $ P2 0 (-1)
    ls1   = rotate (pi/6)   l1
    ls2   = rotate (5*pi/3) l1
    path  = segmentPath [ls1,ls2]

 

dotPlus :: Picture (Point2 Double)
dotPlus = picPath stroke path
  where
    hl    = hline 2 $ P2 (-1) 0
    vl    = vline 2 $ P2 0    (-1)
    path  = segmentPath [hl,vl]


dotSquare :: Picture (Point2 Double)
dotSquare = picPolygon stroke $ translate (-1) (-1) $ rectangle 2 2




dotAsterisk :: Picture (Point2 Double)
dotAsterisk = picPath stroke path
  where
   vl     :: LineSegment (Point2 Double)
   vl     = vline 2 (P2 0 (-1))
   path   = segmentPath . circular $ replicate 5 vl


diamond ::  Double -> Double -> PathAttr -> Picture (Point2 Double)
diamond wth hght attr = picPolygon attr $ Polygon ps 
  where
    vh      = hvec (0.5*wth)
    vv      = vvec (0.5*hght) 
    s       = zeroPt .-^ vv
    w       = zeroPt .+^ vh
    n       = zeroPt .+^ vv
    e       = zeroPt .-^ vh
    ps      = [s,w,n,e]




circle :: Double -> Path (Point2 Double)
circle r = closePath $ foldl' fn (newPath $ P2 r 0) arcs
  where
    arcs :: [Curve (Point2 Double)]
    arcs = bezierCircle 3 r
    fn path (Curve _ p1 p2 p3) = path `curveTo` (p1,p2,p3)

-- | make a circle centered at the origin, radius @r@.
picCircle :: Double -> Picture (Point2 Double)
picCircle = picPath stroke . circle

-- | Make a disk (filled circle)
picDisk :: Double -> Picture (Point2 Double)
picDisk = picPath fill . circle



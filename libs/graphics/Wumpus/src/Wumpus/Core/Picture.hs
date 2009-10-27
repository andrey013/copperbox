{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Picture
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.Picture where

import Wumpus.Core.BoundingBox hiding ( center )
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.PostScript


import Data.FunctionExtras
import Data.Groupoid

import Data.AffineSpace
import Data.VectorSpace

-- import Control.Monad            ( zipWithM_ )
import qualified Data.Foldable  as F
import Data.List                ( foldl', mapAccumR )
import Data.Sequence            ( Seq, (|>) )
import qualified Data.Sequence  as S



data Picture u = Empty
               | Single   (Measure u) (Primitive u)
               | Multi    (Measure u) [Primitive u] -- multiple prims in same affine frame
               | Picture  (Measure u) PictureProps (Picture u) (Picture u)
  deriving (Eq,Show) 

data Primitive u = Path1  PathProps (Path u)
                 | Label1 LabelProps (Label u) 
  deriving (Eq,Show)


type Colour = Maybe PSColour
type Font   = Maybe FontAttr

type PictureProps = (Colour, Seq PenAttr)
type PathProps    = (Colour, Seq PenAttr)
type LabelProps   = (Colour, Font)

-- Measure = (_current_ frame x bounding box)
type Measure u = (Frame2 u, BoundingBox u) 

type DMeasure = Measure Double

data DrawProp = OStroke | CStroke | CFill | CCrop 
  deriving (Eq,Show)


data PathSeg u = Cs (Point2 u) (Point2 u) (Point2 u)
               | Ls (Point2 u)
  deriving (Eq,Show)

type DPathSeg = PathSeg Double

data Path u = Path DrawProp (Point2 u) [PathSeg u]
  deriving (Eq,Show)

type DPath = Path Double


data Label u = Label (Point2 u) String
  deriving (Eq,Show)


--------------------------------------------------------------------------------

instance Groupoid (Path u) where
  Path dp st xs `gappend` Path _ st' xs' = Path dp st (xs ++ (Ls st' : xs'))


instance Pointwise (Path u) where
  type Pt (Path u) = Point2 u
  pointwise f (Path dp st xs) = Path dp (f st) (map (pointwise f) xs)

instance Pointwise (PathSeg u) where
  type Pt (PathSeg u) = Point2 u
  pointwise f (Ls p)        = Ls (f p)
  pointwise f (Cs p1 p2 p3) = Cs (f p1) (f p2) (f p3)


instance (Floating u, Real u) => Rotate (Picture u) where
  rotate = rotatePicture 

instance (Floating u, Real u) => RotateAbout (Picture u) where
  type RotateAboutUnit (Picture u) = u
  rotateAbout = rotatePictureAbout

instance (Num u, Ord u) => Scale (Picture u) where
  type ScaleUnit (Picture u) = u
  scale = scalePicture

instance (Num u, Ord u) => Translate (Picture u) where
  type TranslateUnit (Picture u) = u
  translate = translatePicture


-- Helpers for the affine transformations

rotatePicture :: (Real u, Floating u) => Radian -> Picture u -> Picture u
rotatePicture = subst' transformPicture rotate rotate


rotatePictureAbout :: (Real u, Floating u) 
                   => Radian -> Point2 u -> Picture u -> Picture u
rotatePictureAbout ang pt = 
    transformPicture (rotateAbout ang pt) (rotateAbout ang pt)
  
scalePicture :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
scalePicture x y = transformPicture (scale x y) (scale x y)

translatePicture :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
translatePicture x y = transformPicture (translate x y) (translate x y)


transformPicture :: (Num u, Ord u) 
                 => (Point2 u -> Point2 u) 
                 -> (Vec2 u -> Vec2 u) 
                 -> Picture u 
                 -> Picture u
transformPicture fp fv = 
    mapMeasure $ prod (transformFrame fp fv) (transformBBox fp)



transformFrame :: (Point2 u -> Point2 u) -> (Vec2 u -> Vec2 u) -> Frame2 u -> Frame2 u
transformFrame fp fv (Frame2 o vx vy) = Frame2 (fp o) (fv vx) (fv vy)    


-- Bounding boxes need recalculating after a transformation.
-- For instance after a reflection in the y-axis br becomes bl.
transformBBox :: (Num u, Ord u)
              => (Point2 u -> Point2 u) -> BoundingBox u -> BoundingBox u
transformBBox fp = trace . map fp . corners


--------------------------------------------------------------------------------


noProp :: (Colour,Seq a)
noProp = (Nothing,S.empty)


noFontProp :: (Colour,Font)
noFontProp = (Nothing,Nothing)


nullProps :: (Colour, Seq a) -> Bool
nullProps (Nothing,se) = S.null se
nullProps _            = False

nullFontProps :: (Colour, Font) -> Bool
nullFontProps (Nothing,Nothing) = True
nullFontProps _                 = False



straightLinePath :: [Point2 u] -> (Point2 u, [PathSeg u])
straightLinePath []     = error "polygonPath - empty Polygon"
straightLinePath (x:xs) = (x, map Ls xs)


picEmpty :: Picture u
picEmpty = Empty


picPath :: (Num u, Ord u) => Path u -> Picture u
picPath p = Single (ortho zeroPt, tracePath p) (Path1 noProp p)

picMultiPath :: (Num u, Ord u) => [Path u] -> Picture u
picMultiPath ps = Multi (ortho zeroPt, gconcat (map tracePath ps)) (map f ps)
  where f = Path1 noProp

picLabel1 :: (Num u, Ord u) => Int -> String -> Picture u
picLabel1 fontsz str = Single (ortho zeroPt, bb) lbl where
  bb  = BBox zeroPt (P2 w (fromIntegral fontsz))
  w   = fromIntegral $ fontsz * length str
  lbl = Label1 noFontProp (Label zeroPt str) 


picLabel :: forall u. (Num u, Ord u) => Int -> Int -> String -> Picture u
picLabel fontsz linespace str = Multi (ortho zeroPt, bb) lbls where
  xs   = lines str
  lc   = length xs
  w    = fromIntegral $ fontsz * (maximum . map length) xs
  h    = fromIntegral $ fontsz * lc + linespace*(lc-1)
  bb   = BBox zeroPt (P2 w h)
  lbls = snd $ mapAccumR fn zeroPt xs
  fn pt ss = let pt' = pt .+^ (V2 (0::u) (fromIntegral $ fontsz + linespace))
             in (pt', Label1 noFontProp (Label pt ss))


tracePath :: (Num u, Ord u) => Path u -> BoundingBox u
tracePath = trace . extractPoints

extractPoints :: Path u -> [Point2 u]
extractPoints (Path _ st xs) = st : foldr f [] xs where
    f (Ls p)        acc = p : acc
    f (Cs p1 p2 p3) acc = p1 : p2 : p3 : acc 


arrange :: (Num u, Ord u)
        => (Picture u -> Picture u -> Vec2 u) 
        -> Picture u
        -> Picture u 
        -> Picture u
arrange _ a     Empty = a
arrange _ Empty b     = b
arrange f a     b     = Picture (ortho zeroPt, bb) noProp a b' where
    b' = move (f a b) b
    bb = union (extractBounds a) (extractBounds b')
    
   

(<>) :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
(<>) = arrange $ 
         twine fn (rightPlane . extractBounds) (leftPlane . extractBounds)
  where fn = hvec `oo` (-) 


(</>) :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
(</>) = arrange $ 
          twine fn (lowerPlane . extractBounds) (upperPlane . extractBounds)
  where fn = vvec `oo` (-)


at :: Num u => Picture u -> Point2 u -> Picture u
at p (P2 x y) = move (V2 x y) p

overlay :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
overlay = arrange (\_ _ -> V2 0 0)

overlays :: (Num u, Ord u) => [Picture u] -> Picture u
overlays []     = error "overlays - empty list"
overlays (x:xs) = foldl' overlay x xs

drawBounds :: (Num u, Ord u) => Picture u -> Picture u
drawBounds Empty = Empty
drawBounds p     = p `overlay` (picPath $ Path CStroke p0 ps) where
    bb      = extractBounds p
    (p0,ps) = straightLinePath $ corners bb 


extractBounds :: (Num u, Ord u) => Picture u -> BoundingBox u
extractBounds Empty                  = error $ "extractBounds Empty"
extractBounds (Single  (_,bb) _)     = bb
extractBounds (Multi   (_,bb) _)     = bb
extractBounds (Picture (_,bb) _ _ _) = bb


mapMeasure :: (Measure u -> Measure u) -> Picture u -> Picture u
mapMeasure _ Empty                 = Empty
mapMeasure f (Single  m prim)      = Single (f m) prim
mapMeasure f (Multi   m ps)        = Multi (f m) ps
mapMeasure f (Picture m prop a  b) = Picture (f m) prop a b


move :: Num u => Vec2 u -> Picture u -> Picture u
move v = mapMeasure (moveMeasure v) 

  
moveMeasure :: Num u => Vec2 u -> Measure u -> Measure u
moveMeasure v (fr,bb) = (displaceOrigin v fr, pointwise (.+^ v) bb) 

center :: (Fractional u, Ord u) => Picture u -> Point2 u
center Empty = zeroPt
center p     = fn $ extractBounds p where
    fn (BBox bl tr) = bl .+^ (0.5 *^ (tr .-. bl))



setRGBColour :: DRGB -> Picture u -> Picture u
setRGBColour (RGB3 r g b) = updateProps f f where 
    f (_,xs) = (Just (PSRgb r g b),xs)

setHSBColour :: DHSB -> Picture u -> Picture u
setHSBColour (HSB3 h s b) = updateProps f f where 
    f (_,xs) = (Just (PSHsb h s b),xs)

setGray :: Double -> Picture u -> Picture u
setGray a = updateProps f f where 
    f (_,xs) = (Just (PSGray a),xs)

setLineWidth :: Double -> Picture u -> Picture u
setLineWidth a = updateProps f id where 
    f (c,se) = (c,se |> LineWidth a)   -- must be /last/.

setMiterLimit :: Double -> Picture u -> Picture u
setMiterLimit a = updateProps f id where 
    f (c,se) = (c,se |> MiterLimit a)

setLineCap :: LineCap -> Picture u -> Picture u
setLineCap a = updateProps f id where 
    f (c,se) = (c,se |> LineCap a)

setLineJoin :: LineJoin -> Picture u -> Picture u
setLineJoin a = updateProps f id where 
    f (c,se) = (c,se |> LineJoin a)

setDashPattern :: DashPattern -> Picture u -> Picture u
setDashPattern a = updateProps f id where 
    f (c,se) = (c,se |> DashPattern a)


setFont :: String -> Int -> Picture u -> Picture u
setFont name sz = updateProps id g where 
    g (c,_) = (c,Just $ FontAttr name sz)


-- This is too destructive and needs a rethink...
-- In essence it goes against the grain of picture being created
-- /bottom-up/. 

updateProps :: (PathProps -> PathProps) 
            -> (LabelProps -> LabelProps) 
            -> Picture u  
            -> Picture u
updateProps _ _ Empty                 = Empty
updateProps f g (Single  m prim)      = Single m (updatePrimProps f g prim)
updateProps f g (Multi   m ps)        = Multi m (map (updatePrimProps f g) ps)
updateProps f _ (Picture m prop a  b) = Picture m (f prop) a b


updatePrimProps :: (PathProps -> PathProps) 
                -> (LabelProps -> LabelProps) 
                -> Primitive u  
                -> Primitive u
updatePrimProps f _ (Path1 p a)  = Path1 (f p) a
updatePrimProps _ g (Label1 p a) = Label1 (g p) a


--------------------------------------------------------------------------------
-- Render to PostScript

 
writePicture :: FilePath -> Picture Double -> IO ()
writePicture filepath pic = writeFile filepath $ psDraw pic


-- | Draw a picture, generating PostScript output.
psDraw :: Picture Double -> PostScript
psDraw pic = prologue ++ runWumpus (drawPicture k pic) ++ epilogue
  where
    k        = pointInFrame `flip` (ortho zeroPt)
    prologue = unlines $ [ "%!PS-Adobe-2.0"
                         , "%%Pages: 1"
                         , "%%EndComments"
                         , "%%Page: 1 1"
                         , ""
                         --- these are temporary...
                         , "/Times-Roman findfont"
                         , "10 scalefont"
                         , "setfont"
                         , "200 400 translate"
                         ]

                   
    epilogue = unlines $ [ "showpage", "", "%%EOF", ""]

drawPicture :: (Point2 Double -> Point2 Double) 
            -> Picture Double 
            -> WumpusM ()
drawPicture _ Empty                     = return ()

drawPicture f (Single (fr,_) prim)    =
    drawPrimitive (composeFrames f fr) prim

drawPicture f (Multi (fr,_) ps)         = 
    mapM_ (drawPrimitive (composeFrames f fr)) ps

drawPicture f (Picture (fr,_) prop a b) = updatePen prop $ do
    drawPicture (composeFrames f fr) a
    drawPicture (composeFrames f fr) b


drawPrimitive :: (Point2 Double -> Point2 Double) 
              -> Primitive Double 
              -> WumpusM ()
drawPrimitive f (Path1 props p)  = updatePen props $ drawPath f p
drawPrimitive f (Label1 props l) = updateFont props $ drawLabel f l

composeFrames :: Num u 
              => (Point2 u -> Point2 u) -> Frame2 u -> (Point2 u -> Point2 u)
composeFrames f fr = f . (pointInFrame `flip` fr)

updatePen :: PathProps -> WumpusM () -> WumpusM ()
updatePen prop@(mbc,se) ma
    | nullProps prop = ma
    | otherwise      = do { ps_gsave
                          ; optColourCommand mbc
                          ; F.mapM_ penCommand se
                          ; ma
                          ; ps_grestore
                          }

penCommand :: PenAttr -> WumpusM ()
penCommand (LineWidth d)    = ps_setlinewidth d
penCommand (MiterLimit d)   = ps_setmiterlimit d
penCommand (LineCap lc)     = ps_setlinecap lc
penCommand (LineJoin lj)    = ps_setlinejoin lj
penCommand (DashPattern dp) = ps_setdash dp

updateFont :: LabelProps -> WumpusM () -> WumpusM ()
updateFont prop@(mbc,mfnt) ma 
    | nullFontProps prop = ma
    | otherwise          = do { ps_gsave
                              ; optColourCommand mbc
                              ; optFontCommand mfnt
                              ; ma
                              ; ps_grestore
                              }

optFontCommand :: Maybe FontAttr -> WumpusM ()
optFontCommand = maybe (return ()) fontCommand

fontCommand :: FontAttr -> WumpusM ()
fontCommand (FontAttr name sz) = ps_findfont name >> ps_scalefont sz >> ps_setfont

optColourCommand :: Maybe PSColour -> WumpusM ()
optColourCommand = maybe (return ()) colourCommand

colourCommand :: PSColour -> WumpusM ()
colourCommand (PSRgb r g b) = ps_setrgbcolor r g b
colourCommand (PSHsb h s v) = ps_sethsbcolor h s v
colourCommand (PSGray a)    = ps_setgray a


drawPath :: (Point2 Double -> Point2 Double) -> Path Double -> WumpusM ()
drawPath f (Path dp pt xs) = let P2 x y = f pt in do  
    ps_newpath
    ps_moveto x y
    mapM_ drawLineSeg xs
    finalize dp   
  where
    drawLineSeg (Ls p)        = let P2 x y = f p in ps_lineto x y
    drawLineSeg (Cs p1 p2 p3) = let P2 x1 y1 = f p1
                                    P2 x2 y2 = f p2
                                    P2 x3 y3 = f p3
                                in ps_curveto x1 y1 x2 y2 x3 y3
    finalize OStroke = ps_stroke
    finalize CStroke = ps_closepath >> ps_stroke
    finalize CFill   = ps_closepath >> ps_fill
    finalize CCrop   = ps_closepath >> ps_clip


drawLabel :: (Point2 Double -> Point2 Double)
          -> Label Double 
          -> WumpusM ()
drawLabel fn (Label pt str) = let P2 x y = fn pt in do
    ps_moveto x y
    ps_gsave
    ps_concat a b c d e f
    ps_show str
    ps_grestore
  where
    (a,b,c,d,e,f) = makeCTM fn

makeCTM :: Num u => (Point2 u -> Point2 u) -> (u,u,u,u,u,u)
makeCTM f = (x0-o0, x1-o1, y0-o0, y1-o1, o0, o1) where
   P2 x0 x1 = f (P2 1 0)
   P2 y0 y1 = f (P2 0 1)
   P2 o0 o1 = f (P2 0 0)



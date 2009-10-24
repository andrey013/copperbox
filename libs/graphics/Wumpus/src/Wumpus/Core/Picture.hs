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

import Control.Monad            ( zipWithM_ )
import qualified Data.Foldable  as F
import Data.List                ( foldl' )
import Data.Sequence            ( Seq, (|>) )
import qualified Data.Sequence  as S



data Picture u = Empty
               | Single   (Measure u) PathProps (Path u)
               | Multi    (Measure u) [(PathProps,Path u)]
               | TLabel   (Measure u) LabelProps (Label u)
               | Picture  (Measure u) PictureProps (Picture u) (Picture u)
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


newtype Polygon u = Polygon { vertexList :: [Point2 u] }
  deriving (Eq,Show)

type DPolygon = Polygon Double

data Label u = Label {
      labelInitialHeight  :: u, -- store the height /before/ any affine trafos
      labelFontSize       :: u,
      labelRowDisp        :: u,
      labelText           :: [String] 
    }
  deriving (Eq,Show)


--------------------------------------------------------------------------------

instance Groupoid (Path u) where
  Path dp st xs `gappend` Path _ st' xs' = Path dp st (xs ++ (Ls st' : xs'))


noProp :: (Colour,Seq a)
noProp = (Nothing,S.empty)


noFontProp :: (Colour,Font)
noFontProp = (Nothing,Nothing)

-- | Create a regular polygon with @n@ sides and /radius/ @r@ 
-- centered at the origin.
regularPolygon :: (Floating u, Real u)
               => Int -> u -> Polygon u
regularPolygon n r = Polygon $ circular $ replicate n (zeroPt .+^ (V2 0 r)) 


-- | Create an isosceles rectangle with bottom-left corner at the 
-- origin, the base on the horizontal plane with width @bw@. The 
-- height is @h@.
isoscelesTriangle :: Fractional u => u -> u -> Polygon u
isoscelesTriangle bw h = Polygon $ sequence [id,f2,f3] zeroPt where
  f2 = (.+^ hvec bw)
  f3 = (.+^ V2 (bw/2) h)



straightLinePath :: [Point2 u] -> (Point2 u, [PathSeg u])
straightLinePath []     = error "polygonPath - empty Polygon"
straightLinePath (x:xs) = (x, map Ls xs)

  
picPolygon :: (Num u, Ord u) => DrawProp -> Polygon u -> Picture u
picPolygon dp (Polygon xs) = Single (ortho zeroPt,trace xs) noProp path
  where 
    path         = Path dp start segs
    (start,segs) = straightLinePath xs


picPath :: (Num u, Ord u) => Path u -> Picture u
picPath p = Single (ortho zeroPt, tracePath p) noProp p

picMultiPath :: (Num u, Ord u) => [Path u] -> Picture u
picMultiPath ps = Multi (ortho zeroPt, gconcat (map tracePath ps)) (map f ps)
  where f a = (noProp,a)


picLabel :: (Num u, Ord u) => u -> u -> u -> u -> String -> Picture u
picLabel fonth vdisp w h text = TLabel (ortho zeroPt, bb) noFontProp label where
  bb    = trace [zeroPt, P2 w h]
  label = Label h fonth vdisp (lines text) 


tracePath :: (Num u, Ord u) => Path u -> BoundingBox u
tracePath = trace . extractPoints

extractPoints :: Path u -> [Point2 u]
extractPoints (Path _ st xs) = st : foldr f [] xs where
    f (Ls p)        acc = p : acc
    f (Cs p1 p2 p3) acc = p1 : p2 : p3 : acc 

extractPolygonPath :: Polygon u -> Path u
extractPolygonPath p = Path CStroke p0 ps where
  (p0,ps) = straightLinePath $ vertexList p 



bbPolygon :: (Num u, Ord u) => Polygon u -> BoundingBox u
bbPolygon (Polygon xs)     = trace xs 

arrange :: (Num u, Ord u)
        => (Picture u -> Picture u -> Vec2 u) 
        -> Picture u
        -> Picture u 
        -> Picture u
arrange _ a     Empty = a
arrange _ Empty b     = b
arrange f a     b     = Picture (ortho zeroPt, bb) noProp a b' where
    b' = move (f a b) b
    bb = union (picBounds a) (picBounds b')
    
   

(<>) :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
(<>) = arrange $ twine fn (rightPlane . picBounds) (leftPlane . picBounds)
  where fn = hvec `oo` (-) 


(</>) :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
(</>) = arrange $ twine fn (lowerPlane . picBounds) (upperPlane . picBounds)
  where fn = vvec `oo` (-)

overlay :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
overlay = arrange (\_ _ -> V2 0 0)

overlays :: (Num u, Ord u) => [Picture u] -> Picture u
overlays []     = error "overlays - empty list"
overlays (x:xs) = foldl' overlay x xs

drawBounds :: (Num u, Ord u) => Picture u -> Picture u
drawBounds Empty = Empty
drawBounds p     = p `overlay` pbb where
    bb  = picBounds p
    pbb = picPolygon CStroke (Polygon $ corners bb)
                   


picBounds :: (Num u, Ord u) => Picture u -> BoundingBox u
picBounds Empty                  = error $ "picBounds Empty"
picBounds (Single  (_,bb) _ _)   = bb
picBounds (Multi   (_,bb) _)     = bb
picBounds (TLabel  (_,bb) _ _)   = bb
picBounds (Picture (_,bb) _ _ _) = bb


mapMeasure :: (Measure u -> Measure u) -> Picture u -> Picture u
mapMeasure _ Empty                 = Empty
mapMeasure f (Single  m prop p)    = Single (f m) prop p
mapMeasure f (Multi   m ps)        = Multi (f m) ps
mapMeasure f (TLabel  m prop l)    = TLabel (f m) prop l 
mapMeasure f (Picture m prop a  b) = Picture (f m) prop a b


move :: Num u => Vec2 u -> Picture u -> Picture u
move v = mapMeasure (moveMeasure v) 

  
moveMeasure :: Num u => Vec2 u -> Measure u -> Measure u
moveMeasure v (fr,bb) = (displaceOrigin v fr, pointwise (.+^ v) bb) 

center :: (Fractional u, Ord u) => Picture u -> Point2 u
center Empty = zeroPt
center p     = fn $ picBounds p where
    fn (BBox bl tr) = bl .+^ (0.5 *^ (tr .-. bl))


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


-- This is a rotation about the origin...

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



-- Note /Multi/ pictures are not updated
updateProps :: (PathProps -> PathProps) 
            -> (LabelProps -> LabelProps) 
            -> Picture u  
            -> Picture u
updateProps _ _ Empty                 = Empty
updateProps f _ (Single  m prop p)    = Single m (f prop) p
updateProps _ _ (Multi   m ps)        = Multi m ps              -- NO-OP!
updateProps _ g (TLabel  m prop l)    = TLabel m (g prop) l 
updateProps f _ (Picture m prop a  b) = Picture m (f prop) a b



instance Pointwise (Polygon a) where
  type Pt (Polygon a) = Point2 a
  pointwise f (Polygon xs) = Polygon $ map f xs

instance Pointwise (Path u) where
  type Pt (Path u) = Point2 u
  pointwise f (Path dp st xs) = Path dp (f st) (map (pointwise f) xs)

instance Pointwise (PathSeg u) where
  type Pt (PathSeg u) = Point2 u
  pointwise f (Ls p)        = Ls (f p)
  pointwise f (Cs p1 p2 p3) = Cs (f p1) (f p2) (f p3)

--------------------------------------------------------------------------------


 
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

drawPicture f (Single (fr,_) prop p)    =
    updatePen prop $ drawPath (composeFrames f fr) p

drawPicture f (Multi (fr,_) ps)         = 
    mapM_ (\(prop,p) -> updatePen prop $ drawPath (composeFrames f fr) p) ps

drawPicture f (TLabel (fr,bb) font l)   =
    updateFont font $ drawLabel (composeFrames f fr) (bottomLeft bb) l

drawPicture f (Picture (fr,_) prop a b) = updatePen prop $ do
    drawPicture (composeFrames f fr) a
    drawPicture (composeFrames f fr) b


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
          -> Point2 Double 
          -> Label Double 
          -> WumpusM ()
drawLabel fn (P2 x y) (Label totalh fonth rowdisp xs) = do
    ps_moveto x y
    ps_gsave
    ps_concat a b c d e f
    zipWithM_ drawTextLine xs vecs
    ps_grestore
  where
    drawTextLine str (V2 vx vy) = comment (show vx ++ ", " ++ show vy) >> 
                                  ps_moveto vx vy >> ps_show str
    vecs          = labelDisplacements fonth totalh rowdisp (length xs) 
    (a,b,c,d,e,f) = makeCTM fn

makeCTM :: Num u => (Point2 u -> Point2 u) -> (u,u,u,u,u,u)
makeCTM f = (x0-o0, x1-o1, y0-o0, y1-o1, o0, o1) where
   P2 x0 x1 = f (P2 1 0)
   P2 y0 y1 = f (P2 0 1)
   P2 o0 o1 = f (P2 0 0)


labelDisplacements :: Fractional u => u -> u -> u -> Int -> [Vec2 u]
labelDisplacements fonth totalh rowdisp nrows = 
    reverse $ take nrows $ iterate (^+^ vvec (fonth+rowdisp)) (vvec start)
  where
    n         = fromIntegral nrows
    internalh = fonth*n + rowdisp*(n-1)
    start     = 0.5 * (totalh-internalh)


nullProps :: (Colour, Seq a) -> Bool
nullProps (Nothing,se) = S.null se
nullProps _            = False

nullFontProps :: (Colour, Font) -> Bool
nullFontProps (Nothing,Nothing) = True
nullFontProps _                 = False
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

import Text.PrettyPrint.Leijen

import qualified Data.Foldable  as F
import Data.List                ( foldl', mapAccumR )
import Data.Monoid
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
-- /optimize/ the standard frame representing it as Nothing

type MbFrame u  = Maybe (Frame2 u)
type Measure u = (MbFrame u, BoundingBox u) 

type DMeasure = Measure Double

data DrawProp = OStroke | CStroke | CFill | CCrop 
  deriving (Eq,Show)

data Path u = Path DrawProp (Point2 u) [PathSeg u]
  deriving (Eq,Show)

type DPath = Path Double

data PathSeg u = Cs (Point2 u) (Point2 u) (Point2 u)
               | Ls (Point2 u)
  deriving (Eq,Show)

type DPathSeg = PathSeg Double



data Label u = Label (Point2 u) String
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Pretty printing

instance Pretty u => Pretty (Picture u) where
  pretty Empty                = text "*empty*"
  pretty (Single m prim)      = ppMeasure m <$> indent 2 (pretty prim)

  pretty (Multi m prims)      = ppMeasure m <$> indent 2 (list $ map pretty prims)
  pretty (Picture m _ pl pr)  = 
      ppMeasure m <$> indent 2 (text "LEFT" <+> pretty pl)
                  <$> indent 2 (text "RGHT" <+> pretty pr)

     

ppMeasure :: Pretty u => Measure u -> Doc
ppMeasure (fr,bbox) = align (ppfr fr <$> pretty bbox) where
   ppfr Nothing   = text "*std-frame*"
   ppfr (Just a)  = pretty a


instance Pretty u => Pretty (Primitive u) where
  pretty (Path1 _ path) = pretty "path:" <+> pretty path
  pretty (Label1 _ lbl) = pretty lbl

instance Pretty u => Pretty (Path u) where
   pretty (Path _ pt ps) = pretty pt <> hcat (map pretty ps)

instance Pretty u => Pretty (PathSeg u) where
  pretty (Cs p1 p2 p3) = text ".*" <> pretty p1 <> text ",," <> pretty p2 
                                                <> text "*." <> pretty p3
  pretty (Ls pt)       = text "--" <> pretty pt

instance Pretty u => Pretty (Label u) where
  pretty (Label pt s) = dquotes (text s) <> char '@' <> pretty pt


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


-- Shouldn't transforming the frame be the inverse transformation?

transformFrame :: Num u
               => (Point2 u -> Point2 u) 
               -> (Vec2 u -> Vec2 u) 
               -> MbFrame u 
               -> MbFrame u
transformFrame fp fv = Just . trf . maybe (ortho zeroPt) id  
  where
    trf (Frame2 o vx vy) = Frame2 (fp o) (fv vx) (fv vy)    


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



straightLinePath :: DrawProp -> [Point2 u] -> Path u
straightLinePath _     []     = error "straightLinePath - empty point list"
straightLinePath props (x:xs) = Path props x (map Ls xs)


picEmpty :: Picture u
picEmpty = Empty


picPath :: (Num u, Ord u) => Path u -> Picture u
picPath p = Single (Nothing, tracePath p) (Path1 noProp p)

picMultiPath :: (Num u, Ord u) => [Path u] -> Picture u
picMultiPath ps = Multi (Nothing, gconcat (map tracePath ps)) (map f ps)
  where f = Path1 noProp

-- The width guesses by picLabel1 and picLabel are very poor...

picLabel1 :: (Num u, Ord u) => Int -> String -> Picture u
picLabel1 fontsz str = Single (Nothing, bb) lbl where
  bb  = BBox zeroPt (P2 w (fromIntegral fontsz))
  w   = fromIntegral $ fontsz * length str
  lbl = Label1 noFontProp (Label zeroPt str) 


picLabel :: forall u. (Num u, Ord u) => Int -> Int -> String -> Picture u
picLabel fontsz linespace str = Multi (Nothing, bb) lbls where
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
arrange f a     b     = Picture (Nothing, bb) noProp a b' where
    b' = move (f a b) b
    bb = union (extractBounds a) (extractBounds b')
    
   

(<*>) :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
(<*>) = arrange $ 
         twine fn (rightPlane . extractBounds) (leftPlane . extractBounds)
  where fn = hvec `oo` (-) 


(<||>) :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
(<||>) = arrange $ 
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
drawBounds p     = p `overlay` (picPath path) where
    bb   = extractBounds p
    path = straightLinePath CStroke $ corners bb 


extractBounds :: (Num u, Ord u) => Picture u -> BoundingBox u
extractBounds Empty                  = error $ "extractBounds Empty"
extractBounds (Single  (_,bb) _)     = bb
extractBounds (Multi   (_,bb) _)     = bb
extractBounds (Picture (_,bb) _ _ _) = bb

extractFrame :: Num u => Picture u -> Frame2 u
extractFrame Empty                  = ortho zeroPt
extractFrame (Single  (fr,_) _)     = maybe (ortho zeroPt) id fr
extractFrame (Multi   (fr,_) _)     = maybe (ortho zeroPt) id fr
extractFrame (Picture (fr,_) _ _ _) = maybe (ortho zeroPt) id fr



mapMeasure :: (Measure u -> Measure u) -> Picture u -> Picture u
mapMeasure _ Empty                 = Empty
mapMeasure f (Single  m prim)      = Single (f m) prim
mapMeasure f (Multi   m ps)        = Multi (f m) ps
mapMeasure f (Picture m prop a  b) = Picture (f m) prop a b


move :: Num u => Vec2 u -> Picture u -> Picture u
move v = mapMeasure (moveMeasure v) 

  
moveMeasure :: Num u => Vec2 u -> Measure u -> Measure u
moveMeasure v (fr,bb) = (Just $ displaceOrigin v fr', pointwise (.+^ v) bb) 
  where
    fr' = maybe (ortho zeroPt) id fr

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
psDraw pic = prologue ++ runWumpus (drawPicture Nothing pic) ++ epilogue
  where
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

drawPicture :: MbFrame Double -> Picture Double  -> WumpusM ()
drawPicture _  Empty                      = return ()

drawPicture f0 (Single (fr,bb) prim)      = do
    bbComment bb
    drawPrimitive (f0 `mappend` fr) prim

drawPicture f0 (Multi (fr,bb) ps)         = do
    bbComment bb
    mapM_ (drawPrimitive (f0 `mappend` fr)) ps

drawPicture f0 (Picture (fr,bb) prop a b) = updatePen prop $ do
    bbComment bb
    drawPicture (f0 `mappend` fr) a
    drawPicture (f0 `mappend` fr) b


bbComment :: BoundingBox Double -> WumpusM ()
bbComment (BBox (P2 x0 y0) (P2 x1 y1)) = 
    ps_comment $ "bounding-box " ++ show (x0,y0) ++ ".." ++ show (x1,y1)

drawPrimitive :: MbFrame Double -> Primitive Double -> WumpusM ()
drawPrimitive fr (Path1 props p)  = 
    updatePen props $ updateFrame fr $ drawPath p
--    updatePen props $ drawPathFr fr p

drawPrimitive fr (Label1 props l) = 
    updateFont props $ updateFrame fr $ drawLabel l

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

-- 3 5 scale
-- [ 3 0 0 5 0 0 ] concat
-- 100 300 translate
-- [ 1 0 0 1 100 300 ] concat
-- [ cos(angle) sin(angle) -sin(angle) cos(angle) 0 0 ]

-- wumpus 
-- [ cos(a) -sin(a) 0 | sin(a) cos(a) 0 | 0 0 1 ]

updateFrame :: MbFrame Double -> WumpusM () -> WumpusM ()
updateFrame Nothing    ma = ma
updateFrame (Just frm) ma = do
    ps_gsave
    ps_concat ctm
    ma
    ps_grestore
  where
    ctm = toCTM $ invert $ frame2Matrix frm
 
    
drawPath :: Path Double -> WumpusM ()
drawPath (Path dp pt xs) = let P2 x y = pt in do  
    ps_newpath
    ps_moveto x y
    mapM_ drawLineSeg xs
    closePath dp   
  where
    drawLineSeg (Ls p)        = let P2 x y = p in ps_lineto x y
    drawLineSeg (Cs p1 p2 p3) = let P2 x1 y1 = p1
                                    P2 x2 y2 = p2
                                    P2 x3 y3 = p3
                                in ps_curveto x1 y1 x2 y2 x3 y3


    
drawPathFr :: MbFrame Double -> Path Double -> WumpusM ()
drawPathFr frm (Path dp pt xs) = let P2 x y = pointInFrame pt fr in do  
    ps_newpath
    ps_moveto x y
    mapM_ drawLineSeg xs
    closePath dp   
  where
    fr                        = maybe (ortho zeroPt) id frm
    drawLineSeg (Ls p)        = let P2 x y = pointInFrame p fr in ps_lineto x y
    drawLineSeg (Cs p1 p2 p3) = let P2 x1 y1 = pointInFrame p1 fr
                                    P2 x2 y2 = pointInFrame p2 fr
                                    P2 x3 y3 = pointInFrame p3 fr
                                in ps_curveto x1 y1 x2 y2 x3 y3



closePath :: DrawProp -> WumpusM ()
closePath OStroke = ps_stroke
closePath CStroke = ps_closepath >> ps_stroke
closePath CFill   = ps_closepath >> ps_fill
closePath CCrop   = ps_closepath >> ps_clip


drawLabel :: Label Double -> WumpusM ()
drawLabel (Label pt str) = let P2 x y = pt in do
    ps_moveto x y
    ps_show str

makeCTM :: Num u => (Point2 u -> Point2 u) -> (u,u,u,u,u,u)
makeCTM f = (x0-o0, x1-o1, y0-o0, y1-o1, o0, o1) where
   P2 x0 x1 = f (P2 1 0)
   P2 y0 y1 = f (P2 0 1)
   P2 o0 o1 = f (P2 0 0)



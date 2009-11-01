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
import Wumpus.Core.PictureLanguage hiding ( (<>) )
import Wumpus.Core.PostScript


import Data.FunctionExtras
import Data.Groupoid

import Data.AffineSpace
import Data.VectorSpace

import Text.PrettyPrint.Leijen

import qualified Data.Foldable  as F
import Data.List                ( mapAccumR )
import Data.Sequence            ( Seq, (|>) )
import qualified Data.Sequence  as S



data Picture u = Empty
               | Single   (Measure u) (Primitive u)
               | Multi    (Measure u) [Primitive u] -- multiple prims in same affine frame
               | Picture  (Measure u) PictureProps (Picture u) (Picture u)
  deriving (Eq,Show) 


-- Ellipses are a primitive so they can be drawn efficiently.
-- 
-- Arcs are path primitives in PostScript, however following that
-- example for Wumpus is tricky. Arcs don't have the nice 
-- properties of bezier curves where affine transformations can 
-- just transform the control points and a bounding box can be 
-- considered the bounds of the control points.
-- 
-- So if we making arc a type of path (with curve and line 
-- segment) would create a lot of hard work. Instead we use
-- the PostScript command @arc@ only to create ellipses and
-- circles (ellipses are actually circles with non-uniform 
-- scaling).
--





data Primitive u = Path1    PathProps (Path u)
                 | Label1   LabelProps (Label u) 
                 | Ellipse1 { 
                      ellipseProps  :: EllipseProps,
                      ellipseCenter :: Point2 u,
                      ellipseWidth  :: u,
                      ellipseHeight :: u 
                    } 
  deriving (Eq,Show)


data Path u = Path DrawProp (Point2 u) [PathSeg u]
  deriving (Eq,Show)

type DPath = Path Double


data PathSeg u = PCurve  (Point2 u) (Point2 u) (Point2 u)
               | PLine   (Point2 u)
  deriving (Eq,Show)

type DPathSeg = PathSeg Double

data Label u = Label (Point2 u) String
  deriving (Eq,Show)

type DLabel = Label Double


type MbColour = Maybe PSColour
type MbFont   = Maybe FontAttr

type PictureProps = (MbColour, Seq PenAttr)
type PathProps    = (MbColour, Seq PenAttr)
type LabelProps   = (MbColour, MbFont)
type EllipseProps = (MbColour, DrawProp)

-- Measure = (_current_ frame x bounding box)
-- /optimize/ the standard frame representing it as Nothing

type MbFrame u  = Maybe (Frame2 u)
type Measure u = (MbFrame u, BoundingBox u) 

type DMeasure = Measure Double

data DrawProp = OStroke | CStroke | CFill | CCrop 
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
  pretty (Path1 _ path)     = pretty "path:" <+> pretty path
  pretty (Label1 _ lbl)     = pretty lbl
  pretty (Ellipse1 _ c w h) = pretty "ellipse" <+> pretty c
                                               <+> text "w:" <> pretty w
                                               <+> text "h:" <> pretty h


instance Pretty u => Pretty (Path u) where
   pretty (Path _ pt ps) = pretty pt <> hcat (map pretty ps)

instance Pretty u => Pretty (PathSeg u) where
  pretty (PCurve p1 p2 p3)    = text ".*" <> pretty p1 <> text ",," <> pretty p2 
                                          <> text "*." <> pretty p3
  pretty (PLine pt)           = text "--" <> pretty pt

instance Pretty u => Pretty (Label u) where
  pretty (Label pt s) = dquotes (text s) <> char '@' <> pretty pt


--------------------------------------------------------------------------------

instance Groupoid (Path u) where
  Path dp st xs `gappend` Path _ st' xs' = Path dp st (xs ++ (PLine st' : xs'))


instance Pointwise (Path u) where
  type Pt (Path u) = Point2 u
  pointwise f (Path dp st xs) = Path dp (f st) (map (pointwise f) xs)

instance Pointwise (PathSeg u) where
  type Pt (PathSeg u) = Point2 u
  pointwise f (PLine p)         = PLine (f p)
  pointwise f (PCurve p1 p2 p3) = PCurve (f p1) (f p2) (f p3)
  

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
    trf (Frame2 e0 e1 o) = Frame2 (fv e0) (fv e1) (fp o)


-- Bounding boxes need recalculating after a transformation.
-- For instance after a reflection in the y-axis br becomes bl.
transformBBox :: (Num u, Ord u)
              => (Point2 u -> Point2 u) -> BoundingBox u -> BoundingBox u
transformBBox fp = trace . map fp . corners


--------------------------------------------------------------------------------


instance (Num u, Ord u) => Horizontal (Picture u) where
  type HUnit (Picture u) = u

  moveH a    = movePic (hvec a) 
  leftBound  = leftPlane . extractBounds
  rightBound = rightPlane . extractBounds

instance (Num u, Ord u) => Vertical (Picture u) where
  type VUnit (Picture u) = u

  moveV a     = movePic (vvec a) 
  topBound    = upperPlane . extractBounds
  bottomBound = lowerPlane . extractBounds

instance (Num u, Ord u) => Composite (Picture u) where
  cempty  = picEmpty

  a     `composite` Empty = a
  Empty `composite` b     = b
  a     `composite` b     = Picture (Nothing, bb) noProp a b where
                            bb = union (extractBounds a) (extractBounds b)



instance (Num u, Ord u, Horizontal (Picture u), Vertical (Picture u),
          HUnit (Picture u) ~ VUnit (Picture u)) => 
      PMove (Picture u) where
  pmove x y = movePic (V2 x y)


--------------------------------------------------------------------------------

noProp :: (MbColour,Seq a)
noProp = (Nothing,S.empty)


noFontProp :: (MbColour,MbFont)
noFontProp = (Nothing,Nothing)


nullProps :: (MbColour, Seq a) -> Bool
nullProps (Nothing,se) = S.null se
nullProps _            = False

nullFontProps :: (MbColour, MbFont) -> Bool
nullFontProps (Nothing,Nothing) = True
nullFontProps _                 = False



straightLinePath :: DrawProp -> [Point2 u] -> Path u
straightLinePath _     []     = error "straightLinePath - empty point list"
straightLinePath props (x:xs) = Path props x (map PLine xs)


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

picEllipse :: Num u => EllipseProps -> u -> u -> Picture u
picEllipse dp w h = Single (Nothing,bb) ellp where
    v    = V2 w h
    bb   = BBox (zeroPt .-^ v) (zeroPt .+^ v)
    ellp = Ellipse1 dp zeroPt w h


tracePath :: (Num u, Ord u) => Path u -> BoundingBox u
tracePath = trace . extractPoints

extractPoints :: Path u -> [Point2 u]
extractPoints (Path _ st xs) = st : foldr f [] xs where
    f (PLine p)         acc = p : acc
    f (PCurve p1 p2 p3) acc = p1 : p2 : p3 : acc 


arrange :: (Num u, Ord u)
        => (Picture u -> Picture u -> Vec2 u) 
        -> Picture u
        -> Picture u 
        -> Picture u
arrange _ a     Empty = a
arrange _ Empty b     = b
arrange f a     b     = Picture (Nothing, bb) noProp a b' where
    b' = movePic (f a b) b
    bb = union (extractBounds a) (extractBounds b')
    
   

(<..>) :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
(<..>) = arrange $ 
           twine fn (rightPlane . extractBounds) (leftPlane . extractBounds)
  where fn = hvec `oo` (-) 


(<||>) :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
(<||>) = arrange $ 
          twine fn (lowerPlane . extractBounds) (upperPlane . extractBounds)
  where fn = vvec `oo` (-)



drawBounds :: (Num u, Ord u) => Picture u -> Picture u
drawBounds Empty = Empty
drawBounds p     = p `composite` (picPath path) where
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


movePic :: Num u => Vec2 u -> Picture u -> Picture u
movePic v = mapMeasure (moveMeasure v) 

  
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
updatePrimProps f _ (Path1 p a)        = Path1 (f p) a
updatePrimProps _ g (Label1 p a)       = Label1 (g p) a
updatePrimProps _ _ (Ellipse1 _ _ _ _) = error $ "updatePrimProps - ellipse"
  -- throw an error for the time baing, 
  -- this function as a whole is ill-defined

--------------------------------------------------------------------------------
-- Render to PostScript

 
writePicture :: FilePath -> Picture Double -> IO ()
writePicture filepath pic = writeFile filepath $ psDraw pic


-- | Draw a picture, generating PostScript output.
psDraw :: Picture Double -> PostScript
psDraw pic = prologue ++ runWumpus (drawPicture pic) ++ epilogue
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

-- | DrawPicture 
-- Frame changes, representing scalings translation, rotations...
-- are drawn when they are encountered as a @concat@ statement in a 
-- block of @gsave ... grestore@.

drawPicture :: Picture Double  -> WumpusM ()
drawPicture Empty                     = return ()
drawPicture (Single (fr,_) prim)      = updateFrame fr $ drawPrimitive prim
drawPicture (Multi (fr,_) ps)         = updateFrame fr $ mapM_ drawPrimitive ps
drawPicture (Picture (fr,_) prop a b) = updatePen prop $ do
    updateFrame fr $ drawPicture  a
    updateFrame fr $ drawPicture  b

updateFrame :: MbFrame Double -> WumpusM () -> WumpusM ()
updateFrame Nothing    ma = ma
updateFrame (Just frm) ma = do
    ps_gsave
    ps_concat $ toCTM frm
    ma
    ps_grestore



bbComment :: BoundingBox Double -> WumpusM ()
bbComment (BBox (P2 x0 y0) (P2 x1 y1)) = 
    ps_comment $ "bounding-box " ++ show (x0,y0) ++ ".." ++ show (x1,y1)

drawPrimitive :: Primitive Double -> WumpusM ()
drawPrimitive (Path1 props p)           = updatePen props $ drawPath p
drawPrimitive (Label1 props l)          = updateFont props $ drawLabel l
drawPrimitive (Ellipse1 (mbc,dp) c w h) = updateColour mbc $ 
                                              drawEllipse dp c w h

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

updateColour :: MbColour -> WumpusM () -> WumpusM ()
updateColour Nothing  ma = ma 
updateColour (Just c) ma = do { ps_gsave
                              ; colourCommand c
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

    
drawPath :: Path Double -> WumpusM ()
drawPath (Path dp pt xs) = let P2 x y = pt in do  
    ps_newpath
    ps_moveto x y
    mapM_ drawPathSeg xs
    closePath dp   

drawPathSeg :: PathSeg Double -> WumpusM ()
drawPathSeg (PLine p)         = let P2 x y = p in ps_lineto x y
drawPathSeg (PCurve p1 p2 p3) = let P2 x1 y1 = p1
                                    P2 x2 y2 = p2
                                    P2 x3 y3 = p3
                                in ps_curveto x1 y1 x2 y2 x3 y3

-- | Currently this is not very good as it uses a PostScript's
-- @scale@ operator - this will vary the line width during the
-- drawing of a stroked ellipse.
drawEllipse :: DrawProp -> Point2 Double -> Double -> Double -> WumpusM ()
drawEllipse dp (P2 x y) w h 
    | w==h      = drawArc dp x y w
    | otherwise = do { ps_gsave
                     ; ps_scale 1 (h/w) -- Not so good -- changes stroke width
                     ; drawArc dp x y w
                     ; ps_grestore
                     }

drawArc :: DrawProp -> Double -> Double -> Double -> WumpusM ()
drawArc dp x y r = ps_arc x y r 0 360 >> closePath dp


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



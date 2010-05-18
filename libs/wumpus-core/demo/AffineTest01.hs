
-- ROTATE tests

module PrimAffine where

import Wumpus.Core
import Wumpus.Core.Colour ( black, red )


import Wumpus.Core.PictureInternal
import Wumpus.Core.Utils

main :: IO ()
main = sequence_ [ test_text, test_circle, test_ellipse
                 , test_control_points_ellipse ]


light_blue :: DRGB
light_blue = iRGB3 176 224 231


makePic1      :: DPrimitive -> DPicture
makePic1 prim = illustrateBounds light_blue $ rotate45 $ frame $ prim

makePic2      :: DPrimitive -> DPicture
makePic2 prim = illustrateBoundsPrim light_blue $ rot_prim
  where
   rot_prim :: DPrimitive
   rot_prim = rotate45 $ prim


-- Primitive - Text

test_text = do 
    { putStrLn "Rotated text..."
    ; writeEPS_latin1 "./out/affine_test01_text.eps" rot_text
    ; writeSVG_latin1 "./out/affine_test01_text.svg" rot_text 
    }

rgbLabel :: DRGB -> DPrimitive
rgbLabel rgb = textlabel rgb "Wumpus!" zeroPt

rot_text :: DPicture
rot_text = (makePic1 $ rgbLabel black) `picBeside` (makePic2 $ rgbLabel red)

--------------------------------------------------------------------------------
-- Primitive - Circle (special case of ellipse)

test_circle = do 
    { putStrLn "Rotated circle..."
    ; writeEPS_latin1 "./out/affine_test01_circle.eps" rot_circle
    ; writeSVG_latin1 "./out/affine_test01_circle.svg" rot_circle
    }

rgbCircle :: DRGB -> DPrimitive
rgbCircle rgb = ellipse rgb 30 30 zeroPt


rot_circle :: DPicture
rot_circle = (makePic1 $ rgbCircle black) `picBeside` (makePic1 $ rgbCircle red)

--------------------------------------------------------------------------------
-- Primitive - Ellipse

test_ellipse = do 
    { putStrLn "Rotated ellipse..."
    ; writeEPS_latin1 "./out/affine_test01_ellipse.eps" rot_ellipse
    ; writeSVG_latin1 "./out/affine_test01_ellipse.svg" rot_ellipse
    }

rgbEllipse :: DRGB -> DPrimitive
rgbEllipse rgb = ellipse rgb 30 15 zeroPt


rot_ellipse :: DPicture
rot_ellipse =   (makePic1 $ rgbEllipse black) 
    `picBeside` (makePic1 $ rgbEllipse red)

--------------------------------------------------------------------------------

test_control_points_ellipse = do 
    { putStrLn "Rotated ellipse (control points) ..."
    ; writeEPS_latin1 "./out/affine_test01_ell_cp.eps" ctrl_pt_ellipse
    ; writeSVG_latin1 "./out/affine_test01_ell_cp.svg" ctrl_pt_ellipse
    }

{-
ctrl_pt_ellipse :: DPicture
ctrl_pt_ellipse = 
    frame $ ostroke () $ vertexPath 
                       $ ellipseControlPoints 
                       $ PrimEllipse zeroPt 30 15 identityMatrix
-}

ctrl_pt_ellipse :: DPicture
ctrl_pt_ellipse = 
    illustrateBounds light_blue $ illustrateControlPoints black $ rot_prim
  where
   rot_prim :: DPrimitive
   rot_prim = rotate45 $ rgbEllipse red


output1 = mapM_ (putStrLn . show . fn) points_list
  where
    fn (p1,p2) = (intPoint p1, intPoint p2)

intPoint (P2 a b) = P2 (truncateDouble a) (truncateDouble b)

output2 = mapM_ (putStrLn . show . intPoint) $ reverse $ ellipseControlPoints 
                                             $ PrimEllipse zeroPt 30 15 identityMatrix

points_list = start $ reverse $ ellipseControlPoints $ PrimEllipse zeroPt 30 15 identityMatrix
  where
    start (s:c1:c2:e:xs) = (s,c1) : (c2,e) : rest e xs
    start _              = []

    rest s (c1:c2:e:xs)  = (s,c1) : (c2,e) : rest e xs
    rest _ _             = []


test01 = corners $ boundary (rgbEllipse red)

test02 :: (DPoint2,DPoint2,DPoint2,DPoint2)
test02 = bezierArc 30 0 (pi/2) zeroPt

test03 = ellipseBoundary zeroPt 30 15 identityMatrix

test04 = boundary (rgbEllipse red)

ellipseBoundary :: (Floating u, Ord u)
                => Point2 u -> u -> u -> Matrix3'3 u -> BoundingBox u
ellipseBoundary ctr hw hh ctm = trace $ map (new_mtrx *#) all_points
  where
    (radius,(dx,dy)) = circleScalingProps hw hh
    new_mtrx         = ctm * scalingMatrix dx dy
    circ             = bezierCircle radius ctr
    all_points       = foldr (\(a,b,c,d) acc -> d:c:b:acc ) [] circ


circleScalingProps  :: (Fractional u, Ord u) => u -> u -> (u,(u,u))
circleScalingProps hw hh  = (radius, (dx,dy))
  where
    radius     = max hw hh
    (dx,dy)    = if radius == hw then (1, rescale (0,hw) (0,1) hh)
                                 else (rescale (0,hh) (0,1) hw, 1)




-- | Make a circle from Bezier curves - @n@ is the number of 
-- subdivsions per quadrant.
bezierCircle :: Floating u 
             => u -> Point2 u -> [(Point2 u, Point2 u, Point2 u, Point2 u)]
bezierCircle radius pt = map mkQuad angs
  where
    angs         = [(0, pi*0.5), (pi*0.5,pi), (pi, pi*1.5), (pi*1.5, pi*2)]
    mkQuad (a,b) = bezierArc radius a b pt

test100 = bezierArc 30 (pi*1.5) 0      zeroPt
test101 = bezierArc 30 (pi*1.5) (pi*2) zeroPt

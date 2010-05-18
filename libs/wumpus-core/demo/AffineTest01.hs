
-- ROTATE tests

module PrimAffine where

import Wumpus.Core
import Wumpus.Core.Colour ( black, red, blue )


import Wumpus.Core.PictureInternal
import Wumpus.Core.Utils

main :: IO ()
main = sequence_ [ test_text, test_circle, test_ellipse
                 , test_control_points_circle
                 , test_control_points_ellipse
                 ]


light_blue :: DRGB
light_blue = iRGB3 176 224 231


makePic1      :: DPrimitive -> DPicture
makePic1 prim = illustrateBounds light_blue $ frame $ prim

makePic2      :: DPrimitive -> DPicture
makePic2 prim = illustrateBounds light_blue $ rotate45 $ frame $ prim

makePic3      :: DPrimitive -> DPicture
makePic3 prim = illustrateBoundsPrim light_blue $ rot_prim
  where
   rot_prim :: DPrimitive
   rot_prim = rotate45 $ prim

combine3      :: DPrimitive -> DPrimitive -> DPrimitive -> DPicture
combine3 p1 p2 p3 = 
    makePic1 p1 `picBeside` makePic2 p2 `picBeside` makePic3 p3


-- Primitive - Text

test_text = do 
    { putStrLn "Rotated text..."
    ; writeEPS_latin1 "./out/affine_test01_text.eps" rot_text
    ; writeSVG_latin1 "./out/affine_test01_text.svg" rot_text 
    }

rgbLabel :: DRGB -> DPrimitive
rgbLabel rgb = textlabel rgb "Wumpus!" zeroPt

rot_text :: DPicture
rot_text = combine3 (rgbLabel black) (rgbLabel blue) (rgbLabel red)

--------------------------------------------------------------------------------
-- Primitive - Circle (special case of ellipse)

test_circle = do 
    { putStrLn "Rotated circle..."
    ; writeEPS_latin1 "./out/affine_test01_circle.eps" rot_circle
    ; writeSVG_latin1 "./out/affine_test01_circle.svg" rot_circle
    }

rgbCircle :: DRGB -> DPrimitive
rgbCircle rgb = ellipse rgb 60 60 zeroPt


rot_circle :: DPicture
rot_circle = combine3 (rgbCircle black) (rgbCircle blue) (rgbCircle red)

--------------------------------------------------------------------------------
-- Primitive - Ellipse

test_ellipse = do 
    { putStrLn "Rotated ellipse..."
    ; writeEPS_latin1 "./out/affine_test01_ellipse.eps" rot_ellipse
    ; writeSVG_latin1 "./out/affine_test01_ellipse.svg" rot_ellipse
    }

rgbEllipse :: DRGB -> DPrimitive
rgbEllipse rgb = ellipse rgb 60 30 zeroPt


rot_ellipse :: DPicture
rot_ellipse = combine3 (rgbEllipse black) (rgbEllipse blue) (rgbEllipse red)



--------------------------------------------------------------------------------

test_control_points_circle = do 
    { putStrLn "Rotated circle (control points) ..."
    ; writeEPS_latin1 "./out/affine_test01_crc_cp.eps" ctrl_pt_circle
    ; writeSVG_latin1 "./out/affine_test01_crc_cp.svg" ctrl_pt_circle
    }

ctrl_pt_circle :: DPicture
ctrl_pt_circle = 
    illustrateBounds light_blue $ illustrateControlPoints black $ rot_prim
  where
   rot_prim :: DPrimitive
   rot_prim = rotate45 $ rgbCircle blue


--------------------------------------------------------------------------------

test_control_points_ellipse = do 
    { putStrLn "Rotated ellipse (control points) ..."
    ; writeEPS_latin1 "./out/affine_test01_ell_cp.eps" ctrl_pt_ellipse
    ; writeSVG_latin1 "./out/affine_test01_ell_cp.svg" ctrl_pt_ellipse
    }

ctrl_pt_ellipse :: DPicture
ctrl_pt_ellipse = 
    illustrateBounds light_blue $ illustrateControlPoints black $ rot_prim
  where
   rot_prim :: DPrimitive
   rot_prim = rotate45 $ rgbEllipse red


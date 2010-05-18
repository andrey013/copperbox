
-- ROTATE tests

module PrimAffine where

import Wumpus.Core
import Wumpus.Core.Colour ( black, red )

main :: IO ()
main = sequence_ [ test_text, test_circle, test_ellipse ]


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
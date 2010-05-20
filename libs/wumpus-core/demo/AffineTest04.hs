{-# LANGUAGE TypeFamilies               #-}

--------------------------------------------------------------------------------
-- ROTATE_ABOUT tests
--------------------------------------------------------------------------------

module AffineTest04 where


import AffineTestBase

import Wumpus.Core
import Wumpus.Core.Colour ( black, red, blue )


import Wumpus.Core.PictureInternal
import Wumpus.Core.Utils

main :: IO ()
main = runAlgs [ text_ata, circle_ata, ellipse_ata, path_ata ]
               [ circle_cpa, ellipse_cpa, path_cpa ]


rot30_about_origin :: (RotateAbout t, Fractional u, u ~ DUnit t) => t -> t
rot30_about_origin = rotate30About zeroPt

-- Primitive - Text


text_ata :: AffineTrafoAlg
text_ata = AffineTrafoAlg
    { ata_console_msg       = "Rotation about point, text..."
    , ata_eps_file          = "./out/affine_test/rotate_about_text.eps"
    , ata_svg_file          = "./out/affine_test/rotate_about_text.svg"
    , ata_prim_constructor  = rgbLabel
    , ata_pic_transformer   = rot30_about_origin
    , ata_prim_transformer  = rot30_about_origin
    }

circle_ata :: AffineTrafoAlg
circle_ata = AffineTrafoAlg 
    { ata_console_msg       = "Rotation about point, circle..."
    , ata_eps_file          = "./out/affine_test/rotate_about_circle.eps"
    , ata_svg_file          = "./out/affine_test/rotate_about_circle.svg"
    , ata_prim_constructor  = rgbCircle
    , ata_pic_transformer   = rot30_about_origin
    , ata_prim_transformer  = rot30_about_origin
    }


ellipse_ata :: AffineTrafoAlg
ellipse_ata = AffineTrafoAlg
    { ata_console_msg       = "Rotation about point, ellipse..."
    , ata_eps_file          = "./out/affine_test/rotate_about_ellipse.eps"
    , ata_svg_file          = "./out/affine_test/rotate_about_ellipse.svg"
    , ata_prim_constructor  = rgbEllipse
    , ata_pic_transformer   = rot30_about_origin
    , ata_prim_transformer  = rot30_about_origin
    }

path_ata :: AffineTrafoAlg
path_ata = AffineTrafoAlg
    { ata_console_msg       = "Rotation about point, path..."
    , ata_eps_file          = "./out/affine_test/rotate_about_path.eps"
    , ata_svg_file          = "./out/affine_test/rotate_about_path.svg"
    , ata_prim_constructor  = rgbPath
    , ata_pic_transformer   = rot30_about_origin
    , ata_prim_transformer  = rot30_about_origin
    }


--------------------
--------------------

circle_cpa :: ControlPointAlg
circle_cpa = ControlPointAlg 
    { cpa_console_msg       = "Rotation about point, circle (control points) ..."
    , cpa_eps_file          = "./out/affine_test/rotate_about_crc_cp.eps"
    , cpa_svg_file          = "./out/affine_test/rotate_about_crc_cp.svg"
    , cpa_prim_constructor  = rgbCircle
    , cpa_prim_transformer  = rot30_about_origin
    }



ellipse_cpa :: ControlPointAlg
ellipse_cpa = ControlPointAlg 
    { cpa_console_msg       = "Rotation about point, ellipse (control points) ..."
    , cpa_eps_file          = "./out/affine_test/rotate_about_ell_cp.eps"
    , cpa_svg_file          = "./out/affine_test/rotate_about_ell_cp.svg"
    , cpa_prim_constructor  = rgbEllipse
    , cpa_prim_transformer  = rot30_about_origin
    }


path_cpa :: ControlPointAlg
path_cpa = ControlPointAlg 
    { cpa_console_msg       = "Rotation about point, path (control points)..."
    , cpa_eps_file          = "./out/affine_test/rotate_about_path_cp.eps"
    , cpa_svg_file          = "./out/affine_test/rotate_about_path_cp.svg"
    , cpa_prim_constructor  = rgbPath
    , cpa_prim_transformer  = rot30_about_origin 
    }


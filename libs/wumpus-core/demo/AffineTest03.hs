{-# LANGUAGE TypeFamilies               #-}

--------------------------------------------------------------------------------
-- TRANSLATE tests
--------------------------------------------------------------------------------

module AffineTest03 where


import AffineTestBase

import Wumpus.Core
import Wumpus.Core.Colour ( black, red, blue )


import Wumpus.Core.PictureInternal
import Wumpus.Core.Utils

main :: IO ()
main = runAlgs [ text_ata, circle_ata, ellipse_ata, path_ata ]
               [ circle_cpa, ellipse_cpa, path_cpa ]


translate_20x40 :: (Translate t, Fractional u, u ~ DUnit t) => t -> t
translate_20x40 = translate 20.0 40.0

-- Primitive - Text


text_ata :: AffineTrafoAlg
text_ata = AffineTrafoAlg
    { ata_console_msg       = "Translated text..."
    , ata_eps_file          = "./out/affine_test/trans_text.eps"
    , ata_svg_file          = "./out/affine_test/trans_text.svg"
    , ata_prim_constructor  = rgbLabel
    , ata_pic_transformer   = translate_20x40
    , ata_prim_transformer  = translate_20x40
    }

circle_ata :: AffineTrafoAlg
circle_ata = AffineTrafoAlg 
    { ata_console_msg       = "Translated circle..."
    , ata_eps_file          = "./out/affine_test/trans_circle.eps"
    , ata_svg_file          = "./out/affine_test/trans_circle.svg"
    , ata_prim_constructor  = rgbCircle
    , ata_pic_transformer   = translate_20x40
    , ata_prim_transformer  = translate_20x40
    }


ellipse_ata :: AffineTrafoAlg
ellipse_ata = AffineTrafoAlg
    { ata_console_msg       = "Translated ellipse..."
    , ata_eps_file          = "./out/affine_test/trans_ellipse.eps"
    , ata_svg_file          = "./out/affine_test/trans_ellipse.svg"
    , ata_prim_constructor  = rgbEllipse
    , ata_pic_transformer   = translate_20x40
    , ata_prim_transformer  = translate_20x40
    }

path_ata :: AffineTrafoAlg
path_ata = AffineTrafoAlg
    { ata_console_msg       = "Translated path..."
    , ata_eps_file          = "./out/affine_test/trans_path.eps"
    , ata_svg_file          = "./out/affine_test/trans_path.svg"
    , ata_prim_constructor  = rgbPath
    , ata_pic_transformer   = translate_20x40
    , ata_prim_transformer  = translate_20x40
    }


--------------------
--------------------

circle_cpa :: ControlPointAlg
circle_cpa = ControlPointAlg 
    { cpa_console_msg       = "Translated circle (control points) ..."
    , cpa_eps_file          = "./out/affine_test/trans_crc_cp.eps"
    , cpa_svg_file          = "./out/affine_test/trans_crc_cp.svg"
    , cpa_prim_constructor  = rgbCircle
    , cpa_prim_transformer  = translate_20x40
    }



ellipse_cpa :: ControlPointAlg
ellipse_cpa = ControlPointAlg 
    { cpa_console_msg       = "Translated ellipse (control points) ..."
    , cpa_eps_file          = "./out/affine_test/trans_ell_cp.eps"
    , cpa_svg_file          = "./out/affine_test/trans_ell_cp.svg"
    , cpa_prim_constructor  = rgbEllipse
    , cpa_prim_transformer  = translate_20x40
    }


path_cpa :: ControlPointAlg
path_cpa = ControlPointAlg 
    { cpa_console_msg       = "Translated path (control points)..."
    , cpa_eps_file          = "./out/affine_test/trans_path_cp.eps"
    , cpa_svg_file          = "./out/affine_test/trans_path_cp.svg"
    , cpa_prim_constructor  = rgbPath
    , cpa_prim_transformer  = translate_20x40 
    }


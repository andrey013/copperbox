{-# OPTIONS -Wall #-}

-- Common machinery for the affine tests.

module AffineTestBase 
  ( 
    -- * test common code
    runAlgs
  , AffineTrafoAlg(..)
  , ControlPointAlg(..)

  , rgbLabel
  , rgbCircle
  , rgbEllipse
  , rgbPath

  ) where


import Wumpus.Core
import Wumpus.Core.Colour ( black, red, blue )



import System.Directory


light_blue :: DRGB
light_blue = iRGB3 176 224 231


runAlgs :: [AffineTrafoAlg] -> [ControlPointAlg] -> IO ()
runAlgs ats cps = mkDirs >> mapM_ runATA ats >> mapM_ runCPA cps
  where
    mkDirs = createDirectoryIfMissing True "./out/affine_test/"


data AffineTrafoAlg = AffineTrafoAlg
      { ata_console_msg         :: String
      , ata_eps_file            :: FilePath
      , ata_svg_file            :: FilePath
      , ata_prim_constructor    :: DRGB -> DPrimitive
      , ata_pic_transformer     :: DPicture -> DPicture
      , ata_prim_transformer    :: DPrimitive -> DPrimitive
      }

runATA :: AffineTrafoAlg -> IO ()
runATA ata = do 
    { putStrLn $ ata_console_msg ata
    ; writeEPS_latin1 (ata_eps_file ata) pic
    ; writeSVG_latin1 (ata_svg_file ata) pic 
    }
  where
    pic = buildPictureATA (ata_prim_constructor ata) 
                          (ata_pic_transformer  ata)
                          (ata_prim_transformer ata)


buildPictureATA :: (DRGB -> DPrimitive) 
         -> (DPicture -> DPicture) 
         -> (DPrimitive -> DPrimitive) 
         -> DPicture
buildPictureATA mk picF primF = 
    picture1 `picBeside` picture2 `picBeside` picture3
  where
    picture1 :: DPicture
    picture1 = illustrateBounds light_blue $ frame $ (mk black)
  
    picture2 :: DPicture
    picture2 = illustrateBounds light_blue $ picF $ frame $ (mk blue)

    picture3 :: DPicture
    picture3 = illustrateBoundsPrim light_blue $ prim
      where
        prim :: DPrimitive
        prim = primF $ mk red





--------------------------------------------------------------------------------

data ControlPointAlg = ControlPointAlg
      { cpa_console_msg         :: String
      , cpa_eps_file            :: FilePath
      , cpa_svg_file            :: FilePath
      , cpa_prim_constructor    :: DRGB -> DPrimitive
      , cpa_prim_transformer    :: DPrimitive -> DPrimitive 
      }

runCPA :: ControlPointAlg -> IO ()
runCPA cpa = do 
    { putStrLn $ cpa_console_msg cpa
    ; writeEPS_latin1 (cpa_eps_file cpa) pic
    ; writeSVG_latin1 (cpa_svg_file cpa) pic
    }
  where
    pic = cpPicture (cpa_prim_constructor cpa) (cpa_prim_transformer cpa)

cpPicture :: (DRGB -> DPrimitive) -> (DPrimitive -> DPrimitive) -> DPicture
cpPicture constr trafo = 
    illustrateBounds light_blue $ illustrateControlPoints black 
                                $ transformed_prim
  where
   transformed_prim :: DPrimitive
   transformed_prim = trafo $ constr red


--------------------------------------------------------------------------------

rgbLabel :: DRGB -> DPrimitive
rgbLabel rgb = textlabel rgb "Wumpus!" zeroPt

rgbCircle :: DRGB -> DPrimitive
rgbCircle rgb = ellipse rgb 60 60 zeroPt

rgbEllipse :: DRGB -> DPrimitive
rgbEllipse rgb = ellipse rgb 60 30 zeroPt

rgbPath :: DRGB -> DPrimitive
rgbPath rgb = ostroke rgb $ dog_kennel
--------------------------------------------------------------------------------
-- Demo - draw a dog kennel...


dog_kennel :: DPath
dog_kennel = path zeroPt [ lineTo  (P2 0 60) 
                         , lineTo  (P2 40 100)
                         , lineTo  (P2 80 60)
                         , lineTo  (P2 80 0)
                         , lineTo  (P2 60 0)  
                         , lineTo  (P2 60 30)
                         , curveTo (P2 60 50) (P2 50 60) (P2 40 60)
                         , curveTo (P2 30 60) (P2 20 50) (P2 20 30)
                         , lineTo  (P2 20 0)
                         ]
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
-- Hughes Lists for Hughes Paths

-- At some point /wumpus-extra/ will have code along these lines...

type H a = [a] -> [a]

emptyH :: H a
emptyH = id

snocH :: H a -> a -> H a
snocH hf a = hf . (a:)

toListH :: H a -> [a]
toListH = ($ [])



--------------------------------------------------------------------------------
-- /Hughes/ paths


type HPath u = (Point2 u, H (PathSegment u))

makePath :: HPath u -> Path u
makePath (s,hf) = path s (toListH hf)


start_path :: (u,u) -> HPath u
start_path (x,y) = (P2 x y, emptyH)

infixl 6 `line_to`, `curve_to`

line_to :: HPath u -> (u,u) -> HPath u
line_to (s,f) (x,y) = (s, f `snocH` lineTo (P2 x y))

curve_to :: HPath u -> ((u,u),(u,u),(u,u)) -> HPath u
curve_to (s,f) ((c1x,c1y),(c2x,c2y),(ex,ey)) = 
    (s, f `snocH` curveTo (P2 c1x c1y) (P2 c2x c2y) (P2 ex ey))


--
-- vertical (length) & horizontal (length) might
-- be handy...
-- 
-- But we would need to track current position, vis-a-vis a 
-- state monad, so this is taking things towards a big module.
--
--

--------------------------------------------------------------------------------
-- Demo - draw a dog kennel...

dog_kennel :: DPath
dog_kennel = makePath $ 
    start_path (0,0) `line_to`  (0,60)   `line_to` (40,100)
                     `line_to`  (80,60)  `line_to` (80,0)
                     `line_to`  (60,0)   `line_to` (60,30)
                     `curve_to` ((60,50), (50,60), (40,60))
                     `curve_to` ((30,60), (20,50), (20,30))
                     `line_to`  (20,0)
                        

{-# OPTIONS -Wall #-}

-- Note - how the background is built in this example is very 
-- expensive, i.e. it generates large PostScript and SVG files
-- because the text elements are drawn many more times than they
-- are actually seen.
--
-- This example just illustrates that clipping-paths work and 
-- uses a complicated background to make that point.
--


module ClipPic where

import Wumpus.Basic.Chains
import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths
import Wumpus.Basic.PictureLanguage
import Wumpus.Basic.Text.LRSymbol
import Wumpus.Basic.Text.LRText

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/clip_pic.eps" pic
    >> writeSVG_latin1 "./out/clip_pic.svg" pic


pic_drawing_ctx :: DrawingContext
pic_drawing_ctx = standardContext 14


pic :: DPicture
pic = pic1 `nextToV` (stackOver [cpic1, cpic2, cpic3] cpic4)

fillPath :: Num u => Path u -> Graphic u
fillPath = filledPath . toPrimPathU

pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing pic_drawing_ctx $ 
         localize (fillColour medium_slate_blue) $ do
            draw $ fillPath path01
            localize (fillColour powder_blue) $ 
                     draw $ fillPath path02
            draw $ fillPath path03
            draw $ fillPath path04


background :: RGBi -> DPicture 
background rgb = liftToPictureU $ execDrawing pic_drawing_ctx $ 
                   localize (strokeColour rgb) $ do                  
                     mapM_ iheartHaskell ps

   where
     ps = unchain (coordinateScalingContext 86 16) $ tableDown 18 8

cpic1 :: DPicture 
cpic1 = clip (toPrimPathU path01) (background black)
  
cpic2 :: DPicture 
cpic2 = clip (toPrimPathU path02) (background medium_violet_red)

cpic3 :: DPicture 
cpic3 = clip (toPrimPathU path03) (background black)

cpic4 :: DPicture 
cpic4 = clip (toPrimPathU path04) (background black)


iheartHaskell :: Num u => FromPtSize u => Point2 u -> Drawing u () 
iheartHaskell pt = 
    draw $ (execLRText $ char 'I' >> heart >> mapM_ char "Haskell") `at` pt


path01 :: Floating u => Path u
path01 = execPath zeroPt $ hline 80 >> rlineto (vec 112 160) 
                                    >> rlineto (vec (-112) 160)
                                    >> hline (-80)
                                    >> rlineto (vec 112 (-160))
                                    >> rlineto (vec (-112) (-160))
 

path02 :: Floating u => Path u
path02 = execPath (P2 112 0) $ hline 80 >> rlineto (vec 72 112)
                                        >> rlineto (vec 72 (-112))
                                        >> hline 80
                                        >> rlineto (vec (-224) 320)
                                        >> hline (-80)
                                        >> rlineto (vec 112 (-160))
                                        >> rlineto (vec (-112) (-160))

path03 :: Floating u => Path u
path03 = execPath (P2 384 96) $ hline 96 >> vline 56 >> hline (-136) 

path04 :: Floating u => Path u
path04 = execPath (P2 328 192) $ hline 152 >> vline 56 >> hline (-192) 


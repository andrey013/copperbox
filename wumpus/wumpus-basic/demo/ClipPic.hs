{-# OPTIONS -Wall #-}

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
pic_drawing_ctx = standardContext 9


pic :: DPicture
pic = pic1 `nextToV` (stackOver [cpic1, cpic2, cpic3] cpic4)


pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing pic_drawing_ctx $ 
         localCtx (secondaryColour medium_slate_blue) $ do
            draw $ fillPath path01
            localCtx (secondaryColour powder_blue) $ 
                     draw $ fillPath path02
            draw $ fillPath path03
            draw $ fillPath path04


background :: RGBi -> DPicture 
background rgb = liftToPictureU $ execDrawing pic_drawing_ctx $ 
                   localCtx (primaryColour rgb) $ do                  
                     mapM_ iheartHaskell ps

   where
     ps = unchain $ tableDown 8 4 56 12

cpic1 :: DPicture 
cpic1 = clip (toPrimPathU path01) (background medium_slate_blue)
  
cpic2 :: DPicture 
cpic2 = clip (toPrimPathU path02) (background powder_blue)

cpic3 :: DPicture 
cpic3 = clip (toPrimPathU path03) (background medium_slate_blue)

cpic4 :: DPicture 
cpic4 = clip (toPrimPathU path04) (background medium_slate_blue)


iheartHaskell :: Num u => FromPtSize u => Point2 u -> Drawing u () 
iheartHaskell = \pt -> mf >>= \a -> (draw $ a `at` pt)
  where
    mf = execTextM $ char 'I' >> heart >> mapM_ char "Haskell"


path01 :: Floating u => Path u
path01 = execPath zeroPt $ hline 20 >> rlineto (vec 28 40) 
                                    >> rlineto (vec (-28) 40)
                                    >> hline (-20)
                                    >> rlineto (vec 28 (-40))
                                    >> rlineto (vec (-28) (-40))
 

path02 :: Floating u => Path u
path02 = execPath (P2 28 0) $ hline 20 >> rlineto (vec 18 28)
                                       >> rlineto (vec 18 (-28))
                                       >> hline 20
                                       >> rlineto (vec (-56) 80)
                                       >> hline (-20)
                                       >> rlineto (vec 28 (-40))
                                       >> rlineto (vec (-28) (-40))

path03 :: Floating u => Path u
path03 = execPath (P2 96 24) $ hline 24 >> vline 14 >> hline (-34) 

path04 :: Floating u => Path u
path04 = execPath (P2 82 48) $ hline 38 >> vline 14 >> hline (-48) 


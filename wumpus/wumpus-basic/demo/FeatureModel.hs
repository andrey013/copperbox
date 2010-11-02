{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}



module FeatureModel where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Arrows
import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths 
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Shapes

import Wumpus.Core                      -- package: wumpus-core


import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/feature_model.eps" pic1
    writeSVG_latin1 "./out/feature_model.svg" pic1 

draw_ctx :: DrawingContext
draw_ctx = fontface courier_bold $ standardContext 18

-- Note - I haven't worked out how to do @alternative@, @or@ and
-- @repetitions@ yet.
--
         
pic1 :: Picture Double 
pic1 = liftToPictureU $ execDrawing draw_ctx $ do
    lea <- widebox "e" $ P2 150 160    
    lra <- widebox "r" $ P2  60  80
    lsa <- widebox "s" $ P2 240  80
    cmandatory_ lea lra
    cmandatory_ lea lsa

    uGa <- box "G" $ P2   0 0
    uHa <- box "H" $ P2  60 0
    uIa <- box "I" $ P2 120 0

    uAa <- box "A" $ P2 180 0
    uBa <- box "B" $ P2 240 0
    uCa <- box "C" $ P2 300 0

    cmandatory_ lra uGa
    cmandatory_ lra uHa
    cmandatory_ lra uIa

    cmandatory_ lsa uAa
    coptional_  lsa uBa
    cmandatory_ lsa uCa

    return ()


type Box u = Rectangle u


makeBox :: (Real u, Floating u, FromPtSize u) 
        => u -> String -> Point2 u -> Drawing u (Box u)
makeBox w ss pt = do 
    a <- drawi $ strokedShape $ rectangle w 20 $ pt
    drawi_ $ drawText $ plaintext ss $ center a
    return a

box :: (Real u, Floating u, FromPtSize u) 
    => String -> Point2 u -> Drawing u (Box u)
box = makeBox 40

widebox :: (Real u, Floating u, FromPtSize u) 
        => String -> Point2 u -> Drawing u (Box u)
widebox = makeBox 60


connWith :: ( Real u, Floating u, FromPtSize u ) 
         => Arrowhead u -> Box u -> Box u -> Drawing u (Path u)
connWith arrh b0 b1 = do
   lw <- lineWidth
   let p0 = south b0
   let p1 = northwards (realToFrac lw) b1
   drawi $ situ2 (strokeConnector (rightArrow connLine arrh)) p0 p1

infixr 4 `cmandatory`, `coptional`, `cmandatory_`, `coptional_`

cmandatory :: ( Real u, Floating u, FromPtSize u ) 
           => Box u -> Box u -> Drawing u (Path u)
cmandatory = connWith diskTip

coptional :: ( Real u, Floating u, FromPtSize u ) 
          => Box u -> Box u -> Drawing u (Path u)
coptional = connWith odiskTip


cmandatory_ :: ( Real u, Floating u, FromPtSize u ) 
            => Box u -> Box u -> Drawing u ()
cmandatory_ p0 p1 = connWith diskTip p0 p1 >> return ()

coptional_ :: ( Real u, Floating u, FromPtSize u ) 
           => Box u -> Box u -> Drawing u ()
coptional_ p0 p1 = connWith odiskTip p0 p1 >> return ()
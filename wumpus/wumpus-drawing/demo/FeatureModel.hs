{-# OPTIONS -Wall #-}



module FeatureModel where

import Wumpus.Drawing.Connectors
import qualified Wumpus.Drawing.Connectors.ConnectorPaths as C
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/" 
    base_metrics <- loader [ Left courier_bold ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) feature_model 
    writeEPS "./out/feature_model01.eps" pic1
    writeSVG "./out/feature_model01.svg" pic1 

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font courier_bold . metricsContext 18


-- Note - I haven't worked out how to do @alternative@, @or@ and
-- @repetitions@ yet.
--
         
feature_model :: CtxPicture
feature_model = udrawTracing (0::Double) $ do
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


-- Note - ctrCenterLine does not seem to be working well...

makeBox :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
        => u -> String -> Point2 u -> TraceDrawing u (Box u)
makeBox w ss pt = do 
    a <- drawi $ (strokedShape $ rectangle w 20) `at` pt
    drawl (center a) $ textline CENTER ss
    return a

box :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
    => String -> Point2 u -> TraceDrawing u (Box u)
box = makeBox 40

widebox :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
        => String -> Point2 u -> TraceDrawing u (Box u)
widebox = makeBox 60


connWith :: ( Real u, Floating u, InterpretUnit u ) 
         => ArrowTip -> Box u -> Box u -> TraceDrawing u (AbsPath u)
connWith arrh b0 b1 = do
   lw <- getLineWidth
   let p0 = south b0
   let p1 = projectAnchor north (realToFrac lw) b1
   drawi $ connect (rightArrow arrh connline) p0 p1

infixr 4 `cmandatory`, `coptional`, `cmandatory_`, `coptional_`

cmandatory :: ( Real u, Floating u, InterpretUnit u ) 
           => Box u -> Box u -> TraceDrawing u (AbsPath u)
cmandatory = connWith diskTip

coptional :: ( Real u, Floating u, InterpretUnit u ) 
          => Box u -> Box u -> TraceDrawing u (AbsPath u)
coptional = connWith odiskTip


cmandatory_ :: ( Real u, Floating u, InterpretUnit u ) 
            => Box u -> Box u -> TraceDrawing u ()
cmandatory_ p0 p1 = connWith diskTip p0 p1 >> return ()

coptional_ :: ( Real u, Floating u, InterpretUnit u ) 
           => Box u -> Box u -> TraceDrawing u ()
coptional_ p0 p1 = connWith odiskTip p0 p1 >> return ()


-- Cf. Parsec\'s Token module...

connline :: (Real u, Floating u, InterpretUnit u) => ConnectorPathQuery u
connline = C.connline default_connector_props

{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


-- Acknowledgment - the Arrow diagram is taken from Ross 
-- Paterson\'s slides /Arrows and Computation/.

-- NOTE - this example now rather out-of-date. Wumpus-Drawing has 
-- some new features to make node/connector drawing a bit easier.

module ArrowCircuit where

import Wumpus.Drawing.Connectors
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core


import Data.AffineSpace                         -- package: vector-space

import System.Directory


main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/"    
    base_metrics <- loader [ Right times_roman_family ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) circuit_pic
    writeEPS "./out/arrow_circuit.eps" pic1
    writeSVG "./out/arrow_circuit.svg" pic1 

 
makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font times_roman . metricsContext 11



-- Note - quite a bit of this diagram was produced /by eye/, 
-- rather than using anchors directly - e.g. the placing of the 
-- ptext labels and the anchors displaced by vectors.
--

-- Note `at` currently does not work for Shapes.
         
circuit_pic :: CtxPicture
circuit_pic = drawTracing body 

body :: TraceDrawing Double ()
body = do
    a1 <- drawi $ rrectangle 12 66 30 `at` P2 0 72
    atext a1 "CONST 0"
    a2 <- drawi $ (strokedShape $ circle 16) `at` P2 120 60
    atext a2 "IF"
    a3 <- drawi $ (strokedShape $ circle 16) `at` P2 240 28
    atext a3 "+1"
    a4 <- drawi $ (strokedShape $ rectangle 66 30) `at` P2 120 0
    atext a4 "DELAY 0"
    connWith connline (east a1) ((.+^ hvec 76) $ east a1)
    connWith connline (east a2) ((.+^ hvec 180) $ east a2)
    connWith connline ((.+^ vvec 40) $ north a2) (north a2)
    connWith connline ((.+^ vvec 16) $ north a3) (north a3)  
    connWith connaright  (south a3) (east a4)
    connWith connabar (west a4)  (southwest a2)
    ptext (P2  40  10) "next"
    ptext (P2 152 100) "reset"
    ptext (P2 252  72) "output"
    return ()

    -- Note - need a variant of /bar/ that draws UDLR only.

connWith :: ( Real u, Floating u, InterpretUnit u ) 
         => Connector u -> Anchor u -> Anchor u -> TraceDrawing u ()
connWith con a0 a1 = localize double_point_size $ 
    drawc a0 a1 (rightArrow tri45 con)


atext :: ( CenterAnchor (t u), u ~ DUnit (t u)
         , Real u, Floating u, InterpretUnit u)
      => t u -> String -> TraceDrawing u ()
atext ancr ss = 
    draw $ ccTextline ss `at` (center ancr)


ptext :: (Floating u, InterpretUnit u) 
      => Point2 u -> String -> TraceDrawing u ()
ptext pt ss = localize (font_attr times_italic 14) $ 
    draw $ ccTextline ss `at` pt


-- Note - return type is a LocImage not a shape...
--
rrectangle :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
           => Double -> u -> u -> LocImage u (Rectangle u)
rrectangle _r w h = strokedShape (rectangle w h)
    -- This should have round corners but they are currently
    -- disabled pending a re-think. 
    {- localize (round_corner_factor r) $ -} 

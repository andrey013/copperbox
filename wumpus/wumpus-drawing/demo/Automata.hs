{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

module Automata where

import Wumpus.Drawing.Connectors
import qualified Wumpus.Drawing.Connectors.ConnectorPaths as C
import Wumpus.Drawing.Extras.Loop
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs
import Wumpus.Drawing.Text.Base.Label


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

import System.Directory



main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/"    
    base_metrics <- loader [ Right times_roman_family  ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) automata
    writeEPS "./out/automata.eps" pic1
    writeSVG "./out/automata.svg" pic1 


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = 
    snap_grid_factors 60.0 60.0 . set_font times_roman . metricsContext 14


automata :: CtxPicture
automata = udrawTracing (0::Double) $ do
    q0     <- nodei (8,0)   $ state "q0"
    q1     <- drawi $ state "q1"      `mat` above_right_of q0
    q2     <- drawi $ state "q2"      `mat` below_right_of q0
    q3     <- drawi $ stopstate "q3"  `mat` below_right_of q1

    s0     <- evalQuery $ left_of q0

    draw $ label_midway_of SE (textline `flip` "0") $ straightconn q0 q1
    draw $ label_midway_of SS (textline `flip` "0") $ arrloop (center q1) (north q1) 
    draw $ label_midway_of SW (textline `flip` "1") $ straightconn q1 q3
    draw $ label_midway_of NE (textline `flip` "1") $ straightconn q0 q2
    draw $ label_midway_of NW (textline `flip` "0") $ straightconn q2 q3
    draw $ label_midway_of NN (textline `flip` "1") $ arrloop (center q2) (south q2) 
    draw $ label_atstart_of EE (textline `flip` "start") $ astraightconn s0 (west q0) 

    return ()

-- Monadic at - this is a hack that needs a rethink...
-- 
infixr 1 `mat`

mat :: InterpretUnit u => LocImage u a -> Query u (Point2 u) -> Image u a
mat img mq = liftQuery mq >>= \pt -> img `at` pt

state :: String -> DLocImage DCircle
state ss = 
    localize (set_font times_italic) $ 
        label_center_of (textline `flip` ss) $ strokedShape $ circle 20


stopstate :: String -> DLocImage DCircle 
stopstate ss = 
    localize (set_font times_italic) $ 
        label_center_of (textline `flip` ss) $ dblStrokedShape $ circle 20



straightconn :: ( Real u, Floating u, InterpretUnit u, Tolerance u
                , u ~ DUnit a, u ~ DUnit b
                , CenterAnchor a, RadialAnchor a
                , CenterAnchor b, RadialAnchor b
                )
             => a -> b -> Image u (AbsPath u)
straightconn a b =
    let (p0,p1) = radialConnectorPoints a b in connect conn_line p0 p1


astraightconn :: ( Real u, Floating u, InterpretUnit u, Tolerance u)
              => Anchor u -> Anchor u -> Image u (AbsPath u)
astraightconn p0 p1 = connect conn_line p0 p1


-- Note - there is a problem with @rightArrow@ as @loop@
-- manufactures the start and end points...
--
arrloop :: ( Real u, Floating u, InterpretUnit u, Tolerance u)
        => Anchor u -> Anchor u -> Image u (AbsPath u)
arrloop ctr p1 = 
    rightArrowPath $ loopPath zradius ctr zincl
  where
    v1      = pvec ctr p1
    zradius = vlength v1
    zincl   = vdirection v1


-- Cf. Parsec\'s Token module...

conn_line :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
         => ArrowConnector u
conn_line = rightArrowConnector default_connector_props C.conn_line tri45




rightArrowPath :: (Real u, Floating u, InterpretUnit u)
               => AbsPath u -> Image u (AbsPath u)
rightArrowPath = arrowDecoratePath Nothing (Just tri45) 

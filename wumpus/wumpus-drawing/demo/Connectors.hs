{-# OPTIONS -Wall #-}

module Connectors where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Connectors
import qualified Wumpus.Drawing.Connectors.ConnectorPaths as C
import Wumpus.Drawing.Text.DirectionZero
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core


import Data.Monoid
import System.Directory


main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/"    
    base_metrics <- loader [ Left helvetica ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) conn_pic
    writeEPS "./out/connectors.eps" pic1
    writeSVG "./out/connectors.svg" pic1 
          

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 11

conn_pic :: CtxPicture 
conn_pic = drawTracing $ tableGraphic conntable


conntable :: [(String, ConnectorPathQuery Double)]
conntable = 
    [ ("connline",      C.connline props)
    , ("connarc",       C.connarc props)
    , ("connhdiagh",    C.connhdiagh props)
    , ("connvdiagv",    C.connvdiagv props)
    , ("conndiagh",     C.conndiagh props)
    , ("conndiagv",     C.conndiagv props)
    , ("connhdiag",     C.connhdiag props)
    , ("connvdiag",     C.connvdiag props)
    , ("connabar",      C.connabar props)
    , ("connbbar",      C.connbbar props)
    , ("connaright",    C.connaright props)
    , ("connbright",    C.connbright props)
    , ("connhrr",       C.connhrr  props)
    , ("connrrh",       C.connrrh props)
    , ("connvrr",       C.connvrr props)
    , ("connrrv",       C.connrrv props)
    , ("connaloop",     C.connaloop props)
    , ("connbloop",     C.connbloop props)
    , ("connhbezier",   C.connhbezier props)
    , ("connvbezier",   C.connvbezier props)
    ]
  where
    props = default_connector_props { conn_dst_arm   = 2
                                    , conn_src_space = 0.5
                                    , conn_dst_space = 0.5 } 


tableGraphic :: [(String, ConnectorPathQuery Double)] -> TraceDrawing Double ()
tableGraphic conns = 
    draw $ runChain (mapM (onChain .  makeConnDrawing) conns) chn_alg `at` start
  where
    chn_alg   = tableDown 8 (180,64) 
    start     = P2 0 520 

 
std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18



makeConnDrawing :: (String, ConnectorPathQuery Double) -> DLocGraphic 
makeConnDrawing (ss,conn) = 
    promoteLoc $ \p0 -> fn p0 (displace (vec 60 40) p0) 
  where
    fn p0 p1   = mconcat [disk p0, disk p1, dcon p0 p1, lbl p1]

    disk pt    = localize (fill_colour red) $ dcDisk DRAW_FILL 2 `at` pt
    dcon p0 p1 = ignoreAns $ connect p0 p1 (uniformArrow curveTip conn)

    lbl  pt    = ignoreAns $ textline ss WW `at` (displace (hvec 10) pt)



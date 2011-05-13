{-# OPTIONS -Wall #-}

module Connectors where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Connectors
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
    base_metrics <- loader [ Left helvetica ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) conn_pic
    writeEPS "./out/connectors.eps" pic1
    writeSVG "./out/connectors.svg" pic1 
          

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 11

conn_pic :: CtxPicture 
conn_pic = drawTracing $ localize (dest_arm_len (0.75::Em))
                       $ tableGraphic conntable

conntable :: [(String, Connector Double)]
conntable = 
    [ ("connline",      connline)
    , ("connarc",       connarc)
    , ("connhdiagh",    connhdiagh)
    , ("connvdiagv",    connvdiagv)
    , ("conndiagh",     conndiagh)
    , ("conndiagv",     conndiagv)
    , ("connhdiag",     connhdiag)
    , ("connvdiag",     connvdiag)
    , ("connabar",      connabar)
    , ("connbbar",      connbbar)
    , ("connaright",    connaright)
    , ("connbright",    connbright)
    , ("connhrr",       connhrr)
    , ("connrrh",       connrrh)
    , ("connvrr",       connvrr)
    , ("connrrv",       connrrv)
    , ("connaloop",     connaloop)
    , ("connbloop",     connbloop)
    , ("connhbezier",   connhbezier)
    , ("connvbezier",   connvbezier)
    ]

tableGraphic :: [(String, Connector Double)] -> TraceDrawing Double ()
tableGraphic conns = 
    draw $ chain_ chn_alg (map makeConnDrawing conns) `at` start
  where
    chn_alg   = tableDown 8 (180,64) 
    start     = P2 0 520 

 
std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18



makeConnDrawing :: (String, Connector Double) -> DLocGraphic 
makeConnDrawing (ss,conn) = 
    promoteR1 $ \p0 -> fn p0 (displace 60 40 p0) 
  where
    fn p0 p1   = disk p0 `oplus` disk p1 `oplus` dcon p0 p1 `oplus` lbl p1

    disk pt    = localize (fill_colour red) $ dcDisk FILL 2 `at` pt
    dcon p0 p1 = fmap ignoreAns $ connect (uniformArrow curveTip conn) p0 p1

    lbl  pt    = fmap ignoreAns $ atStartAddr (textline ss) (dispH 10 pt) WW



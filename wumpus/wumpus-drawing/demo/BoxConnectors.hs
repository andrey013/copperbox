{-# OPTIONS -Wall #-}

module BoxConnectors where


import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Connectors
import qualified Wumpus.Drawing.Connectors.BoxConnectors as C
-- import Wumpus.Drawing.Paths hiding ( length )
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
    writeEPS "./out/box_connectors.eps" pic1
    writeSVG "./out/box_connectors.svg" pic1 
          



makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 11

conn_pic :: CtxPicture 
conn_pic = drawTracing $ tableGraphic conntable

conntable :: [(String, C.ConnectorBoxSpec Double)]
conntable = 
    [ ("conn_box",              C.conn_box)
    , ("conn_tube",             C.conn_tube)
    , ("conn_chamf_box",        C.conn_chamf_box)
    ]

tableGraphic :: [(String, C.ConnectorBoxSpec Double)] -> TraceDrawing Double ()
tableGraphic conns = 
    drawl (P2 0 520) $ runTableColumnwise 6 (180,80) $ mapM makeConnDrawing conns


 
std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18



makeConnDrawing :: (String, C.ConnectorBoxSpec Double) 
                -> Chain Double (UNil Double) 
makeConnDrawing (ss,conn) = 
    chain1 $ promoteLoc $ \p0 -> fn p0 (displace (vec 60 40) p0) 
  where
    fn p0 p1   = mconcat [ disk p0, disk p1, dcon p0 p1, lbl p1 ]

    disk pt    = localize (fill_colour red) $ dcDisk DRAW_FILL 2 `at` pt
    dcon p0 p1 = ignoreAns $ connect connbox p0 p1

    connbox    = C.renderConnectorBoxSpec default_connector_props conn

    lbl  pt    = ignoreAns $ textline WW ss `at` (displace (hvec 20) pt)


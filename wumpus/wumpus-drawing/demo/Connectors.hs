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


conntable :: [(String, ConnectorPathSpec Double)]
conntable = 
    [ ("conn_line",             C.conn_line)
    , ("conna_arc",             C.conna_arc)
    , ("connb_arc",             C.connb_arc)
    , ("conn_hdiagh",           C.conn_hdiagh)
    , ("conn_vdiagv",           C.conn_vdiagv)
    , ("conn_diagh",            C.conn_diagh)
    , ("conn_diagv",            C.conn_diagv)
    , ("conn_hdiag",            C.conn_hdiag)
    , ("conn_vdiag",            C.conn_vdiag)
    , ("conna_bar",             C.conna_bar)
    , ("connb_bar",             C.connb_bar)
    , ("conna_flam",            C.conna_flam)
    , ("connb_flam",            C.connb_flam)
    , ("conna_orthohbar",       C.conna_orthohbar)
    , ("connb_orthohbar",       C.connb_orthohbar)
    , ("conna_orthovbar",       C.conna_orthovbar)
    , ("connb_orthovbar",       C.connb_orthovbar)
    , ("conna_right",           C.conna_right)
    , ("connb_right",           C.connb_right)
    , ("conn_hrr",              C.conn_hrr )
    , ("conn_rrh",              C.conn_rrh)
    , ("conn_vrr",              C.conn_vrr)
    , ("conn_rrv",              C.conn_rrv)
    , ("conna_loop",            C.conna_loop)
    , ("connb_loop",            C.connb_loop)
    , ("conn_hbezier",          C.conn_hbezier)
    , ("conn_vbezier",          C.conn_vbezier)
    ]

props :: ConnectorProps
props = default_connector_props { conn_src_arm   = 1
                                , conn_dst_arm   = 1.5
                                , conn_src_space = 0.5
                                , conn_dst_space = 0.5 } 


tableGraphic :: [(String, ConnectorPathSpec Double)] -> TraceDrawing Double ()
tableGraphic conns = 
    drawl start $ ignoreAns $ runTableColumnwise 6 (200,80)
                $ mapM (chain1 .  makeConnDrawing) conns
  where
    start = P2 0 520 

 
std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18



makeConnDrawing :: (String, ConnectorPathSpec Double) -> DLocGraphic 
makeConnDrawing (ss,conn) = 
    promoteLoc $ \p0 -> fn p0 (displace (vec 72 42) p0) 
  where
    fn p0 p1   = mconcat [disk p0, disk p1, dcon p0 p1, lbl p1]

    disk pt    = localize (fill_colour red) $ dcDisk DRAW_FILL 2 `at` pt
    dcon p0 p1 = ignoreAns $ connect biarrow p0 p1

    lbl  pt    = ignoreAns $ textline WW ss `at` (displace (V2 10 (-10)) pt)

    biarrow    = renderConnectorConfig props conf
                    

    conf       = ConnectorConfig { conn_arrowl    = Just curveTip
                                 , conn_arrowr    = Just curveTip
                                 , conn_path_spec = conn
                                 }

                    


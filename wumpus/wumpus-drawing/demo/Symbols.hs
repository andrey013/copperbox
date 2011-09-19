{-# OPTIONS -Wall #-}

module Symbols where

import Wumpus.Drawing.Basis.DrawingPrimitives
import Wumpus.Drawing.Basis.Symbols
import Wumpus.Drawing.Colour.SVGColours
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
    base_metrics <- loader [ Left times_roman ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) symb_pic
    writeEPS "./out/symbols.eps" pic1
    writeSVG "./out/symbols.svg" pic1 
          

makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font times_roman . metricsContext 14

symb_pic :: CtxPicture 
symb_pic = drawTracing $ tableGraphic symbtable


symbtable :: [(String, LocGraphic Double)]
symbtable = 
    [ ("ocircle",               ocircle 8) 
    , ("ochar",                 ochar $ CharLiteral 'a')
    , ("ochar - bad",           ochar $ CharLiteral 'g')
    , ("ocharDescender",        ocharDescender $ CharLiteral 'g')
    , ("ocharUpright",          ocharUpright $ CharLiteral '8')
    , ("ocharUpright - bad",    ocharUpright $ CharLiteral 'a')
    , ("ocurrency",             ocurrency 8)
    , ("left_slice",            left_slice 14)
    , ("right_slice",           right_slice 14)
    , ("left_triangle",         left_triangle 14)
    , ("right_triangle",        right_triangle 14)
    , ("hbar",                  hbar 14)
    , ("vbar",                  vbar 14)
    , ("dbl_hbar",              dbl_hbar 14)
    , ("dbl_vbar",              dbl_vbar 14)
    ]


tableGraphic :: [(String, LocGraphic Double)] -> TraceDrawing Double ()
tableGraphic symbs = 
    drawl start $ ignoreAns $ runTableColumnwise 8 (180,24)
                $ mapM (chain1 .  makeSymbDrawing) symbs
  where
    start = P2 0 520 

 
std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18



makeSymbDrawing :: (String, LocGraphic Double) -> DLocGraphic 
makeSymbDrawing (ss,symb) = symb <> moveStart (hvec 20) lbl
  where
    lbl = ignoreAns $ textline WW ss



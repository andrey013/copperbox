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
makeCtx = fill_colour khaki . set_font times_roman . metricsContext 14

symb_pic :: CtxPicture 
symb_pic = drawTracing $ tableGraphic symbtable


symbtable :: [(String, LocGraphic Double)]
symbtable = 
    [ ("scircle",               scircle 8) 
    , ("fcircle",               fcircle 8) 
    , ("fscircle",              fscircle 8) 
    , ("ssquare",               ssquare 8) 
    , ("fsquare",               fsquare 8) 
    , ("fssquare",              fssquare 8) 
    , ("sleft_slice",           sleft_slice 14)
    , ("fleft_slice",           fleft_slice 14)
    , ("fsleft_slice",          fsleft_slice 14)
    , ("sright_slice",          sright_slice 14)
    , ("fright_slice",          fright_slice 14)
    , ("fsright_slice",         fsright_slice 14)
    , ("sleft_triangle",        sleft_triangle 14)
    , ("fleft_triangle",        fleft_triangle 14)
    , ("fsleft_triangle",       fsleft_triangle 14)
    , ("sright_triangle",       sright_triangle 14)
    , ("fright_triangle",       fright_triangle 14)
    , ("fsright_triangle",      fsright_triangle 14)
    , ("ochar",                 ochar $ CharLiteral 'a')
    , ("ochar - bad",           ochar $ CharLiteral 'g')
    , ("ocharDescender",        ocharDescender $ CharLiteral 'g')
    , ("ocharUpright",          ocharUpright $ CharLiteral '8')
    , ("ocharUpright - bad",    ocharUpright $ CharLiteral 'a')
    , ("ocurrency",             ocurrency 8)
    , ("hbar",                  hbar 14)
    , ("vbar",                  vbar 14)
    , ("dbl_hbar",              dbl_hbar 14)
    , ("dbl_vbar",              dbl_vbar 14)
    ]


tableGraphic :: [(String, LocGraphic Double)] -> TraceDrawing Double ()
tableGraphic symbs = 
    drawl start $ ignoreAns $ distribColumnwiseTable 14 (180,24)
                $ map makeSymbDrawing symbs
  where
    start = P2 0 520 

 
std_ctx :: DrawingContext
std_ctx = fill_colour peru $ standardContext 18



makeSymbDrawing :: (String, LocGraphic Double) -> DLocGraphic 
makeSymbDrawing (ss,symb) = symb <> moveStart (hvec 20) lbl
  where
    lbl = ignoreAns $ textline WW ss



{-# OPTIONS -Wall #-}

module TableChains where


import Wumpus.Basic.Kernel
import Wumpus.Drawing.Chains
import Wumpus.Drawing.Colour.SVGColours

import Wumpus.Core                              -- package: wumpus-core

import System.Directory

dummy1, dummy2 :: [Point2 Double]
dummy1 = innerHorizontals 20.0 (P2 (-30) 0) (P2 50 0)
dummy2 = innerHorizontals 20.0 (P2  10 0) (P2 50 0)

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx table_drawing
    writeEPS "./out/table_chains01.eps" pic1
    writeSVG "./out/table_chains01.svg" pic1

table_drawing :: CtxPicture Double
table_drawing = drawTracing $ do 
    tableGraphic
    draw $ connect (grid 10) (P2 (-20) (-20)) (P2 150 80)

tableGraphic :: (Real u, Floating u, FromPtSize u) 
             => TraceDrawing u ()
tableGraphic = do 
    draw $ filledDisk 3  `at` dstart
    draw $ filledDisk 3  `at` rstart
    zipchainWith (textline . show) [1..20::Int] downs
    zipchainWith (textline . show) [1..20::Int] rights
  where
    downs   = tableDown  4 (36,24) dstart
    rights  = tableRight 5 (36,24) rstart

    dstart  = P2 0   200   -- note grows down...
    rstart  = P2 240 200   -- ditto

 
std_ctx :: DrawingContext
std_ctx = fillColour peru $ standardContext 18



grid :: RealFrac u => u -> ConnectorGraphic u
grid incr = promoteR2 $ \p0 p1 ->
    let xs          = innerHorizontals incr p0 p1
        ys          = innerVerticals   incr p0 p1
        (V2 vx vy)  = pvec p0 p1
        no_line     = openStroke $ emptyPath p0
        vlines      = map (\pt -> straightLine (vvec vy) `at` pt) xs
        hlines      = map (\pt -> straightLine (hvec vx) `at` pt) ys
    in safeconcat no_line hlines `oplus` safeconcat no_line vlines


-- | 'safeconcat' : @ alternative * [graphic] -> Graphic
-- 
-- 'safeconcat' produces a composite Graphic from a list of 
-- Graphic. If the list is empty the alternative Graphic is used.
-- This contrasts to 'oconcat' - when used for Graphic @oconcat@ 
-- has the same type signature as @safeconcat@ but @oconcat@ 
-- considers its arguments to be an already destructured list:
-- 
-- > oconcat (head::Graphic) (rest::[Graphic])
-- 
safeconcat :: Graphic u -> [Graphic u] -> Graphic u    
safeconcat _   (x:xs) = oconcat x xs
safeconcat alt []     = alt

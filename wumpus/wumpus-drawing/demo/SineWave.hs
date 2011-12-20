{-# OPTIONS -Wall #-}


module SineWave where

import Wumpus.Drawing.Basis.DrawingPrimitives
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Extras.Grids
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Text.StandardFontDefs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader

import Wumpus.Core                              -- package: wumpus-core

-- import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import System.Directory



main :: IO ()
main = simpleFontLoader main1 >> return ()

main1 :: FontLoader -> IO ()
main1 loader = do
    createDirectoryIfMissing True "./out/"    
    base_metrics <- loader [ Left helvetica, Left times_roman ]
    printLoadErrors base_metrics
    let pic1 = runCtxPictureU (makeCtx base_metrics) grid_pic
    writeEPS "./out/sine_wave.eps" pic1
    writeSVG "./out/sine_wave.svg" pic1


makeCtx :: FontLoadResult -> DrawingContext
makeCtx = set_font helvetica . metricsContext 18


grid_pic :: CtxPicture
grid_pic = udrawTracing (0::Double) $ do 
    node (0,(-2)) $ grid (dotted_major_grid) 10 4

    drawl (P2 0 0) $ renderCatTrail OSTROKE $ sineWave 3 100 0
    draw $ renderPath_ OSTROKE $ catTrailPath (P2 0 0) (sineWave 3 100 0)

    return ()



-- curve-with-derivative
-- plot f when we know the derivative f'

cwd :: (Floating u, Ord u, Tolerance u) => u -> u -> AbsPath u
cwd c d  = curve1 p0 p1 p2 p3 where
  h  = d - c
  p0 = P2 c         (sin c)
  p1 = P2 (c+h/3)   ((sin c) + (h/3)*(cos c))
  p2 = P2 (d - h/3) ((sin d) - (h/3)*(cos d))
  p3 = P2 d         (sin d)

sine2 :: AbsPath Double
sine2 = curve1 p0 p1 p2 p3 `snocCurveTo` (p4,p5,p6) `snocCurveTo` (p7,p8,p9)
                           `snocCurveTo` (p10,p11,p12)
  where 
    p0 = P2 0 0
    p1 = P2 (100 * (1/3)) (100 * (pi / 6))
    p2 = P2 (100 * (2/3)) 100
    p3 = P2 100 100
    p4 = P2 (100 + (100 * (1/3))) 100 
    p5 = P2 (100 + (100 * (2/3))) (100 * (pi / 6))
    p6 = P2 200 0
    p7 = P2 (200 + (100 * (1/3))) (negate $ 100 * (pi / 6))
    p8 = P2 (200 + (100 * (2/3))) (-100) 
    p9 = P2 300 (-100)
    p10 = P2 (300 + (100 * (1/3))) (-100) 
    p11 = P2 (300 + (100 * (2/3))) (negate $ 100 * (pi / 6))
    p12 = P2 400 0


sinewave :: Double -> Vec2 Double -> Point2 Double -> AbsPath Double
sinewave h v0 p0 = curve1 p0 p1 p2 p3 `snocCurveTo` (p4,p5,p6) 
                                      `snocCurveTo` (p7,p8,p9)
                                      `snocCurveTo` (p10,p11,p12)
  where
    base1 = vlength v0 / 12
    h2    = h * (pi / 6)
    ang   = vdirection v0
    p1    = displace (orthoVec     base1    h2  ang) p0
    p2    = displace (orthoVec  (2*base1)   h   ang) p0
    p3    = displace (orthoVec  (3*base1)   h   ang) p0
    p4    = displace (orthoVec  (4*base1)   h   ang) p0
    p5    = displace (orthoVec  (5*base1)   h2  ang) p0
    p6    = displace (orthoVec  (6*base1)   0   ang) p0
    p7    = displace (orthoVec  (7*base1) (-h2) ang) p0
    p8    = displace (orthoVec  (8*base1) (-h)  ang) p0
    p9    = displace (orthoVec  (9*base1) (-h)  ang) p0
    p10   = displace (orthoVec (10*base1) (-h)  ang) p0
    p11   = displace (orthoVec (11*base1) (-h2) ang) p0
    p12   = displace (orthoVec (12*base1)   0   ang) p0



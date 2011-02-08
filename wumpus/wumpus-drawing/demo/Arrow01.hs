{-# OPTIONS -Wall #-}

module Arrow01 where

import Wumpus.Drawing.Paths 

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 48) arrow_pic
    writeEPS "./out/_temp_arrow01.eps" pic1
    writeSVG "./out/_temp_arrow01.svg" pic1 

         
arrow_pic :: CtxPicture Double 
arrow_pic = drawTracing $
    localize ultrathick $ do
      draw $ openStroke $ toPrimPath large_curve2
      localize (joinRound . capRound) $ do 
        draw $ openStroke $ toPrimPath $ curveyArr 24 (P2 0 50)

drawing02 :: CtxPicture Double 
drawing02 = drawTracing $
    localize ultrathick $ do
      draw $ openStroke $ toPrimPath large_curve

large_curve :: Path Double
large_curve = curve (P2 168 457) (P2 256 506) (P2 332 571) (P2 346 658)


large_curve2 :: Path Double
large_curve2 = curve (P2 0 0) (P2 88 48) (P2 164 114) (P2 178 200)

curveyArr :: Double -> Point2 Double -> Path Double
curveyArr h pt = mkCurve h pt `append` line pt pt `append` mkCurveZ h pt

mkCurve :: Double -> Point2 Double -> Path Double
mkCurve h pt = curve (pt .+^ vec (negate $ 0.45 * h) (0.5 * h))
                     (pt .+^ vec (negate $ 0.40 * h) (0.28 * h))
                     (pt .+^ vec (negate $ 0.22 * h) (0.12 * h))
                     pt
                     
                     
               


mkCurveZ :: Double -> Point2 Double -> Path Double
mkCurveZ h pt = curve pt
                     (pt .+^ vec (negate $ 0.22 * h) (negate $ 0.12 * h))
                     (pt .+^ vec (negate $ 0.40 * h) (negate $ 0.28 * h))
                     (pt .+^ vec (negate $ 0.45 * h) (negate $ 0.5 * h))

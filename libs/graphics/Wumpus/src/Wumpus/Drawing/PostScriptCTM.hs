{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.PostScriptCTM
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Datatype and primitives to handle the CTM (current-transformation-matrix)
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.PostScriptCTM where

import Wumpus.Core.Matrix
import Wumpus.Core.Vector

import Data.VectorSpace


data PsMatrix = PsMatrix { matrix_component :: DMatrix2'2, vector_component :: DVec2 }
  deriving (Eq,Show)


psMatrix :: ((Double,Double),(Double,Double)) -> (Double,Double) -> PsMatrix
psMatrix ((a,b),(c,d)) (e,f) = PsMatrix (M2'2 a b c d) (V2 e f)

-- square22 :: Double -> Double -> Double -> Double -> SquareMatrix Pair Double
-- square22 a b c d = SquareMatrix (Pair (Pair(a,b), Pair(c,d)))

-- vect2 :: Double -> Double -> Pair Double
-- vect2 e f = Pair (e,f)


initmatrix :: PsMatrix
initmatrix = PsMatrix identityMatrix zeroV


multiply :: PsMatrix -> PsMatrix -> PsMatrix
multiply (PsMatrix m v) (PsMatrix m' v') = PsMatrix (m*m') (v*v')

translate :: Double -> Double -> PsMatrix -> PsMatrix
translate trx try (PsMatrix m v) = 
    PsMatrix (m * identityMatrix) (v * V2 trx try)

rotate :: Double -> PsMatrix -> PsMatrix
rotate ang (PsMatrix m v) = 
    PsMatrix (m * M2'2 (cos ang) (sin ang) (negate $ sin ang) (cos ang))
             (v * V2 0 0)                                 


scale :: Double -> Double -> PsMatrix -> PsMatrix
scale sx sy (PsMatrix m v) = PsMatrix (scaleSq sx sy m) v


scaleSq :: Double -> Double -> DMatrix2'2 -> DMatrix2'2
scaleSq sx sy m =  (M2'2 sx 0 0 sy) * m


printmatrix :: PsMatrix -> String
printmatrix (PsMatrix m v) = ('[' :) . (fm m `sp` fv v) . (']':) $ []
  where
    fm (M2'2 a b  c d)  = shows a `sp`  shows b `sp` shows c `sp` shows d
    fv (V2 e f)         = shows e `sp` shows f

    sp :: ShowS -> ShowS -> ShowS
    sp l r = l . showChar ' ' . r







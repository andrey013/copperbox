{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.TrafoInternal
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable 
-- Portability :  GHC
--
-- Transformations on (Path) Primtives in Wumpus-Core are 
-- performed on the control points rather than transmitted to 
-- PostScript. 
-- 
-- However because text labels are opaque to Wumpus, the corner
-- start point is manipulated in Wumpus, but transformations on 
-- the actual text are communicated to PostScript as matrix 
-- transformations. 
-- 
--------------------------------------------------------------------------------


module Wumpus.Core.TrafoInternal
  (

  -- * Types
    PrimCTM(..)
  , AffineTrafo(..) 

  -- * CTM operations
  , identityCTM
  , thetaCTM

  , scaleCTM
  , rotateCTM
  , matrixRepCTM
  , translMatrixRepCTM

  -- * AffineTrafo operations
  , concatTrafos
  , matrixRepr
  
  ) where


import Wumpus.Core.Geometry
import Wumpus.Core.Utils.Common
import Wumpus.Core.Utils.FormatCombinators


-- Note - primitives are not considered to exist in an affine 
-- space. 
--
data PrimCTM u = PrimCTM 
      { ctm_scale_x     :: u
      , ctm_scale_y     :: u
      , ctm_rotation    :: Radian 
      }
  deriving (Eq,Show)




-- | Affine transformations are represented as /syntax/ so they
-- can be manipulated easily.
--
data AffineTrafo u = Matrix (Matrix3'3 u)
                   | Rotate Radian
                   | RotAbout Radian (Point2 u)
                   | Scale u u
                   | Translate u u
  deriving (Eq,Show)                 




instance PSUnit u => Format (PrimCTM u) where
  format (PrimCTM x y ang) = 
      parens (text "CTM" <+> text "sx="   <> dtruncFmt x 
                         <+> text "sy="   <> dtruncFmt y 
                         <+> text "ang="  <> format ang  )




--------------------------------------------------------------------------------
-- Manipulating the PrimCTM

identityCTM :: Num u => PrimCTM u
identityCTM = PrimCTM { ctm_scale_x = 1, ctm_scale_y = 1, ctm_rotation = 0 }

thetaCTM :: Num u => Radian -> PrimCTM u
thetaCTM ang = PrimCTM { ctm_scale_x = 1, ctm_scale_y = 1, ctm_rotation = ang }

scaleCTM :: Num u => u -> u -> PrimCTM u -> PrimCTM u
scaleCTM x1 y1 (PrimCTM sx sy ang) = PrimCTM (x1*sx) (y1*sy) ang

rotateCTM :: Radian -> PrimCTM u -> PrimCTM u
rotateCTM ang1 (PrimCTM sx sy ang) = PrimCTM sx sy (circularModulo $ ang1+ang)

matrixRepCTM :: (Floating u, Real u) => PrimCTM u -> Matrix3'3 u
matrixRepCTM (PrimCTM sx sy ang) = 
    rotationMatrix (circularModulo ang) * scalingMatrix sx sy


-- Note - the order of combining a translation (i.e. the 
-- location of a point) and the CTM is crucial as matrix
-- multiplication is not commutative.
--
-- This function encapsulates the correct order.
--
translMatrixRepCTM :: (Floating u, Real u) 
                   => u -> u -> PrimCTM u -> Matrix3'3 u
translMatrixRepCTM x y ctm = translationMatrix x y * matrixRepCTM ctm


--------------------------------------------------------------------------------
-- AffineTrafo operations



concatTrafos :: (Floating u, Real u) => [AffineTrafo u] -> Matrix3'3 u
concatTrafos = foldr (\e ac -> matrixRepr e * ac) identityMatrix

matrixRepr :: (Floating u, Real u) => AffineTrafo u -> Matrix3'3 u
matrixRepr (Matrix mtrx)        = mtrx
matrixRepr (Rotate theta)       = rotationMatrix theta
matrixRepr (RotAbout theta pt)  = originatedRotationMatrix theta pt
matrixRepr (Scale sx sy)        = scalingMatrix sx sy 
matrixRepr (Translate dx dy)    = translationMatrix dx dy


{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.TrafoInternal
-- Copyright   :  (c) Stephen Tetley 2010-2011
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
  , makeThetaCTM
  , makeTranslCTM

  , translateCTM
  , scaleCTM
  , rotateCTM
  , rotateAboutCTM
  , matrixRepCTM
  , unCTM

  -- * AffineTrafo operations
  , concatTrafos
  , matrixRepr
  
  ) where


import Wumpus.Core.AffineTrans
import Wumpus.Core.Geometry
import Wumpus.Core.Units
import Wumpus.Core.Utils.FormatCombinators


-- Primitives support affine transformations.
--
-- The control points of a path, baseline-left of text or center
-- of ellipse are transformed as points; scaling and rotation 
-- or text and ellipses are transmitted to PostScript and SVG.
-- 
-- Note - line thickness of a stroked path will not be scaled.
--
data PrimCTM u = PrimCTM 
      { ctm_transl_x    :: !u
      , ctm_transl_y    :: !u
      , ctm_scale_x     :: !u
      , ctm_scale_y     :: !u
      , ctm_rotation    :: !Radian 
      }
  deriving (Eq,Show)




-- | For Pictures - Affine transformations are represented as 
-- /syntax/ so they can be manipulated easily.
--
data AffineTrafo u = Matrix (Matrix3'3 u)
                   | Rotate Radian
                   | RotAbout Radian (Point2 u)
                   | Scale u u
                   | Translate u u
  deriving (Eq,Show)                 




instance Format u => Format (PrimCTM u) where
  format (PrimCTM dx dy sx sy ang) = 
      parens (text "CTM" <+> text "dx =" <> format dx
                         <+> text "dy =" <> format dy
                         <+> text "sx =" <> format sx 
                         <+> text "sy =" <> format sy 
                         <+> text "ang=" <> format ang  )



--------------------------------------------------------------------------------
-- Manipulating the PrimCTM

identityCTM :: Num u => PrimCTM u
identityCTM = PrimCTM { ctm_transl_x = 0
                      , ctm_transl_y = 0
                      , ctm_scale_x  = 1
                      , ctm_scale_y  = 1
                      , ctm_rotation = 0   }


makeThetaCTM :: Num u => u -> u -> Radian -> PrimCTM u
makeThetaCTM dx dy ang = PrimCTM { ctm_transl_x = dx, ctm_transl_y = dy
                                 , ctm_scale_x = 1, ctm_scale_y = 1
                                 , ctm_rotation = ang }


makeTranslCTM :: Num u => u -> u -> PrimCTM u
makeTranslCTM dx dy = PrimCTM { ctm_transl_x = dx, ctm_transl_y = dy
                              , ctm_scale_x = 1, ctm_scale_y = 1
                              , ctm_rotation = 0 }




translateCTM :: Num u => u -> u -> PrimCTM u -> PrimCTM u
translateCTM x1 y1 (PrimCTM dx dy sx sy ang) = 
    PrimCTM (x1+dx) (y1+dy) sx sy ang




-- Note - the matrix is not used entirely conventionally.
--
-- It is expected that the point is extracted from the matrix, so
-- scales and rotations operate on the point coordinates as well
-- as the scale and rotation components. 

scaleCTM :: Num u => u -> u -> PrimCTM u -> PrimCTM u
scaleCTM x1 y1 (PrimCTM dx dy sx sy ang) = 
    let P2 x y = scale x1 y1 (P2 dx dy) 
    in PrimCTM x y (x1*sx) (y1*sy) ang

rotateCTM :: (Real u, Floating u) => Radian -> PrimCTM u -> PrimCTM u
rotateCTM theta (PrimCTM dx dy sx sy ang) = 
    let P2 x y = rotate theta (P2 dx dy) 
    in PrimCTM x y sx sy (circularModulo $ theta+ang)

rotateAboutCTM :: (Real u, Floating u) 
               => Radian -> Point2 u -> PrimCTM u -> PrimCTM u
rotateAboutCTM theta pt (PrimCTM dx dy sx sy ang) = 
    let P2 x y = rotateAbout theta pt (P2 dx dy)
    in PrimCTM x y sx sy (circularModulo $ theta+ang)


-- Note - the order of combining a translation (i.e. the location 
-- of a point) and the CTM is crucial as matrix multiplication is 
-- not commutative.
--
-- This function encapsulates the correct order (or does it? - 
-- some of the demos are not working properly...).
--
matrixRepCTM :: (Real u, Floating u) => PrimCTM u -> Matrix3'3 u
matrixRepCTM (PrimCTM dx dy sx sy ang) = 
    translationMatrix dx dy * rotationMatrix (circularModulo ang) 
                            * scalingMatrix sx sy


-- | Destructor for the CTM extracts the /start point/ and a
-- /residual/ CTM. 
-- 
-- If the residual CTM is the identity CTM, the SVG or PostScript
-- output can be optimized.
-- 
unCTM :: Num u => PrimCTM  u -> (Point2 u, PrimCTM u)
unCTM (PrimCTM dx dy sx sy ang) = (P2 dx dy, PrimCTM 0 0 sx sy ang)


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


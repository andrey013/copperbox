{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Utils
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Utility functions.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Utils (
  bitwiseOr, unbits 

  ) where


import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
        VGint, VGenum, VGbitfield, 
        marshalBool )
import Data.Bits

{-
class Marshal a where marshal :: a -> VGenum
class Unmarshal a where unmarshal :: VGenum -> a 
-}

{-
marshalVGint :: VGint -> VGenum
marshalVGint = fromIntegral 
-}

{-
instance Marshal VGint where marshal = fromIntegral
instance Marshal Bool where marshal = fromIntegral . marshalBool
-}

{-
enumValue :: Marshal a => a -> VGint
enumValue = fromIntegral . marshal
-}

{-
unmarshalIntegral :: (Integral a, Unmarshal b)  => a -> b
unmarshalIntegral = unmarshal . fromIntegral
-}


bitwiseOr :: (a -> VGenum) ->  [a] -> VGbitfield
bitwiseOr fn = sum . map (fromIntegral . fn)

-- not the world's best formulation...
unbits :: (VGenum -> a) -> VGbitfield -> [a]
unbits fn field = map fn $ step field 0 1 where
    step _ i _ | i > 32         = error $ "bomb"
    step a _ _ | a <= 0         = []
    step a i j | a `testBit` i  = j : step (a - fromIntegral j) (i+1) (j*2)
               | otherwise      = step a (i+1) (j*2) 




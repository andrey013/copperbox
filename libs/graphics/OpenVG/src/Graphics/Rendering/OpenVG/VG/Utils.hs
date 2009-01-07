{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
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
  Marshal(..), Unmarshal(..), enumValue, unmarshalIntegral, 
  bitwiseOr, unbits 
) where


import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
        VGint, VGenum, VGbitfield, 
        marshalBool )
import Data.Bits

class Marshal a where marshal :: a -> VGenum
class Unmarshal a where unmarshal :: VGenum -> a 

instance Marshal VGint where marshal = fromIntegral
instance Marshal Bool where marshal = fromIntegral . marshalBool

enumValue :: Marshal a => a -> VGint
enumValue = fromIntegral . marshal


unmarshalIntegral :: (Integral a, Unmarshal b)  => a -> b
unmarshalIntegral = unmarshal . fromIntegral


bitwiseOr :: Marshal a => [a] -> VGbitfield
bitwiseOr = sum . map (fromIntegral . marshal)


-- not the world's best formulation...
unbits :: Unmarshal a => VGbitfield -> [a]
unbits bf = map unmarshal $ step bf 0 1 where
    step _ i _ | i > 32         = error $ "bomb"
    step a _ _ | a <= 0         = []
    step a i j | a `testBit` i  = j : step (a - fromIntegral j) (i+1) (j*2)
               | otherwise      = step a (i+1) (j*2) 




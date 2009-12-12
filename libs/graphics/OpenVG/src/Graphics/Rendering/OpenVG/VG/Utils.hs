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
  marshalBool, unmarshalBool,
  bitwiseOr, unbits,
  unSize, unSizeF, unSizeM

  ) where


import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
        VGint, VGenum, VGbitfield, VGboolean, vg_TRUE, vg_FALSE )

import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )


import Data.Bits




marshalBool :: Bool -> VGboolean
marshalBool x = case x of
  True -> vg_TRUE
  False -> vg_FALSE

unmarshalBool :: VGboolean -> Bool
unmarshalBool x
    | x == vg_TRUE  = True
    | x == vg_FALSE = False
    | otherwise = error ("unmarshalBool: illegal value " ++ show x)
  



bitwiseOr :: (a -> VGenum) ->  [a] -> VGbitfield
bitwiseOr fn = sum . map (fromIntegral . fn)

-- not the world's best formulation...
unbits :: (VGenum -> a) -> VGbitfield -> [a]
unbits fn field = map fn $ step field 0 1 where
    step _ i _ | i > 32         = error $ "bomb"
    step a _ _ | a <= 0         = []
    step a i j | a `testBit` i  = j : step (a - fromIntegral j) (i+1) (j*2)
               | otherwise      = step a (i+1) (j*2) 


-- Helper - unwrap Size

unSize :: Size -> (VGint,VGint)
unSize (Size w h) = (fromIntegral w, fromIntegral h)

unSizeF :: (VGint -> VGint -> a) -> Size -> a
unSizeF f (Size w h) = f (fromIntegral w) (fromIntegral h)

unSizeM :: Monad m => (VGint -> VGint -> m a) -> Size -> m a
unSizeM f (Size w h) = f (fromIntegral w) (fromIntegral h)



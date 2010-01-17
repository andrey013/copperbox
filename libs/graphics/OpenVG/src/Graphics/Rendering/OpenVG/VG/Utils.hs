{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Utils
-- Copyright   :  (c) Stephen Tetley 2008-2010
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
  bitwiseOr, unbits32,
  unSize, unSizeF, unSizeM

  ) where

import Graphics.Rendering.OpenVG.Raw.VG.Core101 ( 
    VGboolean, VGenum, VGint, VGbitfield, 
    vg_TRUE, vg_FALSE )

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

-- unbits is not productive at every step so cannot be an unfold.

unbits32 :: (VGenum -> a) -> VGbitfield -> [a]
unbits32 fn field = step field (0::Int) where
    step a i | i >= 32        = []      -- should be unreachable anyway
             | a <= 0         = []
             | a `testBit` i  = (fn $ 2 ^ i) : step (a `clearBit` i) (i+1) 
             | otherwise      = step a (i+1)


-- Helper - unwrap Size

unSize :: Size -> (VGint,VGint)
unSize (Size w h) = (fromIntegral w, fromIntegral h)

unSizeF :: (VGint -> VGint -> a) -> Size -> a
unSizeF f (Size w h) = f (fromIntegral w) (fromIntegral h)

unSizeM :: Monad m => (VGint -> VGint -> m a) -> Size -> m a
unSizeM f (Size w h) = f (fromIntegral w) (fromIntegral h)



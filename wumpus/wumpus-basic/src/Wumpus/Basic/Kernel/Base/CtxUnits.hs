{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.CtxUnits
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- \"Contextual\" units - @em@, @en@ and @AfmUnit@.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.CtxUnits
  (


    Em
  , En

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs


--------------------------------------------------------------------------------




-- | Wrapped Double representing an Em. 
-- 
newtype Em = Em { getEm :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show Em where
  showsPrec p d = showsPrec p (getEm d)

instance CxSize Em where
  cfSize sz a = fromIntegral sz * realToFrac a
  csSize sz d = fromIntegral sz * realToFrac d




-- | Wrapped Double representing an En.
-- 
newtype En = En { getEn :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show En where
  showsPrec p d = showsPrec p (getEn d)



instance CxSize En where
  cfSize sz a = (realToFrac  a) * 0.5 * fromIntegral sz
  csSize sz d = 2 * (realToFrac d) / (fromIntegral sz)

{-
-- 1 en at 1pt == 0.5
instance PtSize En where
  fromPsPoint a = En $ realToFrac $ a * 2
  toPsPoint a   = realToFrac $ a / 2

-}
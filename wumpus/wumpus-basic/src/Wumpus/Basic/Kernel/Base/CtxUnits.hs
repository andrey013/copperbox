{-# LANGUAGE MultiParamTypeClasses      #-}
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
  , UEm(..)
  , En
  , UEn(..)

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs

import Wumpus.Core                              -- package: wumpus-core

--------------------------------------------------------------------------------




-- | Wrapped Double representing an Em. 
-- 
newtype Em = Em { getEm :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show Em where
  showsPrec p d = showsPrec p (getEm d)

instance CxSize Em where
  cfSize sz a = fromIntegral sz * realToFrac a
  csSize sz d = realToFrac d / fromIntegral sz

data UEm = UEm

instance Unit UEm Em


-- | Wrapped Double representing an En.
-- 
newtype En = En { getEn :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)

instance Show En where
  showsPrec p d = showsPrec p (getEn d)


instance CxSize En where
  cfSize sz a = (realToFrac  a) * 0.5 * fromIntegral sz
  csSize sz d = 2 * (realToFrac d) / (fromIntegral sz)


data UEn = UEn

instance Unit UEn En


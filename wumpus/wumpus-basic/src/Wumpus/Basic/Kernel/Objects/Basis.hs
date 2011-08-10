{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Basis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Common types and operations.
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Basis
  (

    PrimResult

  , UConvert(..)

  , ignoreAns
  , replaceAns

  , Decorate(..)
  , sdecorate
  , adecorate

  , selaborate
  , aelaborate

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.WrappedPrimitive

import Wumpus.Core                              -- package: wumpus-core


type PrimResult u a = (a, CatPrim)



--------------------------------------------------------------------------------


class UConvert (f :: * -> * -> *) where
  uconvF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
         => f u (t u) -> f u1 (t u1)

  uconvZ :: (InterpretUnit u, InterpretUnit u1) 
         => f u a -> f u1 a


--------------------------------------------------------------------------------



-- | Note - the kind of f allows fo unit annotation.
--
ignoreAns :: Functor (f u) => f u a -> f u (UNil u)
ignoreAns = fmap (const UNil)

-- | Replace the answer produced by a graphic object.
--
replaceAns :: Functor (f u) => a -> f u z -> f u a
replaceAns a = fmap (const a)




-- | Decorate an object
--
-- oliterate - drops the graphic from the first object replacing 
-- it with the graphic from the second.
--
class Decorate (f :: * -> * -> *) where
  decorate   :: ZDeco -> f u a -> f u z -> f u a
  elaborate  :: ZDeco -> f u a -> (a -> f u z) -> f u a
  obliterate :: f u a -> f u a
  hyperlink  :: XLink -> f u a -> f u a


sdecorate :: Decorate f => f u a -> f u z -> f u a
sdecorate = decorate SUPERIOR

adecorate :: Decorate f => f u a -> f u z -> f u a
adecorate = decorate ANTERIOR


selaborate :: Decorate f => f u a -> (a -> f u z) -> f u a
selaborate = elaborate SUPERIOR

aelaborate :: Decorate f => f u a -> (a -> f u z) -> f u a
aelaborate = elaborate ANTERIOR





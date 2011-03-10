{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.ImageBasis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Experimental extras for BaseDefs...
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.ImageBasis
  (
    Localize(..)
  , Hyperlink(..)
  , Annotate(..)
  , IgnoreAns(..)
  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext

import Wumpus.Core                              -- package: wumpus-core



-- Need an equivalent to @localize@...

class Localize t where
  localize :: forall (r :: * -> *) (u :: *). 
              (DrawingContext -> DrawingContext) -> t r u -> t r u



class Hyperlink obj where
  hyperlink :: XLink -> obj -> obj

-- Would these be better as parameterized modules?

class Annotate t where 
  decorate  :: t r u -> t UNil u -> t r u
  annotate  :: t r u -> (r u -> t UNil u) -> t r u
    
class IgnoreAns t where 
  ignoreAns  :: t r u -> t UNil u
  replaceAns :: r1 u ->  t r u -> t r1 u

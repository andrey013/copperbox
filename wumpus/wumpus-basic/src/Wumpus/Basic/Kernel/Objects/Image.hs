{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Image
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Image and Graphic types - these are functional types from the 
-- DrawingContext to a graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Image
   (
     Graphic
   , Image

   , DImage
   , DGraphic

   , intoImage
   , graphic_

   , uconvImageF
   , uconvImageZ

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.Basis

import Control.Applicative


-- | Image - function from the DrawingContext to a polymorphic 
-- /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
type Image u a          = CF (ImageAns u a)



-- | Graphic - function from the DrawingContext to a graphic 
-- /primitive/.
--
type Graphic u          = CF (GraphicAns u)



-- | Type specialized version of 'Image'.
--
type DImage a           = Image Double a 


-- | Type specialized version of 'Graphic'.
--
type DGraphic           = Graphic Double         


-- | 'intoImage' : @ query * graphic -> Image @
--
-- Build an 'Image' from a context function ('CF') that generates 
-- the answer and a 'Graphic' that draws the 'Image'.
--
intoImage :: Query a -> Graphic u -> Image u a
intoImage qf ma = replaceAns <$> qf <*> ma


-- | /Downcast/ an 'Image' to a 'Graphic'.
-- 
-- This means forgetting the answer of the Image, replacing it 
-- with @()@.
--
graphic_ :: Image u a -> Graphic u
graphic_ = fmap ignoreAns





-- | Use this to convert 'Graphic' or 'Image' with Functor answer.
--
uconvImageF :: (Functor t, InterpretUnit u, InterpretUnit u1)
            => Image u (t u) -> Image u1 (t u1)
uconvImageF = uconvR0 szconvAnsF


uconvImageZ :: (InterpretUnit u, InterpretUnit u1)
            => Image u a -> Image u1 a
uconvImageZ = uconvR0 szconvAnsZ



--------------------------------------------------------------------------------


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
   , uconvGraphic
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
type Graphic u          = Image u ()



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


-- | Use this to convert 'Graphic'.
--
uconvGraphic :: (InterpretUnit u, InterpretUnit u1) 
            => Graphic u -> Graphic u1
uconvGraphic = uconvR0 szconvGraphicAns


-- | Use this to convert 'Image' with Functor answer.
--
uconvImageF :: (Functor t, InterpretUnit u, InterpretUnit u1)
            => Image u (t u) -> Image u1 (t u1)
uconvImageF = uconvR0 szconvImageAnsF


uconvImageZ :: (InterpretUnit u, InterpretUnit u1)
            => Image u a -> Image u1 a
uconvImageZ = uconvR0 szconvImageAnsZ



--------------------------------------------------------------------------------


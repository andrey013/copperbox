{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}


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
   , uconvertImg

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.Basis

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative


-- | Image - function from the DrawingContext to a polymorphic 
-- /answer/ and a graphic /primitive/.
--
-- The answer is expected to be a Functor.
--
type Image t u          = CF (ImageAns t u)


-- | Graphic - function from the DrawingContext to a graphic 
-- /primitive/.
--
type Graphic u          = Image UNil u



-- | Type specialized version of 'Image'.
--
type DImage t           = Image t Double         


-- | Type specialized version of 'Graphic'.
--
type DGraphic           = Graphic Double         


-- | 'intoImage' : @ query * graphic -> Image @
--
-- Build an 'Image' from a context function ('CF') that generates 
-- the answer and a 'Graphic' that draws the 'Image'.
--
intoImage :: Query (t u) -> Graphic u -> Image t u
intoImage = liftA2 (\a (Ans _ p) -> Ans a p)

-- | Use this to convert both 'Image' and 'Graphic'.
--
uconvertImg :: (InterpretUnit u, InterpretUnit u1, Functor t) 
            => Image t u -> Image t u1
uconvertImg = uconvertR0


--------------------------------------------------------------------------------

instance (Rotate (t Double), Functor t, InterpretUnit u) => 
    Rotate (Image t u) where
  rotate ang            = affineTransR0 (rotate ang)

instance (RotateAbout (t Double), Functor t, InterpretUnit u) => 
    RotateAbout (Image t u) where
  rotateAbout ang pt    = affineTransR0 (rotateAbout ang pt)

instance (Scale (t Double), Functor t, InterpretUnit u) => 
    Scale (Image t u) where
  scale sx sy           = affineTransR0 (scale sx sy)

instance (Translate (t Double), Functor t, InterpretUnit u) => 
    Translate (Image t u) where
  translate dx dy       = affineTransR0 (translate dx dy)



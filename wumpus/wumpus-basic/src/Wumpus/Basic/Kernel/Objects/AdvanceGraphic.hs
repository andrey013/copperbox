{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.AdvanceGraphic
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - an AdvanceGraphic is a Graphic 
-- twinned with and advance vector.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.AdvanceGraphic
  (

  -- * Advance-vector graphic
    AdvGraphic
  , DAdvGraphic

  , emptyAdvGraphic

  -- * Composition
  , advcat
  , advsep
  , advconcat
  , advspace
  , advpunctuate
  , advfill

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.ImageBasis
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space



-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- advance (width) vector as each character is drawn.
--
type AdvGraphic u      = LocImage Vec2 u

type DAdvGraphic       = AdvGraphic Double



--------------------------------------------------------------------------------



{-
-- | 'intoAdvGraphic' : @ loc_context_function * graphic -> Image @
--
-- Build an 'AdvGraphic' from a context function ('CF') that 
-- generates the answer displacement vector and a 'LocGraphic' 
-- that draws the 'AdvGraphic'.
--
intoAdvGraphic :: LocCF u (Vec2 u)
               -> LocGraphic u 
               -> AdvGraphic u
intoAdvGraphic = intoLocImage
-}

-- | 'emptyAdvGraphicAU' : @ AdvGraphic @
--
-- Build an empty 'AdvGraphic'.
-- 
-- The 'emptyAdvGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, the answer vetor generated is
-- the empty vector @(V2 0 0)@.
-- 
emptyAdvGraphic :: InterpretUnit u => AdvGraphic u
emptyAdvGraphic = replaceAns (V2 0 0) $ emptyLocGraphic


--------------------------------------------------------------------------------
-- composition

-- Note there are opportunities for extra composition operators
-- like the /picture language/...


-- Naming convention - binary functions are favoured for shorter names.

infixr 6 `advcat`
infixr 5 `advsep`


comb :: (Vec2 u -> Vec2 u -> Vec2 u)
     -> AdvGraphic u
     -> (Vec2 u -> AdvGraphic u) 
     -> AdvGraphic u
comb op gf fn = gf `bind` \a -> fn a  
  where
    bind = opbind op


-- | Concatenate the two AdvGraphics.
--
advcat :: Num u => AdvGraphic u -> AdvGraphic u -> AdvGraphic u
advcat af ag = comb (^+^) af (\v1 -> moveStart v1 ag)

-- to sort out in displacement...
moveStart :: Vec2 u -> AdvGraphic u -> AdvGraphic u
moveStart _ = id


-- | Concatenate the two AdvGraphics spacing them by the supplied 
-- vector.
--
advsep :: Num u => Vec2 u -> AdvGraphic u -> AdvGraphic u -> AdvGraphic u
advsep sep af ag = comb (\v1 v2 -> v1 ^+^ sep ^+^  v2) af
                        (\v1 -> moveStart (v1 ^+^ sep) ag)


-- | Helper function - general combiner.
--
advcombine :: AdvGraphic u 
           -> (AdvGraphic u -> AdvGraphic u -> AdvGraphic u) 
           -> [AdvGraphic u] 
           -> AdvGraphic u
advcombine empty _  []     = empty
advcombine _     op (x:xs) = step x xs
  where
    step a (b:bs) = step (a `op` b) bs
    step a []     = a



-- | Concatenate the list of AdvGraphic with 'advcat'.
--
advconcat :: InterpretUnit u => [AdvGraphic u] -> AdvGraphic u
advconcat = advcombine emptyAdvGraphic advcat



-- | Concatenate the list of AdvGraphic with 'advsep'.
--
advspace :: InterpretUnit u => Vec2 u -> [AdvGraphic u] -> AdvGraphic u
advspace sv = advcombine emptyAdvGraphic (advsep sv) 



-- | Concatenate the list of AdvGraphic with 'advsep'.
--
advpunctuate :: InterpretUnit u => AdvGraphic u -> [AdvGraphic u] -> AdvGraphic u
advpunctuate sep = 
    advcombine emptyAdvGraphic (\a b -> a `advcat` sep `advcat` b)


-- | Render the supplied AdvGraphic, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: Num u => Vec2 u -> AdvGraphic u -> AdvGraphic u
advfill sv = replaceAns sv


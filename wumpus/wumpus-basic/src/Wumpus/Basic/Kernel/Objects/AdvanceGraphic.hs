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

  , intoAdvGraphic
  , emptyAdvGraphic

  -- * Composition
{-  , catAdv
  , sepAdv
  , repeatAdv
  , concatAdv
  , spaceAdv
  , encloseAdv
  , punctuateAdv
  , fillAdv
-}
  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space



-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- advance (width) vector as each character is drawn.
--
type AdvGraphic u      = LocImage u (Vec2 u)

type DAdvGraphic       = AdvGraphic Double



--------------------------------------------------------------------------------




-- | 'intoAdvGraphic' : @ loc_context_function * graphic -> Image @
--
-- Build an 'AdvGraphic' from a context function ('CF') that 
-- generates the answer displacement vector and a 'LocGraphic' 
-- that draws the 'AdvGraphic'.
--
intoAdvGraphic :: LocQuery u (Vec2 u)
               -> LocGraphic u 
               -> AdvGraphic u
intoAdvGraphic = intoLocImage


-- | 'emptyAdvGraphicAU' : @ AdvGraphic @
--
-- Build an empty 'AdvGraphic'.
-- 
-- The 'emptyAdvGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, the answer vector generated is
-- the zero vector @(V2 0 0)@.
-- 
emptyAdvGraphic :: InterpretUnit u => AdvGraphic u
emptyAdvGraphic = fmap (fmap (replaceAns (V2 0 0))) $ emptyLocGraphic


--------------------------------------------------------------------------------
-- Combining AdvGraphics

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



{-

-- Naming convention - binary functions are favoured for shorter names.

infixr 6 `catAdv`
infixr 5 `sepAdv`



-- | Concatenate the two AdvGraphics.
--
-- Note - the concatenation uses the answer vector of the first 
-- AdvGraphic to move the start of the second AdvGraphic.
--
-- This is different behaviour to the concatenation of LocImage.
--
-- Usually @cat@ is expected to put two AdvGraphics 
-- \"side-by-side\", whereas @cat@ on a LocImage is expected to 
-- draw the second LocImage on top of the first.
--
catAdv :: Num u => AdvGraphic u -> AdvGraphic u -> AdvGraphic u
catAdv af ag = combind (^+^) af (\v1 -> moveStart (displaceVec v1) ag)



-- | Concatenate the two AdvGraphics spacing them by the supplied 
-- vector.
--
-- Note - the concatenation uses the answer vector of the first 
-- AdvGraphic and the separator to move the start of the second 
-- AdvGraphic.
--
-- This is different behaviour to the sep-concat of LocImage.
--
sepAdv :: Num u => Vec2 u -> AdvGraphic u -> AdvGraphic u -> AdvGraphic u
sepAdv sep af ag = combind (\v1 v2 -> v1 ^+^ sep ^+^  v2) af
                           (\v1 -> moveStart (displaceVec (v1 ^+^ sep)) ag)

-- | Repeat the AdvGraphic @n@ times concatenating the result.
--
repeatAdv :: InterpretUnit u => Int -> AdvGraphic u -> AdvGraphic u
repeatAdv n = concatAdv . replicate n


-- | Concatenate the list of AdvGraphic with 'catAdv'.
--
-- Note - unlike 'LocImage', AdvGraphic has a singular definition
-- of /empty/, so the list combinators like 'concatAdv' do not 
-- need to be supplied an alternative to draw when the list is 
-- empty.
--
concatAdv :: InterpretUnit u => [AdvGraphic u] -> AdvGraphic u
concatAdv = advcombine emptyAdvGraphic catAdv



-- | Concatenate the list of AdvGraphic with 'sepAdv'.
--
spaceAdv :: InterpretUnit u => Vec2 u -> [AdvGraphic u] -> AdvGraphic u
spaceAdv sv = advcombine emptyAdvGraphic (sepAdv sv) 



-- | Enclose l r x
--
encloseAdv :: InterpretUnit u 
           => AdvGraphic u -> AdvGraphic u -> AdvGraphic u -> AdvGraphic u
encloseAdv l r obj = l `catAdv` obj `catAdv` r 


-- | Concatenate the list of AdvGraphic with 'advsep'.
--
punctuateAdv :: InterpretUnit u => AdvGraphic u -> [AdvGraphic u] -> AdvGraphic u
punctuateAdv sep = 
    advcombine emptyAdvGraphic (\a b -> a `catAdv` sep `catAdv` b)


-- | Render the supplied AdvGraphic, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
fillAdv :: Num u => Vec2 u -> AdvGraphic u -> AdvGraphic u
fillAdv sv = replaceAns sv

-}
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
  , emptyAdvGraphicAU
  , emptyAdvGraphicRU


  -- * Composition
  , advcat
  , advsep
  , advconcatAU
  , advconcatRU
  , advspaceAU
  , advspaceRU
  , advpunctuateAU
  , advpunctuateRU
  , advfill

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace



-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- advance (width) vector as each character is drawn.
--
type AdvGraphic u      = LocImage Vec2 u

type DAdvGraphic       = AdvGraphic Double



--------------------------------------------------------------------------------




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


-- | 'emptyAdvGraphicAU' : @ AdvGraphic @
--
-- Build an empty 'AdvGraphic'.
-- 
-- The 'emptyAdvGraphic' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, the answer vetor generated is
-- the empty vector @(V2 0 0)@.
-- 
emptyAdvGraphicAU :: PsDouble u => AdvGraphic u
emptyAdvGraphicAU = replaceAns (V2 0 0) $ emptyLocGraphicAU

-- | Relative unit version of 'emptyAdvGraphicAU'.
--
emptyAdvGraphicRU :: CtxSize u => AdvGraphic u
emptyAdvGraphicRU = replaceAns (V2 0 0) $ emptyLocGraphicRU




--------------------------------------------------------------------------------
-- composition

-- Note there are opportunities for extra composition operators
-- like the /picture language/...


-- Naming convention - binary functions are favoured for shorter names.

infixr 6 `advcat`
infixr 5 `advsep`

comb :: Monad m 
     => (t u -> t u -> t u) 
     -> m (ImageAns t u) 
     -> (t u -> m (ImageAns t u)) 
     -> m (ImageAns t u)
comb h mf mg = mf >>= \a1 -> mg (answer a1) >>= \a2 -> 
   return $ imageAns (h (answer a1) (answer a2)) 
                     (imageOutput a1 `oplus` imageOutput a2)

-- | Concatenate the two AdvGraphics.
--
advcat :: Num u => AdvGraphic u -> AdvGraphic u -> AdvGraphic u
advcat af ag = promoteR1 $ \start -> 
                 comb (^+^)
                      (af `at` start)
                      (\v1 -> ag `at` start .+^ v1)

-- | Concatenate the two AdvGraphics spacing them by the supplied 
-- vector.
--
advsep :: Num u => Vec2 u -> AdvGraphic u -> AdvGraphic u -> AdvGraphic u
advsep sv af ag = promoteR1 $ \start -> 
                    comb (\v1 v2 -> v1 ^+^ sv ^+^  v2)
                         (af `at` start)
                         (\v1 -> ag `at` start .+^ sv ^+^ v1)

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
advconcatAU :: PsDouble u => [AdvGraphic u] -> AdvGraphic u
advconcatAU = advcombine emptyAdvGraphicAU advcat

-- | Relative unit version of 'advconcatAU'.
--
advconcatRU :: CtxSize u => [AdvGraphic u] -> AdvGraphic u
advconcatRU = advcombine emptyAdvGraphicRU advcat


-- | Concatenate the list of AdvGraphic with 'advsep'.
--
advspaceAU :: PsDouble u => Vec2 u -> [AdvGraphic u] -> AdvGraphic u
advspaceAU sv = advcombine emptyAdvGraphicAU (advsep sv) 

-- | Relative unit version of 'advspaceAU'.
--
advspaceRU :: CtxSize u => Vec2 u -> [AdvGraphic u] -> AdvGraphic u
advspaceRU sv = advcombine emptyAdvGraphicRU (advsep sv) 


-- | Concatenate the list of AdvGraphic with 'advsep'.
--
advpunctuateAU :: PsDouble u => AdvGraphic u -> [AdvGraphic u] -> AdvGraphic u
advpunctuateAU sep = 
    advcombine emptyAdvGraphicAU (\a b -> a `advcat` sep `advcat` b)

-- | Relative unit version of 'advpunctuateAU'.
--
advpunctuateRU :: CtxSize u => AdvGraphic u -> [AdvGraphic u] -> AdvGraphic u
advpunctuateRU sep = 
    advcombine emptyAdvGraphicRU (\a b -> a `advcat` sep `advcat` b)
    

-- | Render the supplied AdvGraphic, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: Num u => Vec2 u -> AdvGraphic u -> AdvGraphic u
advfill sv = replaceAns sv
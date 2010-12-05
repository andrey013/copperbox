{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.AdvanceGraphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - an AdvanceGraphic is a Graphic 
-- twinned with and AdvanceV vector.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.AdvanceGraphic
  (

  -- * Advance-vector graphic
    AdvGraphic
  , DAdvGraphic


  , makeAdvGraphic
  , extractLocGraphic
  , runAdvGraphic

  -- * Composition
  , advplus
  , advconcat

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.LocGraphic
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core




-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- width (advance) vector as each character is drawn.
--
type AdvGraphic u      = LocImage u (Point2 u)

type DAdvGraphic       = AdvGraphic Double


type instance DUnit (AdvGraphic u) = u






--------------------------------------------------------------------------------



-- | Construction is different to intoZZ functions hence the 
-- different name.
--
makeAdvGraphic :: PointDisplace u
               -> LocGraphic u 
               -> AdvGraphic u
makeAdvGraphic pf df = postcomb1 (,) (postpro1 pf locPoint) df


extractLocGraphic :: AdvGraphic u -> LocGraphic u
extractLocGraphic = postpro1 snd

runAdvGraphic :: DrawingContext  -> Point2 u -> AdvGraphic u 
              -> (Point2 u, PrimGraphic u)
runAdvGraphic ctx pt df = runCF ctx (unCF1 pt df)



--------------------------------------------------------------------------------
-- composition

-- Note there are opportunities for extra composition operators
-- like the /picture language/...

infixr 6 `advplus`

advplus :: AdvGraphic u -> AdvGraphic u -> AdvGraphic u
advplus = accumulate1 oplus


advconcat :: Num u => [AdvGraphic u] -> AdvGraphic u
advconcat []     = makeAdvGraphic id emptyLocGraphic
advconcat [x]    = x
advconcat (x:xs) = x `advplus` advconcat xs 

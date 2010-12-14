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
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative

-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- width (advance) vector as each character is drawn.
--
type AdvGraphic u      = LocImage u (Point2 u)

type DAdvGraphic       = AdvGraphic Double



--------------------------------------------------------------------------------



-- | Construction is different to intoZZ functions hence the 
-- different name.
--
makeAdvGraphic :: DrawingInfo (PointDisplace u)
               -> LocGraphic u 
               -> AdvGraphic u
makeAdvGraphic dispf gf = postcomb1 fn (promote1 $ \pt -> dispf `at` pt) gf
  where
    fn ans (_,prim) = (ans, prim)




-- This should probably go - the name is not exact enough...

extractLocGraphic :: AdvGraphic u -> LocGraphic u
extractLocGraphic = postpro1 (replaceL uNil)

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
advconcat []     = makeAdvGraphic (pure id) emptyLocGraphic
advconcat [x]    = x
advconcat (x:xs) = x `advplus` advconcat xs 

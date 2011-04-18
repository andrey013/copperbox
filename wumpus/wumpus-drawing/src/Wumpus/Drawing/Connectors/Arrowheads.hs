{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Connectors.Arrowheads
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Arrowheads.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Connectors.Arrowheads
  (
   
    ArrowTip 
    
  , leftRightArrow      -- temporarily here

  , tri90

  ) where


import Wumpus.Drawing.Connectors.ConnectorPaths ( Connector )
import Wumpus.Drawing.Paths.Absolute ( AbsPath )
import qualified Wumpus.Drawing.Paths.Absolute as Abs
import Wumpus.Drawing.Paths.Relative

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative

type ArrowConnector u = ConnectorImage u (AbsPath u)

type TipDraw = Point2 Em -> Radian -> GraphicAns Em


data ArrowTip u = ArrowTip { getArrowTip :: CF (u, TipDraw) }


-- | Connector with two arrow tips, possibly different.
--
leftRightArrow :: (Real u, Floating u, InterpretUnit u) 
               => ArrowTip u -> ArrowTip u -> Connector u 
               -> ArrowConnector u
leftRightArrow tipl tipr conn = promoteR2 $ \p0 p1 ->
    apply2R2 conn p0 p1 >>= \full_path -> 
    getArrowTip tipl    >>= \(dl,mkl) -> 
    getArrowTip tipr    >>= \(dr,mkr) -> 
    uconvertCtxF p0     >>= \emp0     ->       
    uconvertCtxF p1     >>= \emp1     ->       
    let angl            = tipDirectionL dl full_path
        angr            = tipDirectionR dr full_path
        short_path      = Abs.shortenPath dl dr full_path
        deco            = convertTipAns $ mkl emp0 angl `oplus`  mkr emp1 angr  
    in fmap (replaceAns full_path) $ 
         decorateR0 deco $ Abs.toPrimPath short_path >>= openStroke


convertTipAns :: InterpretUnit u => GraphicAns Em -> Graphic u
convertTipAns = uconvImageF . pure 

-- | Helper - direction looks best at half the retract distance.
--
tipDirectionL :: (Real u, Floating u) => u -> AbsPath u -> Radian
tipDirectionL u absp | u <= 0   = Abs.directionL absp
                     |otherwise = Abs.directionL $ Abs.shortenL (0.5*u) absp
   
tipDirectionR :: (Real u, Floating u) => u -> AbsPath u -> Radian
tipDirectionR u absp | u <= 0   = Abs.directionR absp
                     |otherwise = Abs.directionR $ Abs.shortenR (0.5*u) absp
   


makeArrowTip :: Query u -> LocThetaGraphic Em -> ArrowTip u
makeArrowTip mq gf = ArrowTip body
  where
    body = drawingCtx >>= \ctx -> 
           let rdist = runCF ctx mq
               drawf = runCF ctx gf
           in return (rdist,drawf)

filledTipPath :: RelBuild Em a -> LocThetaGraphic Em
filledTipPath path_spec = 
    localize fill_use_stroke_colour $ promoteR2 $ \pt ang -> 
       let mf = setIncline ang >> path_spec 
       in toPrimPath pt (evalRelBuild mf) >>= filledPath
  


tri90 :: InterpretUnit u => ArrowTip u
tri90 = makeArrowTip (uconvertCtx1 (1::Em)) (filledTipPath spec)
  where
    spec = move_up_left 1 >> move_down 2 >> move_up_right 1
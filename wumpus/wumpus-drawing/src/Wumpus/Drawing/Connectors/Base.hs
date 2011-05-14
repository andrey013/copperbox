{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Connectors.Base
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Connectors...
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Connectors.Base
  ( 

    Connector    

  , ArrowAlg(..)
  , rightArrow'
  , rightArrowPath
  , ArrowConnector
  , ArrowTip
  , makeArrowTip

  , leftArrow
  , rightArrow  
  , leftRightArrow
  , uniformArrow  
  
  , promoteConn

  ) where

import Wumpus.Drawing.Paths.Absolute

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


import Control.Applicative


-- | The type of Connectors - a query from start and end point to 
-- a Path.
--
type Connector u = ConnectorQuery u (AbsPath u)

-- | Arrowhead /algorithm/ the components of an arrowhead.
-- 
-- Retract distance may have to account for line width.
--
data ArrowAlg = ArrowAlg
      { retract_distance :: Double -> En
      , tip_half_len     :: En
      , tip_deco         :: LocThetaGraphic En
      }

-- Ideally there should be a plus operation to combine tips 
-- allowing double tips.
-- 

type ArrowConnector u = ConnectorImage u (AbsPath u)

type TipDraw = Point2 En -> Radian -> GraphicAns En

-- | TipAns - retract_distance * tip length * TipDraw
--
data TipAns u = TipAns
      { retract_dist :: u
      , tip_length   :: u
      , tip_draw     :: TipDraw
      }


data ArrowTip u = ArrowTip { getArrowTip :: CF (TipAns u) }

runArrowAlg :: InterpretUnit u => ArrowAlg -> CF (u, u, LocThetaGraphic u)
runArrowAlg (ArrowAlg df len deco) = 
   getLineWidth             >>= \lw    -> 
   uconvertCtx1 (df lw)     >>= \uretd ->
   uconvertCtx1 len         >>= \ulen  ->
   return (uretd, ulen, uconvLocThetaImageF deco)


-- | Connector with an arrow tip at the end point (i.e right).
--
rightArrow' :: (Real u, Floating u, InterpretUnit u) 
            => ArrowAlg -> Connector u -> ArrowConnector u
rightArrow' alg conn = promoteR2 $ \p0 p1 ->
    apply2R2 conn p0 p1 >>= \full_path -> 
    rightArrowPath alg full_path 

-- | Connector with an arrow tip at the end point (i.e right).
--
-- TODO - shortening a curve does not seem to be working properly...
-- 
--
rightArrowPath :: (Real u, Floating u, InterpretUnit u) 
               => ArrowAlg -> AbsPath u -> Image u (AbsPath u)
rightArrowPath alg full_path =
    runArrowAlg alg     >>= \(retract, len, deco) -> 
    let short_path      = if retract > 0 then shortenR retract full_path 
                                         else full_path
        mid_ang         = tipDirectionR len full_path
        tip             = apply2R2 deco (tipR full_path) mid_ang
    in fmap (replaceAns full_path) $ 
         decorateR0 tip $ toPrimPath short_path >>= dcOpenPath



-- Note - extracting TipDraw is unsafe. 
--
-- The code below relies on there being no changes to the 
-- DrawingContext between the tip being extracted and when it is 
-- used to decorate the Image.
--
-- This means TipAns cannot be exported and any decoration with
-- arrow tips has to be defined in this module.
--
--
-- Is there a better definition for TipAns?
-- (preferably where the decoration is a LocThetaGraphic ?)
--

-- | Connector with an arrow tip at the start point (i.e left).
--
leftArrow :: (Real u, Floating u, InterpretUnit u) 
          => ArrowTip u -> Connector u -> ArrowConnector u
leftArrow tipl conn = promoteR2 $ \p0 p1 ->
    apply2R2 conn p0 p1 >>= \full_path -> 
    getArrowTip tipl    >>= \(TipAns dxl wl mkl) -> 
    uconvertCtxF p0     >>= \emp0     ->       
    let angl            = tipDirectionL wl full_path
        short_path      = shortenL dxl full_path
        deco            = convertTipAns $ mkl emp0 angl
    in fmap (replaceAns full_path) $ 
         decorateR0 deco $ toPrimPath short_path >>= dcOpenPath

-- | Connector with an arrow tip at the end point (i.e right).
--
rightArrow :: (Real u, Floating u, InterpretUnit u) 
           => ArrowTip u -> Connector u -> ArrowConnector u
rightArrow tipr conn = promoteR2 $ \p0 p1 ->
    apply2R2 conn p0 p1 >>= \full_path -> 
    getArrowTip tipr    >>= \(TipAns dxr wr mkl) -> 
    uconvertCtxF p1     >>= \emp1     ->       
    let angr            = tipDirectionR wr full_path
        short_path      = shortenR dxr full_path
        deco            = convertTipAns $ mkl emp1 angr
    in fmap (replaceAns full_path) $ 
         decorateR0 deco $ toPrimPath short_path >>= dcOpenPath


-- | Connector with two arrow tips, possibly different.
--
leftRightArrow :: (Real u, Floating u, InterpretUnit u) 
               => ArrowTip u -> ArrowTip u -> Connector u 
               -> ArrowConnector u
leftRightArrow tipl tipr conn = promoteR2 $ \p0 p1 ->
    apply2R2 conn p0 p1 >>= \full_path -> 
    getArrowTip tipl    >>= \(TipAns dxl wl mkl) -> 
    getArrowTip tipr    >>= \(TipAns dxr wr mkr) -> 
    uconvertCtxF p0     >>= \emp0     ->       
    uconvertCtxF p1     >>= \emp1     ->       
    let angl            = tipDirectionL wl full_path
        angr            = tipDirectionR wr full_path
        short_path      = shortenPath dxl dxr full_path
        deco            = convertTipAns $ mkl emp0 angl `oplus`  mkr emp1 angr  
    in fmap (replaceAns full_path) $ 
         decorateR0 deco $ toPrimPath short_path >>= dcOpenPath


-- | Connector with the same arrow tip at both ends.
--
uniformArrow :: (Real u, Floating u, InterpretUnit u) 
             => ArrowTip u -> Connector u -> ArrowConnector u
uniformArrow utip conn = leftRightArrow utip utip conn




convertTipAns :: InterpretUnit u => GraphicAns En -> Graphic u
convertTipAns = uconvImageF . pure 

-- | Helper - direction looks best at half the retract distance.
--
tipDirectionL :: (Real u, Floating u) => u -> AbsPath u -> Radian
tipDirectionL u absp | u <= 0   = directionL absp
                     |otherwise = directionL $ shortenL (0.5*u) absp
   
tipDirectionR :: (Real u, Floating u) => u -> AbsPath u -> Radian
tipDirectionR u absp | u <= 0   = directionR absp
                     |otherwise = directionR $ shortenR (0.5*u) absp
   


makeArrowTip :: Query u -> Query u -> LocThetaGraphic En -> ArrowTip u
makeArrowTip retractq lengthq gf = ArrowTip body
  where
    body = drawingCtx >>= \ctx -> 
           let rdist = runCF ctx retractq
               tlen  = runCF ctx lengthq
               drawf = runCF ctx gf
           in return $ TipAns { retract_dist  = rdist
                              , tip_length    = tlen
                              , tip_draw      = drawf }


-- | Promote a function from source and dest points to a connector 
-- function accounting for the separator values in the 
-- DrawingContext.
--
-- This should be used instead of @promoteR2@ for functions 
-- building connectors.
--
promoteConn :: (Real u, Floating u, InterpretUnit u) 
            => (Point2 u -> Point2 u -> CF a) 
            -> CF (Point2 u -> Point2 u -> a)
promoteConn fn = promoteR2 $ \p0 p1 -> 
    connectorSrcSpace  >>= \sep0 ->
    connectorDstSpace  >>= \sep1 ->
    connectorSrcOffset >>= \off0 ->
    connectorDstOffset >>= \off1 ->
    let ang = vdirection $ pvec p0 p1
    in fn (dispPerpendicular off0 ang $ p0 .+^ avec ang sep0) 
          (dispPerpendicular off1 ang $ p1 .-^ avec ang sep1)
   
--
-- CAUTION - promoteConn projects the spacers along the (straight)
-- connector line. This might not be what is wanted for jointed
-- connectors.
-- 
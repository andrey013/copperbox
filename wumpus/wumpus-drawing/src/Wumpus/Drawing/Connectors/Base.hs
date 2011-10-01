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


    ConnectorPathQuery

  , ArrowTip(..)
  , ArrowConnector

  , leftArrow
  , rightArrow  
  , leftRightArrow
  , uniformArrow  

  , rightArrowPath

  ) where

import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid




-- | The type of Connectors - a query from start and end point 
-- returning an AbsPath.
--
type ConnectorPathQuery u = ConnectorQuery u (AbsPath u)



-- | Arrowhead /algorithm/ - the components of an arrowhead.
-- 
-- Retract distance is rather vague - depending on the arrowhead
-- it may represent a flush join between the path and the tip
-- or a join that uses the z-order (tip over path) to create the 
-- join.
--
-- \*\* WARNING \*\* - pending revision...
--
data ArrowTip = ArrowTip
      { retract_distance :: En
      , tip_half_len     :: En
      , tip_deco         :: LocThetaGraphic En
      }

-- Ideally there should be a plus operation to combine tips 
-- allowing double tips.
-- 

type ArrowConnector u = ConnectorImage u (AbsPath u)



runArrowTip :: InterpretUnit u 
            => ArrowTip -> Query u (u, u, LocThetaGraphic u)
runArrowTip (ArrowTip rd hlen deco) = 
   uconvertCtx1 rd     >>= \urd ->
   uconvertCtx1 hlen   >>= \uhlen  ->
   return (urd, uhlen, uconvF deco)


-- | Connector with an arrow tip at the end point (i.e right).
--
rightArrow :: (Real u, Floating u, InterpretUnit u) 
           => ArrowTip -> ConnectorPathQuery u -> ArrowConnector u
rightArrow alg conn = promoteConn $ \p0 p1 ->
    applyConn (liftConnectorQuery conn) p0 p1 >>= \full_path -> 
    rightArrowPath alg full_path 



-- | Connector with an arrow tip at the start point (i.e left).
--
leftArrow :: (Real u, Floating u, InterpretUnit u) 
            => ArrowTip -> ConnectorPathQuery u -> ArrowConnector u
leftArrow alg conn = promoteConn $ \p0 p1 ->
    applyConn (liftConnectorQuery conn) p0 p1 >>= \full_path -> 
    leftArrowPath alg full_path 


-- | Connector with different arrow tips at the start point and 
-- end points.
--
leftRightArrow :: (Real u, Floating u, InterpretUnit u) 
               => ArrowTip -> ArrowTip -> ConnectorPathQuery u -> ArrowConnector u
leftRightArrow algl algr conn = promoteConn $ \p0 p1 ->
    applyConn (liftConnectorQuery conn) p0 p1 >>= \full_path -> 
    leftRightArrowPath algl algr full_path 



-- | Connector with the same arrow tip at the start point and 
-- end points.
--
uniformArrow :: (Real u, Floating u, InterpretUnit u) 
               => ArrowTip -> ConnectorPathQuery u -> ArrowConnector u
uniformArrow alg conn = promoteConn $ \p0 p1 ->
    applyConn (liftConnectorQuery conn) p0 p1 >>= \full_path -> 
    leftRightArrowPath alg alg full_path 



-- TODO - possible there are opportunities to be more 
-- compositional here.


-- | Path with an arrow tip at the start point (i.e left).
--
-- TODO - shortening a curve does not seem to be working properly...
-- 
--
leftArrowPath :: (Real u, Floating u, InterpretUnit u) 
              => ArrowTip -> AbsPath u -> Image u (AbsPath u)
leftArrowPath alg full_path =
    liftQuery (runArrowTip alg) >>= \(retract, len, deco) -> 
    let short_path      = if retract > 0 then shortenL retract full_path 
                                         else full_path
        mid_ang         = tipDirectionL len full_path
        tip             = applyLocTheta deco (tipL full_path) mid_ang
    in replaceAns full_path $ 
         sdecorate tip $ drawPath OSTROKE short_path



-- | Path with an arrow tip at the end point (i.e right).
--
-- TODO - shortening a curve does not seem to be working properly...
-- 
--
rightArrowPath :: (Real u, Floating u, InterpretUnit u) 
               => ArrowTip -> AbsPath u -> Image u (AbsPath u)
rightArrowPath alg full_path =
    liftQuery (runArrowTip alg) >>= \(retract, len, deco) -> 
    let short_path      = if retract > 0 then shortenR retract full_path 
                                         else full_path
        mid_ang         = tipDirectionR len full_path
        tip             = applyLocTheta deco (tipR full_path) mid_ang
    in replaceAns full_path $ 
         sdecorate tip $ drawPath OSTROKE short_path




-- | Path with an arrow tip at the end point (i.e right).
--
-- TODO - shortening a curve does not seem to be working properly...
-- 
--
leftRightArrowPath :: (Real u, Floating u, InterpretUnit u) 
                   => ArrowTip -> ArrowTip -> AbsPath u -> Image u (AbsPath u)
leftRightArrowPath algl algr full_path =
    liftQuery (runArrowTip algl) >>= \(retractl, lenl, decol) -> 
    liftQuery (runArrowTip algr) >>= \(retractr, lenr, decor) -> 
    let short_path      = shortenPath retractl retractr full_path
        mid_angl        = tipDirectionL lenl full_path
        mid_angr        = tipDirectionR lenr full_path
        tipl            = applyLocTheta decol (tipL full_path) mid_angl
        tipr            = applyLocTheta decor (tipR full_path) mid_angr
    in replaceAns full_path $ 
         sdecorate (tipl `mappend` tipr) $ drawPath OSTROKE short_path
          




-- | Helper - direction looks best at half the retract distance.
--
tipDirectionL :: (Real u, Floating u) => u -> AbsPath u -> Radian
tipDirectionL u absp | u <= 0   = directionL absp
                     |otherwise = directionL $ shortenL (0.5*u) absp
   
tipDirectionR :: (Real u, Floating u) => u -> AbsPath u -> Radian
tipDirectionR u absp | u <= 0   = directionR absp
                     |otherwise = directionR $ shortenR (0.5*u) absp
   



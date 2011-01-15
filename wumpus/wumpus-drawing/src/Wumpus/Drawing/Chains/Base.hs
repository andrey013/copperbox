{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Chains.Base
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Generate points in an iterated chain.
--
-- \*\* WARNING \*\* - unstable. Names are not so good, also 
-- Wumpus-Basic has a @chain1@ operator...
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Chains.Base
  (

    Chain
  , LocChain
  , ConnectorChain
  , unchain
  , zipchain
  , zipchainWith

  , unchainTD
  , zipchainTD
  , zipchainWithTD



  ) where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core



-- | A 'Chain' is a list of points. The list is often expected to 
-- be inifinte, but if it was a Stream  it would loose the ability
-- to use list comprehensions.
-- 
type Chain u = [Point2 u]


-- | A LocChain is a function from a starting point to a 'Chain'.
-- 
type LocChain u = Point2 u -> Chain u


-- | A ConnectorChain is a function from a start- and end-point 
-- to a 'Chain'.
-- 
type ConnectorChain u = Point2 u -> Point2 u -> Chain u




-- | Note - commonly a 'Chain' may be infinite, so it is only 
-- unrolled a finite number of times.
--
unchain :: Int -> LocGraphic u -> Chain u -> TraceDrawing u ()
unchain i op chn = go i chn
  where
    go n _      | n <= 0 = return ()
    go _ []              = return () 
    go n (x:xs)          = draw (op `at` x) >> go (n-1) xs






zipchain :: [LocGraphic u] -> Chain u -> TraceDrawing u ()
zipchain (g:gs) (p:ps)   = draw (g `at` p) >> zipchain gs ps
zipchain _      _        = return ()


zipchainWith :: (a -> LocGraphic u) -> [a] -> Chain u -> TraceDrawing u ()
zipchainWith op xs chn = go xs chn 
  where
    go (a:as) (p:ps)   = draw (op a `at` p) >> go as ps
    go _      _        = return ()



-- | Variant of 'unchain' where the drawing argument is a 
-- @TraceDrawing@ not a @LocGraphic@. 
--
unchainTD :: Int -> (Point2 u -> TraceDrawing u ()) -> Chain u -> TraceDrawing u ()
unchainTD i op chn = go i chn
  where
    go n _      | n <= 0 = return ()
    go _ []              = return () 
    go n (x:xs)          = (op x) >> go (n-1) xs


zipchainTD :: [Point2 u -> TraceDrawing u ()] -> Chain u -> TraceDrawing u ()
zipchainTD (g:gs) (p:ps)   = g p >> zipchainTD gs ps
zipchainTD _      _        = return ()



zipchainWithTD :: (a -> Point2 u -> TraceDrawing u ()) -> [a] -> Chain u -> TraceDrawing u ()
zipchainWithTD op xs chn = go xs chn 
  where
    go (a:as) (p:ps)   = op a p >> go as ps
    go _      _        = return ()



-- Notes - something like TikZ\'s chains could possibly be 
-- achieved with a Reader monad (@local@ initially seems better 
-- for \"state change\" than @set@ as local models a stack). 
--
-- It\'s almost tempting to put point-supply directly in the
-- Trace monad so that TikZ style chaining is transparent.
-- (The argument against is: how compatible this would be with
-- the Turtle monad for example?).
--


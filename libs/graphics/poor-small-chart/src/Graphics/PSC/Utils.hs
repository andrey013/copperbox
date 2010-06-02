{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Utilities
--
--------------------------------------------------------------------------------

module Graphics.PSC.Utils
  (

  -- * drawing
    makeStrokeProps
  , makeProjector

  , concatBackgrounds
  , straightLine

  -- * functions
  , unfoldrM
  , mbM

  -- * Hughes list
  , H
  , emptyH
  , consH
  , snocH
  , appendH
  , toListH
  , fromListH

  ) where


import Graphics.PSC.Core

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import Data.Maybe

--------------------------------------------------------------------------------
-- drawing

makeStrokeProps :: LineConfig -> (DRGB,[StrokeAttr])
makeStrokeProps (LineConfig rgb lw mb_dash) = 
    (rgb, catMaybes [ Just $ LineWidth lw, fmap mkDash mb_dash] )
  where
    mkDash (DashConfig offset xs) = DashPattern $ Dash offset xs


makeProjector :: Projection u -> (u -> Double)
makeProjector (Projection {proj_conv,proj_trans,proj_scale}) = 
    \u -> ((proj_conv u) - proj_trans) * proj_scale



concatBackgrounds :: (Num u, Ord u) 
                  => Picture u -> [Maybe (Picture u)] -> Picture u
concatBackgrounds top bkgrds = foldr fn top bkgrds
  where
    fn Nothing      p1 = p1
    fn (Just bkgrd) p1 = p1 `picOver` bkgrd

straightLine :: Num u => Point2 u -> Vec2 u -> Path u
straightLine pt v = path pt [lineTo $ pt .+^ v]


--------------------------------------------------------------------------------


unfoldrM :: Monad m => (st -> m (Maybe (a,st))) -> st -> m [a]
unfoldrM phi st = phi st >>= \ans -> case ans of 
    Nothing      -> return []
    Just (x,st') -> do {xs <- unfoldrM phi st'; return (x:xs) }

 
mbM :: Monad m => (a -> m b) -> Maybe a ->  m (Maybe b)
mbM _  Nothing  = return Nothing
mbM mf (Just a) = mf a >>= return . Just 


--------------------------------------------------------------------------------
-- Hughes list

type H a = [a] -> [a]

emptyH :: H a
emptyH = id

consH :: a -> H a -> H a
consH a f = (a:) . f

snocH :: H a -> a -> H a
snocH  f a = f . (a:)

appendH :: H a -> H a -> H a
appendH f g = f . g


toListH :: H a -> [a]
toListH = ($ [])

fromListH :: [a] -> H a
fromListH xs = (xs++)

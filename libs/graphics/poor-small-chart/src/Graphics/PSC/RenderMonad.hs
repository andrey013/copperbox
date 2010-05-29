{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.RenderMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC - NamedFieldPuns
--
-- Render monad
--
--------------------------------------------------------------------------------

module Graphics.PSC.RenderMonad
  (

  -- * RenderMonad
    RenderM  
  , runRender
  , Geom(..)
  , makeGeom

  , ask
  , asks
  , tell
  , tellList
  , mbTell

  , scaleCoord
  , scaleX
  , scaleY
  , origin
  , horizontalBounds
  , verticalBounds
  , containsPoint
  , containsCoord

  , hstroke
  , vstroke

  , generatePoints
  
  -- * General monad function
  , mbM

  ) where

import Graphics.PSC.Core
import Graphics.PSC.Utils

import Wumpus.Core

import Control.Applicative

type Trace = H DPrimitive

newtype RenderM xu yu a = RenderM { 
            getRenderM :: Geom xu yu -> Trace -> (a,Trace) }

data Geom xu yu = Geom 
      { rect_height         :: Double
      , rect_width          :: Double
      , rescale_x           :: xu -> Double
      , rescale_y           :: yu -> Double
      , origin_point        :: DPoint2
      }


makeGeom :: Double -> Double -> Range xu -> Range yu -> Geom xu yu
makeGeom width height range_x@(x0,_,_) range_y@(y0,_,_) = Geom 
    { rect_width    = width
    , rect_height   = height
    , rescale_x     = rescaleX
    , rescale_y     = rescaleY
    , origin_point  = P2 (rescaleX x0) (rescaleY y0)
    }
  where
    rescaleX        = makeRescaleX width  range_x
    rescaleY        = makeRescaleY height range_y

makeRescaleX :: Double -> Range u -> (u -> Double)
makeRescaleX width (x0,x1,fn) = rescale (fn x0) (fn x1) 0 width . fn

makeRescaleY :: Double -> Range u -> (u -> Double)
makeRescaleY height (y0,y1,fn) = rescale (fn y0) (fn y1) 0 height . fn


instance Functor (RenderM xu yu) where
  fmap f (RenderM mf) = RenderM $ \e w -> let (a,w') = mf e w in (f a,w')

instance Applicative (RenderM xu yu) where
  pure v    = RenderM $ \_ w -> (v,w)
  mf <*> mx = RenderM $ \e w -> let (f,w')  = (getRenderM mf) e w
                                    (x,w'') = (getRenderM mx) e w'
                                in (f x,w'')
           
                                 

instance Monad (RenderM xu yu) where
  return a  = RenderM $ \_ w -> (a,w)
  ma >>= mf = RenderM $ \e w -> let (a,w') = getRenderM ma e w
                                in getRenderM (mf a) e w'


runRender :: Geom xu yu -> RenderM xu yu a -> (a,DPicture)
runRender env (RenderM f) = 
    let (a,prims) = f env emptyH in (a, frameMulti $ toListH prims)


ask :: RenderM xu yu (Geom xu yu)
ask = RenderM $ \e w -> (e,w)

asks :: (Geom xu yu -> a) -> RenderM xu yu a
asks f = RenderM $ \e w -> (f e,w) 

tell :: DPrimitive -> RenderM xu yu ()
tell p = RenderM $ \_ w -> ((),p `consH` w)

tellList :: [DPrimitive] -> RenderM xu yu ()
tellList ps = RenderM $ \_ w -> ((), (fromListH ps) `appendH` w)


mbTell :: Maybe DPrimitive -> RenderM xu yu ()
mbTell mbp = RenderM $ \_ w -> ((), mbCons mbp w)
  where
    mbCons Nothing  = id
    mbCons (Just a) = consH a


scaleCoord :: (u,v) -> RenderM u v DPoint2
scaleCoord (u,v) = P2 <$> scaleX u <*> scaleY v



scaleX :: u -> RenderM u v Double
scaleX u = ($ u) <$> asks rescale_x

scaleY :: v -> RenderM u v Double
scaleY v = ($ v) <$> asks rescale_y

origin :: RenderM u v DPoint2
origin = asks origin_point

horizontalBounds :: RenderM u v (Double,Double)
horizontalBounds = fn <$> origin <*> asks rect_width
  where
    fn (P2 x0 _) w = (x0, x0+w)

verticalBounds :: RenderM u v (Double,Double)
verticalBounds = fn <$> origin <*> asks rect_height
  where
    fn (P2 _ y0) h = (y0, y0+h)

containsPoint :: DPoint2 -> RenderM u v Bool
containsPoint (P2 x y) = fn <$> horizontalBounds <*> verticalBounds
  where
    fn (x0,x1) (y0,y1) = contains x0 x1 x && contains y0 y1 y

containsCoord :: (u,v) -> RenderM u v Bool
containsCoord coord = scaleCoord coord >>= containsPoint


hstroke :: Stroke t => t -> v -> RenderM u v DPrimitive
hstroke props v = fn <$> scaleY v <*> horizontalBounds
  where
    fn y (x0,x1) = ostroke props $ path (P2 x0 y) [lineTo $ P2 x1 y]

vstroke :: Stroke t => t -> u -> RenderM u v DPrimitive
vstroke props h = fn <$> scaleX h <*> verticalBounds
  where
    fn x (y0,y1) = ostroke props $ path (P2 x y0) [lineTo $ P2 x y1]


generatePoints :: (u,v) -> (u -> u, v -> v) -> RenderM u v [DPoint2]
generatePoints (u,v) (f,g) = unfoldrM phi (u,v) where
  phi (x,y) = scaleCoord (x,y) >>= \pt  -> 
              containsPoint pt >>= \ans ->
              if ans then return $ Just (pt,(f x,g y)) else return Nothing


 
mbM :: Monad m => (a -> m b) -> Maybe a ->  m (Maybe b)
mbM _  Nothing  = return Nothing
mbM mf (Just a) = mf a >>= return . Just 




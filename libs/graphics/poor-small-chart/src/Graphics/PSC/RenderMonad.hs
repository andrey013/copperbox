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
  , ask
  , asks
  , tell
  , mbTell
  
  -- * General monad function
  , mbM

  ) where

import Wumpus.Core

import Control.Applicative

type Trace = [DPrimitive]

newtype RenderM e a = RenderM { getRenderM :: e -> Trace -> (a,Trace) } 

instance Functor (RenderM e) where
  fmap f (RenderM mf) = RenderM $ \e w -> let (a,w') = mf e w in (f a,w')

instance Applicative (RenderM e) where
  pure v    = RenderM $ \_ w -> (v,w)
  mf <*> mx = RenderM $ \e w -> let (f,w')  = (getRenderM mf) e w
                                    (x,w'') = (getRenderM mx) e w'
                                in (f x,w'')
           
                                 

instance Monad (RenderM e) where
  return a  = RenderM $ \_ w -> (a,w)
  ma >>= mf = RenderM $ \e w -> let (a,w') = getRenderM ma e w
                                in getRenderM (mf a) e w'


runRender :: e -> RenderM e a -> (a,DPicture)
runRender env (RenderM f) = let (a,prims) = f env [] in (a, frameMulti prims)


ask :: RenderM e e
ask = RenderM $ \e w -> (e,w)

asks :: (e -> a) -> RenderM e a
asks f = RenderM $ \e w -> (f e,w) 

tell :: DPrimitive -> RenderM e ()
tell p = RenderM $ \_ w -> ((),p:w)

mbTell :: Maybe DPrimitive -> RenderM e ()
mbTell mbp = RenderM $ \_ w -> ((), mbCons mbp w)
  where
    mbCons Nothing  = id
    mbCons (Just a) = (a:)




mbM :: Monad m => (a -> m b) -> Maybe a ->  m (Maybe b)
mbM _  Nothing  = return Nothing
mbM mf (Just a) = mf a >>= return . Just 




{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.MicroPrint.Render
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Render
--
--------------------------------------------------------------------------------

module Wumpus.MicroPrint.Render
  (
  
    Graphic
  , DGraphic
  , MP_config(..)
  , greekF

  , drawMicroPrint

  ) where

import Wumpus.Core hiding ( trace ) 

import Wumpus.MicroPrint.DrawMonad ( Tile(..), Height ) 
import Wumpus.MicroPrint.HughesList
import Wumpus.MicroPrint.TraceMonad
import Wumpus.MicroPrint.TurtleMonad

import Data.AffineSpace                 -- package: vector-space
import MonadLib                         -- package: monadLib

import Control.Applicative
import Control.Monad

-- | Note - this representation allows for zero, one or more
-- Primitives to be collected together.
--
type Graphic u = H (Primitive u)

type DGraphic  = Graphic Double

type GraphicF u = Point2 u -> Graphic u

type DGraphicF = GraphicF Double 



data MP_config = MP_config 
       { char_height    :: Double
       , char_width     :: Double
       , line_spacing   :: Double 
       , drawF          :: Double -> Double -> DRGB -> DGraphicF
       }

greekF :: Double -> Double -> DRGB -> DGraphicF
greekF w h rgb = filledRectangle rgb w h 



newtype RenderMonad a = RM { getRM :: ReaderT MP_config
                                    ( TraceT  DPrimitive Turtle)  a }

instance Functor RenderMonad where
  fmap f = RM . fmap f . getRM

instance Monad RenderMonad where
  return a = RM $ return a
  m >>= k  = RM $ getRM m >>= getRM . k

instance Applicative RenderMonad where
  pure  = return
  (<*>) = ap

instance TraceM RenderMonad DPrimitive where
  trace  h = RM $ lift $ trace h
  trace1 i = RM $ lift $ trace1 i

instance ReaderM RenderMonad MP_config where
  ask      = RM $ ask

instance TurtleM RenderMonad where
  getLoc   = RM $ lift $ lift $ getLoc
  setLoc c = RM $ lift $ lift $ setLoc c


drawMicroPrint :: MP_config -> ([Tile],Height) -> Maybe DPicture
drawMicroPrint cfg (xs,h) = 
    let (_,hf) = runRender cfg (moveUpN h >> interpret xs) in post $ hf []
  where
    post [] = Nothing
    post ps = Just $ frameMulti $ ps

runRender :: MP_config -> RenderMonad a -> (a, DGraphic)
runRender cfg m = 
    post $ runTurtle $ runTraceT $ runReaderT cfg $ getRM $ m
  where
    post ((a,w), _) = (a,w)

interpret :: [Tile] -> RenderMonad ()
interpret = mapM_ interp1

interp1 :: Tile -> RenderMonad ()
interp1 LineBreak     = nextLine
interp1 (Space    i)  = moveRightN i
interp1 (Word rgb i)  = do
    w  <- scaleWidth i 
    h  <- asks char_height
    pt <- scaleCurrentCoord
    dF <- asks drawF
    trace (dF w h rgb pt)
    moveRightN i
   
moveRightN :: Int -> RenderMonad ()
moveRightN i = setsLoc_ (\(Coord x y) -> Coord (x+i) y )

moveUpN :: Int -> RenderMonad ()
moveUpN i = setsLoc_ (\(Coord x y) -> Coord x (y+i) )

scaleCurrentCoord :: RenderMonad DPoint2
scaleCurrentCoord = 
    fn <$> getLoc <*> asks char_width <*> asks char_height <*> asks line_spacing
  where
    fn (Coord x y) cw ch sp = P2 (cw * fromIntegral x) ((ch+sp) * fromIntegral y)

scaleWidth :: Int -> RenderMonad Double
scaleWidth i = (\cw -> cw * fromIntegral i) <$> asks char_width


--------------------------------------------------------------------------------

filledRectangle :: (Num u, Ord u, Fill t) 
                => t -> u -> u -> GraphicF u
filledRectangle t w h bl = wrapH $ fill t $ rectangle w h bl

rectangle :: Num u => u -> u -> Point2 u -> Path u
rectangle w h bl = path bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 
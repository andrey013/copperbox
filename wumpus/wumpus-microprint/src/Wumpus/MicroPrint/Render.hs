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

    DrawF  
  , MP_config(..)
  , greekF
  , borderedF

  , drawMicroPrint

  ) where

import Wumpus.Core
import Wumpus.Core.Colour ( black )
import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.TraceMonad
import Wumpus.Basic.Monads.TurtleMonad
import Wumpus.Basic.Utils.HList

import Wumpus.MicroPrint.DrawMonad ( Tile(..), Height ) 

import MonadLib                         -- package: monadLib
import Data.AffineSpace                 -- package: vector-space

import Control.Applicative
import Control.Monad

-- | 'DrawF' :
-- @ (num_chars, char_unit_width) * (full_width, full_height) -> rgb -> DGraphicF @
--
type DrawF = (Int,Double) -> (Double,Double) -> DRGB -> DGraphicF


data MP_config = MP_config 
       { char_height    :: Double
       , char_width     :: Double
       , line_spacing   :: Double 
       , drawF          :: DrawF
       }

greekF :: DrawF
greekF _ (w,h) rgb = filledRectangle rgb w h 

borderedF :: Double -> DrawF
borderedF line_width (i,uw) (w,h) rgb = 
    srect `cc` seps `cc` greekF (i,uw) (w,h) rgb
  where
    props = (black, LineWidth line_width)

    srect :: DGraphicF
    srect = strokedRectangle props w h
 
    seps  :: DGraphicF
    seps  = \pt -> unfoldrH (phi pt) (1,uw) 
    
    phi pt (n,disp) | n >= i    = Nothing
                    | otherwise = let ln = vline props h (pt .+^ hvec disp)
                                  in  Just (ln,(n+1,disp+uw))
 
vline :: (Stroke t, Num u, Ord u) => t -> u -> Point2 u -> Primitive u
vline t h = \pt -> ostroke t $ path pt [lineTo $ pt .+^ vvec h]
    

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
    uw <- asks char_width
    pt <- scaleCurrentCoord
    dF <- asks drawF
    trace (dF (i,uw) (w,h) rgb pt)
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


-- Wow a new bird combinator...

infixr 9 `cc`

cc :: (r1 -> a -> ans) -> (r1 -> r2 -> a) -> r1 -> r2 -> ans
cc f g = \x y -> f x (g x y)

unfoldrH :: (b -> Maybe (a,b)) -> b -> H a
unfoldrH phi = step
  where step b = case phi b of
                  Nothing -> emptyH
                  Just (a,s) -> a `consH` step s
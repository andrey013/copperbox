{-# LANGUAGE TypeFamilies               #-}
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

    DrawWordF  
  , MicroPrintConfig(..)
  , greekF
  , borderedF

  , drawMicroPrint

  ) where

import Wumpus.Core
import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.TurtleMonad

import Wumpus.MicroPrint.DrawMonad ( Tile(..), Height ) 

import Data.AffineSpace                 -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.List


-- | 'DrawWordF' :
-- @ (num_chars, char_unit_width) * (full_width, full_height) -> rgb -> DGraphicF @
--
-- The libraries currently provides two styles - 'greekF' and
-- 'borderedF'.
--
type DrawWordF = (Int,Double) -> (Double,Double) -> RGBi -> DLocGraphic


-- | Style properties for micro-print drawing.
--
data MicroPrintConfig = MicroPrintConfig 
       { char_height    :: Double
       , char_width     :: Double
       , line_spacing   :: Double 
       , drawWordF      :: DrawWordF
       }

-- | Draw the word as a single coloured rectangle.
--
greekF :: DrawWordF
greekF _ (w,h) rgb = 
    localLG (secondaryColour rgb) (filledRectangle w h) 


-- | Draw the word as a coloured rectangle, with a border grid.
--
borderedF :: DrawWordF
borderedF (i,uw) (w,h) rgb = concatAt srect seps
  where
    srect :: DLocGraphic
    srect = localLG (secondaryColour rgb) (borderedRectangle w h)

    seps  :: [DLocGraphic]
    seps  = unfoldr phi (1,uw) 
    
    phi (n,hshift) | n >= i    = Nothing
                   | otherwise = let fn = \pt -> vline h (pt .+^ hvec hshift)
                                 in  Just (fn,(n+1,hshift+uw))



-- Note - this needs attention due to Z-Order handling in 
-- Wumpus-Basic. There are better ways to accomplish what 
-- borderedF does...
--
concatAt :: DLocGraphic -> [DLocGraphic] -> DLocGraphic 
concatAt x [] = x
concatAt x xs = foldr lgappend x xs

vline :: (Num u, Ord u) => u -> LocGraphic u
vline h = \pt -> openStroke $ path pt [lineTo $ pt .+^ vvec h]
    

newtype RenderMonad a = RM { 
          getRM :: MicroPrintConfig -> TurtleDrawing Double a }


type instance MonUnit RenderMonad = Double

instance Functor RenderMonad where
  fmap f ma = RM $ \cfg -> fmap f $ getRM ma cfg

instance Monad RenderMonad where
  return a = RM $ \_   -> return a
  m >>= k  = RM $ \cfg -> getRM m cfg >>= \a -> (getRM . k) a cfg

instance Applicative RenderMonad where
  pure  = return
  (<*>) = ap

instance TraceM RenderMonad where
  trace  h = RM $ \_ -> trace h

instance DrawingCtxM RenderMonad where
  askCtx          = RM $ \ _ -> askCtx
  localCtx ctx ma = RM $ \cfg -> localCtx ctx (getRM ma cfg)

ask :: RenderMonad MicroPrintConfig
ask = RM $ \cfg -> return cfg

asks :: (MicroPrintConfig -> a) -> RenderMonad a
asks f = f <$> ask

instance TurtleM RenderMonad where
  getLoc        = RM $ \_ -> getLoc
  setLoc c      = RM $ \_ -> setLoc c
  getOrigin     = RM $ \_ -> getOrigin
  setOrigin o   = RM $ \_ -> setOrigin o


drawMicroPrint :: MicroPrintConfig -> ([Tile],Height) -> Maybe DPicture
drawMicroPrint cfg (xs,h) = 
    let (_,hf) = runRender cfg (moveUpN h >> interpret xs) in liftToPictureMb hf

runRender :: MicroPrintConfig -> RenderMonad a -> (a, HPrim Double)
runRender cfg m = 
    runTurtleDrawing (regularConfig 1) (0,0) (standardContext 14) 
         $ (getRM m) cfg

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
    dF <- asks drawWordF
    draw $ (dF (i,uw) (w,h) rgb) `at` pt
    moveRightN i
   
moveRightN   :: Int -> RenderMonad ()
moveRightN i = setsLoc_ $ \(x,y) -> (x+i,y)

moveUpN      :: Int -> RenderMonad ()
moveUpN i    = setsLoc_ $ \(x,y) -> (x,y+i)

scaleCurrentCoord :: RenderMonad DPoint2
scaleCurrentCoord = 
    fn <$> getLoc <*> asks char_width <*> asks char_height <*> asks line_spacing
  where
    fn (x,y) cw ch sp = P2 (cw * fromIntegral x) ((ch+sp) * fromIntegral y)

scaleWidth :: Int -> RenderMonad Double
scaleWidth i = (\cw -> cw * fromIntegral i) <$> asks char_width


--------------------------------------------------------------------------------



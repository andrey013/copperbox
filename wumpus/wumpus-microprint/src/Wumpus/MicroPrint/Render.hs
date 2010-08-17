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
import Wumpus.Core.Colour ( black )
import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.TurtleMonad
import Wumpus.Basic.Utils.HList

import Wumpus.MicroPrint.DrawMonad ( Tile(..), Height ) 

import MonadLib                         -- package: monadLib
import Data.AffineSpace                 -- package: vector-space

import Control.Applicative
import Control.Monad

-- | 'DrawWordF' :
-- @ (num_chars, char_unit_width) * (full_width, full_height) -> rgb -> DGraphicF @
--
-- The libraries currently provides two styles - 'greekF' and
-- 'borderedF'.
--
type DrawWordF = (Int,Double) -> (Double,Double) -> DRGB -> DGraphicF


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
greekF _ (w,h) rgb = wrapG . fill rgb . rectanglePath w h 


-- | Draw the word as a coloured rectangle, with a border grid.
--
borderedF :: Double -> DrawWordF
borderedF ln_width (i,uw) (w,h) rgb = 
    srect `cc` seps `cc` greekF (i,uw) (w,h) rgb
  where
    props = (black, LineWidth ln_width)

    srect :: DGraphicF
    srect = wrapG . cstroke props . rectanglePath w h
 
    seps  :: DGraphicF
    seps  = \pt -> unfoldrH (phi pt) (1,uw) 
    
    phi pt (n,hshift) | n >= i    = Nothing
                      | otherwise = let ln = vline props h (pt .+^ hvec hshift)
                                    in  Just (ln,(n+1,hshift+uw))
 
vline :: (Stroke t, Num u, Ord u) => t -> u -> Point2 u -> Primitive u
vline t h = \pt -> ostroke t $ path pt [lineTo $ pt .+^ vvec h]
    

newtype RenderMonad a = RM { 
          getRM :: ReaderT MicroPrintConfig 
                 ( TurtleDrawing Double ) a }

instance Functor RenderMonad where
  fmap f = RM . fmap f . getRM

instance Monad RenderMonad where
  return a = RM $ return a
  m >>= k  = RM $ getRM m >>= getRM . k

instance Applicative RenderMonad where
  pure  = return
  (<*>) = ap

instance TraceM RenderMonad Double where
  trace  h = RM $ lift $ trace h

instance ReaderM RenderMonad MicroPrintConfig where
  ask      = RM $ ask

instance TurtleM RenderMonad where
  getLoc        = RM $ lift $ getLoc
  setLoc c      = RM $ lift $ setLoc c
  getOrigin     = RM $ lift $ getOrigin
  setOrigin o   = RM $ lift $ setOrigin o


drawMicroPrint :: MicroPrintConfig -> ([Tile],Height) -> Maybe DPicture
drawMicroPrint cfg (xs,h) = 
    let (_,hf) = runRender cfg (moveUpN h >> interpret xs) in post $ hf []
  where
    post [] = Nothing
    post ps = Just $ frameMulti $ ps

runRender :: MicroPrintConfig -> RenderMonad a -> (a, DGraphic)
runRender cfg m = 
    runTurtleDrawing (regularConfig 1) (0,0) (standardAttr 14) 
         $ runReaderT cfg $ getRM $ m

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
    trace (dF (i,uw) (w,h) rgb pt)
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



{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Clave.ClaveMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Core types, functions...
--
--------------------------------------------------------------------------------

module Wumpus.Clave.ClaveMonad
  (

    ClaveM
  , ClaveConfig(..)

  , evalClaveM
  , runClaveM

  , beat
  , rest
  , endLine
  , highlight

  ) where

import Wumpus.Clave.Core
import Wumpus.Clave.Drawing

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( black )
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Monads.TurtleMonad

import MonadLib                                 -- package: monadLib


data ClaveConfig = ClaveConfig 
      { box_height      :: BoxHeight
      , scalefun        :: Int -> Double 
      }

newtype ClaveM a = ClaveM { getClaveM ::  ReaderT ClaveConfig 
                                        ( TurtleDrawing Double) a }


instance Functor ClaveM where
  fmap f = ClaveM . fmap f . getClaveM

instance Monad ClaveM where
  return a = ClaveM $ return a
  m >>= k  = ClaveM $ getClaveM m >>= getClaveM . k

instance TraceM ClaveM Double where
  trace  h = ClaveM $ lift $ trace h

instance ReaderM ClaveM ClaveConfig where
  ask      = ClaveM $ ask

instance TurtleM ClaveM where
  getLoc      = ClaveM $ lift $ getLoc
  setLoc c    = ClaveM $ lift $ setLoc c
  getOrigin   = ClaveM $ lift $ getOrigin
  setOrigin o = ClaveM $ lift $ setOrigin o


instance TurtleScaleM ClaveM Double where
  xStep    = ClaveM $ lift $ xStep
  yStep    = ClaveM $ lift $ yStep


evalClaveM :: ClaveConfig -> ClaveM a -> DGraphic
evalClaveM cfg mf = snd $ runClaveM cfg mf 

runClaveM :: ClaveConfig -> ClaveM a -> (a,DGraphic)
runClaveM cfg mf = runTurtleDrawing (regularConfig 20) (0,0) (standardAttr 12)
                 ( runReaderT cfg $ getClaveM mf )


nextPoint :: ClaveM DPoint2
nextPoint = moveRight >> getPos

-- xdistance :: ClaveM Int
-- xdistance = getLoc >>= \(Coord x _) -> return x



beat :: ClaveM ()
beat = nextPoint                >>= \pt -> 
       asks box_height          >>= \h  ->
       trace (circleF h black pt)

rest :: ClaveM ()
rest = nextPoint >> return ()

endLine :: ClaveM ()
endLine = getPos                    >>= \pt    ->
          getLoc                    >>= \(n,_) ->
          asks box_height           >>= \h     ->
          trace (gridF n h 0.5 pt)  >>
          nextLine

-- Doesn't work well with monoid/writer...
 
highlight :: Int -> DRGB -> ClaveM ()
highlight n rgb = getPos                    >>= \pt ->
                  asks box_height           >>= \h  ->
                  trace (backgroundF n h rgb pt)
          
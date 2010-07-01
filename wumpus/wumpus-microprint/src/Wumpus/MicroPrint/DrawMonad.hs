{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.MicroPrint.DrawMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- MicroPrints drawing monad 
--
--------------------------------------------------------------------------------

module Wumpus.MicroPrint.DrawMonad
  (

    TraceM
  , runTrace
  , execTrace

  , Tile(..)
  , Height
  , linebreak
  , setRGB
  , char
  , space

  ) where

import Wumpus.Core
import Wumpus.Core.Colour ( black )

import Wumpus.MicroPrint.HughesList

import Control.Monad

data Tile = LineBreak | Space Int | Word DRGB Int

-- Interim version without colour annotation...
data TileState = Start | S0 Int | W0 Int



type Text       = H Tile
type Trace      = Text
type Height     = Int
type State      = (TileState, DRGB, Height)

newtype TraceM a = TraceM { getTraceM :: Trace -> State -> (a,Trace,State) }

instance Functor TraceM where
  fmap f m = TraceM $ \w s -> let (a,w',s') = getTraceM m w s in (f a,w',s')

instance Monad TraceM where
  return a = TraceM $ \w s -> (a,w,s)
  m >>= k  = TraceM $ \w s -> let (a,w',s') = getTraceM m w s in
                              (getTraceM . k) a w' s'


runTrace :: TraceM a -> (a,[Tile],Height)
runTrace m = post $ getTraceM m emptyH (Start,black,1)
  where
    post (a,f,(W0 n, rgb, h)) = (a, toListH $ f `snocH` (Word rgb n), h)
    post (a,f,(_, _, h))      = (a, f [], h)

execTrace :: TraceM a -> ([Tile],Height)
execTrace = post . runTrace
  where post (_,xs,h) = (xs,h)

enqueueTile :: TraceM ()
enqueueTile = TraceM $ \w (opt,rgb,h) -> 
    let tileF = step rgb opt in ((), tileF w, (Start, rgb,h))
  where
    step _   Start  = id
    step _   (S0 n) = (\f -> f `snocH` (Space n))
    step rgb (W0 n) = (\f -> f `snocH` (Word rgb n))


linebreak :: TraceM ()
linebreak = enqueueTile >> next
  where
    
    next = TraceM $ \w (opt,rgb,h) -> ((),w `snocH` LineBreak, (opt,rgb,h+1))

-- Note - it is permissible to change colour mid-word.
-- But this is the same as having a no-space break.
--
setRGB :: DRGB -> TraceM ()
setRGB rgb = enqueueTile >> next
  where
    -- tip will always be Start here...
    next = TraceM $ \w (tip,_,h) -> ((),w,(tip,rgb,h))

char :: TraceM ()
char = TraceM $ \w (tip,rgb,h) ->
    let (f,tip') = addChar tip in ((),f w,(tip',rgb,h))
  where
    addChar Start  = (id, W0 1)
    addChar (W0 n) = (id, W0 $ n+1)
    addChar (S0 n) = (\f -> f `snocH` (Space n), W0 1)


space :: TraceM ()
space = TraceM $ \w (tip,rgb,h) ->
    let (f,tip') = addSpace tip rgb in ((),f w,(tip',rgb,h))
  where
    addSpace Start  _   = (id, S0 1)
    addSpace (W0 n) rgb = (\f -> f `snocH` (Word rgb n), S0 1)
    addSpace (S0 n) _   = (id, S0 $ n+1)

 


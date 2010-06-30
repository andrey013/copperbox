{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.MicroPrint.TraceM
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- MicroPrints
--
--------------------------------------------------------------------------------

module Wumpus.MicroPrint.TraceM
  (

    TraceM
  , runTrace
  , execTrace

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
type State      = (TileState, DRGB)

newtype TraceM a = TraceM { getTraceM :: Trace -> State -> (a,Trace,State) }

instance Functor TraceM where
  fmap f m = TraceM $ \w s -> let (a,w',s') = getTraceM m w s in (f a,w',s')

instance Monad TraceM where
  return a = TraceM $ \w s -> (a,w,s)
  m >>= k  = TraceM $ \w s -> let (a,w',s') = getTraceM m w s in
                              (getTraceM . k) a w' s'


runTrace :: TraceM a -> (a,[Tile])
runTrace m = post $ getTraceM m emptyH (Start,black)
  where
    post (a,f,(W0 n,rgb)) = (a, toListH $ f `snocH` (Word rgb n))
    post (a,f,_)          = (a, f [])

execTrace :: TraceM a -> [Tile]
execTrace = snd . runTrace

enqueueTile :: TraceM ()
enqueueTile = TraceM $ \w (opt,rgb) -> 
    let tileF = step rgb opt in ((), tileF w, (Start, rgb))
  where
    step _   Start  = id
    step _   (S0 n) = (\f -> f `snocH` (Space n))
    step rgb (W0 n) = (\f -> f `snocH` (Word rgb n))


linebreak :: TraceM ()
linebreak = enqueueTile >> next
  where
    next = TraceM $ \w s -> ((),w `snocH` LineBreak, s)

-- Note - it is permissible to change colour mid-word.
-- But this is the same as having a no-space break.
--
setRGB :: DRGB -> TraceM ()
setRGB rgb = enqueueTile >> next
  where
    -- tip will always be Start here...
    next = TraceM $ \w (tip,_) -> ((),w,(tip,rgb))

char :: TraceM ()
char = TraceM $ \w (tip,rgb) ->
    let (f,tip') = addChar tip in ((),f w,(tip',rgb))
  where
    addChar Start  = (id, W0 1)
    addChar (W0 n) = (id, W0 $ n+1)
    addChar (S0 n) = (\f -> f `snocH` (Space n), W0 1)


space :: TraceM ()
space = TraceM $ \w (tip,rgb) ->
    let (f,tip') = addSpace tip in ((),f w,(tip',rgb))
  where
    addSpace Start  = (id, S0 1)
    addSpace (W0 n) = (\f -> f `snocH` (Space n), S0 1)
    addSpace (S0 n) = (id, S0 $ n+1)

 


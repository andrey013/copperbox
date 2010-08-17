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
-- MicroPrints drawing monad - drawing here is analogous to a 
-- /teletype/ drawing characters, spaces and linebreaks one at a 
-- time.
--
--------------------------------------------------------------------------------

module Wumpus.MicroPrint.DrawMonad
  (

    MicroPrint
  , runMicroPrint
  , execMicroPrint

  , Tile(..)
  , Height
  , linebreak
  , setRGB
  , char
  , space

  ) where

import Wumpus.Core
import Wumpus.Core.Colour ( black )

import Wumpus.Basic.Utils.HList

import Control.Monad

data Tile = LineBreak | Space Int | Word DRGB Int

-- Interim version without colour annotation...
data TileState = Start | S0 Int | W0 Int



type Text       = H Tile
type Trace      = Text
type Height     = Int
type State      = (TileState, DRGB, Height)

-- | Build a /microprint/ within a monad...
--
-- Drawings are made in a /teletype/ fashion emitting a character,
-- space or line-break at each step.
--
newtype MicroPrint a = MicroPrint { 
          getMicroPrint :: Trace -> State -> (a,Trace,State) }

instance Functor MicroPrint where
  fmap f m = MicroPrint $ \w s -> 
                let (a,w',s') = getMicroPrint m w s in (f a,w',s')

instance Monad MicroPrint where
  return a = MicroPrint $ \w s -> (a,w,s)
  m >>= k  = MicroPrint $ \w s -> let (a,w',s') = getMicroPrint m w s 
                                  in (getMicroPrint . k) a w' s'


runMicroPrint :: MicroPrint a -> (a,[Tile],Height)
runMicroPrint m = post $ getMicroPrint m emptyH (Start,black,1)
  where
    post (a,f,(W0 n, rgb, h)) = (a, toListH $ f `snocH` (Word rgb n), h)
    post (a,f,(_, _, h))      = (a, f [], h)

execMicroPrint :: MicroPrint a -> ([Tile],Height)
execMicroPrint = post . runMicroPrint
  where post (_,xs,h) = (xs,h)

enqueueTile :: MicroPrint ()
enqueueTile = MicroPrint $ \w (opt,rgb,h) -> 
    let tileF = step rgb opt in ((), tileF w, (Start, rgb,h))
  where
    step _   Start  = id
    step _   (S0 n) = (\f -> f `snocH` (Space n))
    step rgb (W0 n) = (\f -> f `snocH` (Word rgb n))


-- | Emit a linebreak in the output.
--
linebreak :: MicroPrint ()
linebreak = enqueueTile >> next
  where
    next = MicroPrint $ 
             \w (opt,rgb,h) -> ((),w `snocH` LineBreak, (opt,rgb,h+1))


-- | Change the current drawing colour.
--
-- Note - it is permissible to change colour mid-word, but this 
-- is the same as having a no-space break.
--
setRGB :: DRGB -> MicroPrint ()
setRGB rgb = enqueueTile >> next
  where
    -- tip will always be Start here...
    next = MicroPrint $ \w (tip,_,h) -> ((),w,(tip,rgb,h))


-- | Draw a character - note in the microprint, characters will 
-- be concatenated together to make a word.
--
char :: MicroPrint ()
char = MicroPrint $ \w (tip,rgb,h) ->
    let (f,tip') = addChar tip in ((),f w,(tip',rgb,h))
  where
    addChar Start  = (id, W0 1)
    addChar (W0 n) = (id, W0 $ n+1)
    addChar (S0 n) = (\f -> f `snocH` (Space n), W0 1)

-- | Draw a space.
--
space :: MicroPrint ()
space = MicroPrint $ \w (tip,rgb,h) ->
    let (f,tip') = addSpace tip rgb in ((),f w,(tip',rgb,h))
  where
    addSpace Start  _   = (id, S0 1)
    addSpace (W0 n) rgb = (\f -> f `snocH` (Word rgb n), S0 1)
    addSpace (S0 n) _   = (id, S0 $ n+1)

 


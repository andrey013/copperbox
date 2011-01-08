{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Microprint.Teletype
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- A drawing monad where drawing is analogous to a /teletype/ 
-- printing characters, spaces and linebreaks one at a time.
--
--------------------------------------------------------------------------------

module Wumpus.Microprint.Teletype
  (

    Teletype
--  , runTeletype
--  , execTeletype

  , renderTeletype

  , Tile(..)
  , Height
  , linebreak
  , setRGB
  , char
  , space

  ) where

import Wumpus.Microprint.Datatypes
import Wumpus.Microprint.Render

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( black )

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Control.Applicative
import Control.Monad


-- Interim version without colour annotation...
--
-- Note could start with Space S0 - as spaces aren\'t printed (S0 0) is not a problem 
-- 
data TileTip = Sp Int | Wo Int
  deriving (Eq,Show)


type Trace      = (H [Tile], H Tile)
type State      = (RGBi, Height, TileTip)

-- | Build a /microprint/ within a monad...
--
-- Drawings are made in a /teletype/ fashion emitting a character,
-- space or lineMicroprint-break at each step.
--
newtype Teletype a = Teletype { 
          getTeletype :: Trace -> State -> (a,Trace,State) }

instance Functor Teletype where
  fmap f m = Teletype $ \w s -> 
                let (a,w',s') = getTeletype m w s in (f a,w',s')

instance Applicative Teletype where
  pure a    = Teletype $ \w s -> (a,w,s)
  mf <*> ma = Teletype $ \w s -> let (f,w1,s1) = getTeletype mf w  s
     	      	       	      	     (a,w2,s2) = getTeletype ma w1 s1
				  in (f a,w2,s2)

instance Monad Teletype where
  return a = Teletype $ \w s -> (a,w,s)
  m >>= k  = Teletype $ \w s -> let (a,w1,s1) = getTeletype m w s 
                                 in (getTeletype . k) a w1 s1


runTeletype :: Teletype a -> (a,GreekText)
runTeletype m = post $ getTeletype m (emptyH,emptyH) (black,1,Sp 0)
  where
    post (a, (u,v), (rgb,h,tip)) = let v1 = snocTip v rgb tip
                                   in (a,(h,finalizeTrace (u,v1)))


finalizeTrace :: Trace -> [[Tile]]
finalizeTrace (a,b) = toListH $ a `snocH` (toListH b)	

execTeletype :: Teletype a -> GreekText
execTeletype = snd . runTeletype



-- | Build a picture from a Teletype drawing.
--
-- This function returns Nothing if the picture is empty.
-- 
renderTeletype :: RenderScalingCtx -> DrawWordF -> Teletype a 
               -> TraceDrawing Double ()
renderTeletype sctx fn mf = render sctx fn $ execTeletype mf


snocTip :: H Tile -> RGBi -> TileTip -> H Tile
snocTip a _   (Sp n)  | n > 0 = a `snocH` (Space n)
snocTip a rgb (Wo n)  | n > 0 = a `snocH` (Word rgb n)
snocTip a _   _               = a 



-- | Emit a linebreak in the output.
--
linebreak :: Teletype ()
linebreak = Teletype $ \(a,b) (rgb, h, tip) -> 
    let b1 = snocTip b rgb tip 
        ac = (a `snocH` toListH b1, emptyH)
    in ((), ac, (rgb, h+1, Sp 0))


-- | Change the current drawing colour.
--
-- Note - it is permissible to change colour mid-word, but this 
-- is the same as having a no-space break and forms a new word.
--
setRGB :: RGBi -> Teletype ()
setRGB rgb = Teletype $ \(a,b) (old,h,tip) -> 
    ((), (a, snocTip b old tip), (rgb,h,Sp 0))


-- | Draw a character - note in the microprint, characters will 
-- be concatenated together to make a word.
--
char :: Teletype ()
char = Teletype $ \(a,b) (rgb,h,tip) ->
    case tip of 
      Sp _ -> ((), (a, snocTip b rgb tip), (rgb,h,Wo 1))
      Wo n -> ((), (a,b), (rgb,h,Wo $ n+1))
  

-- | Draw a space.
--
space :: Teletype ()
space = Teletype $ \(a,b) (rgb,h,tip) ->
    case tip of
      Sp n -> ((), (a,b), (rgb,h,Sp $ n+1))
      Wo _ -> ((), (a, snocTip b rgb tip), (rgb,h,Sp 1))

 


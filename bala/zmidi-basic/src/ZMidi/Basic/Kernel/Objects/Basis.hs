{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.Basis
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Wrapped Primitives supporting concatenation.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.Basis
  ( 

    Decorate(..)

  , ignoreAns
  , replaceAns

  ) where



-- | Ignore an answer changing it to @()@.
--
ignoreAns :: Functor f => f a -> f ()
ignoreAns = fmap (const ())

-- | Replace the answer produced by a graphic object.
--
replaceAns :: Functor f => a -> f z -> f a
replaceAns a = fmap (const a)



-- 
-- Note - @decorating@ as per Wumpus actually is musically useful 
-- if we view music as a stream of control events dispatched to a
-- synthesizer. Sometimes we need to bundle a number of events 
-- together to properly control the synth.
-- 
-- If sequential concat is the better interpretation of mappend 
-- for CatPrim, then decorate might not be useful at all...
--



-- | Decorate an object
--
-- @oliterate@ - drops the residual event from the first object 
-- replacing it with the one from the second but returning the
-- original /answer/.
--
-- @decorate@ and @elaborate@ considered instantaneous - rendering
-- to MIDI may change the ordering but not the delta time.
--
class Decorate (f :: * -> * -> *) where
  decorate   :: f u a -> f u z -> f u a
  elaborate  :: f u a -> (a -> f u z) -> f u a
  obliterate :: f u a -> f u z -> f u a


-- Note - with elaborate of the answer TimeSpan you can mix in
-- crescendos, decrescendos...



-- If we just have one event type, Decorate doesn\'t need to be a 
-- type class...

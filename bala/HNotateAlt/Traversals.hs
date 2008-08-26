
--------------------------------------------------------------------------------
-- |
-- Module      :  Traversals
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Traverse a DScore tracking duration only if it changes (c.f. LilyPond).
--
--------------------------------------------------------------------------------

module Traversals where

import Bifunctor
import Duration
import Pitch
import ScoreRepresentation

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Sequence
import Data.Traversable

type St = Duration

instance Applicative Identity where
  pure = return
  (<*>) = ap
  
  

-- Three useful run functions
traversalIdentity :: (Traversable t) => 
                     (a -> Identity b) -> t a -> t b
traversalIdentity f a = runIdentity $ traverse f a 

traversalReader :: (Traversable t) =>
                  (a -> WrappedMonad (Reader env) b) -> t a -> env -> t b
traversalReader f a env = (runReader $ unwrapMonad $ traverse f a) env 

traversalState :: (Traversable t) =>
                  (a -> WrappedMonad (State st) b) -> t a -> st -> t b
traversalState f a st = evalState (unwrapMonad $ traverse f a) st 

-- Reader without the monad...
traversalEnv :: (Traversable t) =>
                (a -> (->) env b) -> t a -> env -> t b
traversalEnv f a env = (traverse f a) env 


changeDuration :: Glyph p d -> d' -> Glyph p d'
changeDuration a od = bimap id (const od) a

changePitch :: Glyph p d -> p' -> Glyph p' d
changePitch a op = bimap (const op) id a


--------------------------------------------------------------------------------
-- run length encode the duration - LilyPond uses this method


runLengthEncodeDuration :: ScStrata (Glyph pch Duration)
                        -> Duration
                        -> ScStrata (Glyph pch (Maybe Duration))
runLengthEncodeDuration strata initial_duration = 
    traversalState run_length_encode_GD_body strata initial_duration

run_length_encode_GD_body :: Glyph pch Duration 
    -> WrappedMonad (State Duration) (Glyph pch (Maybe Duration))

run_length_encode_GD_body g@(GlyGraceNotes _) = WrapMonad $ 
    return $ changeDuration g Nothing
    
run_length_encode_GD_body e = WrapMonad $ do
    od <- diffDuration (glyphDuration e)
    return $ changeDuration e od    
  where    
    diffDuration :: Duration -> State St (Maybe Duration)
    diffDuration d = do
        old <- get 
        if (old == d) then return Nothing
                      else do {put d; return $ Just d}



--------------------------------------------------------------------------------
-- 'default encode' the duration - if the duration matches the default 
-- don't specify it - Abc uses this method

-- Too simple to need a reader monad.

defaultEncodeDuration :: ScStrata (Glyph pch Duration)
                      -> Duration
                      -> ScStrata (Glyph pch (Maybe Duration))
defaultEncodeDuration strata default_duration = 
    traversalEnv default_encode_GD_body strata default_duration

default_encode_GD_body :: Glyph p Duration 
                       -> Duration 
                       -> Glyph p (Maybe Duration)
default_encode_GD_body e = \env -> 
    let d = glyphDuration e in 
    if d == env then changeDuration e Nothing else changeDuration e (Just d)
    



       
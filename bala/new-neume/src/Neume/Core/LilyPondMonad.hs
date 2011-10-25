{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.LilyPondMonad
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- LilyPond monad...
-- 
-- State writer monad
--
--------------------------------------------------------------------------------


module Neume.Core.LilyPondMonad
  ( 

    LilyPondM
  , runLilyPondM

  , relNote

  ) where 


import Neume.Core.Duration
import Neume.Core.LilyPondPretty
import Neume.Core.Pitch

import Text.PrettyPrint.HughesPJ

import Control.Applicative hiding ( empty )

data LyState = LyState 
   { prev_duration :: Duration
   , prev_pitch    :: Pitch
   }

-- If we are concatenating Docs in the monad should we follow 
-- Conal Elliott\'s context-monoid pattern so we can choose
-- between @<>@ and @<+>@ ?
-- 
-- Or do barlines, for example, know how to space themselves.
--

newtype LilyPondM a = LilyPondM { 
    getLilyPondM :: LyState -> (a,LyState,Doc) }

instance Functor LilyPondM where
  fmap f ma = LilyPondM $ \s -> 
                let (a,s1,d1) = getLilyPondM ma s in (f a, s1, d1) 

instance Applicative LilyPondM where
  pure a    = LilyPondM $ \s -> (a, s, empty)
  mf <*> ma = LilyPondM $ \s -> let (f,s1,d1) = getLilyPondM mf s
                                    (a,s2,d2) = getLilyPondM ma s1
                                in (f a,s2,d1 <> d2) 

instance Monad LilyPondM where
  return  = pure
  m >>= k = LilyPondM $ \s -> let (a,s1,d1) = getLilyPondM m s
                                  (b,s2,d2) = getLilyPondM (k a) s1 
                              in (b,s2,d1 <> d2)


-- | LilyPond\'s default duration is a quarter note.
--
-- Seeding the initial pitch is a pain point. Absolute scores
-- should not need to worry about seeding pitch, relative scores
-- should do it explicitly with @relative@. 
-- 
runLilyPondM :: LilyPondM a -> (a,Doc)
runLilyPondM ma = post $ getLilyPondM ma s0
  where
    post (a,_,d1) = (a,d1)

    s0 = LyState { prev_duration = dQuarter
                 , prev_pitch    = middle_c
                 }



relNote :: Pitch -> Duration -> LilyPondM ()
relNote p d = LilyPondM $ \(LyState d0 p0) -> 
                let doc1 = relNext p0 p d0 d in ((), LyState d p, doc1) 

relNext :: Pitch -> Pitch -> Duration -> Duration -> Doc
relNext p0 p d0 d 
    | d0 == d = pdoc
    | otherwise = pdoc <> duration d
  where
    pdoc = let i = lyOctaveDist p0 p in pitch $ setOctave i p



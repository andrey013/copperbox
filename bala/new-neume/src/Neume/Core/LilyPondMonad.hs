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

  , note

  ) where 


import Neume.Core.Duration
import Neume.Core.LilyPondPretty hiding ( note )
import Neume.Core.Pitch
import Neume.Core.Utils.JoinList ( JoinList, one, toList )

import Text.PrettyPrint.HughesPJ

import Control.Applicative hiding ( empty )
import Data.Monoid

data LyState = LyState 
    { prev_duration :: Duration
    , prev_pitch    :: Pitch
    }

type NextNote = Pitch -> Duration -> LyState -> (Doc, LyState)


data LyEnv = LyEnv 
    { note_func     :: NextNote
    }

-- If we are concatenating Docs in the monad should we follow 
-- Conal Elliott\'s context-monoid pattern so we can choose
-- between @<>@ and @<+>@ ?
-- 
-- Or do barlines, for example, know how to space themselves.
--

newtype LilyPondM a = LilyPondM { 
    getLilyPondM :: LyEnv -> LyState -> (a, LyState, JoinList Doc) }

instance Functor LilyPondM where
  fmap f ma = LilyPondM $ \r s -> 
                let (a,s1,d1) = getLilyPondM ma r s in (f a, s1, d1) 

instance Applicative LilyPondM where
  pure a    = LilyPondM $ \r s -> (a, s, mempty)
  mf <*> ma = LilyPondM $ \r s -> let (f,s1,d1) = getLilyPondM mf r s
                                      (a,s2,d2) = getLilyPondM ma r s1
                                  in (f a,s2,d1 `mappend` d2) 

instance Monad LilyPondM where
  return  = pure
  m >>= k = LilyPondM $ \r s -> let (a,s1,d1) = getLilyPondM m r s
                                    (b,s2,d2) = getLilyPondM (k a) r s1 
                                in (b,s2,d1 `mappend` d2)


-- | LilyPond\'s default duration is a quarter note.
--
-- Seeding the initial pitch is a pain point. Absolute scores
-- should not need to worry about seeding pitch, relative scores
-- should do it explicitly with @relative@. 
-- 
runLilyPondM :: LilyPondM a -> (a,Doc)
runLilyPondM ma = post $ getLilyPondM ma r0 s0
  where
    post (a,_,jl) = (a,hsep $ toList jl)

    s0 = LyState { prev_duration = dQuarter
                 , prev_pitch    = middle_c
                 }

    r0 = LyEnv { note_func = relNote }

note :: Pitch -> Duration -> LilyPondM ()
note p d = LilyPondM $ \(LyEnv fn) s -> 
    let (doc1,s1) = fn p d s in ((), s1, one doc1)


relNote :: NextNote
relNote p d (LyState d0 p0) = (relNext p0 p d0 d, LyState d p) 


relNext :: Pitch -> Pitch -> Duration -> Duration -> Doc
relNext p0 p d0 d 
    | d0 == d   = pdoc
    | otherwise = pdoc <> duration d
  where
    pdoc = let i = lyOctaveDist p0 p in pitch $ setOctave i p

absNote :: (Int -> Int) -> NextNote
absNote fn p d (LyState d0 p0) = (absNext p0 fn d0 d, LyState d p0)

absNext :: Pitch -> (Int -> Int) -> Duration -> Duration -> Doc
absNext (Pitch l oa o) fn d0 d 
    | d0 == d   = pdoc
    | otherwise = pdoc <> duration d
  where
    pdoc = pitch $ Pitch l oa (fn o)



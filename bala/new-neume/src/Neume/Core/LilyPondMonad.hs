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


    LyScoreM
  , runLyScore
  , execLyScore

  , version
  , score
  , relative

  , LyNoteListM
  , runLyNoteListM

  , relPitch
  , relDuration
  , absPitch

  , note
  , chord
  , rest
  , spacer
  , beam

  ) where 


import Neume.Core.Duration
import qualified Neume.Core.LilyPondPretty as PP
import Neume.Core.Pitch
import Neume.Core.Utils.Basis
import Neume.Core.Utils.Pretty

import Text.PrettyPrint.HughesPJ

import Control.Applicative hiding ( empty )
import Data.Monoid


-- LilyPond has two monads - NoteList and Score.

--------------------------------------------------------------------------------
-- Score monad

-- | Score monad is a writer (Doc) and a reader 
-- (Doc concatenation function).
newtype LyScoreM a = LyScoreM { getLyScoreM :: DocCat -> (a,Doc) }

type DocCat = Doc -> Doc -> Doc



instance Functor LyScoreM where
  fmap f ma = LyScoreM $ \op -> let (a,doc1) = getLyScoreM ma op in (f a, doc1)

instance Applicative LyScoreM where
  pure a    = LyScoreM $ \_  -> (a,empty)
  mf <*> ma = LyScoreM $ \op -> let (f,d1) = getLyScoreM mf op
                                    (a,d2) = getLyScoreM ma op
                                in (f a, d1 `op` d2)

instance Monad LyScoreM where
  return  = pure
  m >>= k = LyScoreM $ \op -> let (a,d1) = getLyScoreM m op
                                  (b,d2) = getLyScoreM (k a) op
                              in (b, d1 `op` d2)


instance Monoid a => Monoid (LyScoreM a) where
  mempty          = pure mempty
  ma `mappend` mb = LyScoreM $ \op -> let (a,d1) = getLyScoreM ma op
                                          (b,d2) = getLyScoreM mb op
                                      in (a `mappend` b, d1 `op` d2)



runLyScore :: LyScoreM a -> (a,Doc)
runLyScore ma = getLyScoreM ma ($+$)


execLyScore :: LyScoreM a -> Doc
execLyScore = snd . runLyScore


scoreDoc_ :: Doc -> LyScoreM ()
scoreDoc_ d1 = LyScoreM $ \_ -> ((),d1)

mapDoc :: (Doc -> Doc) -> LyScoreM a -> LyScoreM a
mapDoc f ma = LyScoreM $ \r -> let (a,d1) = getLyScoreM ma r in (a, f d1)

version :: String -> LyScoreM ()
version ss = scoreDoc_ $ command "version" <+> doubleQuotes (text ss)

score :: LyScoreM a -> LyScoreM a
score = mapDoc $ block (command "score")

relative :: Pitch -> LyNoteListM a -> LyScoreM a
relative p ma = LyScoreM $ \_ -> 
    mapSnd fn $ runLyNoteListM (relPitch,relDuration) (p,dQuarter) ma
  where
    fn = block (command "relative" <+> PP.pitchTreble p)
   

--------------------------------------------------------------------------------
-- NoteList monad

-- Articulations are annotated after pitch and duration.


data NoteSt = NoteSt 
    { prev_pitch    :: Pitch
    , prev_duration :: Duration
    }


type NextPitch = Pitch -> Pitch -> (Pitch,Pitch)
type NextDur   = Duration -> Duration -> (Maybe Duration,Duration)

data NoteEnv = NoteEnv 
    { next_pitch   :: NextPitch
    , next_dur     :: NextDur
    , note_cat     :: DocCat
    }

-- If we are concatenating Docs in the monad should we follow 
-- Conal Elliott\'s context-monoid pattern so we can choose
-- between @<>@ and @<+>@ ?
-- 
-- Or do barlines, for example, know how to space themselves.
--

newtype LyNoteListM a = LyNoteListM { 
    getLyNoteListM :: NoteEnv -> NoteSt -> (a, NoteSt, Doc) }

instance Functor LyNoteListM where
  fmap f ma = LyNoteListM $ \r s -> 
                let (a,s1,d1) = getLyNoteListM ma r s in (f a, s1, d1) 

instance Applicative LyNoteListM where
  pure a    = LyNoteListM $ \_ s -> (a, s, empty)
  mf <*> ma = LyNoteListM $ \r s -> let op        = note_cat r
                                        (f,s1,d1) = getLyNoteListM mf r s
                                        (a,s2,d2) = getLyNoteListM ma r s1
                                    in (f a,s2,d1 `op` d2) 

instance Monad LyNoteListM where
  return  = pure
  m >>= k = LyNoteListM $ \r s -> let op        = note_cat r
                                      (a,s1,d1) = getLyNoteListM m r s
                                      (b,s2,d2) = getLyNoteListM (k a) r s1 
                                  in (b,s2,d1 `op` d2)

instance Monoid a => Monoid (LyNoteListM a) where
  mempty          = pure mempty
  ma `mappend` mb = LyNoteListM $ \r s -> let op        = note_cat r
                                              (a,s1,d1) = getLyNoteListM ma r s
                                              (b,s2,d2) = getLyNoteListM mb r s1 
                                          in (a `mappend` b, s2, d1 `op` d2)


-- | LilyPond\'s default duration is a quarter note.
--
-- Seeding the initial pitch is a pain point. Absolute scores
-- should not need to worry about seeding pitch, relative scores
-- should do it explicitly with @relative@. 
-- 
runLyNoteListM :: (NextPitch,NextDur) -> (Pitch,Duration) 
               -> LyNoteListM a -> (a,Doc)
runLyNoteListM (fp,fd) (p0,d0) ma = post $ getLyNoteListM ma r0 s0
  where
    post (a,_,d1) = (a,d1)

    s0 = NoteSt { prev_pitch    = p0 
                , prev_duration = d0
                }

    r0 = NoteEnv { next_pitch = fp 
                 , next_dur   = fd 
                 , note_cat   = (<+>)
                 }

runsLyNoteListM :: NoteEnv -> NoteSt -> [LyNoteListM a] -> ([a],NoteSt,[Doc])
runsLyNoteListM r0 s0 xs = step s0 xs 
  where
    step s []       = ([],s,[])
    step s (ma:ms)  = let (a,s1,d)   = getLyNoteListM ma r0 s
                          (as,s2,ds) = step s1 ms
                      in (a:as,s2,d:ds)




relPitch :: Pitch -> Pitch -> (Pitch,Pitch)
relPitch p p0 = let i = lyOctaveDist p0 p in (setOctave i p, p)

relDuration :: Duration -> Duration -> (Maybe Duration,Duration)
relDuration d d0 | d == d0   = (Nothing,d)
                 | otherwise = (Just d, d)

absPitch :: (Int -> Int) -> Pitch -> Pitch -> (Pitch,Pitch)
absPitch fn (Pitch l oa o) p0 = (Pitch l oa $ fn o,p0)



note :: Pitch -> Duration -> LyNoteListM ()
note p d = LyNoteListM $ \(NoteEnv fp fd _) (NoteSt p0 d0) -> 
    let (p_ans,p1) = fp p p0
        (mb_d,d1)  = fd d d0
    in ((), NoteSt p1 d1, PP.note p_ans mb_d)

chord :: [Pitch] -> Duration -> LyNoteListM ()
chord ps d = LyNoteListM $ \(NoteEnv fp fd _) (NoteSt p0 d0) -> 
   let (ns,p1)   = mapFst (map PP.pitch) $ stmap fp p0 ps 
       (mb_d,d1) = fd d d0  
   in ((), NoteSt p1 d1, PP.chordForm ns mb_d)


rest :: Duration -> LyNoteListM ()
rest d = LyNoteListM $ \(NoteEnv _ fd _) (NoteSt p0 d0) -> 
    let (mb_d,d1) = fd d d0
    in ((), NoteSt p0 d1, PP.rest mb_d)


spacer :: Duration -> LyNoteListM ()
spacer d = LyNoteListM $ \(NoteEnv _ fd _) (NoteSt p0 d0) -> 
    let (mb_d,d1) = fd d d0
    in ((), NoteSt p0 d1, PP.spacer mb_d)





-- | beam has to collect Docs as a list, not as a concatenated Doc.
--
beam :: [LyNoteListM a] -> LyNoteListM ()
beam ms = LyNoteListM $ \s r -> 
    let (_,s1,ds) = runsLyNoteListM s r ms 
    in ((), s1, PP.beamForm ds)


{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
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

    Doc                 -- re-export...
  , writeScore

  , Score
  , runScore
  , execScore

  , version
  , score
  , relative
  , notelist


  , NoteList
--  , runLyNoteListM

  , relPitch
  , relDuration
  , absPitch

  , key
  , time
  , clef
--  , repeat_volta

  , note
  , chord
  , rest
  , spacer
  , beam


  ) where 


import Neume.Core.Duration
import qualified Neume.Core.LilyPondPretty as PP
import Neume.Core.LilyPondPretty ( SDoc, LDoc )
import Neume.Core.Pitch
import Neume.Core.Utils.Basis
import Neume.Core.Utils.Pretty

import Text.PrettyPrint.HughesPJ

import Control.Applicative hiding ( empty )
import Data.Monoid


writeScore :: FilePath -> Doc -> IO ()
writeScore path = writeFile path . renderDocEighty



 



--------------------------------------------------------------------------------
-- Score monad


data NoteSt = NoteSt 
    { prev_pitch    :: Pitch
    , prev_duration :: Duration
    }


type NextPitch = Pitch -> Pitch -> (Pitch,Pitch)
type NextDur   = Duration -> Duration -> (Maybe Duration,Duration)


data NoteEnv = NoteEnv 
    { next_pitch    :: NextPitch
    , next_duration :: NextDur
    }


newtype LyMonad doc a = LyMonad { 
      getLyMonad :: NoteEnv -> NoteSt -> (a, NoteSt, doc) }


type family Docu m :: *

type instance Docu (LyMonad doc) = doc

class DocWriter m where
  tellDoc :: doc ~ Docu m => doc -> m ()


instance Functor (LyMonad doc) where
  fmap f ma = LyMonad $ \r s -> 
                let (a,s1,d1) = getLyMonad ma r s in (f a, s1, d1) 


instance Monoid doc => Applicative (LyMonad doc) where
  pure a    = LyMonad $ \_ s -> (a, s, mempty)
  mf <*> ma = LyMonad $ \r s -> let (f,s1,d1) = getLyMonad mf r s
                                    (a,s2,d2) = getLyMonad ma r s1
                                in (f a, s2, d1 `mappend` d2) 


instance Monoid doc => Monad (LyMonad doc) where
  return  = pure
  m >>= k = LyMonad $ \r s -> let (a,s1,d1) = getLyMonad m r s
                                  (b,s2,d2) = getLyMonad (k a) r s1 
                              in (b, s2, d1 `mappend` d2)


instance (Monoid a, Monoid doc) => Monoid (LyMonad doc a) where
  mempty = pure mempty
  ma `mappend` mb = mappend <$> ma <*> mb

instance DocWriter (LyMonad doc) where
  tellDoc d1 = LyMonad $ \_ s -> ((),s, d1)


newtype NoteList a = NoteList { getNoteList :: LyMonad SDoc a }
  deriving (Functor, Monad, Applicative, Monoid)

newtype Score a = Score { getScore :: LyMonad LDoc a }
  deriving (Functor, Monad, Applicative, Monoid)


type instance Docu NoteList = SDoc
type instance Docu Score = LDoc


instance DocWriter NoteList where
  tellDoc = NoteList . tellDoc


instance DocWriter Score where
  tellDoc = Score . tellDoc


-- | Initial values \"guaranteed\" not to match.
--
noteStZero :: NoteSt
noteStZero = NoteSt { prev_pitch = setOctave (-10) middle_c
                    , prev_duration = dZero
                    }

runScore :: Score a -> (a,Doc)
runScore ma = post $ getLyMonad (getScore ma) undefined noteStZero
  where
    post (a,_s,w) = (a, PP.unLDoc w)

execScore :: Score a -> Doc
execScore = snd . runScore


mapDoc :: (Doc -> Doc) -> Score a -> Score a
mapDoc f ma = Score $ LyMonad $ \r s -> 
    let (a,s1,w) = getLyMonad (getScore ma) r s
    in (a, s1, PP.mapLDoc f w)


tellLn :: Doc -> Score ()
tellLn = tellDoc . PP.newline

version :: String -> Score ()
version ss = tellDoc $ PP.newline $ command "version" <+> doubleQuotes (text ss)



score :: Score a -> Score a
score = mapDoc $ block (command "score")


relative :: Pitch -> Score a -> Score a
relative p ma = mapDoc post inner
  where
    e0    = NoteEnv { next_pitch = relPitch, next_duration = relDuration }
    s0    = NoteSt { prev_pitch = p, prev_duration = dZero}
    post  = block (command "relative" <+> PP.pitchTreble p)
    inner = Score $ LyMonad $ \_ _ -> getLyMonad (getScore ma) e0 s0 




notelist :: NoteList a -> Score a
notelist ma = Score $ LyMonad $ \r s -> 
    let (a,s1,w) = getLyMonad (getNoteList ma) r s
    in (a,s1, PP.newline $ PP.unSDoc w)


--------------------------------------------------------------------------------
-- NoteList monad

-- Articulations are annotated after pitch and duration.



-- If we are concatenating Docs in the monad should we follow 
-- Conal Elliott\'s context-monoid pattern so we can choose
-- between @<>@ and @<+>@ ?
-- 
-- Or do barlines, for example, know how to space themselves.
--



{-

-- | LilyPond\'s default duration is a quarter note.
--
-- Seeding the initial pitch is a pain point. Absolute scores
-- should not need to worry about seeding pitch, relative scores
-- should do it explicitly with @relative@. 
-- 
runLyNoteListM :: (NextPitch,NextDur) -> (Pitch,Duration) 
               -> LyNoteListM a -> (a,SDoc)
runLyNoteListM (fp,fd) (p0,d0) ma = post $ getLyNoteListM ma r0 s0
  where
    post (a,_,d1) = (a,d1)

    s0 = NoteSt { prev_pitch    = p0 
                , prev_duration = d0
                }

    r0 = NoteEnv { next_pitch = fp 
                 , next_dur   = fd 
                 }

-}

transformOutput :: (SDoc -> SDoc) -> NoteList a -> NoteList a
transformOutput f ma = NoteList $ LyMonad $ \r s -> 
    let (a,s1,w) = getLyMonad (getNoteList ma) r s in (a, s1, f w)

-- cxfTell :: SDoc -> LyNoteListM ()
-- cxfTell d1 = LyNoteListM $ \_ s -> ((), s, d1)



{-
runsLyNoteListM :: NoteEnv -> NoteSt -> [LyNoteListM a] -> ([a],NoteSt,[Doc])
runsLyNoteListM r0 s0 xs = step s0 xs 
  where
    step s []       = ([],s,[])
    step s (ma:ms)  = let (a,s1,d)   = getLyNoteListM ma r0 s
                          (as,s2,ds) = step s1 ms
                      in (a:as,s2,d:ds)
-}



relPitch :: Pitch -> Pitch -> (Pitch,Pitch)
relPitch p p0 = let i = lyOctaveDist p0 p in (setOctave i p, p)

relDuration :: Duration -> Duration -> (Maybe Duration,Duration)
relDuration d d0 | d == d0   = (Nothing,d)
                 | otherwise = (Just d, d)

absPitch :: (Int -> Int) -> Pitch -> Pitch -> (Pitch,Pitch)
absPitch fn (Pitch l oa o) p0 = (Pitch l oa $ fn o,p0)



key :: PitchLabel -> String -> Score ()
key pl ss = tellLn $ command "key" <+> PP.pitchLabel pl <+> command ss


time :: (Int,Int) -> Score ()
time (n,d) = tellLn $ command "time" <+> int n <> char '/' <> int d


clef :: String -> Score ()
clef ss = tellLn $ command "clef" <+> text ss


{-

repeat_volta :: Int -> LyNoteListM a -> LyNoteListM a
repeat_volta i = mapDoc (block prefix) 
  where
    prefix = command "repeat" <+> text "volta" <+> int i
-}




note :: Pitch -> Duration -> NoteList ()
note p d = NoteList $ LyMonad $ \(NoteEnv fp fd) (NoteSt p0 d0) -> 
    let (p_ans,p1) = fp p p0
        (mb_d,d1)  = fd d d0
    in ((), NoteSt p1 d1, PP.note p_ans mb_d)


chord :: [Pitch] -> Duration -> NoteList()
chord ps d = NoteList $ LyMonad $ \(NoteEnv fp fd) (NoteSt p0 d0) -> 
   let (ns,p1)   = stmap fp p0 ps 
       (mb_d,d1) = fd d d0  
   in ((), NoteSt p1 d1, PP.chordForm ns mb_d)



rest :: Duration -> NoteList ()
rest d = NoteList $ LyMonad $ \(NoteEnv _ fd) (NoteSt p0 d0) -> 
    let (mb_d,d1) = fd d d0
    in ((), NoteSt p0 d1, PP.rest mb_d)


spacer :: Duration -> NoteList ()
spacer d = NoteList $ LyMonad $ \(NoteEnv _ fd) (NoteSt p0 d0) -> 
    let (mb_d,d1) = fd d d0
    in ((), NoteSt p0 d1, PP.spacer mb_d)





-- | beam has to collect Docs as a list, not as a concatenated Doc.
--
beam :: NoteList a -> NoteList a
beam = transformOutput PP.beamForm


{-
-- Note - it might be better to have beam in a higher level layer
-- and just define lbeam and rbeam here.

lbeam :: LyNoteListM ()
lbeam = liftDoc lparen

rbeam :: LyNoteListM ()
rbeam = liftDoc rparen
-}

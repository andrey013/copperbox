
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Traversals
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Traverse a NoteList tracking to perform shape-preserving transformations
--
--------------------------------------------------------------------------------

module HNotate.Traversals where

import HNotate.CommonUtils (prod)
import HNotate.Duration
import HNotate.Env
import HNotate.NotateMonad
import HNotate.MusicRepDatatypes (naturalize)
import HNotate.NoteListDatatypes
import HNotate.Pitch


import Control.Applicative
import Control.Monad.Identity hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)
import Data.Monoid
import Data.Ratio
import Data.Sequence
import Data.Traversable
import Prelude hiding (mapM)

--------------------------------------------------------------------------------
-- Three useful run functions


traverseIdentity :: (Traversable t) => 
                    (a -> WrappedMonad Identity b) -> t a -> t b
traverseIdentity f a = runIdentity (unwrapMonad $ traverse f a) 


traverseReader :: (Traversable t) =>
                  (a -> WrappedMonad (Reader env) b) -> t a -> env -> t b
traverseReader f a env = runReader (unwrapMonad $ traverse f a) env 

traverseState :: (Traversable t) =>
                  (a -> WrappedMonad (State st) b) -> t a -> st -> t b
traverseState f a st = evalState (unwrapMonad $ traverse f a) st 

--------------------------------------------------------------------------------
-- 



instance Applicative Identity where
  pure = return
  (<*>) = ap


-- from Jeremy Gibbons and Bruno Oliviera's Iterator paper
-- (doesn't seem to be in the Hierarchical Libraries)

data Comp m n a = Comp { unComp :: m (n a) }

instance (Functor m, Functor n) => Functor (Comp m n) where
  fmap f (Comp x) = Comp $ (fmap . fmap) f x
  
  
comp :: (Functor n, Functor m) => (b -> n c) -> (a -> m b) -> a -> Comp m n c
f `comp` g = Comp . fmap f . g

instance (Applicative m, Applicative n) => Applicative (Comp m n) where
  pure x    = Comp (pure (pure x))
  mf <*> mx = Comp (pure (<*>) <*> unComp mf <*> unComp mx)
 


--------------------------------------------------------------------------------
-- A state type for stateful LilyPond transformations 

data LyState = LyState { rel_pitch :: Pitch, rel_duration :: Duration }

mkLyState :: Pitch -> LyState
mkLyState pch = LyState { rel_pitch = pch, rel_duration = quarter }
                     
                     
--------------------------------------------------------------------------------
-- run length encode the duration - LilyPond uses this method

-- drle - duration run length encode
drleBody :: Grouping -> WrappedMonad (State LyState) Grouping

-- never modify a tuplets unit_duration (its real duration is syntesized) 
drleBody e@(Nplet _ ud _ _) = WrapMonad $ do
    modify (\s -> s {rel_duration=ud})
    return e
  

drleBody e = WrapMonad $ do
    od <- diffDuration (rhythmicValue e)
    return $ modifyDuration e od    
   
diffDuration :: Duration -> State LyState Duration
diffDuration d = do
    old <- gets rel_duration
    if (old == d) then return no_duration
                  else modify (\s -> s {rel_duration=d}) >> return d

-- Here it would be helpful to know if we are at the start of
-- a measure - for readability in the output it makes a lot of 
-- sense to give the duration of the first note in a measure
-- even if it is the same as the last note in the previous
-- measure.
-- Also grace notes cause a problem - ideally we should store 
-- the last duration before the gracenotes and restore it after
-- them. 

--------------------------------------------------------------------------------
-- change pitches according to relative octave - LilyPond uses this method

-- pro - pitch relative octave
-- The first note of a chord changes the 'global' relative pitch
-- and the subsequent notes only change it 'locally'.
-- All successive notes in a grace notes change 'global' relative pitch
 
proBody :: Grouping -> WrappedMonad (State LyState) Grouping
proBody = WrapMonad . step
  where 
    step (Singleton e)        = Singleton <$> gstep e 

    step (Chord se d a)       = (\se' -> Chord se' d a) <$> convChordPitches se
    
    step (GraceNotes se m a)  = (\se' -> GraceNotes se' m a) <$> mapM fn se
      where fn (p,d) = convPitch p >>= \p' -> return (p',d)
      
    step (Nplet i ud se a)    = (\se' -> Nplet i ud se' a) <$> mapM convPitch se
                                       
    gstep :: Atom -> State LyState Atom
    gstep e@(Note p _ _)      = (\p' -> e `modifyPitch` [p]) <$> convPitch p 
    gstep e                   = return e


convChordPitches :: Seq Pitch -> State LyState (Seq Pitch)
convChordPitches se = case viewl se of
  (e :< sse) -> do e'  <- convPitch e
                   se' <- localPitch $ mapM convPitch sse 
                   return $ e' <| se'
  EmptyL     -> return mempty

localPitch :: State LyState ans -> State LyState ans
localPitch mf = do 
    initial <- gets rel_pitch
    ans     <- mf
    modify (\s -> s { rel_pitch = initial })
    return ans   
        

convPitch :: Pitch -> State LyState Pitch
convPitch p = do
    base <-  gets rel_pitch
    modify (\s -> s { rel_pitch = p })
    return $ p `changeOctaveWrt` base


changeOctaveWrt :: Pitch -> Pitch -> Pitch
changeOctaveWrt pch@(Pitch l a _) base = Pitch l a (base `octaveDist` pch)
    

--------------------------------------------------------------------------------
-- lilypond octave shift - lilypond middle c is c' 
-- HNotate middle c is c4, the shift will be (-3) so that the octave spec
-- tells how many ''' 's need to be printed 
-- (a negative number gives the number of ,,, 's)   


losBody :: Grouping -> WrappedMonad Identity Grouping
losBody gp = WrapMonad $ return $ fn gp
  where
    fn (Singleton (Note p d anno))  = Singleton (Note (down3ve p)  d anno)
    fn (Singleton e)                = Singleton e
    fn (Chord se d anno)            = Chord (fmap down3ve se) d anno
    fn (GraceNotes se d anno)       = 
        GraceNotes (fmap (prod down3ve id) se) d anno
    
    fn (Nplet i ud se anno)         = Nplet i ud (fmap down3ve se) anno 
    
    down3ve (Pitch l a o) = Pitch l a (o-3)

--------------------------------------------------------------------------------
-- 'default encode' the duration - if the duration matches the unit note length
-- don't specify it - Abc uses this method 


-- unit note length encode
unleBody :: Monad m => Grouping -> WrappedMonad (NotateMonadT Env Config m) Grouping
unleBody gp = 
    WrapMonad $ fn gp (rhythmicValue gp) <$> asks unit_note_length
  where
    fn e drn unl | drn == unl = modifyDuration e no_duration
                 | otherwise  = modifyDuration e (abcScaleDuration drn unl)           

    abcScaleDuration drn unl = 
        let (nr,dr)  = ratioElements drn
            (un,ud)  = ratioElements unl
        in  (nr%dr) / (un%ud)                         

--------------------------------------------------------------------------------
-- pitch label rename     

-- unit note length encode
plrBody :: Monad m => Grouping -> WrappedMonad (NotateMonadT Env Config m) Grouping
plrBody gp = WrapMonad $ (respell gp) <$> asks label_set
  where
    respell (Singleton e)       ls = Singleton (respell' e ls)

    respell (Chord se d a)      ls = Chord (fmap (cf ls) se) d a
    
    respell (GraceNotes se m a) ls = GraceNotes (fmap (gf ls) se) m a
    
    respell (Nplet i ud se a)   ls = Nplet i ud (fmap (cf ls) se) a
    

    respell' (Note p d a)       ls = Note (p `naturalize` ls) d a
    respell' e                  _  = e
    
    cf ls p     = p `naturalize` ls
    gf ls (p,d) = (p `naturalize` ls, d)
    

       

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
-- Traverse a DScore tracking duration only if it changes (c.f. LilyPond).
--
--------------------------------------------------------------------------------

module HNotate.Traversals where


import HNotate.Duration
import HNotate.Env
import HNotate.NoteList
import HNotate.Pitch


import Control.Applicative
import Control.Monad.Identity hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence
import Data.Traversable
import Prelude hiding (mapM)

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




changeDuration :: ScoreGlyph -> Duration -> ScoreGlyph
changeDuration a od = onDuration (const od) a

changePitch :: ScoreGlyph -> Pitch -> ScoreGlyph
changePitch a op = onPitch (const op) a


--------------------------------------------------------------------------------
-- run length encode the duration - LilyPond uses this method


runLengthEncodeDuration :: ScNoteList (ScoreGlyph)
                        -> Duration
                        -> ScNoteList (ScoreGlyph)
runLengthEncodeDuration strata initial_duration = 
    traversalState drleBody strata initial_duration

-- drle - duration run length encode
drleBody :: ScoreGlyph -> WrappedMonad (State Duration) ScoreGlyph
drleBody e = WrapMonad $ do
    od <- diffDuration (glyphDuration e)
    return $ changeDuration e od    
  where    
    diffDuration :: Duration -> State St Duration
    diffDuration d = do
        old <- get 
        if (old == d) then return no_duration
                      else do {put d; return d}


--------------------------------------------------------------------------------
-- change pitches according to relative octave - LilyPond uses this method

-- pro - pitch relative octave
-- The first note of a chord changes the 'global' relative pitch
-- and the subsequent notes only change it 'locally'.
-- All successive notes in a grace notes change 'global' relative pitch
 
proBody :: ScoreGlyph -> WrappedMonad (State Pitch) ScoreGlyph
proBody e = WrapMonad $ step e
  where 
    step e@(SgNote p _)       = do 
        nt <- convPitch p 
        return $ changePitch e nt

    step (SgChord se d)       = 
        (\se' -> SgChord se' d) <$> convChordPitches se
    
    step (SgGraceNotes se)    = 
        SgGraceNotes <$> mapM convPitch se

    step e                    = return e         


convChordPitches :: Seq Pitch -> State Pitch (Seq Pitch)
convChordPitches se = case viewl se of
  (e :< sse) -> do e'  <- convPitch e
                   se' <- localPitch $ mapM convPitch sse 
                   return $ e' <| se'
  EmptyL     -> return mempty

localPitch :: State st ans -> State st ans
localPitch mf = do 
    initial <- get
    ans     <- mf
    put initial
    return ans   
        

convPitch :: Pitch -> State Pitch Pitch
convPitch p = do
    base <- get
    put p
    return $ p `changeOctaveWrt` base


changeOctaveWrt :: Pitch -> Pitch -> Pitch
changeOctaveWrt pch@(Pitch l a o) base = Pitch l a (base `octaveDist` pch)
    
    

--------------------------------------------------------------------------------
-- 'default encode' the duration - if the duration matches the default 
-- don't specify it - Abc uses this method 



unitNoteLengthEncode :: ScNoteList ScoreGlyph
                      -> Env
                      -> ScNoteList ScoreGlyph
unitNoteLengthEncode strata env = 
    traversalReader unleBody strata env

-- unit note length encode
unleBody :: ScoreGlyph -> WrappedMonad (Reader Env) ScoreGlyph
unleBody e = let drn = glyphDuration e in WrapMonad $ 
             fn e drn <$> asks unit_note_length
  where
    fn e drn unl | drn == unl = changeDuration e no_duration
                 | otherwise  = e            

    



       
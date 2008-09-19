
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
import Data.Ratio
import Data.Sequence
import Data.Traversable
import Prelude hiding (mapM)

type St = Duration

data LyState = LyState { rel_pitch :: Pitch, rel_duration :: Duration }

lyState0 = LyState { rel_pitch    = middleC,
                     rel_duration = quarter }

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

-- drle - duration run length encode
drleBody :: ScoreGlyph -> WrappedMonad (State LyState) ScoreGlyph
drleBody e = WrapMonad $ do
    od <- diffDuration (glyphDuration e)
    return $ changeDuration e od    
  where    
    diffDuration :: Duration -> State LyState Duration
    diffDuration d = do
        old <- gets rel_duration
        if (old == d) then return no_duration
                      else modify (\s -> s {rel_duration=d}) >> return d


--------------------------------------------------------------------------------
-- change pitches according to relative octave - LilyPond uses this method

-- pro - pitch relative octave
-- The first note of a chord changes the 'global' relative pitch
-- and the subsequent notes only change it 'locally'.
-- All successive notes in a grace notes change 'global' relative pitch
 
proBody :: ScoreGlyph -> WrappedMonad (State LyState) ScoreGlyph
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
changeOctaveWrt pch@(Pitch l a o) base = Pitch l a (base `octaveDist` pch)
    

--------------------------------------------------------------------------------
-- lilypond octave shift - lilypond middle c is c' 
-- HNotate middle c is c4, the shift will be (-3) so that the octave spec
-- tells how many ''' 's need to be printed 
-- (a negative number gives the number of ,,, 's)   


losBody :: ScoreGlyph -> WrappedMonad Identity ScoreGlyph
losBody e = WrapMonad $ return $ onPitch fn e
  where
    fn (Pitch l a o) = Pitch l a (o-3)
    

--------------------------------------------------------------------------------
-- 'default encode' the duration - if the duration matches the unit note length
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
                 | otherwise  = changeDuration e (abcScaleDuration drn unl)           

    abcScaleDuration drn unl = 
        let (nr,dr,dc)  = durationElements drn
            (un,ud,_)   = durationElements unl
        in Duration ((nr%dr) / (un%ud)) dc                         

--------------------------------------------------------------------------------
-- pitch label rename     

-- unit note length encode
plrBody :: ScoreGlyph -> WrappedMonad (Reader Env) ScoreGlyph
plrBody e = WrapMonad $ (respell e) <$> asks label_set
  where
    respell (SgNote p d)      lbls  = SgNote (naturalize p lbls) d

    respell (SgChord se d)    lbls  = SgChord (fmap (naturalizef lbls) se) d
    
    respell (SgGraceNotes se) lbls  = SgGraceNotes (fmap (naturalizef lbls) se)

    respell e                 lbls  = e  
    
    naturalizef = flip naturalize

       
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit.Builder
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Monadic building for /high level/ MIDI, with a reader writer monad. 
-- 
-- The reader monad makes some of the parameters implicit to the
-- build functions, which would otherwise require many arguments.
-- The writer monad accumulates notes efficiently. 
--
--------------------------------------------------------------------------------

module ZMidi.Emit.Builder
  (


  -- * Build monad
    BuildEnv(..)
  , EnvTransformer
  , build_env_zero

  , NoteList
  , runNoteList
  , execNoteList
 
  , localize 
  , report
  , askEnv
  , asksEnv
  , noteProps


  ) where


import ZMidi.Emit.SyntaxInternal
import ZMidi.Emit.Utils.HList

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Word



--------------------------------------------------------------------------------



-- | Common params for every note on that it would be tedious to
-- include with every constructor.
--
-- It would be nice to have volume settable on individual notes, 
-- but in MIDI volume is a message t the synthesizer to change the 
-- volume level (techinically it is two messages one for the volume 
-- MSB and one for the LSB).
--
data BuildEnv = BuildEnv
      { note_on_velocity        :: Word8
      , note_off_velocity       :: Word8
      }
  deriving (Eq,Ord,Show)



-- | Change the reader environment of the 'NoteList' monad.
--
-- As the NoteList monad is a reader monad and not a state monad,
-- changes to the enviroment are /local/ and they should be 
-- actioned with the 'localize' function.
--
type EnvTransformer = BuildEnv -> BuildEnv


build_env_zero :: BuildEnv
build_env_zero = BuildEnv
      { note_on_velocity     = 127
      , note_off_velocity    = 64
      }

-- | Note lists are built within the 'NoteList' monad.
--
-- This is a reader-writer monad with the reader environment 
-- tracking implicit parameters for note creation (note-on 
-- velocity, note-off velocity) and the writer allowing efficient
-- creation of the note list.
-- 
newtype NoteList a = NoteList { getNoteList :: BuildEnv -> (a, H Primitive) }

instance Functor NoteList where
  fmap f ma = NoteList $ \r -> let (a,w) = getNoteList ma r in (f a, w)


instance Applicative NoteList where
  pure a    = NoteList $ \_ -> (a, mempty)
  mf <*> ma = NoteList $ \r -> let (f,w1) = getNoteList mf r
                                   (a,w2) = getNoteList ma r
                               in (f a, w1 `mappend` w2)


instance Monad NoteList where
  return a  = NoteList $ \_ -> (a,mempty)
  m >>= k   = NoteList $ \r -> let (a,w1) = getNoteList m r 
                                   (b,w2) = getNoteList (k a) r
                               in (b, w1 `mappend` w2)
                          

runNoteList :: BuildEnv -> NoteList a -> (a, [Primitive])
runNoteList env ma = post $ getNoteList ma env
  where
    post (a,f) = (a, toListH f)

execNoteList :: BuildEnv -> NoteList a -> [Primitive]
execNoteList env ma = post $ getNoteList ma env
  where
    post (_,f) = toListH f



-- | Run a NoteList action within the modified environment.
--
localize :: (BuildEnv -> BuildEnv) -> NoteList a -> NoteList a
localize f ma = NoteList  $ \r -> getNoteList ma (f r)


askEnv   :: NoteList BuildEnv
askEnv   = NoteList  $ \r -> (r, mempty)


report   :: Primitive -> NoteList  ()
report a = NoteList  $ \_ -> ((), wrapH a)


asksEnv :: (BuildEnv -> a) -> NoteList  a
asksEnv extr = extr <$> askEnv


noteProps :: NoteList PrimProps
noteProps = (\r -> PrimProps { velocity_on    = note_on_velocity r
                             , velocity_off   = note_off_velocity r
                             })
              <$> askEnv




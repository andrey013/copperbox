

module MulleinTemp where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Gen.Syntax

import Control.Monad.State
import Data.Ratio

type NoteCtx a = State NoteListCtx a

data NoteListEnv = NoteListEnv 
      { note_list_key    :: Key,
        note_list_meter  :: Meter }
  deriving (Eq,Show)

-- NoteListCtx represents /shorthand state/ so we can omit
-- some details when building the notelist (e.g. duration) 
data NoteListCtx = NoteListCtx 
      { unit_note_length :: Duration }
  deriving (Eq,Show)

type NoteList = [Element]

notelist :: [NoteCtx Element] -> [Element]
notelist fs = evalState (sequence fs) ctx0 where
    ctx0 = NoteListCtx { unit_note_length = 1%4 }

(&) :: NoteCtx Element -> NoteCtx () -> NoteCtx Element
(&) f upd  = upd >> f 

 
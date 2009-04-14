

module MulleinTemp where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Gen.Syntax

import Control.Monad.State

type NoteCtx a = State NoteListCtx a


data NoteListCtx = NoteListCtx 
      { unit_note_length :: Duration,
        note_list_key    :: Key,
        note_list_meter  :: Meter }
  deriving (Eq,Show)

type NoteList = [NoteCtx Element]

notelist = undefined

(&) :: NoteCtx Element -> NoteCtx () -> NoteCtx Element
(&) f upd  = upd >> f 

 
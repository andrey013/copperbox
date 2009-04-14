

module Bulgarian6 where

import Mullein.Core
import Mullein.CoreTypes hiding (NoteList)
import Mullein.NamedElements ( a_major )
import Mullein.ScoreNames

-- import MulleinTemp
-- import NamedTemp

import Control.Monad.State

{-
- WithMeter and withKey aren't quite right as they run at a later
-- phase than building the note list.
withMeter :: Meter -> NoteCtx a -> NoteCtx a
withMeter meter f = modify (\s -> s {note_list_meter=meter}) >> f 


withKey :: Key -> NoteCtx a -> NoteCtx a
withKey key f = modify (\s -> s {note_list_key=key}) >> f
-}


bars1_4 :: NoteList
bars1_4 = notelist $ 
           [ a4 & du16, b4, cis5, cis5, cis5, a4, cis5, cis5
           , cis5, a4, b4, cis5, b4, a4, a4, rest        
           , e5, d5, cis5, b4, cis5, a4, b4, cis5
           , a4, b4, b4, a4 , a4 & du8, rest    
           ]

bars5_8 = notelist $ 
           [ c5 & du8, b4 & du16, a4, b4 & du8, a4 & du16, gis4
           , fis4, e4, fis4, gis4, a4 & du8, b4
           , cis5, b4 & du16, a4, b4 & du8, a4 & du16, gis4
           , fis4, e4, fis4 & du8, rest
           ]

 
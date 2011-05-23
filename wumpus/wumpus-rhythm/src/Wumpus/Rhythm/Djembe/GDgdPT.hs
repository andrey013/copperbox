{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.GDgdPT
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Note heads following the GDgdPT conventions.
--
--------------------------------------------------------------------------------

module Wumpus.Rhythm.Djembe.GDgdPT 
  (

  -- * Utility  
    period
  , blank

  -- * Djembe
  , bass_Gun
  , bass_Dun
  , muffled_bass_Gun
  , muffled_bass_Dun

  , tone_go
  , tone_do
  , muffled_tone_go
  , muffled_tone_do

  , slap_Pa 
  , slap_Ta
  , muffled_slap_Pa
  , muffled_slap_Ta

  , bass_flam_DGun
  , tone_flam_dgo
  , slap_flam_TPa

  , bass_flam_GDun
  , tone_flam_gdo
  , slap_flam_PTa

  -- * Kenkeni
  , kenkeni_stroke
  , kenkeni_pressed_stroke
  , kenkeni_lo_stroke
  , kenkeni_stroke_plus_hi
  , kenkeni_hi_stroke

  -- * Sangban
  , sangban_stroke
  , sangban_pressed_stroke
  , sangban_hi_stroke
  , sangban_stroke_plus_hi
  , sangban_pressed_stroke_plus_hi

  -- * Doundounba
  , doundounba_stroke
  , doundounba_pressed_stroke
  , doundounba_stroke_plus_hi
  , doundounba_hi_stroke

  -- * Atoke
  , atoke_bass_drum_bell
  , atoke_bell_stroke

  -- * Agogo
  , agogo_hi_stroke
  , agogo_lo_stroke

  -- * Shekere
  , shekere_downstroke
  , shekere_upstroke
  , shekere_downstroke_plus_hand_hit

  ) where


import Wumpus.Rhythm.Djembe.Draw
import Wumpus.Rhythm.Djembe.GraphicPrimitives


period                  :: NoteHead
period                  = noteHead periodNote

blank                   :: NoteHead
blank                   = noteHead noNote 


-- | Note - 'G'.
--
bass_Gun                :: NoteHead
bass_Gun                = noteHead $ charNoteHead 'G'

-- | Note - 'D'.
--
bass_Dun                :: NoteHead
bass_Dun                = noteHead $ charNoteHead 'D'



-- | Note - 'G' with slash (strikethough).
--
muffled_bass_Gun        :: NoteHead
muffled_bass_Gun        = noteHead $ strikeNoteHead $ charDesc 'G'

-- | Note - 'D' with slash (strikethough).
--
muffled_bass_Dun        :: NoteHead
muffled_bass_Dun        = noteHead $ strikeNoteHead $ charDesc 'D'


-- | Note - 'g'.
--
tone_go                 :: NoteHead
tone_go                 = noteHead $ charNoteHead 'g'

-- | Note - 'd'.
--
tone_do                 :: NoteHead
tone_do                 = noteHead $ charNoteHead 'd'


-- | Note - 'g' with slash (strikethough).
--
muffled_tone_go         :: NoteHead
muffled_tone_go         = noteHead $ strikeNoteHead $ charDesc 'G'

-- | Note - 'd' with slash (strikethough).
--
muffled_tone_do         :: NoteHead
muffled_tone_do         = noteHead $ strikeNoteHead $ charDesc 'D'


-- | Note - 'P'.
--
slap_Pa                 :: NoteHead
slap_Pa                 = noteHead $ charNoteHead 'P'

-- | Note - 'T'.
--
slap_Ta                 :: NoteHead
slap_Ta                 = noteHead $ charNoteHead 'T'



-- | Note - 'P' with slash (strikethough).
--
muffled_slap_Pa         :: NoteHead
muffled_slap_Pa         = noteHead $ strikeNoteHead $ charDesc 'P'

-- | Note - 'T' with slash (strikethough).
--
muffled_slap_Ta         :: NoteHead
muffled_slap_Ta         = noteHead $ strikeNoteHead $ charDesc 'T'



-- | Flam - @[D]G@
-- 
-- Note the type is Note, not NoteHead.
--
bass_flam_DGun          :: Note
bass_flam_DGun          = F (charFlam 'D') bass_Gun noAnno

-- | Flam - @[d]g@
-- 
-- Note the type is Note, not NoteHead.
--
tone_flam_dgo           :: Note
tone_flam_dgo           = F (charFlam 'd') tone_go noAnno

-- | Flam - @[T]P@
-- 
-- Note the type is Note, not NoteHead.
--
slap_flam_TPa           :: Note
slap_flam_TPa           = F (charFlam 'T') slap_Pa noAnno


-- | Flam - @[G]D@
-- 
-- Note the type is Note, not NoteHead.
--
bass_flam_GDun          :: Note
bass_flam_GDun          = F (charFlam 'G') bass_Dun noAnno

-- | Flam - @[g]d@
-- 
-- Note the type is Note, not NoteHead.
--
tone_flam_gdo           :: Note
tone_flam_gdo           = F (charFlam 'g') tone_do noAnno

-- | Flam - @[P]T@
-- 
-- Note the type is Note, not NoteHead.
--
slap_flam_PTa           :: Note
slap_flam_PTa           = F (charFlam 'P') slap_Ta noAnno


-- | Note is disk.
--
kenkeni_stroke          :: NoteHead
kenkeni_stroke          = noteHead $ diskNote


-- | Note is disk.
--
kenkeni_pressed_stroke  :: NoteHead
kenkeni_pressed_stroke  = noteHead $ strikeNoteHead $ diskDesc


-- | Note is disk.
--
kenkeni_lo_stroke       :: NoteHead
kenkeni_lo_stroke       = noteHead $ lowNoteHead diskDesc

-- | Note is disk, plus 'X' at high position.
--
kenkeni_stroke_plus_hi  :: NoteHead
kenkeni_stroke_plus_hi  = noteHead $ highBiNoteHead (charDesc 'X') diskNote

-- | Note is 'X' at the high position.
--
kenkeni_hi_stroke       :: NoteHead
kenkeni_hi_stroke       = noteHead $ highNoteHead $ charDesc 'X'

-- Note - terminology here is getting confused, hi and lo
-- strokes for kenkeni use different note heads...

-- | Note is disk.
--
sangban_stroke          :: NoteHead
sangban_stroke          = noteHead $ diskNote


-- | Note is disk.
--
sangban_pressed_stroke  :: NoteHead
sangban_pressed_stroke  = noteHead $ strikeNoteHead $ diskDesc


-- | Note is 'X' at the high position.
--
sangban_hi_stroke       :: NoteHead
sangban_hi_stroke       = noteHead $ highNoteHead $ charDesc 'X'


-- | Note is disk, plus 'X' at high position.
--
sangban_stroke_plus_hi  :: NoteHead
sangban_stroke_plus_hi  = 
    noteHead $ highBiNoteHead (charDesc 'X') diskNote



-- | Note is stroked disk, plus 'X' at high position.
--
sangban_pressed_stroke_plus_hi  :: NoteHead
sangban_pressed_stroke_plus_hi  = 
    noteHead $ highBiNoteHead (charDesc 'X') (strikeNoteHead $ diskDesc)


-- | Note is 'B'
--
doundounba_stroke       :: NoteHead
doundounba_stroke       = noteHead $ charNoteHead 'B'


-- | Note is 'B'.
--
doundounba_pressed_stroke  :: NoteHead
doundounba_pressed_stroke  = noteHead $ strikeNoteHead $ charDesc 'B'



-- | Note is 'B', plus 'X' at high position.
--
doundounba_stroke_plus_hi  :: NoteHead
doundounba_stroke_plus_hi  = 
    noteHead $ highBiNoteHead (charDesc 'X') (charNoteHead 'B')


-- | 'X' at high position.
--
doundounba_hi_stroke       :: NoteHead
doundounba_hi_stroke       = noteHead $ highNoteHead $ charDesc 'X'



-- | Note is 'X' at the high position.
--
atoke_bass_drum_bell     :: NoteHead
atoke_bass_drum_bell     = noteHead $ highNoteHead $ charDesc 'X'


-- | Note is 'X'.
--
atoke_bell_stroke       :: NoteHead
atoke_bell_stroke       = noteHead $ charNoteHead 'X'



-- | Note is 'X' at the high position.
--
agogo_hi_stroke         :: NoteHead
agogo_hi_stroke         = noteHead $ highNoteHead $ charDesc 'X'

-- | Note is 'X'.
--
agogo_lo_stroke         :: NoteHead
agogo_lo_stroke         = noteHead $ charNoteHead 'X'



-- | Note is 'X'.
--
shekere_downstroke      :: NoteHead
shekere_downstroke      = noteHead $ charNoteHead 'X'


-- | Note is 'X' at the high position.
--
shekere_upstroke        :: NoteHead
shekere_upstroke        = noteHead $ highNoteHead $ charDesc 'X'


-- | Note is 'X', low is disk.
--
shekere_downstroke_plus_hand_hit  :: NoteHead
shekere_downstroke_plus_hand_hit  = 
    noteHead $ lowBiNoteHead diskDesc (charNoteHead 'X')

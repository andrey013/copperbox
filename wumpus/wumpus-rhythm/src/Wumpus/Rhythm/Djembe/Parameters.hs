{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.Parameters
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Parameters
--
--------------------------------------------------------------------------------

module Wumpus.Rhythm.Djembe.Parameters 
  (
  

  -- * Unit width
    unit_width_12_8

  -- * Cap height
  , helvetica_cap_height    




  -- * Notehead params
  , disk_radius
  , disk_ydist
  , period_radius
  , period_ydist



  -- * Accents
  , accent_stroke_ydist
  , accent_hand_ydist

  , hand_side_length
  , underscore_ydist

  , leadin_mark_width
  , leadin_mark_height

  -- * Stems
  , stem_top
  , stem_length
  , flam_xdist
  , flam_ydist

  , flam_vstem_length
  , flam_disk_radius
  , divstem_beam_ydrop

  -- * Bar lines and repeats
  , repeat_line_hdist
  , repeat_dot_hdist
  , repeat_dot_radius
  , hi_repeat_dot_ydist
  , lo_repeat_dot_ydist


  -- * Tuplet grouping
  , plet_ydist



  ) where



import Wumpus.Core                              -- package: wumpus-core



-- | Unit width - i.e. the stem to stem distance - for 12\/8 
-- time.
-- 
-- Other meters should calculated the unit width proprtional to 
-- this value.
--  
unit_width_12_8         :: AfmUnit
unit_width_12_8         = 1454


-- | Note heads should use this for the y_major.
--
helvetica_cap_height    :: AfmUnit
helvetica_cap_height    = 718




--------------------------------------------------------------------------------
-- Noteheads

-- | Radius of disk notehead - this is the /tone/ note for a 
-- Djembe.
--
disk_radius             :: AfmUnit
disk_radius             = 272

-- | Baseline to disk center, vertical measure.
--
disk_ydist              :: AfmUnit 
disk_ydist              = 306


-- | Radius of period notehead - this is the /rest/ note (??) for 
-- a Djembe.
--
period_radius           :: AfmUnit
period_radius           = 60


-- | Baseline to period center, vertical measure.
--
period_ydist            :: AfmUnit
period_ydist            = 108




--------------------------------------------------------------------------------
-- Accents and annotations



-- | stem_top to char baseline of \'<\'
--
accent_stroke_ydist     :: AfmUnit
accent_stroke_ydist     = 32


-- | baseline to center of hand rectangle.
--
accent_hand_ydist       ::  AfmUnit
accent_hand_ydist       = 448


-- | side length of a hand symbol. 
--
-- Hand symbols are squares so all sides are the same.
--
hand_side_length        :: AfmUnit
hand_side_length        = 260


-- | Baseline to underscore position, vertical measure.
--
underscore_ydist        :: AfmUnit
underscore_ydist        = 92


-- | Width of the leadin mark - leadin is a @v@ with a stick.
--
leadin_mark_width       :: AfmUnit
leadin_mark_width       = 702


-- | Height of the leadin mark - leadin is a @v@ with a stick.
--
leadin_mark_height      :: AfmUnit
leadin_mark_height      = 408


--------------------------------------------------------------------------------
-- Stems


-- | Top of the stem - i.e. distance from the baseline to the
-- stem top.
--
stem_top                :: AfmUnit
stem_top                = 2272


-- | Length of a stem.
--
stem_length             :: AfmUnit
stem_length             = 1454

-- | Horizontal distance from the stem to the flam stem.
--
flam_xdist             :: AfmUnit
flam_xdist             = 354

-- | Baseline to flam stem start.
--
flam_ydist              :: AfmUnit
flam_ydist              = 904   


-- | Length of the initial vertical stem of a flam.
--
flam_vstem_length       :: AfmUnit
flam_vstem_length       = 636

flam_disk_radius        :: AfmUnit
flam_disk_radius        = 134




-- | Drop from the beam line to the \'H\' bar of a div stem.
--
divstem_beam_ydrop      :: AfmUnit
divstem_beam_ydrop      = 140




--------------------------------------------------------------------------------
-- Bar lines


-- | Horizontal distance the thin and thick lines of a repeat 
-- symbol.
--
repeat_line_hdist       :: AfmUnit
repeat_line_hdist       = 148

-- | Horizontal distance the first stem of a repeat and the dot
-- centers.
--
repeat_dot_hdist        :: AfmUnit
repeat_dot_hdist        = 228


-- | Radius of a repeat dot.
--
repeat_dot_radius       :: AfmUnit
repeat_dot_radius       = 126

-- | Baseline to center of the upper repeat dot.
--
hi_repeat_dot_ydist     :: AfmUnit
hi_repeat_dot_ydist     = 1554

-- | Baseline to center of the lower repeat dot.
--
lo_repeat_dot_ydist     :: AfmUnit
lo_repeat_dot_ydist     = 842


--------------------------------------------------------------------------------
-- Tuplets

-- | stem_top to plet base.
--
plet_ydist              :: AfmUnit
plet_ydist              = 32








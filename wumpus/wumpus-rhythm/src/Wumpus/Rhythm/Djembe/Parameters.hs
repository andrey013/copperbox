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
  
  -- * Cap height
    helvetica_cap_height    

  -- * Synthesized width vectors
  , disk_width_vector
  , period_width_vector

  -- * Notehead params
  , disk_radius
  , disk_ycenter
  , period_radius
  , period_ycenter
  , lostroke_disk_ycenter
  , histroke_char_baseline



  -- * Accents
  , accent_baseline
  , hand_baseline
  , hand_side_length
  , strike_baseline
  , angle_strike_width
  , paren_baseline
  , leadin_mark_width
  , leadin_mark_height

  -- * Stems
  , stem_top
  , stem_base
  , stem_length
  , flam_xminor
  , flam_ynorth

  , flam_char_baseline
  , flam_stem_length
  , flam_disk_radius
  , flam_disk_ycenter
  , divstem_beam_ypos
  , divstem_beam_ydrop
  , swing_symbol_baseline

  -- * Bar lines and repeats
  , barline_top
  , repeat_baseline
  , repeat_line_hspacing
  , repeat_dot_hspacing
  , repeat_dot_radius
  , hi_repeat_dot_center
  , lo_repeat_dot_center

  -- * Tuplet grouping
  , plet_bracket_baseline
  , plet_number_height
  , plet_number_width

  ) where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


-- | Note heads should use this for the y_major.
--
helvetica_cap_height    :: AfmUnit
helvetica_cap_height    = 718


-- | Note - the width is the same width as capital O in Helvetica.
-- Choosing this particular dimension is perhaps arbitrary.
--
disk_width_vector       :: AdvanceVec AfmUnit
disk_width_vector       = hvec 778 


-- | Note - the width is the same width as capital T in Helvetica.
-- Choosing this particular dimension is perhaps arbitrary.
--
period_width_vector     :: AdvanceVec AfmUnit
period_width_vector     = hvec 611

--------------------------------------------------------------------------------
-- Noteheads

-- | Radius of disk notehead - this is the /tone/ note for a 
-- Djembe.
--
disk_radius             :: AfmUnit
disk_radius             = 272

-- | Baseline to disk center, vertical measure.
--
disk_ycenter            :: AfmUnit 
disk_ycenter            = 306


-- | Radius of period notehead - this is the /rest/ note (??) for 
-- a Djembe.
--
period_radius           :: AfmUnit
period_radius           = 60


period_ycenter          :: AfmUnit
period_ycenter          = 108


lostroke_disk_ycenter   :: AfmUnit
lostroke_disk_ycenter   = 1000 -- ??


histroke_char_baseline  :: AfmUnit
histroke_char_baseline  = 1380


--------------------------------------------------------------------------------
-- Accents and annotations

-- | Baseline for accents drawn with the less-than \'\<\' char.
--
accent_baseline         :: AfmUnit
accent_baseline         = 2284

hand_baseline           :: AfmUnit
hand_baseline           = (-448)

hand_side_length        :: AfmUnit
hand_side_length        = 260


strike_baseline         :: AfmUnit
strike_baseline         = (-92)

angle_strike_width      :: AfmUnit
angle_strike_width      = 612


paren_baseline          :: AfmUnit
paren_baseline          = 72


leadin_mark_width       :: AfmUnit
leadin_mark_width       = 702

leadin_mark_height      :: AfmUnit
leadin_mark_height      = 408


--------------------------------------------------------------------------------
-- Stems

stem_top                :: AfmUnit
stem_top                = 2272

stem_base               :: AfmUnit
stem_base               = 818

stem_length             :: AfmUnit
stem_length             = stem_top - stem_base


flam_xminor             :: AfmUnit
flam_xminor             = 354

flam_ynorth             :: AfmUnit
flam_ynorth             = flam_char_baseline -- TODO

flam_char_baseline      :: AfmUnit
flam_char_baseline      = 904

flam_stem_length        :: AfmUnit
flam_stem_length        = 636

flam_disk_radius        :: AfmUnit
flam_disk_radius        = 134

flam_disk_ycenter       :: AfmUnit
flam_disk_ycenter       = 950


-- | Vertical position of the horizontal beam forming the \'H\' 
-- of a div stem.
--
divstem_beam_ypos       :: AfmUnit
divstem_beam_ypos       = 2200


-- | Drop from the beam line to the \'H\' bar of a div stem.
--
divstem_beam_ydrop      :: AfmUnit
divstem_beam_ydrop      = 140


-- | Baseline for the \'\>\' symbol of a swing stem.
--
swing_symbol_baseline    :: AfmUnit
swing_symbol_baseline    = 1600


--------------------------------------------------------------------------------
-- Bar lines

barline_top             :: AfmUnit
barline_top             = 2710

repeat_baseline         :: AfmUnit
repeat_baseline         = 78

-- | Horizontal spacing between the two line of a repeat symbol.
--
repeat_line_hspacing    :: AfmUnit
repeat_line_hspacing    = 148

repeat_dot_hspacing     :: AfmUnit
repeat_dot_hspacing     = 228

repeat_dot_radius       :: AfmUnit
repeat_dot_radius       = 126

hi_repeat_dot_center    :: AfmUnit
hi_repeat_dot_center    = 1554

lo_repeat_dot_center    :: AfmUnit
lo_repeat_dot_center    = 842


--------------------------------------------------------------------------------
-- Tuplets

plet_bracket_baseline   :: AfmUnit
plet_bracket_baseline   = 2356

plet_number_height      :: AfmUnit 
plet_number_height      = 718

plet_number_width       :: AfmUnit 
plet_number_width       = 556







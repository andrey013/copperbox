{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.GraphicPrimitives
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Djembe drawing primitives
--
--------------------------------------------------------------------------------

module Wumpus.Rhythm.Djembe.GraphicPrimitives 
  (
  
    NoteGlyph
  , disk_note
  , char_note
  , period_note
  , histroke_char
  , lostroke_disk

  , char_flam
  , disk_flam
  , other_hand
  , dom_hand
  , underscore
  , angle_strike
  , parens
  , stroke_accent
  , leadin_accent

  , single_stem
  , flam_stem
  , div_stem
  , swing_stem

  , barline
  , lrepeat
  , rrepeat
  , plet_bracket

  , beam_line

  ) where


import Wumpus.Rhythm.Djembe.Parameters

import Wumpus.Drawing.Text.PosChar              -- package: wumpus-drawing
import Wumpus.Drawing.Text.RotTextLR


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Base.Datatypes

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative
import Data.Ratio

-- Maybe this is better than the one currently in Wumpus-Basic?
--
scale_point_size :: Double -> DrawingContextF
scale_point_size sc = 
    (\s sz -> s { dc_font_size = floor $ sc * fromIntegral sz}) 
      <*> dc_font_size

-- For clarity Wumpus-Basic needs flipped versions of the 
-- @startPos@, @atRot@, etc. operators. However they need some
-- system for naming that I haven\'t yet worked out.
--
startPosR :: Floating u 
          => RectPosition -> PosImage t u -> LocImage t u
startPosR = flip startPos




--------------------------------------------------------------------------------



type NoteGlyph = AdvGraphic AfmUnit

noteGlyph :: NoteGlyph -> PointDisplace AfmUnit -> NoteGlyph
noteGlyph = flip moveStart

baselineCharGlyph :: (Floating u, InterpretUnit u) 
                  => EscapedChar -> LocImage Vec2 u
baselineCharGlyph ch = 
    descender &=> \dy -> 
    mapAns fn $ moveStart (move_down (abs dy)) $ startPosR SS $ posEscChar ch
  where
    fn bb = V2 (boundaryWidth bb) 0


-- PosRects for hands...

strokedPosRect :: (Fractional u, InterpretUnit u) => u -> u -> PosGraphic u 
strokedPosRect w h = 
    posImage (oposRectSW w h) (strokedRectangle w h)


filledPosRect :: (Fractional u, InterpretUnit u) => u -> u -> PosGraphic u 
filledPosRect w h = 
    posImage (oposRectSW w h) (filledRectangle w h)

oposRectSW :: Num u => u -> u -> ObjectPos u 
oposRectSW w h  = ObjectPos { op_x_minor = 0
                            , op_x_major = w
                            , op_y_minor = 0
                            , op_y_major = h }
 

openStrokePath :: InterpretUnit u => [Vec2 u] -> LocGraphic u
openStrokePath vs = promoteR1 $ \pt -> 
    uconvertFDC pt      &=> \pt1 -> 
    mapM uconvertFDC vs &=> \vs1 ->
    openStroke $ vectorPrimPath pt1 vs1

filledRelativePath :: InterpretUnit u => [Vec2 u] -> LocGraphic u
filledRelativePath vs = promoteR1 $ \pt -> 
    uconvertFDC pt      &=> \pt1 -> 
    mapM uconvertFDC vs &=> \vs1 ->
    filledPath $ vectorPrimPath pt1 vs1



--------------------------------------------------------------------------------

disk_note :: NoteGlyph
disk_note = noteGlyph note_draw start_move
  where
    note_draw  = replaceAns disk_width_vector $ filledDisk disk_radius
    start_move = move_up disk_ycenter
    
char_note :: Char -> NoteGlyph
char_note ch = baselineCharGlyph $ CharLiteral ch

period_note :: NoteGlyph
period_note =  noteGlyph note_draw start_move
  where
    note_draw  = replaceAns period_width_vector $ filledDisk period_radius
    start_move = displaceV period_ycenter

-- | Draw a Char at the histroke position usually @X@.
--
histroke_char :: Char -> NoteGlyph
histroke_char ch = noteGlyph note_draw start_move
  where
    note_draw  = baselineCharGlyph $ CharLiteral ch
    start_move = displaceV histroke_char_baseline 
   


-- | Draw a disk at the lostroke position.
--
lostroke_disk :: NoteGlyph
lostroke_disk = noteGlyph note_draw start_move
  where
    note_draw  = replaceAns disk_width_vector $ filledDisk disk_radius
    start_move = displaceV lostroke_disk_ycenter



char_flam :: Char -> LocGraphic AfmUnit
char_flam ch = 
    local_ctx (scale_point_size 0.75) $ moveStart start_move note_draw
  where
    note_draw  = ignoreAns $ baselineCharGlyph $ CharLiteral ch
    start_move = move_left flam_xminor . move_up flam_char_baseline

disk_flam :: LocGraphic AfmUnit
disk_flam = moveStart start_move (filledDisk flam_disk_radius)
  where
    start_move = move_left flam_xminor . move_up flam_disk_ycenter


decohand :: (AfmUnit -> AfmUnit -> PosGraphic AfmUnit) -> NoteGlyph -> NoteGlyph
decohand fn = (`decorate` body) 
  where
    body = moveStart (displaceV hand_baseline) rect
    rect = startPosR SS $ fn hand_side_length hand_side_length

other_hand :: NoteGlyph -> NoteGlyph
other_hand = decohand strokedPosRect

dom_hand :: NoteGlyph -> NoteGlyph
dom_hand = decohand filledPosRect


underscore :: NoteGlyph -> NoteGlyph
underscore = (`annotate` (body . vector_x))
  where
    body w = moveStart (move_left (0.5*w) . displaceV strike_baseline) 
               $ locStraightLine (hvec w) 


angle_strike :: NoteGlyph -> NoteGlyph
angle_strike mf = mf `annotate` (\a -> ignoreAns $ astrike $ vector_x a)
  where
    astrike w = let ang  = 0.25*pi
                    dist = (1.25*w) / fromRadian (cos ang)
                in moveStart (move_left (0.625*w) . displaceV strike_baseline)
                             (locStraightLine (avec ang dist))



parens :: NoteGlyph -> NoteGlyph
parens nh = nh `annotate` (lparen . vector_x) `annotate` (rparen . vector_x)
  where
    lparen w = ignoreAns $ 
               moveStart (move_left (0.66*w)  . displaceV paren_baseline)
                         (baselineCharGlyph $ CharEscName "parenleft")

    rparen w = ignoreAns $ 
               moveStart (move_right (0.66*w) . displaceV paren_baseline)
                         (baselineCharGlyph $ CharEscName "parenright")


-- | A \'<\' sign about the stem.
--
stroke_accent :: NoteGlyph -> NoteGlyph
stroke_accent = (`decorate` moveStart start_move accent)
  where
    accent     = ignoreAns $ baselineCharGlyph $ CharEscName "greater"
    start_move = move_up accent_baseline
    


-- | A \'V\' with a stalk.
--
leadin_accent :: LocGraphic AfmUnit
leadin_accent = 
    moveStart (move_up stem_top) (vertical_stalk `oplus` triangle_tip)
  where
    vertical_stalk = openStrokePath [vvec leadin_mark_height]
    triangle_tip   = filledRelativePath vpath
    vh             = 0.75 * leadin_mark_height
    vhmid          = 0.4  * vh
    vhw            = 0.5  * leadin_mark_width
    vpath          = [ vec vhw vh
                     , vec (-vhw) (negate vhmid)
                     , vec (-vhw) vhmid          ]


-- Stems 

single_stem :: LocGraphic AfmUnit
single_stem = 
    moveStart (move_up stem_base)  (locStraightLine $ vvec stem_length)


flam_stem :: LocGraphic AfmUnit
flam_stem = moveStart (move_up stem_base) (openStrokePath flam_path)
  where
    flam_path  = [ vvec stem_length
                 , vec  (negate flam_xminor) (negate flam_xminor)
                 , vvec (negate flam_stem_length)
                 ]


-- | divstem needs the unit width which is a global property.
--
-- divstem is a \'H\' with the horizontal bar near the top.
--
div_stem :: AfmUnit -> LocGraphic AfmUnit
div_stem uw = stem 0 `oplus` stem hw `oplus` hbar
  where
    hw   = 0.5 * uw
    stem = \dx -> moveStart (move_up stem_base . move_right dx) 
                            (locStraightLine $  vvec stem_length)

    hbar = moveStart (move_up divstem_beam_ypos) (locStraightLine $ hvec hw)


swing_stem :: LocGraphic AfmUnit
swing_stem = stem 0 `oplus` stem flam_xminor `oplus` angle
  where
    stem     = \dx -> moveStart (move_up stem_base . move_right dx) 
                                (locStraightLine $  vvec stem_length)
    
    angle    = moveStart (move_up swing_symbol_baseline) 
                         (openStrokePath ang_path)

    ang_path = let w = 0.9 * flam_xminor in [ vec w w, vec (-w) w ]




--------------------------------------------------------------------------------
-- bar lines


barline :: LocGraphic AfmUnit
barline = openStrokePath [vvec barline_top]


-- Note - Repeats are not on the char baseline, instead they are 
-- on a baseline with the period note head.
--

lrepeat :: LocGraphic AfmUnit
lrepeat = moveStart (move_up repeat_baseline) $ body
  where
    body = repeatSglStem `oplus` repeatDblStem (-repeat_line_hspacing)
                         `oplus` repeatDots      repeat_dot_hspacing



rrepeat :: LocGraphic AfmUnit
rrepeat = moveStart (move_up repeat_baseline) $ body
  where
    body = repeatSglStem `oplus` repeatDblStem   repeat_line_hspacing
                         `oplus` repeatDots    (-repeat_dot_hspacing)



repeatSglStem :: LocGraphic AfmUnit
repeatSglStem = openStrokePath [vvec stem_top]


repeatDblStem :: AfmUnit -> LocGraphic AfmUnit
repeatDblStem dx = 
   local_ctx line_thick $ 
       moveStart (displaceH dx) $ openStrokePath [vvec stem_top]


repeatDots :: AfmUnit -> LocGraphic AfmUnit
repeatDots dx = moveStart (displaceH dx) $ hi_dot `oplus` lo_dot
  where
    hi_dot  = moveStart (move_up hi_repeat_dot_center) dot1
    lo_dot  = moveStart (move_up lo_repeat_dot_center) dot1 
    dot1    = filledDisk repeat_dot_radius 


--------------------------------------------------------------------------------
-- plets


plet_bracket :: Int -> Ratio Int -> AfmUnit -> LocGraphic AfmUnit
plet_bracket n wr unit_width = 
    moveStart (move_up plet_bracket_baseline) body 
  where
    hh           = 0.5  * plet_number_height
    th           = 0.4  * plet_number_width
    hw           = 0.5  * unit_width * (realToFrac wr) 
    textw        = 0.66 * numberWidth n
    bracketw     = hw - textw
    body         = lbracket `oplus` num_text `oplus` rbracket

    lbracket     = openStrokePath [ vvec hh, hvec bracketw ]

    rbracket     = moveStart (displaceH (2*hw)) $ 
                      openStrokePath [ vvec hh, hvec (-bracketw) ]

    num_text     = moveStart (displaceH hw . displaceV th) $ 
                      centeredTwoThirdsText (show n) 


numberWidth :: Int -> AfmUnit
numberWidth i | i < 10    = plet_number_width
              | otherwise = plet_number_width + numberWidth (i `div` 10)

-- ERROR - currently this uses singleLineCC, but the examples
-- aren\'t loading font metrics...

centeredTwoThirdsText :: String -> LocGraphic AfmUnit
centeredTwoThirdsText ss =
    local_ctx (scale_point_size (2/3)) $ 
      ignoreAns $ startPosR CENTER $ textbox ss 




--------------------------------------------------------------------------------
-- beaming

beam_line :: Int -> AfmUnit -> LocGraphic AfmUnit
beam_line n unit_width = 
    moveStart (move_up stem_top) $ openStrokePath [ hvec beamlen ]
  where
    beamlen = unit_width * fromIntegral n


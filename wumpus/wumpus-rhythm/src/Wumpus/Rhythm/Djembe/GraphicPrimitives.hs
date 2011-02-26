{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.GraphicPrimitives
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Djembe drawing primitives
--
--------------------------------------------------------------------------------

module Wumpus.Rhythm.Djembe.GraphicPrimitives where


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
          => RectPosition -> PosImage u a -> LocImage u a
startPosR = flip apply1R2

-- For Wumpus-Basic
func_conv :: (Functor t, PtSize u1, PtSize u) => ConvertAlg (t u1) u1 (t u) u
func_conv = 
    ConvertAlg (fromPsPoint . toPsPoint) (fromPsPoint . toPsPoint)
                                         (fmap (fromPsPoint . toPsPoint)) 
               

-- Possibly for Displacement @move_left@ seems a more attrctive 
-- name than @southwards@.
--
move_left :: Num u => u -> PointDisplace u
move_left = displaceH . negate

move_right :: Num u => u -> PointDisplace u
move_right = displaceH

move_up :: Num u => u -> PointDisplace u
move_up = displaceV

move_down :: Num u => u -> PointDisplace u
move_down = displaceV . negate



--------------------------------------------------------------------------------



type NoteGlyph = AdvGraphic AfmUnit

noteGlyph :: NoteGlyph -> PointDisplace AfmUnit -> NoteGlyph
noteGlyph = flip moveStart

baselineCharGlyph :: (Floating u, PtSize u) 
                  => EscapedChar -> LocImage u (AdvanceVec u)
baselineCharGlyph ch = 
    glyphDescender >>= \dy -> 
    mapAns fn $ moveStart (displaceV dy) $ startPosR SS $ posEscChar ch
  where
    fn bb = V2 (boundaryWidth bb) 0


-- PosRects for hands...

strokedPosRect :: Fractional u => u -> u -> PosGraphic u 
strokedPosRect w h = 
    makePosImage (oposRectSW w h) (strokedRectangle w h)


filledPosRect :: Fractional u => u -> u -> PosGraphic u 
filledPosRect w h = 
    makePosImage (oposRectSW w h) (filledRectangle w h)

oposRectSW :: Num u => u -> u -> ObjectPos u 
oposRectSW w h  = ObjectPos { op_x_minor = 0
                            , op_x_major = w
                            , op_y_minor = 0
                            , op_y_major = h }
 

openStrokePath :: Num u => [Vec2 u] -> LocGraphic u
openStrokePath vs = promoteR1 $ \pt -> openStroke $ vectorPath pt vs

filledRelativePath :: Num u => [Vec2 u] -> LocGraphic u
filledRelativePath vs = promoteR1 $ \pt -> filledPath $ vectorPath pt vs



--------------------------------------------------------------------------------

disk_note :: NoteGlyph
disk_note = noteGlyph note_draw start_move
  where
    note_draw  = replaceAns disk_width_vector $ filledDisk disk_radius
    start_move = displaceV disk_ycenter
    
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



letter_flam :: Char -> LocGraphic AfmUnit
letter_flam ch = 
    localize (scale_point_size 0.75) $ moveStart start_move note_draw
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
               $ straightLine (hvec w) 



parens :: NoteGlyph -> NoteGlyph
parens nh = nh `annotate` (lparen . vector_x) `annotate` (rparen . vector_x)
  where
    lparen w = moveStart (move_left (0.66*w)  . displaceV paren_baseline)
                         (baselineCharGlyph $ CharEscName "parenleft")

    rparen w = moveStart (move_right (0.66*w) . displaceV paren_baseline)
                         (baselineCharGlyph $ CharEscName "parenright")



anglestrike :: NoteGlyph -> NoteGlyph
anglestrike = (`annotate` (astrike . vector_x))
  where
    astrike w = let ang  = 0.25*pi
                    dist = (1.25*w) / fromRadian (cos ang)
                in moveStart (move_left (0.625*w) . displaceV strike_baseline)
                             (straightLine (avec ang dist))

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

singlestem :: LocGraphic AfmUnit
singlestem = moveStart (move_up stem_base) (straightLine $ vvec stem_length)


flamstem :: LocGraphic AfmUnit
flamstem = moveStart (move_up stem_base) (openStrokePath flam_path)
  where
    flam_path  = [ vvec stem_length
                 , vec  (negate flam_xminor) (negate flam_xminor)
                 , vvec (negate flam_stem_length)
                 ]


-- | divstem needs the unit width which is a global property.
--
-- divstem is a \'H\'with the horizontal bar near the top.
--
divstem :: AfmUnit -> LocGraphic AfmUnit
divstem uw = stem 0 `oplus` stem hw `oplus` hbar
  where
    hw   = 0.5 * uw
    stem = \dx -> moveStart (move_up stem_base . move_right dx) 
                            (straightLine $  vvec stem_length)

    hbar = moveStart (move_up divstem_beam_ypos) (straightLine $ hvec hw)


swingstem :: LocGraphic AfmUnit
swingstem = stem 0 `oplus` stem flam_xminor `oplus` angle
  where
    stem     = \dx -> moveStart (move_up stem_base . move_right dx) 
                                (straightLine $  vvec stem_length)
    
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
   localize line_thick $ 
       moveStart (displaceH dx) $ openStrokePath [vvec stem_top]


repeatDots :: AfmUnit -> LocGraphic AfmUnit
repeatDots dx = moveStart (displaceH dx) $ hi_dot `oplus` lo_dot
  where
    hi_dot  = moveStart (move_up hi_repeat_dot_center) dot1
    lo_dot  = moveStart (move_up lo_repeat_dot_center) dot1 
    dot1    = filledDisk repeat_dot_radius 


--------------------------------------------------------------------------------
-- plets


pletBracket :: Int -> Ratio Int -> AfmUnit -> LocGraphic AfmUnit
pletBracket n wr unit_width = 
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
    localize (scale_point_size (2/3)) $ 
      ignoreAns $ startPosR CENTER $ textbox ss 



--------------------------------------------------------------------------------


-- | Non-functorial scaling / conversion function.
--
scaleFromAfm :: PtSize u 
             => LocImage AfmUnit a -> LocImage u a
scaleFromAfm obj = getFontSize >>= \sz -> 
                   let fsz  = (* (fromIntegral sz))
                       conv = convertli $ adaptConv fsz id unit_conv
                   in conv obj


-- | Functorial scaling / conversion function.
--
scaleFromAfm_f :: (Functor t, PtSize u) 
               => LocImage AfmUnit (t AfmUnit) -> LocImage u (t u)
scaleFromAfm_f obj = getFontSize >>= \sz -> 
                     let fsz  = (* (fromIntegral sz))
                         conv = convertli $ adaptConv fsz (fmap fsz) func_conv
                     in conv obj








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

    NoteHead
  , runNoteHead
  , runStem

  , charNote
  , diskNote  
  , periodNote
  , noNote

  , parens2
  , underscore2
  , angleStrike

  , beamBracket
  , singleStem
  , singleStemX
  , flamStem
  , flamStemX
  , divStem
  , divStemLX
  , divStemRX
  , swingStem
  , swingStemLX
  , swingStemRX


  ) where


import Wumpus.Rhythm.Djembe.Parameters

import Wumpus.Drawing.Paths.Relative
import Wumpus.Drawing.Text.DirectionZero        -- package: wumpus-drawing


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Data.Ratio
import Prelude hiding ( lines )



type NoteHead = PosObject AfmUnit
type Stem     = LocGraphic AfmUnit





runNoteHead :: InterpretUnit u => NoteHead -> LocGraphic u
runNoteHead nh = uconvLocImageF $ locGraphic_ $ runPosObjectR1 BLC nh

runStem :: InterpretUnit u => Stem -> LocGraphic u
runStem = uconvLocImageF . moveStart (dispV stem_top)



-- Note - we expect to draw histroke glyph at the horizontal center 

data NoteHeadDesc = NoteHeadDesc
      { nhd_note_head           :: PosObject AfmUnit
      , nhd_histroke_glyph      :: LocGraphic AfmUnit
      , nhd_histroke_ypos       :: AfmUnit
      , nhd_flam_glyph          :: LocGraphic AfmUnit
      }



-- | The height of a NoteHead is always the same - all we really
-- care about is adding parens to the left an right and drawing 
-- at @baseline center@.
--
-- Except for on of the character note heads, we always treat
-- width as the same too - the diameter of @disk@.
-- 
stdNoteHead :: AfmUnit -> LocGraphic AfmUnit -> NoteHead
stdNoteHead blc_to_c gf = 
    makePosObject (pure disk_orientation) (moveStart (dispV blc_to_c) gf)
  where
    disk_orientation :: Orientation AfmUnit
    disk_orientation = Orientation { or_x_minor = disk_radius
                                   , or_x_major = disk_radius
                                   , or_y_minor = 0
                                   , or_y_major = helvetica_cap_height 
                                   }


charNote :: Char -> NoteHead
charNote = nhd_note_head . charDesc 



charDesc :: Char -> NoteHeadDesc
charDesc ch = NoteHeadDesc 
    { nhd_note_head         = posChar ch
    , nhd_histroke_glyph    = glyph
    , nhd_histroke_ypos     = 0 -- TODO
    , nhd_flam_glyph        = flam_glyph
    }
  where
    glyph       = locGraphic_ $ runPosObjectR1 BLC $ posChar ch
    flam_glyph  = localize (scale_point_size 0.75) glyph



toneDesc :: NoteHeadDesc
toneDesc = NoteHeadDesc 
    { nhd_note_head         = stdNoteHead disk_ycenter glyph
    , nhd_histroke_glyph    = glyph
    , nhd_histroke_ypos     = 0 -- TODO
    , nhd_flam_glyph        = flam_glyph
    }
  where
    glyph       = dcDisk FILL disk_radius
    flam_glyph  = dcDisk FILL flam_disk_radius



diskNote :: NoteHead
diskNote = nhd_note_head toneDesc


periodDesc :: NoteHeadDesc
periodDesc = NoteHeadDesc 
    { nhd_note_head         = stdNoteHead period_ycenter glyph
    , nhd_histroke_glyph    = glyph
    , nhd_histroke_ypos     = 0 -- TODO
    , nhd_flam_glyph        = glyph
    }
  where
    glyph       = dcDisk FILL period_radius


periodNote :: NoteHead 
periodNote = nhd_note_head periodDesc 

 
-- | Note - we still want to build the note head with 
-- 'stdNoteHead' this is so we get a satifying blank inbetween
-- parens.
-- 
noNoteDesc :: NoteHeadDesc
noNoteDesc = NoteHeadDesc 
    { nhd_note_head         = stdNoteHead 0 emptyLocGraphic
    , nhd_histroke_glyph    = emptyLocGraphic
    , nhd_histroke_ypos     = 0
    , nhd_flam_glyph        = emptyLocGraphic
    }

noNote :: NoteHead 
noNote = nhd_note_head noNoteDesc 



-- | Note - parens would look better shifted up a little.
--
-- Also this is probably an elaborate...
--
parens2 :: NoteHead -> NoteHead
parens2 obj = hcat [ lparen, obj, rparen ]
  where
    lparen = posEscChar $ CharEscName "parenleft"
    rparen = posEscChar $ CharEscName "parenright"
    
    


underscore2 :: NoteHead -> NoteHead
underscore2 = elaboratePO body
  where
    body ortt  = let xmin = or_x_minor ortt
                     xmaj = or_x_major ortt
                 in  moveStart (dispV strike_baseline) 
                               (pivotLine xmin xmaj 0)




angleStrike :: NoteHead -> NoteHead
angleStrike = elaboratePO body
  where
    body _ = let ang  = 0.25*pi
                 dist = angle_strike_width / fromRadian (cos ang)
                 hw   = 0.5 * angle_strike_width
             in execPathSpec $ moveBy (vsum [ vvec strike_baseline, go_left hw])
                             >> aline ang dist



pivotLine :: (Floating u, InterpretUnit u) => u -> u -> Radian -> LocGraphic u
pivotLine lu ru ang = promoteR1 $ \pt -> 
    straightLine (pt .+^ avec (ang+pi) lu) (pt .+^ avec ang ru)

--
-- Stems are drawn from top 
-- 
-- Using paths doesn\'t seem to work because we aren\'t working 
-- from a useful start point. Flams go both ways from the start.
--

-- | Just the extremity stems are drawn.
--
beamBracket :: AfmUnit -> Int -> LocGraphic AfmUnit
beamBracket _  n | n < 1 = emptyLocGraphic
beamBracket uw n         = 
    let w = uw * fromIntegral n 
    in execPathSpec $  moveBy (go_up stem_base) 
                    >> lines [ go_up    stem_length 
                             , go_right w 
                             , go_down  stem_length ]


singleStem :: LocGraphic AfmUnit
singleStem = execPathSpec (line $ go_down stem_length)


-- | Extremity stem - draw if the stem is first or last in a beam
-- group. 
-- 
singleStemX :: LocGraphic AfmUnit
singleStemX = emptyLocGraphic


-- TODO - Flam needs to be contextual on Unit width / font size

flamStem :: LocGraphic AfmUnit
flamStem = execPivot flaml flamr
  where
    flaml = lines [ go_up flam_stem_length, go_up_right flam_xminor ]
    flamr = lines [ go_down stem_length ]


flamStemX :: LocGraphic AfmUnit
flamStemX = execPivot flam_path (return ())
  where
    flam_path =  lines [go_up flam_stem_length, go_up_right flam_xminor ]
              
    -- Note Pivot prbably needs a re-think...



-- | divstem needs the unit width which is a global property.
--
-- divstem is a \'H\' with the horizontal bar near the top.
--
divStem :: AfmUnit -> LocGraphic AfmUnit
divStem uw = singleStem `oplus` divStemLX uw


divStemRX :: AfmUnit -> LocGraphic AfmUnit
divStemRX uw = divStemLX uw `oplus` hbar
  where
    hw   = 0.5 * uw
    hbar = locStraightLine $ hvec hw


divStemLX :: AfmUnit -> LocGraphic AfmUnit
divStemLX uw = moveStart (dispH hw) singleStem `oplus` hbar
  where
    hw   = 0.5 * uw
    hbar = execPathSpec $ moveBy (go_down divstem_beam_ydrop) >> line (go_right hw)




swingStem :: LocGraphic AfmUnit
swingStem = singleStem `oplus` swingStemLX

swingStemRX :: LocGraphic AfmUnit
swingStemRX = execPathSpec topbar_and_stem `oplus` swingAngle
  where
    topbar_and_stem =  lines [go_right flam_xminor, go_down stem_length]





swingStemLX :: LocGraphic AfmUnit
swingStemLX = moveStart (dispH flam_xminor) singleStem `oplus` swingAngle


swingAngle :: LocGraphic AfmUnit
swingAngle = execPathSpec ang_path
  where
    w        = 0.9 * flam_xminor 
    ang_path =  moveBy (go_down divstem_beam_ydrop)
             >> lines [go_down_right w, go_down_left w]


--------------------------------------------------------------------------------
-- OLD 

-- For clarity Wumpus-Basic needs flipped versions of the 
-- @startPos@, @atRot@, etc. operators. However they need some
-- system for naming that I haven\'t yet worked out.
--
startAddrR :: Floating u 
          => RectAddress -> BoundedLocRectGraphic u -> BoundedLocGraphic u
startAddrR = flip startAddr




--------------------------------------------------------------------------------


{-

type NoteGlyph = AdvGraphic AfmUnit

noteGlyph :: NoteGlyph -> PointDisplace AfmUnit -> NoteGlyph
noteGlyph = flip moveStart

baselineCharGlyph :: (Floating u, InterpretUnit u) 
                  => EscapedChar -> LocImage u (Vec2 u)
baselineCharGlyph ch = 
    descender >>= \dy -> 
    pushR1 (mapAns fn) $ moveStart (disp_down (abs dy)) 
                       $ startAddrR SS $ escCharLabel ch
  where
    fn bb = V2 (boundaryWidth bb) 0



-- PosRects for hands...

strokedPosRect :: (Fractional u, InterpretUnit u) 
               => u -> u -> PosObject u 
strokedPosRect w h = 
    makePosObject (pure $ oposRectSW w h) (dcRectangle STROKE w h)


filledPosRect :: (Fractional u, InterpretUnit u) 
              => u -> u -> PosObject u 
filledPosRect w h = 
    makePosObject (pure $ oposRectSW w h) (dcRectangle FILL w h)

oposRectSW :: Num u => u -> u -> Orientation u 
oposRectSW w h  = Orientation { or_x_minor = 0
                              , or_x_major = w
                              , or_y_minor = 0
                              , or_y_major = h }
 

openStrokePath :: InterpretUnit u => [Vec2 u] -> LocGraphic u
openStrokePath vs = promoteR1 $ \pt -> 
    (locPP vs `at` pt) >>= dcOpenPath 


filledRelativePath :: InterpretUnit u => [Vec2 u] -> LocGraphic u
filledRelativePath vs = promoteR1 $ \pt -> 
    (locPP vs `at` pt) >>= dcClosedPath FILL

--------------------------------------------------------------------------------

disk_note :: NoteGlyph
disk_note = noteGlyph note_draw start_move
  where
    note_draw  = pushR1 (replaceAns disk_width_vector) $ dcDisk FILL disk_radius
    start_move = dispVec $ go_up disk_ycenter
    
char_note :: Char -> NoteGlyph
char_note ch = baselineCharGlyph $ CharLiteral ch

period_note :: NoteGlyph
period_note =  noteGlyph note_draw start_move
  where
    note_draw  = pushR1 (replaceAns period_width_vector) $ dcDisk FILL period_radius
    start_move = dispV period_ycenter

-- | Draw a Char at the histroke position usually @X@.
--
histroke_char :: Char -> NoteGlyph
histroke_char ch = noteGlyph note_draw start_move
  where
    note_draw  = baselineCharGlyph $ CharLiteral ch
    start_move = dispV histroke_char_baseline 
   


-- | Draw a disk at the lostroke position.
--
lostroke_disk :: NoteGlyph
lostroke_disk = noteGlyph note_draw start_move
  where
    note_draw  = pushR1 (replaceAns disk_width_vector) $ dcDisk FILL disk_radius
    start_move = dispV lostroke_disk_ycenter



char_flam :: Char -> LocGraphic AfmUnit
char_flam ch = 
    localize (scale_point_size 0.75) $ moveStart start_move note_draw
  where
    note_draw  = locGraphic_ $ baselineCharGlyph $ CharLiteral ch
    start_move = dispVec $ go_left flam_xminor . dispVec $ go_up flam_char_baseline

disk_flam :: LocGraphic AfmUnit
disk_flam = moveStart start_move (dcDisk FILL flam_disk_radius)
  where
    start_move = dispVec $ go_left flam_xminor . dispVec $ go_up flam_disk_ycenter


decohand :: (AfmUnit -> AfmUnit -> PosObject AfmUnit) 
         -> NoteGlyph -> NoteGlyph
decohand fn = (`decorateR1` body) 
  where
    body = moveStart (dispV hand_baseline) rect
    rect = locGraphic_ $ runPosObjectR1 SS $ fn hand_side_length hand_side_length

other_hand :: NoteGlyph -> NoteGlyph
other_hand = decohand strokedPosRect

dom_hand :: NoteGlyph -> NoteGlyph
dom_hand = decohand filledPosRect


underscore :: NoteGlyph -> NoteGlyph
underscore = (`elaborateR1` (body . vector_x))
  where
    body w = moveStart (dispVec $ go_left (0.5*w) . dispV strike_baseline) 
               $ locStraightLine (hvec w) 


angle_strike :: NoteGlyph -> NoteGlyph
angle_strike mf = 
    mf `elaborateR1` (\a -> astrike $ vector_x a)
  where
    astrike w = let ang  = 0.25*pi
                    dist = (1.25*w) / fromRadian (cos ang)
                in moveStart (dispVec $ go_left (0.625*w) . dispV strike_baseline)
                             (locStraightLine (avec ang dist))



parens :: NoteGlyph -> NoteGlyph
parens nh = 
    nh `elaborateR1` (lparen . vector_x) `elaborateR1` (rparen . vector_x)
  where
    lparen w = locGraphic_ $ 
               moveStart (dispVec $ go_left (0.66*w)  . dispV paren_baseline)
                         (baselineCharGlyph $ CharEscName "parenleft")

    rparen w = locGraphic_ $ 
               moveStart (dispVec $ go_right (0.66*w) . dispV paren_baseline)
                         (baselineCharGlyph $ CharEscName "parenright")


-- | A \'<\' sign about the stem.
--
stroke_accent :: NoteGlyph -> NoteGlyph
stroke_accent = (`decorateR1` moveStart start_move accent)
  where
    accent     = locGraphic_ $ baselineCharGlyph $ CharEscName "greater"
    start_move = dispVec $ go_up accent_baseline
    


-- | A \'V\' with a stalk.
--
leadin_accent :: LocGraphic AfmUnit
leadin_accent = 
    moveStart (dispVec $ go_up stem_top) (vertical_stalk `oplus` triangle_tip)
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
    moveStart (dispVec $ go_up stem_base)  (locStraightLine $ vvec stem_length)


flam_stem :: LocGraphic AfmUnit
flam_stem = moveStart (dispVec $ go_up stem_base) (openStrokePath flam_path)
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
    stem = \dx -> moveStart (dispVec $ go_up stem_base . dispVec $ go_right dx) 
                            (locStraightLine $  vvec stem_length)

    hbar = moveStart (dispVec $ go_up divstem_beam_ypos) (locStraightLine $ hvec hw)


swing_stem :: LocGraphic AfmUnit
swing_stem = stem 0 `oplus` stem flam_xminor `oplus` angle
  where
    stem     = \dx -> moveStart (dispVec $ go_up stem_base . dispVec $ go_right dx) 
                                (locStraightLine $  vvec stem_length)
    
    angle    = moveStart (dispVec $ go_up swing_symbol_baseline) 
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
lrepeat = moveStart (dispVec $ go_up repeat_baseline) $ body
  where
    body = repeatSglStem `oplus` repeatDblStem (-repeat_line_hspacing)
                         `oplus` repeatDots      repeat_dot_hspacing



rrepeat :: LocGraphic AfmUnit
rrepeat = moveStart (dispVec $ go_up repeat_baseline) $ body
  where
    body = repeatSglStem `oplus` repeatDblStem   repeat_line_hspacing
                         `oplus` repeatDots    (-repeat_dot_hspacing)



repeatSglStem :: LocGraphic AfmUnit
repeatSglStem = openStrokePath [vvec stem_top]


repeatDblStem :: AfmUnit -> LocGraphic AfmUnit
repeatDblStem dx = 
   localize line_thick $ 
       moveStart (dispH dx) $ openStrokePath [vvec stem_top]


repeatDots :: AfmUnit -> LocGraphic AfmUnit
repeatDots dx = moveStart (dispH dx) $ hi_dot `oplus` lo_dot
  where
    hi_dot  = moveStart (dispVec $ go_up hi_repeat_dot_center) dot1
    lo_dot  = moveStart (dispVec $ go_up lo_repeat_dot_center) dot1 
    dot1    = dcDisk FILL repeat_dot_radius 


--------------------------------------------------------------------------------
-- plets


plet_bracket :: Int -> Ratio Int -> AfmUnit -> LocGraphic AfmUnit
plet_bracket n wr unit_width = 
    moveStart (dispVec $ go_up plet_bracket_baseline) body 
  where
    hh           = 0.5  * plet_number_height
    th           = 0.4  * plet_number_width
    hw           = 0.5  * unit_width * (realToFrac wr) 
    textw        = 0.66 * numberWidth n
    bracketw     = hw - textw
    body         = lbracket `oplus` num_text `oplus` rbracket

    lbracket     = openStrokePath [ vvec hh, hvec bracketw ]

    rbracket     = moveStart (dispH (2*hw)) $ 
                      openStrokePath [ vvec hh, hvec (-bracketw) ]

    num_text     = moveStart (dispH hw . dispV th) $ 
                      centeredTwoThirdsText (show n) 


numberWidth :: Int -> AfmUnit
numberWidth i | i < 10    = plet_number_width
              | otherwise = plet_number_width + numberWidth (i `div` 10)

-- ERROR - currently this uses singleLineCC, but the examples
-- aren\'t loading font metrics...

centeredTwoThirdsText :: String -> LocGraphic AfmUnit
centeredTwoThirdsText ss =
    localize (scale_point_size (2/3)) $ 
      locGraphic_ $ startAddrR CENTER $ textline ss 




--------------------------------------------------------------------------------
-- beaming

beam_line :: Int -> AfmUnit -> LocGraphic AfmUnit
beam_line n unit_width = 
    moveStart (dispVec $ go_up stem_top) $ openStrokePath [ hvec beamlen ]
  where
    beamlen = unit_width * fromIntegral n

-}
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


  -- * Note heads
    PosNoteHead
  , runPosNoteHead
  , runStem

  , charNote
  , charFlam

  , diskNote  
  , diskFlam

  , periodNote
  , noNote

  -- * Decorations
  , parens2
  , underscore2
  , angleStrike


  -- * Stems
  , StemPos(..)
  , Stem

  , plainStem
  , flamStem
  , divStem
  , swingStem

  , beamBracket


  ) where


import Wumpus.Rhythm.Djembe.Parameters

import Wumpus.Drawing.Paths.Relative
import Wumpus.Drawing.Text.DirectionZero        -- package: wumpus-drawing


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Monoid
import Prelude hiding ( lines )



type PosNoteHead = PosObject AfmUnit




runPosNoteHead :: InterpretUnit u 
               => AfmUnit -> PosNoteHead -> LocGraphic u
runPosNoteHead dx nh = 
    uconvLocImageF $ locGraphic_ $ moveStart (dispH dx) (runPosObjectR1 BLC nh)



runStem :: InterpretUnit u => StemPos -> AfmUnit -> Stem -> LocGraphic u
runStem pos uw mf = uconvLocImageF $ moveStart (dispV stem_top) $ mf pos uw



-- Note - we expect to draw histroke glyph at the horizontal center 


-- | Component parts specifying a note head / flam note head.
-- 
-- > note_head is always drawn at baseline_center.
--
-- (Note heads can be decorated with parens, so they need to be a 
-- PosObject).
--
-- > high and low notes are also draw with north as the start point.
--
-- > flam_note_head is always drawn with north as the start point.
-- 
data NoteHeadDesc = NoteHeadDesc
      { nhd_note_head               :: PosObject AfmUnit
      , nhd_high_low_glyph          :: LocGraphic AfmUnit
      , nhd_high_low_north_disp     :: AfmUnit
      , nhd_flam_glyph              :: LocGraphic AfmUnit
      , nhd_flam_north_disp         :: AfmUnit
      }





-- | The height of a PosNoteHead is always the same - all we really
-- care about is adding parens to the left an right and drawing 
-- at @baseline center@.
--
-- Except for on of the character note heads, we always treat
-- width as the same too - the diameter of @disk@.
-- 
stdPosNoteHead :: AfmUnit -> LocGraphic AfmUnit -> PosNoteHead
stdPosNoteHead blc_to_c gf = 
    makePosObject (pure disk_orientation) (moveStart (dispV blc_to_c) gf)
  where
    disk_orientation :: Orientation AfmUnit
    disk_orientation = Orientation { or_x_minor = disk_radius
                                   , or_x_major = disk_radius
                                   , or_y_minor = 0
                                   , or_y_major = helvetica_cap_height 
                                   }


makeFlamGraphic :: NoteHeadDesc -> LocGraphic AfmUnit
makeFlamGraphic desc = 
    moveStart (dispH $ negate $ nhd_flam_north_disp desc) (nhd_flam_glyph desc)


-- | Drawn at baseline center.
--
charNote :: Char -> PosNoteHead
charNote = nhd_note_head . charDesc 


-- | drawn at north.
--
charFlam :: Char -> LocGraphic AfmUnit
charFlam = makeFlamGraphic . charDesc

charDesc :: Char -> NoteHeadDesc
charDesc ch = NoteHeadDesc 
    { nhd_note_head             = posChar ch
    , nhd_high_low_glyph        = glyph
    , nhd_high_low_north_disp   = 0
    , nhd_flam_glyph            = flam_glyph
    , nhd_flam_north_disp       = 0
    }
  where
    -- Char glyphs can be drawn at north directly.
    --
    glyph       = locGraphic_ $ runPosObjectR1 NN $ posChar ch
    flam_glyph  = localize (scale_point_size 0.75) glyph



diskDesc :: NoteHeadDesc
diskDesc = NoteHeadDesc 
    { nhd_note_head             = stdPosNoteHead disk_ycenter glyph
    , nhd_high_low_glyph        = glyph
    , nhd_high_low_north_disp   = disk_radius
    , nhd_flam_glyph            = flam_glyph
    , nhd_flam_north_disp       = flam_disk_radius
    }
  where
    glyph       = dcDisk FILL disk_radius
    flam_glyph  = dcDisk FILL flam_disk_radius



diskNote :: PosNoteHead
diskNote = nhd_note_head diskDesc

-- | drawn at north.
--
diskFlam :: LocGraphic AfmUnit
diskFlam = makeFlamGraphic diskDesc


periodDesc :: NoteHeadDesc
periodDesc = NoteHeadDesc 
    { nhd_note_head             = stdPosNoteHead period_ycenter glyph
    , nhd_high_low_glyph        = glyph
    , nhd_high_low_north_disp   = disk_radius
    , nhd_flam_glyph            = glyph
    , nhd_flam_north_disp       = flam_disk_radius
    }
  where
    glyph       = dcDisk FILL period_radius


periodNote :: PosNoteHead 
periodNote = nhd_note_head periodDesc 

 
-- | Note - we still want to build the note head with 
-- 'stdNoteHead' this is so we get a satifying blank inbetween
-- parens.
-- 
noNoteDesc :: NoteHeadDesc
noNoteDesc = NoteHeadDesc 
    { nhd_note_head             = stdPosNoteHead 0 mempty
    , nhd_high_low_glyph        = mempty
    , nhd_high_low_north_disp   = 0
    , nhd_flam_glyph            = mempty
    , nhd_flam_north_disp       = 0
    }

noNote :: PosNoteHead 
noNote = nhd_note_head noNoteDesc 



-------------------------------------------------------------------------------
-- Decorations

-- | Note - parens would look better shifted up a little.
--
-- Also this is probably an elaborate...
--
parens2 :: PosNoteHead -> PosNoteHead
parens2 obj = hcat [ lparen, obj, rparen ]
  where
    lparen = posEscChar $ CharEscName "parenleft"
    rparen = posEscChar $ CharEscName "parenright"
    
    


underscore2 :: PosNoteHead -> PosNoteHead
underscore2 = elaboratePO body
  where
    body ortt  = let xmin = or_x_minor ortt
                     xmaj = or_x_major ortt
                 in  moveStart (dispV strike_baseline) 
                               (pivotLine xmin xmaj 0)




angleStrike :: PosNoteHead -> PosNoteHead
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



--------------------------------------------------------------------------------
-- Stems



-- | Stems have three drawings depending whether they are the 
-- first, last or inside elements of a beam group.
--
-- This is because the beam is always drawn first and it includes
-- single stems for the outermost elements. It draws the outer 
-- stems so the corner joints will be properly formed.
--
data StemPos = LEFT_EXT | STEM_INNER | RIGHT_EXT
  deriving (Enum,Eq,Ord,Show)


-- | Stem is a function from @unit width@ to graphic.
--
type Stem = StemPos -> (AfmUnit -> LocGraphic AfmUnit)

single_stem_down :: PathSpec AfmUnit ()
single_stem_down = line $ go_down stem_length


single_stem_up :: PathSpec AfmUnit ()
single_stem_up = line $ go_up stem_length

-- | Draw either a single stem (inner) or nothing for the 
-- extremities.
--
plainStem :: Stem
plainStem LEFT_EXT      = const mempty
plainStem STEM_INNER    = const $ execPathSpec single_stem_down
plainStem RIGHT_EXT     = const mempty



flam_stem_left :: PathSpec AfmUnit ()
flam_stem_left = lines [ go_up flam_stem_length, go_up_right flam_xminor ]


-- | Draw the left stem for both extremities. Draw the combined
-- stem for the inners.
--
flamStem :: Stem
flamStem LEFT_EXT      = const $ execPathSpec flam_stem_left
flamStem STEM_INNER    = const $ execPivot flam_stem_left single_stem_down
flamStem RIGHT_EXT     = const $ execPathSpec flam_stem_left


data RightExt = LINE | MOVE
  deriving (Enum,Eq,Ord,Show)


-- | Plot the right part of a div stem. The initial segment may be
-- either a line or a move.
--
-- >  ...
-- >    |
-- >  --|
-- >    |
-- >    |
--
div_stem_right :: RightExt -> AfmUnit -> PathSpec AfmUnit ()
div_stem_right ext uw = step1 ext >> insertl crossbar >> single_stem_down
  where
    hw          = 0.5 * uw
    step1 MOVE  = moveBy (go_right hw)
    step1 LINE  = line   (go_right hw) 

    crossbar    = moveStart (dispVec startvec) (horizontalLine (-hw))
    startvec    = go_down divstem_beam_ydrop


horizontalLine :: InterpretUnit u => u -> LocGraphic u
horizontalLine = locStraightLine . hvec

divStem :: Stem 
divStem LEFT_EXT   uw = execPathSpec $ div_stem_right MOVE uw
divStem STEM_INNER uw = execPivot single_stem_up (div_stem_right MOVE uw)
divStem RIGHT_EXT  uw = execPathSpec $ div_stem_right LINE uw




-- | Plot the right part of a swing stem. The initial segment may be
-- either a line or a move.
--
-- >  ...
-- >   \|
-- >   /|
-- >    |
-- >    |
--
swing_stem_right :: RightExt -> PathSpec AfmUnit ()
swing_stem_right ext  = step1 ext >> insertl swingAngle >> single_stem_down
  where
    step1 MOVE  = moveBy (go_right flam_xminor)
    step1 LINE  = line   (go_right flam_xminor) 


swingAngle :: LocGraphic AfmUnit
swingAngle = execPathSpec ang_path
  where
    w        = 0.8 * flam_xminor 
    w1       = 0.9 * flam_xminor
    ang_path =  moveBy (go_left w1 ^+^ go_down divstem_beam_ydrop)
             >> lines [go_down_right w, go_down_left w]




swingStem :: Stem
swingStem LEFT_EXT   = const $ execPathSpec $ swing_stem_right MOVE
swingStem STEM_INNER = const $ execPivot single_stem_up (swing_stem_right MOVE)
swingStem RIGHT_EXT  = const $ execPathSpec $ swing_stem_right LINE




-- 


-- | Start point is top left - the path is drawn as a pivot.
--
--
-- >  o--------.
-- >  |        |
-- >
-- 
beamBracket :: AfmUnit -> Int -> LocGraphic AfmUnit
beamBracket _  n | n < 1 = mempty
beamBracket uw n         = execPivot pathl pathr
  where
    w = uw * fromIntegral n 
    pathl = lines [ go_up    stem_length ]
    pathr = lines [ go_right w, go_down stem_length ]






--------------------------------------------------------------------------------


{-

-- OLD ...

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
-- OLD ...

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
   
-- OLD ...


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

-- OLD ...

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

-- OLD ...

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

-- OLD ...

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

-- OLD ...

beam_line :: Int -> AfmUnit -> LocGraphic AfmUnit
beam_line n unit_width = 
    moveStart (dispVec $ go_up stem_top) $ openStrokePath [ hvec beamlen ]
  where
    beamlen = unit_width * fromIntegral n

-}
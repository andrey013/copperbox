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

  , charDesc
  , charNoteHead
  , charFlam

  , diskDesc
  , diskNote  
  , diskFlam

  , periodNote
  , noNote

  , strikeNoteHead
  , underscoreNoteHead

  , highNoteHead
  , lowNoteHead
  , highBiNoteHead
  , lowBiNoteHead


  -- * Decorations
  , parenthesis


  -- * Stems
  , StemPos(..)
  , Stem

  , plainStem
  , flamStem
  , divStem
  , swingStem

  , beamBracket

  -- * bar lines
  , singleBarline
  , leftRepeat
  , rightRepeat

  -- * Accents 
  , Accent(..)
  , leadinAccent
  , strokeAccent
  , domHand
  , otherHand

  -- * plet bracket
  , pletBracket

  ) where


import Wumpus.Rhythm.Djembe.Parameters


import Wumpus.Drawing.Basis.DrawingPrimitives   -- package: wumpus-drawing
import Wumpus.Drawing.Paths.Relative
import Wumpus.Drawing.Text.DirectionZero        


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


-- import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Monoid
import Prelude hiding ( lines )



type PosNoteHead = PosObject AfmUnit




runPosNoteHead :: InterpretUnit u 
               => AfmUnit -> PosNoteHead -> LocGraphic u
runPosNoteHead dx nh = 
    uconvF $ ignoreAns $ moveStart (hvec dx) (runPosObject BLC nh)



runStem :: InterpretUnit u => StemPos -> AfmUnit -> Stem -> LocGraphic u
runStem pos uw mf = uconvF $ moveStart (go_up stem_top) $ mf pos uw



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
      , nhd_low_ydist               :: AfmUnit
      , nhd_high_ydist              :: AfmUnit
      , nhd_flam_glyph              :: LocGraphic AfmUnit
      , nhd_flam_ydist              :: AfmUnit
      , nhd_strike_ydist            :: AfmUnit
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
    makePosObject (pure disk_orientation) $ moveStart (vvec blc_to_c) gf
  where
    disk_orientation :: Orientation AfmUnit
    disk_orientation = Orientation { or_x_minor = disk_radius
                                   , or_x_major = disk_radius
                                   , or_y_minor = 0
                                   , or_y_major = helvetica_cap_height 
                                   }


makeFlamGraphic :: NoteHeadDesc -> LocGraphic AfmUnit
makeFlamGraphic desc = 
    moveStart (go_up $ nhd_flam_ydist desc) (nhd_flam_glyph desc)

{-
makeHighStroke :: NoteHeadDesc -> LocGraphic AfmUnit
makeHighStroke desc = 
    moveStart (dispH $ negate $ nhd_flam_north_disp desc) (nhd_flam_glyph desc)
-}


-- | Drawn at baseline center.
--
charNoteHead :: Char -> PosNoteHead
charNoteHead = nhd_note_head . charDesc 


-- | drawn at north.
--
charFlam :: Char -> LocGraphic AfmUnit
charFlam = makeFlamGraphic . charDesc

charDesc :: Char -> NoteHeadDesc
charDesc ch = NoteHeadDesc 
    { nhd_note_head             = posChar ch
    , nhd_high_low_glyph        = glyph
    , nhd_low_ydist             = 560
    , nhd_high_ydist            = 780
    , nhd_flam_glyph            = flam_glyph
    , nhd_flam_ydist            = 364
    , nhd_strike_ydist          = 0 -- 0.5 * helvetica_cap_height
    }
  where
    -- Char glyphs can be drawn at north directly.
    --
    glyph       = ignoreAns $ runPosObject NN (posChar ch)
    flam_glyph  = localize (scale_point_size 0.75) glyph



diskDesc :: NoteHeadDesc
diskDesc = NoteHeadDesc 
    { nhd_note_head             = stdPosNoteHead disk_ydist glyph
    , nhd_high_low_glyph        = glyph
    , nhd_low_ydist             = 1164
    , nhd_high_ydist            = 1100
    , nhd_flam_glyph            = flam_glyph
    , nhd_flam_ydist            = flam_disk_radius
    , nhd_strike_ydist          = disk_ydist
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
    { nhd_note_head             = stdPosNoteHead period_ydist glyph
    , nhd_high_low_glyph        = glyph
    , nhd_low_ydist             = 0
    , nhd_high_ydist            = 0
    , nhd_flam_glyph            = glyph
    , nhd_flam_ydist            = flam_disk_radius
    , nhd_strike_ydist          = period_ydist
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
    , nhd_low_ydist             = 0
    , nhd_high_ydist            = 0
    , nhd_flam_glyph            = mempty
    , nhd_flam_ydist            = 0
    , nhd_strike_ydist          = 0.5 * helvetica_cap_height
    }

noNote :: PosNoteHead 
noNote = nhd_note_head noNoteDesc 



-- Note - this produces an ugly graphic.
--
-- Strike does not seem to work well as a PosObject transformer
-- as PosObject says nothing about the center of balance of the 
-- object (strike looks best if it cuts the center).
--

strikeNoteHead :: NoteHeadDesc -> PosNoteHead
strikeNoteHead desc = 
    decoPosObject SUPERIOR body $ nhd_note_head desc
  where
    body ortt  = let xmin = 1.5 * (or_x_minor ortt)
                     xmaj = 1.5 * (or_x_major ortt)
                     ypos = nhd_strike_ydist desc
                 in  moveStart (go_up ypos) (pivotLine xmin xmaj (0.25*pi))



underscoreNoteHead :: NoteHeadDesc -> PosNoteHead
underscoreNoteHead desc = 
    decoPosObject SUPERIOR body $ nhd_note_head desc
  where
    body ortt  = let xmin = or_x_minor ortt
                     xmaj = or_x_major ortt
                 in  moveStart (go_down underscore_ydist) $ 
                               pivotLine xmin xmaj 0



lowNoteHead :: NoteHeadDesc -> PosNoteHead
lowNoteHead desc = lowBiNoteHead desc noNoteDesc


highNoteHead :: NoteHeadDesc -> PosNoteHead
highNoteHead desc = highBiNoteHead desc noNoteDesc

highBiNoteHead :: NoteHeadDesc -> NoteHeadDesc -> PosNoteHead
highBiNoteHead high desc = 
    elabPosObject SUPERIOR BLC gf $ nhd_note_head desc 
  where
    dy  = nhd_high_ydist high
    gf  = moveStart (go_up dy) $ nhd_high_low_glyph high



lowBiNoteHead :: NoteHeadDesc -> NoteHeadDesc -> PosNoteHead
lowBiNoteHead low desc = 
    elabPosObject SUPERIOR BLC gf $ nhd_note_head desc 
  where
    dy  = nhd_low_ydist low
    gf  = moveStart (go_up dy) $ nhd_high_low_glyph low


-------------------------------------------------------------------------------
-- Decorations

-- | Note - parens would look better shifted up a little.
--
-- Also this is probably an elaborate...
--
parenthesis :: PosNoteHead -> PosNoteHead
parenthesis obj = hcat [ lparen, obj, rparen ]
  where
    lparen = posEscChar $ CharEscName "parenleft"
    rparen = posEscChar $ CharEscName "parenright"
    
    



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
single_stem_down = lineto $ go_down stem_length


single_stem_up :: PathSpec AfmUnit ()
single_stem_up = lineto $ go_up stem_length

-- | Draw either a single stem (inner) or nothing for the 
-- extremities.
--
plainStem :: Stem
plainStem LEFT_EXT      = const mempty
plainStem STEM_INNER    = const $ execPathSpec single_stem_down
plainStem RIGHT_EXT     = const mempty



flam_stem_left :: PathSpec AfmUnit ()
flam_stem_left = lines [ go_up flam_vstem_length, go_up_right flam_xdist ]


-- | Draw the left stem for both extremities. Draw the combined
-- stem for the inners.
--
flamStem :: Stem
flamStem LEFT_EXT      = const $ execPivot flam_stem_left (return ())
flamStem STEM_INNER    = const $ execPivot flam_stem_left single_stem_down
flamStem RIGHT_EXT     = const $ execPivot flam_stem_left (return ())


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
    step1 LINE  = lineto (go_right hw) 

    crossbar    :: LocGraphic AfmUnit
    crossbar    = moveStart startvec $ hline (-hw)
    startvec    = go_down divstem_beam_ydrop


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
    step1 MOVE  = moveBy (go_right flam_xdist)
    step1 LINE  = lineto (go_right flam_xdist) 


swingAngle :: LocGraphic AfmUnit
swingAngle = execPathSpec ang_path
  where
    w        = 0.8 * flam_xdist
    w1       = 0.9 * flam_xdist
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

-- Start point for bar lines is the base line.
-- Top of the bar line is stem top.


singleBarline :: LocGraphic AfmUnit
singleBarline = vline stem_top

thickBarline :: LocGraphic AfmUnit
thickBarline = localize dbl_thick $ vline stem_top
  where
    dbl_thick = relative_line_width (2*)

repeatDots :: LocGraphic AfmUnit
repeatDots = hi_dot <> lo_dot
  where
    hi_dot  = moveStart (go_up hi_repeat_dot_ydist) dot1
    lo_dot  = moveStart (go_up lo_repeat_dot_ydist) dot1 
    dot1    = dcDisk FILL repeat_dot_radius 


-- | The thin line is draw at the original h-position.
--
leftRepeat :: LocGraphic AfmUnit
leftRepeat = singleBarline 
          <> moveStart (go_left  repeat_line_hdist) thickBarline
          <> moveStart (go_right repeat_dot_hdist) repeatDots

-- | The thin line is draw at the original h-position.
--
rightRepeat :: LocGraphic AfmUnit
rightRepeat = singleBarline 
           <> moveStart (go_left  repeat_dot_hdist) repeatDots
           <> moveStart (go_right repeat_line_hdist) thickBarline




--------------------------------------------------------------------------------
-- Accents and marks

-- Accents are either positioned relative to the stem top
-- or the baseline

data Accent = StemAccent (LocGraphic AfmUnit)
            | BaselineAccent (LocGraphic AfmUnit)

-- | A \'V\' with a stalk.
--
leadinAccent :: Accent
leadinAccent = StemAccent ( vertical_stalk <> triangle_tip)
  where
    vertical_stalk = vline leadin_mark_height

    triangle_tip :: LocGraphic AfmUnit
    triangle_tip   = drawClosedPath_ FILL $ evalPathSpec $ lines vpath

    vh             = 0.75 * leadin_mark_height
    vhmid          = 0.4  * vh
    vhw            = 0.5  * leadin_mark_width
    vpath          :: [Vec2 AfmUnit]
    vpath          = [ vec (-vhw)    vh
                     , vec   vhw   (-vhmid)
                     , vec   vhw     vhmid 
                     ]



-- | A \'>\' sign about the stem.
--
strokeAccent :: Accent
strokeAccent = StemAccent $ moveStart (go_up accent_stroke_ydist) greater
  where
    greater    = ignoreAns $ escCharLabel (CharEscName "greater") BLC


domHand :: Accent 
domHand = 
    BaselineAccent $ moveStart (go_down accent_hand_ydist) filled_square
  where
    filled_square = ctrRectangle FILL hand_side_length hand_side_length



otherHand :: Accent 
otherHand = 
    BaselineAccent $ moveStart (go_down accent_hand_ydist) filled_square
  where
    filled_square = ctrRectangle STROKE hand_side_length hand_side_length



--------------------------------------------------------------------------------

-- | Drawn at stem top
--
-- Internally the drawing starts from the horizontal-center, this 
-- is so the text can be drawn first.
--
pletBracket :: AfmUnit -> Int -> LocGraphic AfmUnit
pletBracket pw num = 
    moveStart (go_right hw ^+^ go_up plet_ydist) gf
  where
    hw        = 0.5 * pw
    gf        = ignoreAns $ localize (scale_point_size 0.75 . text_margin 0.2 0)
                          $ selaborate (textline (show num) BLC) elab
    elab bb   = let ee    = east bb 
                    ww    = west bb
                    bbw   = boundaryWidth bb
                    bbhh  = 0.5 * boundaryHeight bb
                    lx    = max 0 (0.5 * (pw - bbw))
                in promoteLoc $ \_ -> let l1 = lpath lx bbhh `at` ww
                                          l2 = rpath lx bbhh `at` ee
                                      in l1 <> l2

    lpath w h = execPathSpec $ lines [ go_left w, go_down h ]
    rpath w h = execPathSpec $ lines [ go_right w, go_down h ]

    -- pw wants to be upscaled to accommodate the 0.75 downscale...
    -- Also there is some case that this graphic should be a connector


-- It is prabably easier to draw this with explicit calculations 
-- rather than elaborating a PosObject. 
-- 
-- Scaling the point size is not really a good approaching when 
-- working with contextual units.
--
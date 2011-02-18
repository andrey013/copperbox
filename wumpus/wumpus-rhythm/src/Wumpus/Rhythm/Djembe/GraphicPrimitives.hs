{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.GraphicPrimitives
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Djembe
--
--------------------------------------------------------------------------------

module Wumpus.Rhythm.Djembe.GraphicPrimitives where


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


--------------------------------------------------------------------------------



udisk_radius            :: FromPtSize u => DrawingInfo u
udisk_radius            = scaleValue 272


-- | Note - the width is the same width as capital O in Helvetica.
-- Choosing this particular dimension is perhaps arbitrary.
--
udisk_width_vector      :: FromPtSize u => DrawingInfo (AdvanceVec u)
udisk_width_vector      = (\w -> V2 w 0) <$> scaleValue 778


udisk_ycenter           :: FromPtSize u => DrawingInfo u
udisk_ycenter           = scaleValue 306


uperiod_radius          :: FromPtSize u => DrawingInfo u
uperiod_radius          = scaleValue 60


-- | Note - the width is the same width as capital T in Helvetica.
-- Choosing this particular dimension is perhaps arbitrary.
--
uperiod_width_vector    :: FromPtSize u => DrawingInfo (AdvanceVec u)
uperiod_width_vector    = (\w -> V2 w 0) <$> scaleValue 611

uperiod_ycenter         :: FromPtSize u => DrawingInfo u
uperiod_ycenter         = scaleValue 108


uhand_baseline          :: FromPtSize u => DrawingInfo u
uhand_baseline          = scaleValue (-448)

uhand_side_length       :: FromPtSize u => DrawingInfo u
uhand_side_length       = scaleValue 260

-- | Parens do not look good printed on the notehead baseline
--
uparen_baseline         :: FromPtSize u => DrawingInfo u
uparen_baseline         = scaleValue 72

ustrike_baseline        :: FromPtSize u => DrawingInfo u
ustrike_baseline        = scaleValue (-92)

ustem_top               :: FromPtSize u => DrawingInfo u
ustem_top               = scaleValue 2272

ustem_start             :: FromPtSize u => DrawingInfo u
ustem_start             = scaleValue 818

ustem_length            :: FromPtSize u => DrawingInfo u
ustem_length            = (-) <$> ustem_top <*> ustem_start


------------------------------------------
unit_width              :: AfmUnit
unit_width              = 1380

line_height             :: AfmUnit
line_height             = 2545

barline_top             :: AfmUnit
barline_top             = 2710

cap_size                :: AfmUnit 
cap_size                = 718

number_width            :: AfmUnit 
number_width            = 556

dot_radius              :: AfmUnit
dot_radius              = 272


period_radius           :: AfmUnit
period_radius           = 60


flam_dot_radius         :: AfmUnit
flam_dot_radius         = 134

-- for bracketing...
dot_notehead_width      :: AfmUnit
dot_notehead_width      = 540


period_center           :: AfmUnit
period_center           = 108

dot_center              :: AfmUnit
dot_center              = 306

flam_dot_center         :: AfmUnit
flam_dot_center         = 950

stem_top                :: AfmUnit
stem_top                = 2272

stem_length             :: AfmUnit
stem_length             = stem_top - stem_start

stem_start              :: AfmUnit
stem_start              = 818

half_line_pos           :: AfmUnit
half_line_pos           = 2200

flam_xminor             :: AfmUnit
flam_xminor             = 354

flam_baseline           :: AfmUnit
flam_baseline           = 904

flam_stem_length        :: AfmUnit
flam_stem_length        = 636

swing_angle_lower_left  :: AfmUnit
swing_angle_lower_left  = 1600
     
paren_baseline          :: AfmUnit
paren_baseline          = 72

accent_baseline         :: AfmUnit
accent_baseline         = 2284

leadin_mark_width       :: AfmUnit
leadin_mark_width       = 702

leadin_mark_height      :: AfmUnit
leadin_mark_height      = 408

baseline_strike         :: AfmUnit
baseline_strike         = (-92)

hand_baseline           :: AfmUnit
hand_baseline           = (-448)

hand_base_length        :: AfmUnit
hand_base_length        = 260

char_upstroke           :: AfmUnit
char_upstroke           = 1380

char_downstroke         :: AfmUnit
char_downstroke         = 260 -- ???


dot_upstroke            :: AfmUnit
dot_upstroke            = 760 -- ???

dot_downstroke          :: AfmUnit
dot_downstroke          = 260 -- ???

plet_bracket_baseline   :: AfmUnit
plet_bracket_baseline   = 2356

half_beam_baseline      :: AfmUnit
half_beam_baseline      = 2072

repeat_h_spacing        :: AfmUnit
repeat_h_spacing        = 148

repeat_dot_spacing      :: AfmUnit
repeat_dot_spacing      = 228

repeat_dot_radius       :: AfmUnit
repeat_dot_radius       = 126

hi_repeat_dot_center    :: AfmUnit
hi_repeat_dot_center    = 1554

lo_repeat_dot_center    :: AfmUnit
lo_repeat_dot_center    = 842



--------------------------------------------------------------------------------
-- Note heads are positioned...

baselineCharGlyph :: (Floating u, FromPtSize u) 
                  => EscapedChar -> LocImage u (AdvanceVec u)
baselineCharGlyph ch = 
    glyphDescender >>= \dy -> 
    mapAns fn $ moveStart (displaceV dy) $ startPosR SS $ posEscChar ch
  where
    fn bb = V2 (boundaryWidth bb) 0


type Notehead u = LocImage u (AdvanceVec u)


vmove :: Num u => DrawingInfo u -> LocCF u a -> LocCF u a
vmove mf gf = lift0R1 mf >>= \dy -> moveStart (displaceV dy) gf




disk :: FromPtSize u => Notehead u
disk = 
    lift0R1 udisk_radius        >>= \rd -> 
    lift0R1 udisk_width_vector  >>= \ans -> 
    replaceAns ans $ vmove udisk_ycenter $ filledDisk rd


char :: (Floating u, FromPtSize u) => Char -> Notehead u
char = baselineCharGlyph . CharLiteral


period :: FromPtSize u => Notehead u
period = 
    lift0R1 uperiod_radius        >>= \rd -> 
    lift0R1 uperiod_width_vector  >>= \ans -> 
    replaceAns ans $ vmove uperiod_ycenter $ filledDisk rd



decoHand :: FromPtSize u => Notehead u -> LocGraphic u -> Notehead u
decoHand nh gf = decorate nh (vmove uhand_baseline gf)

otherhand :: (Floating u, FromPtSize u) => LocGraphic u
otherhand = 
    lift0R1 uhand_side_length     >>= \w -> 
    startPosR SS $ strokedPosRect w w

domhand :: (Floating u, FromPtSize u) => LocGraphic u
domhand = 
    lift0R1 uhand_side_length     >>= \w -> 
    startPosR SS $ filledPosRect w w


withWidth :: Num u 
          => (u -> u) -> (u -> LocImage u a) -> (AdvanceVec u -> LocImage u a) 
withWidth fn gf = \(V2 w _) -> moveStart (displaceH $ fn w) (gf w)

parens :: (Floating u, FromPtSize u) => Notehead u -> Notehead u
parens nh = nh `annotate` oparen `annotate` cparen
  where
    oparen  = withWidth (\w -> negate $ 0.66*w)  
                        (const $ vmove uparen_baseline $ parenl)

    cparen  = withWidth (\w -> 0.66*w) (const $ vmove uparen_baseline $ parenr)

    parenl  = baselineCharGlyph $ CharEscName "parenleft"
    parenr  = baselineCharGlyph $ CharEscName "parenright"




underscore :: (Fractional u, FromPtSize u) => Notehead u -> Notehead u
underscore = (`annotate` underline)
  where
    underline = withWidth (\w -> negate $0.5*w) 
                          (\w -> udown $ straightLine (hvec w))
    udown     = vmove ustrike_baseline


anglestrike :: (Floating u, FromPtSize u) => Notehead u -> Notehead u
anglestrike = (`annotate` astrike)
  where
    astrike = withWidth (\w -> negate $ 0.625*w) 
                        (\w -> let dist = (1.25*w) / fromRadian (cos ang)
                               in udown $ straightLine (avec ang dist))

    ang     = 0.25*pi
    udown   = vmove ustrike_baseline


singlestem :: (Fractional u, FromPtSize u) => Notehead u -> Notehead u
singlestem = (`decorate` sstem)
  where
    sstem   = lift0R1 ustem_length >>= \len -> 
              vmove ustem_start $ straightLine $ vvec len


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
 

--------------------------------------------------------------------------------
-- OLD...

-- These need generalizing so that ypos is parameteric.
-- Then they can handle upstrokes downstokes and normal positioned note-heads

dotNotehead :: (Fractional u, FromPtSize u) => LocGraphic u
dotNotehead = makeDotNotehead dot_center


-- not an Image as upstrokes cannot be parenthesized.
upstrokeDot :: (Fractional u, FromPtSize u) => LocGraphic u
upstrokeDot = fmap (replaceL uNil) $ makeDotNotehead dot_upstroke

downstrokeDot :: (Fractional u, FromPtSize u) => LocGraphic u
downstrokeDot = fmap (replaceL uNil) $ makeDotNotehead dot_downstroke



makeDotNotehead :: (Fractional u, FromPtSize u) 
                => AfmUnit -> LocGraphic u
makeDotNotehead ypos = 
    lift0R1 (scaleValue dot_radius) >>= \radius -> 
    scaleVMove ypos $ filledDisk radius


-- period notehead always drawn just above baseline
-- 
periodNotehead :: (Fractional u, FromPtSize u) => LocGraphic u
periodNotehead = 
    lift0R1 (scaleValue period_radius) >>= \radius -> 
    scaleVMove period_center $ filledDisk radius


letterNotehead :: (Real u, Floating u, FromPtSize u) 
               => EscapedChar -> Notehead u
letterNotehead = makeLetterNotehead 0


upstrokeLetter :: (Real u, Floating u, FromPtSize u) 
               => EscapedChar -> LocGraphic u
upstrokeLetter ch = 
    fmap (replaceL uNil) $ makeLetterNotehead char_upstroke ch


downstrokeLetter :: (Real u, Floating u, FromPtSize u) 
                 => EscapedChar -> LocGraphic u
downstrokeLetter ch = 
    fmap (replaceL uNil) $ makeLetterNotehead char_downstroke ch


makeLetterNotehead :: (Real u, Floating u, FromPtSize u) 
                   => AfmUnit -> EscapedChar -> Notehead u
makeLetterNotehead ypos ch = 
    scaleVMove ypos $ baselineCharGlyph ch


bboxAfmWidth :: Real u => BoundedLocGraphic u -> LocImage u AfmUnit
bboxAfmWidth = fmap (bimapL (realToFrac . boundaryWidth))


--
letterFlamGlyph :: (Real u, Floating u, FromPtSize u) 
                => EscapedChar -> LocGraphic u
letterFlamGlyph ch = 
    lift0R1 getFontSize >>= \sz -> 
    localize (scale_point_size 0.75) $ 
        let x = negate $ flam_xminor
            y = flam_baseline
        in ignoreAns $ scaleMove x y $ baselineCharGlyph ch

dotFlamGlyph :: (Fractional u, FromPtSize u) => LocGraphic u
dotFlamGlyph = 
    lift0R1 (scaleValue flam_dot_radius) >>= \radius -> 
    scaleMove (negate $ flam_xminor) flam_dot_center $ filledDisk radius

--------------------------------------------------------------------------------
-- stems

singleStem :: (Fractional u, FromPtSize u) => LocGraphic u
singleStem =
   lift0R1 (scaleValue stem_length) >>= \len -> 
   scaleVMove stem_start (straightLine $ vvec len)


flamStem :: (Fractional u, FromPtSize u) => LocGraphic u
flamStem = 
    lift0R1 (scaleVecPath flam_path) >>= \vs -> 
    scaleVMove stem_start (openStrokePath vs)
  where
    flam_path  = [ vvec stem_length
                 , vec  (negate flam_xminor) (negate flam_xminor)
                 , vvec (negate flam_stem_length)
                 ]


swingStem :: (Fractional u, FromPtSize u) => LocGraphic u
swingStem = singleStem `oplus` swingAngle
    

swingAngle :: (Fractional u, FromPtSize u) => LocGraphic u
swingAngle = 
    lift0R1 (scaleVecPath angle_path) >>= \vs -> 
    scaleVMove swing_angle_lower_left (openStrokePath vs)
  where
    angle_path = let w = 0.9*flam_xminor in [ vec w w, vec (-w) w ]



openStrokePath :: Num u => [Vec2 u] -> LocGraphic u
openStrokePath vs = promoteR1 $ \pt -> openStroke $ vectorPath pt vs

filledRelativePath :: Num u => [Vec2 u] -> LocGraphic u
filledRelativePath vs = promoteR1 $ \pt -> filledPath $ vectorPath pt vs



--------------------------------------------------------------------------------

djembeNote :: (Fractional u, FromPtSize u) 
           => LocImage u AfmUnit -> LocImage u AfmUnit
djembeNote note_head = superimposeLocImage note_head singleStem 


flamNote :: (Fractional u, FromPtSize u) 
         => LocImage u AfmUnit -> LocGraphic u -> LocImage u AfmUnit
flamNote note_head flam_head = 
    superimposeLocImage note_head (flam_head `oplus` flamStem) 

restNote :: (Fractional u, FromPtSize u) => LocImage u AfmUnit
restNote = fmap (replaceL dot_notehead_width) singleStem


--------------------------------------------------------------------------------
-- parens for optional, accents...


-- This pattern could be caputured as a combinator...
--

addParens :: (Fractional u, FromPtSize u)  
          => LocImage u AfmUnit -> LocImage u AfmUnit
addParens img = 
    img >>= \(w,a) -> drawParens w >>= \(_,b) -> return (w, a `oplus` b)


drawParens :: (Fractional u, FromPtSize u) => AfmUnit -> LocGraphic u
drawParens w = open_paren `oplus` close_paren
  where
    open_paren  = scaleMove (negate $ 1.0*w) paren_baseline (textline "&parenleft;")
    close_paren = scaleMove (0.4*w) paren_baseline (textline "&parenright;")


addAccent :: (Fractional u, FromPtSize u)  
          => AfmUnit -> LocImage u AfmUnit -> LocImage u AfmUnit
addAccent accent_width note_head = 
    superimposeLocImage note_head (drawAccent accent_width) 

drawAccent :: (Fractional u, FromPtSize u) 
            => AfmUnit -> LocGraphic u
drawAccent cw = scaleMove (negate $ 0.5*cw) accent_baseline (textline "&greater;")

addLeadin :: FromPtSize u => LocImage u AfmUnit -> LocImage u AfmUnit
addLeadin note_head = superimposeLocImage note_head drawLeadin 

drawLeadin :: FromPtSize u => LocGraphic u
drawLeadin = scaleVMove stem_top arr
  where
    arr = leadinArrowhead leadin_mark_width leadin_mark_height


leadinArrowhead :: FromPtSize u => AfmUnit -> AfmUnit -> LocGraphic u
leadinArrowhead w h = vertical_stalk `oplus` triangle_tip
  where
    vertical_stalk = lift0R1 (scaleVecPath [vvec h]) >>= openStrokePath
    triangle_tip   = lift0R1 (scaleVecPath vpath) >>= filledRelativePath
    vh             = 0.75 * h
    vhmid          = 0.4  * vh
    vhw            = 0.5  * w
    vpath          = [ vec vhw vh
                     , vec (-vhw) (negate vhmid)
                     , vec (-vhw) vhmid
                     ]

addDominantHand :: (Fractional u, FromPtSize u) 
                => LocImage u AfmUnit -> LocImage u AfmUnit
addDominantHand note_head = 
    superimposeLocImage note_head (drawHand filledRectangle)
 

addOtherHand :: (Fractional u, FromPtSize u) 
             => LocImage u AfmUnit -> LocImage u AfmUnit
addOtherHand note_head = 
    superimposeLocImage note_head (drawHand strokedRectangle)


drawHand :: FromPtSize u => (u -> u -> LocGraphic u) -> LocGraphic u
drawHand fn = scaleMove (negate $ 0.5*hand_base_length) hand_baseline loc_rect
  where
    loc_rect = lift0R1 (scaleValue hand_base_length) >>= \uw -> fn uw uw


addBell :: (Real u, Floating u, FromPtSize u)  
        => LocImage u AfmUnit -> LocImage u AfmUnit
addBell img = 
    img >>= \(w,a) -> upstrokeX >>= \(_,b) -> return (w, a `oplus` b)
  where
    upstrokeX = upstrokeLetter (CharLiteral 'X')

--------------------------------------------------------------------------------
-- strike throughs


addBaselineStrike :: (Fractional u, FromPtSize u)  
                    => LocImage u AfmUnit -> LocImage u AfmUnit
addBaselineStrike img = 
    img >>= \(w,a) -> drawHStrikeLine w >>= \(_,b) -> return (w, a `oplus` b)

drawHStrikeLine :: FromPtSize u => AfmUnit -> LocGraphic u
drawHStrikeLine cw = scaleMove (negate $ 0.5*cw) baseline_strike loc_strike
  where
    loc_strike = lift0R1 (scaleVecPath [hvec cw]) >>= openStrokePath


addAngledStrike :: (Fractional u, FromPtSize u)  
                    => LocImage u AfmUnit -> LocImage u AfmUnit
addAngledStrike img = 
    img >>= \(w,a) -> drawAngStrikeLine w >>= \(_,b) -> return (w, a `oplus` b)


drawAngStrikeLine :: FromPtSize u => AfmUnit -> LocGraphic u
drawAngStrikeLine cw = scaleMove (negate $ 0.5*cw) baseline_strike loc_strike
  where
    loc_strike = lift0R1 (scaleVecPath [vec cw cap_size]) >>= openStrokePath

--------------------------------------------------------------------------------
-- plets


pletBracket :: (Real u, Floating u, FromPtSize u) 
            => Int -> Ratio Int -> LocGraphic u
pletBracket n wr = 
    scaleVMove plet_bracket_baseline $ lbracket `oplus` num_text `oplus` rbracket
  where
    hh           = 0.5  * cap_size
    th           = 0.4  * cap_size
    hw           = 0.5  * unit_width * (realToFrac wr) 
    textw        = 0.66 * numberWidth n
    bracketw     = hw - textw
    lbracket     = lift0R1 (scaleVecPath [ vvec hh, hvec bracketw ]) 
                      >>= openStrokePath
    rbracket     = scaleHMove (2*hw) $ 
                      lift0R1 (scaleVecPath [ vvec hh, hvec (-bracketw) ]) 
                              >>= openStrokePath
    num_text     = scaleMove hw th $ centeredTwoThirdsText (show n) 


numberWidth :: Int -> AfmUnit
numberWidth i | i < 10    = number_width
              | otherwise = number_width + numberWidth (i `div` 10)

-- ERROR - currently this uses singleLineCC, but the examples
-- aren\'t loading font metrics...

centeredTwoThirdsText :: (Real u, Floating u, FromPtSize u) 
                      => String -> LocGraphic u
centeredTwoThirdsText ss =
    localize (scale_point_size (2/3)) $ 
      ignoreAns $ startPosR CENTER $ textbox ss 

--------------------------------------------------------------------------------
-- half beam

halfBeam :: FromPtSize u => LocGraphic u
halfBeam = scaleVMove half_beam_baseline loc_beam 
  where
    half_width  = 0.5 * unit_width
    loc_beam    = lift0R1 (scaleVecPath [hvec half_width]) >>= openStrokePath


--------------------------------------------------------------------------------
-- bar lines

barline :: FromPtSize u => LocGraphic u
barline = lift0R1 (scaleVecPath [vvec barline_top]) >>= openStrokePath

lrepeat :: FromPtSize u => LocGraphic u
lrepeat = scaleVMove period_center $ 
             repeatSglStem `oplus` repeatDblStem (-repeat_h_spacing)
                           `oplus` repeatDots      repeat_dot_spacing

    

rrepeat :: FromPtSize u => LocGraphic u
rrepeat = scaleVMove period_center $ 
             repeatSglStem `oplus` repeatDblStem repeat_h_spacing
                           `oplus` repeatDots  (-repeat_dot_spacing)


repeatSglStem :: FromPtSize u => LocGraphic u
repeatSglStem = lift0R1 (scaleVecPath [vvec stem_top]) >>= openStrokePath

repeatDblStem :: FromPtSize u => AfmUnit -> LocGraphic u
repeatDblStem dx = 
   localize line_thick $ scaleHMove dx $ 
                      lift0R1 (scaleVecPath [vvec stem_top]) >>= openStrokePath


repeatDots :: FromPtSize u => AfmUnit -> LocGraphic u
repeatDots dx = scaleHMove dx $ 
    lift0R1 (scaleValue repeat_dot_radius) >>= \r -> 
    mkDisk r hi_repeat_dot_center `oplus` mkDisk r lo_repeat_dot_center 
  where
    mkDisk r dy = scaleVMove dy $ filledDisk r


--------------------------------------------------------------------------------

scaleValue :: FromPtSize u => AfmUnit -> DrawingInfo u
scaleValue u1 = fmap (\sz -> afmValue u1 (fromIntegral sz)) getFontSize




superimposeLocImage :: LocImage u a -> LocGraphic u -> LocImage u a
superimposeLocImage = liftA2 (\(a,b) (_,c) -> (a,b `oplus` c))


superimposeAdvGraphic :: AdvGraphic u -> LocGraphic u -> AdvGraphic u
superimposeAdvGraphic = liftA2 (\(a,b) (_,c) -> (a,b `oplus` c))
 



scaleMove :: FromPtSize u 
          => AfmUnit -> AfmUnit -> LocImage u a -> LocImage u a
scaleMove x y cf = 
    lift0R1 (scaleValue x) >>= \xu -> 
    lift0R1 (scaleValue y) >>= \yu -> 
    moveStart (displace xu yu) cf

scaleHMove :: FromPtSize u => AfmUnit -> LocImage u a -> LocImage u a
scaleHMove x cf = 
    lift0R1 (scaleValue x) >>= \xu -> moveStart (displaceH xu) cf

scaleVMove :: FromPtSize u => AfmUnit -> LocImage u a -> LocImage u a
scaleVMove y cf = 
    lift0R1 (scaleValue y) >>= \yu -> moveStart (displaceV yu) cf


scaleVecPath :: FromPtSize u => [Vec2 AfmUnit] -> DrawingInfo [Vec2 u]
scaleVecPath = mapM scaleVec2 


scaleVec2 :: FromPtSize u => Vec2 AfmUnit -> DrawingInfo (Vec2 u)
scaleVec2 (V2 x y) = V2 <$> scaleValue x <*> scaleValue y


--------------------------------------------------------------------------------





advanceUnitWidth :: FromPtSize u => DrawingInfo (PointDisplace u)
advanceUnitWidth = scaleValue unit_width >>= return . displaceH


advanceNUnits :: FromPtSize u => Int -> DrawingInfo (PointDisplace u)
advanceNUnits i = 
    let n = fromIntegral i in 
    scaleValue unit_width >>= return . displaceH . (*n)

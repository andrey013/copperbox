{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Djembe.GraphicPrimitives
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

module Wumpus.Djembe.GraphicPrimitives where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Base

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative

-- type NoteheadWidth u = u


unit_width              :: AfmUnit
unit_width              = 1380

cap_size                :: AfmUnit 
cap_size                = 718

dot_radius              :: AfmUnit
dot_radius              = 272

period_radius           :: AfmUnit
period_radius           = 60


flam_dot_radius         :: AfmUnit
flam_dot_radius         = 134

-- for bracketing...
dot_notehead_width      :: AfmUnit
dot_notehead_width      = 540

line_height             :: AfmUnit
line_height             = 2545

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
accent_baseline         = 2234

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
char_upstroke           = 760 -- ???

char_downstroke         :: AfmUnit
char_downstroke         = 260 -- ???


dot_upstroke            :: AfmUnit
dot_upstroke            = 760 -- ???

dot_downstroke          :: AfmUnit
dot_downstroke          = 260 -- ???


--------------------------------------------------------------------------------
-- Note heads are positioned...

-- THese need generalizing so that ypos is parameteric.
-- Then they can handle upstrokes downstokes and normal positioned note-heads

dotNotehead :: (Fractional u, FromPtSize u) => LocImage u AfmUnit
dotNotehead = makeDotNotehead dot_center


-- not an Image as upstrokes cannot be parenthesized.
upstrokeDot :: (Fractional u, FromPtSize u) => LocGraphic u
upstrokeDot = postpro1 snd $ makeDotNotehead dot_upstroke

downstrokeDot :: (Fractional u, FromPtSize u) => LocGraphic u
downstrokeDot = postpro1 snd $ makeDotNotehead dot_downstroke


makeDotNotehead :: (Fractional u, FromPtSize u) 
                => AfmUnit -> LocImage u AfmUnit
makeDotNotehead ypos = 
    scaleValue dot_radius >>= \radius -> 
    intoLocImage (pure $ pure dot_notehead_width)
                 (scaleVMove ypos $ filledDisk radius)


-- period notehead always drawn just above baseline
-- 
periodNotehead :: (Fractional u, FromPtSize u) => LocImage u AfmUnit
periodNotehead = 
    scaleValue period_radius >>= \radius -> 
    intoLocImage (pure $ pure dot_notehead_width)
                 (scaleVMove period_center $ filledDisk radius)


letterNotehead :: (Fractional u, FromPtSize u) 
               => AfmUnit -> Char -> LocImage u AfmUnit
letterNotehead = makeLetterNotehead 0


upstrokeLetter :: (Fractional u, FromPtSize u) 
               => AfmUnit -> Char -> LocGraphic u
upstrokeLetter cw ch = postpro1 snd $ makeLetterNotehead char_upstroke cw ch


downstrokeLetter :: (Fractional u, FromPtSize u) 
                         => AfmUnit -> Char -> LocGraphic u
downstrokeLetter cw ch = postpro1 snd $ makeLetterNotehead char_downstroke cw ch


makeLetterNotehead :: (Fractional u, FromPtSize u) 
                   => AfmUnit -> AfmUnit -> Char -> LocImage u AfmUnit
makeLetterNotehead ypos cw ch = 
    intoLocImage (pure $ pure cw) 
                 (scaleMove (negate $ 0.5*cw) ypos (textline [ch]))



--
letterFlamGlyph :: (Fractional u, FromPtSize u) 
                => AfmUnit -> Char -> LocGraphic u
letterFlamGlyph cw ch = 
    getFontSize >>= \sz -> 
    localize (fontSize $ (3 * sz) `div` 4) $ 
        let x = negate $ flam_xminor + (0.675 * cw)
            y = flam_baseline
        in scaleMove x y $ textline [ch]

dotFlamGlyph :: (Fractional u, FromPtSize u) => LocGraphic u
dotFlamGlyph = 
    scaleValue flam_dot_radius >>= \radius -> 
    scaleMove (negate $ flam_xminor) flam_dot_center $ filledDisk radius

--------------------------------------------------------------------------------
-- stems

singleStem :: (Fractional u, FromPtSize u) => LocGraphic u
singleStem =
   scaleValue stem_length >>= \len -> 
   scaleVMove stem_start (straightLine $ vvec len)


flamStem :: (Fractional u, FromPtSize u) => LocGraphic u
flamStem = 
    scaleVecPath flam_path >>= \vs -> 
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
    scaleVecPath angle_path >>= \vs -> 
    scaleVMove swing_angle_lower_left (openStrokePath vs)
  where
    angle_path = let w = 0.9*flam_xminor in [ vec w w, vec (-w) w ]



openStrokePath :: Num u => [Vec2 u] -> LocGraphic u
openStrokePath vs = promote1 $ \pt -> openStroke $ vectorPath pt vs

filledRelativePath :: Num u => [Vec2 u] -> LocGraphic u
filledRelativePath vs = promote1 $ \pt -> filledPath $ vectorPath pt vs



--------------------------------------------------------------------------------

djembeNote :: (Fractional u, FromPtSize u) 
           => LocImage u AfmUnit -> LocImage u AfmUnit
djembeNote note_head = superimposeLocImage note_head singleStem 


flamNote :: (Fractional u, FromPtSize u) 
         => LocImage u AfmUnit -> LocGraphic u -> LocImage u AfmUnit
flamNote note_head flam_head = 
    superimposeLocImage note_head (flam_head `oplus` flamStem) 

restNote :: (Fractional u, FromPtSize u) => LocImage u AfmUnit
restNote = intoLocImage (pure $ pure dot_notehead_width) singleStem


--------------------------------------------------------------------------------
-- parens for optional, accents...

addParens :: (Fractional u, FromPtSize u)  
          => LocImage u AfmUnit -> LocImage u AfmUnit
addParens img = 
    img `bind1` \(w,a) -> drawParens w `bind1` \b -> wrap1 (w, a `oplus` b)


drawParens :: (Fractional u, FromPtSize u) => AfmUnit -> LocGraphic u
drawParens w = open_paren `oplus` close_paren
  where
    open_paren  = scaleMove (negate $ 1.0*w) paren_baseline (textline "(")
    close_paren = scaleMove (0.4*w) paren_baseline (textline ")")


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
    vertical_stalk = scaleVecPath [vvec h] >>= openStrokePath
    triangle_tip   = scaleVecPath vpath >>= filledRelativePath
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
    loc_rect = scaleValue hand_base_length >>= \uw -> fn uw uw

--------------------------------------------------------------------------------
-- strike throughs


addBaselineStrike :: (Fractional u, FromPtSize u)  
                    => LocImage u AfmUnit -> LocImage u AfmUnit
addBaselineStrike img = 
    img `bind1` \(w,a) -> drawHStrikeLine w `bind1` \b -> wrap1 (w, a `oplus` b)

drawHStrikeLine :: FromPtSize u => AfmUnit -> LocGraphic u
drawHStrikeLine cw = scaleMove (negate $ 0.5*cw) baseline_strike loc_strike
  where
    loc_strike = scaleVecPath [hvec cw] >>= openStrokePath


addAngledStrike :: (Fractional u, FromPtSize u)  
                    => LocImage u AfmUnit -> LocImage u AfmUnit
addAngledStrike img = 
    img `bind1` \(w,a) -> 
    drawAngStrikeLine w `bind1` \b -> wrap1 (w, a `oplus` b)


drawAngStrikeLine :: FromPtSize u => AfmUnit -> LocGraphic u
drawAngStrikeLine cw = scaleMove (negate $ 0.5*cw) baseline_strike loc_strike
  where
    loc_strike = scaleVecPath [vec cw cap_size] >>= openStrokePath



--------------------------------------------------------------------------------

scaleValue :: FromPtSize u => AfmUnit -> DrawingInfo u
scaleValue u1 = fmap (\sz -> afmValue u1 (fromIntegral sz)) getFontSize




superimposeLocImage :: LocImage u a -> LocGraphic u -> LocImage u a
superimposeLocImage img gfx = 
     img `bind1` \(a,b) -> gfx `bind1` \z -> wrap1 (a, b `oplus` z)


scaleMove :: FromPtSize u => AfmUnit -> AfmUnit -> LocCF u a -> LocCF u a
scaleMove x y cf = 
    scaleValue x >>= \xu -> scaleValue y >>= \yu -> prepro1 (displace xu yu) cf

scaleHMove :: FromPtSize u => AfmUnit -> LocCF u a -> LocCF u a
scaleHMove x cf = scaleValue x >>= \xu -> prepro1 (hdisplace xu) cf

scaleVMove :: FromPtSize u => AfmUnit -> LocCF u a -> LocCF u a
scaleVMove y cf = scaleValue y >>= \yu -> prepro1 (vdisplace yu) cf


scaleVecPath :: FromPtSize u => [Vec2 AfmUnit] -> DrawingInfo [Vec2 u]
scaleVecPath = mapM scaleVec2 


scaleVec2 :: FromPtSize u => Vec2 AfmUnit -> DrawingInfo (Vec2 u)
scaleVec2 (V2 x y) = V2 <$> scaleValue x <*> scaleValue y


--------------------------------------------------------------------------------






{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.PosObject
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - a rectangular /positionable/ Image.
-- 
-- This graphic object has a more flexible API for positioning 
-- than other graphic objects. Rather than a LocGraphic which 
-- supports a single method of positioning at some start-point,
-- a @PosGraphic@ can be drawn at its center or locations on its 
-- outer rectangle.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.PosObject
  (

  -- * Positionable image

    PosObject
  , DPosObject

  , PosGraphic
  , DPosGraphic

  -- * Operations
  , runPosObject
  , runPosObjectBBox

  , makePosObject
  , emptyPosObject

  , elaboratePosObject
  , decoratePosObject

  , extendPosObject
  , mapOrientation

  , illustratePosObject

  , posChar
  , posEscChar 
  , posCharUpright
  , posEscCharUpright
  
  , posCharPrim

  , posText
  , posEscText 
  , posTextUpright
  , posEscTextUpright

  , posTextPrim

  , multilinePosText
  , multilinePosEscText

  , rposText
  , rposEscText

  , posHKernText

  , monospaceText
  , monospaceEscText

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Concat
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.LocThetaImage
import Wumpus.Basic.Kernel.Objects.Orientation

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( red, blue )

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Monoid

type DOrt = Orientation Double

-- | A positionable \"Object\".
--
newtype PosObject u a = PosObject 
          { getPosObject :: DrawingContext -> DPoint2 -> (a, DOrt, CatPrim) }

type instance DUnit (PosObject u a) = u
    
-- | Version of PosObject specialized to Double for the unit type.
--
type DPosObject a = PosObject Double a


-- | Version of PosObject with answer specialized to UNil.
--
type PosGraphic u = PosObject u (UNil u)

-- | Version of PosGraphic specialized to Double for the unit type.
--
type DPosGraphic = PosGraphic Double




instance Functor (PosObject u) where
  fmap f mf = PosObject $ \ctx pt -> 
              let (a,o1,w1) = getPosObject mf ctx pt in (f a,o1,w1)


instance Applicative (PosObject u) where
  pure a    = PosObject $ \_   _  -> (a,mempty,mempty)
  mf <*> ma = PosObject $ \ctx pt -> 
              let (f,o1,w1) = getPosObject mf ctx pt
                  (a,o2,w2) = getPosObject ma ctx pt
              in (f a, o1 `mappend` o2, w1 `mappend` w2)



instance Monad (PosObject u) where
  return a  = PosObject $ \_   _  -> (a,mempty,mempty)
  mf >>= k  = PosObject $ \ctx pt -> 
              let (a,o1,w1) = getPosObject mf ctx pt
                  (b,o2,w2) = getPosObject (k a) ctx pt
              in (b, o1 `mappend` o2, w1 `mappend` w2)



instance DrawingCtxM (PosObject u) where
  askDC           = PosObject $ \ctx _ -> (ctx, mempty, mempty)
  asksDC fn       = PosObject $ \ctx _ -> (fn ctx, mempty, mempty)
  localize upd ma = PosObject $ \ctx pt -> getPosObject ma (upd ctx) pt




instance (Monoid a, InterpretUnit u) => Monoid (PosObject u a) where
  mempty = PosObject $ \_ _ -> (mempty, mempty, mempty)
  ma `mappend` mb = PosObject $ \ctx pt -> 
                    let (a,o1,w1) = getPosObject ma ctx pt
                        (b,o2,w2) = getPosObject mb ctx pt
                    in (a `mappend` b, o1 `mappend` o2, w1 `mappend` w2)



-- | Running an PosObject produces a LocImage.
--
runPosObject :: InterpretUnit u 
             => PosObject u a -> RectAddress -> LocImage u a
runPosObject ma addr = promoteLoc $ \ot -> 
    askDC >>= \ctx -> 
    let dot       = normalizeF (dc_font_size ctx) ot
        (a,o1,ca) = getPosObject ma ctx dot
        v1        = vtoOrigin addr o1
    in replaceAns a $ primGraphic $ cpmove v1 ca



-- | Run a PosObject producing a LocImage (BoundingBox u).
--
runPosObjectBBox :: InterpretUnit u 
                 => PosObject u a -> RectAddress -> LocImage u (BoundingBox u)
runPosObjectBBox ma addr = promoteLoc $ \pt -> 
    askDC >>= \ctx -> 
    let sz        = dc_font_size ctx 
        dpt       = normalizeF sz pt
        (_,o1,w1) = getPosObject ma ctx dpt
        v1        = vtoOrigin addr o1
        bb        = dinterpF sz $ orientationBounds o1 (dpt .+^ v1)
    in replaceAns bb $ primGraphic $ cpmove v1 w1




--------------------------------------------------------------------------------


-- | 'makePosObject' : @ object_pos * loc_image -> PosObject @ 
--
-- Create a 'PosObject' from an 'Orientation' describing how it
-- is orientated within a border rectangle and a 'LocImage' that 
-- draws it.
--
-- This is the /primary/ constructor for PosObjects. Because the
-- PosObject type is considered as a specialized object it does
-- not have the range of functions of LocImage or LocThetaImage.
-- 
makePosObject :: InterpretUnit u
              => Query u (Orientation u) -> LocImage u a -> PosObject u a
makePosObject ma gf = PosObject $ \ctx pt -> 
    let ort1  = runQuery ma ctx
        dort1 = normalizeF (dc_font_size ctx) ort1
        upt   = dinterpF (dc_font_size ctx) pt
        (a,w) = runLocImage gf ctx upt
    in (a,dort1,w)


-- | 'emptyPosObject' : @ PosObject @
--
-- Build an empty 'PosGraphicObject'.
--
emptyPosObject :: (Monoid a, InterpretUnit u) => PosObject u a
emptyPosObject = PosObject $ \_ _ -> (mempty, mempty, mempty) 

    

--
-- decorate  - oblivious to /answer/.
-- elaborate - derives annotation from the /answer/ and makes a 
--             cumulative graphic.
--


elaboratePosObject :: (Fractional u, Ord u, InterpretUnit u)
                   => ZDeco -> RectAddress -> LocGraphic u -> PosObject u a
                   -> PosObject u a
elaboratePosObject zdec raddr gf ma = decoratePosObject zdec fn ma
  where
    fn ortt = moveStart (vtoOrigin raddr ortt) gf



decoratePosObject :: InterpretUnit u 
                  => ZDeco -> (Orientation u -> LocGraphic u) -> PosObject u a
                  -> PosObject u a
decoratePosObject zdec fn ma = PosObject $ \ctx pt -> 
    let (a,o1,w1) = getPosObject ma ctx pt
        uortt     = dinterpF (dc_font_size ctx) o1
        upt       = dinterpF (dc_font_size ctx) pt
        (_,w2)    = runLocImage (fn uortt) ctx upt
        wout      = case zdec of
                         ANTERIOR -> w2 `mappend` w1
                         SUPERIOR -> w1  `mappend` w2
    in (a,o1,wout)




-- | Extend the orientation.
--
extendPosObject :: InterpretUnit u 
                => u -> u -> u -> u -> PosObject u a -> PosObject u a
extendPosObject x0 x1 y0 y1 ma = PosObject $ \ctx pt ->
    let (a,o1,w1) = getPosObject ma ctx pt
        sz        = dc_font_size ctx        
        ux0       = normalize sz x0
        ux1       = normalize sz x1
        uy0       = normalize sz y0
        uy1       = normalize sz y1
        o2        = extendOrientation ux0 ux1 uy0 uy1 o1
    in (a,o2,w1)


-- | Note - this is a bad API, it would be better to have padders
-- and fillers and not expose the orientation directly.
-- 
mapOrientation :: InterpretUnit u
               => (Orientation u -> Orientation u) 
               -> PosObject u a -> PosObject u a
mapOrientation fn mf = PosObject $ \ctx pt -> 
    let (a,o1,w1) = getPosObject mf ctx pt
        uort      = fn $ dinterpF (dc_font_size ctx) o1
        o2        = normalizeF (dc_font_size ctx) uort
    in (a,o2,w1)


--------------------------------------------------------------------------------


-- | Illustrate a 'PosObject' by super-imposing its 'Orientation'.
--
-- This turns the 'PosObject' into a 'LocImage' drawn at the locus
-- of the PosObject.
--
illustratePosObject :: InterpretUnit u 
                   => PosObject u a -> LocGraphic u
illustratePosObject mf  = promoteLoc $ \pt ->   
    askDC >>= \ctx ->
    let dpt       = normalizeF (dc_font_size ctx) pt 
        (_,o1,w1) = getPosObject mf ctx dpt
        uort      = dinterpF (dc_font_size ctx) o1
    in adecorate (primGraphic w1) (illustrateOrientation uort `at` pt)


illustrateOrientation :: InterpretUnit u 
                      => Orientation u -> LocGraphic u
illustrateOrientation (Orientation xmin xmaj ymin ymaj) = promoteLoc $ \pt -> 
    dinterpCtx 3 >>= \radius -> 
    let upd = localize (fill_colour blue . dotted_line)
        bl  = pt .-^ V2 xmin ymin
        dot = localize (fill_colour red) $ dcDisk FILL radius `at` pt
        hln = upd $ locStraightLine (hvec $ xmin+xmaj) `at` pt .-^ hvec xmin
        vln = upd $ locStraightLine (vvec $ ymin+ymaj) `at` pt .-^ vvec ymin
        bdr = upd $ dcRectangle STROKE (xmin+xmaj) (ymin+ymaj) `at` bl
    in mconcat [ bdr, hln, vln, dot ]



--------------------------------------------------------------------------------
-- Char PosObjects


-- Note - because the TextHeight constructors are so long winded,
-- using them directly makes for a bad API. Instead we have two 
-- versions for each function.

-- | Note - no margins are added to the containing rectangle.
-- 
-- To get a Char with margins, use 'posText' instead:
--
-- > posText ['1']
-- 
posChar             :: InterpretUnit u => Char -> PosGraphic u
posChar             = makeCharPO CAP_HEIGHT_PLUS_DESCENDER . CharLiteral

posEscChar          :: InterpretUnit u => EscapedChar -> PosGraphic u
posEscChar          = makeCharPO CAP_HEIGHT_PLUS_DESCENDER

posCharUpright      :: InterpretUnit u => Char -> PosGraphic u
posCharUpright      = makeCharPO JUST_CAP_HEIGHT . CharLiteral

posEscCharUpright   :: InterpretUnit u => EscapedChar -> PosGraphic u
posEscCharUpright   = makeCharPO JUST_CAP_HEIGHT


-- | Primtive builder that does not add margins.
--
posCharPrim         :: InterpretUnit u 
                    => Either Char EscapedChar -> PosGraphic u
posCharPrim = makeCharPO CAP_HEIGHT_PLUS_DESCENDER . either CharLiteral id


makeCharPO :: InterpretUnit u 
           => TextHeight -> EscapedChar -> PosGraphic u
makeCharPO hspec esc = 
    makePosObject (charOrientation hspec esc) 
                  (dcEscapedlabel $ wrapEscChar esc)




-- | Build the Orientation of an EscapedChar.
-- 
-- The locus of the Orientation is baseline left - margins are 
-- added.
--
charOrientation :: (DrawingCtxM m, InterpretUnit u)
                => TextHeight -> EscapedChar -> m (Orientation u)
charOrientation hspec esc = 
    (\(V2 x _ ) (ymin,ymaj) -> Orientation 0 x ymin ymaj) 
      <$> escCharVector esc <*> heightSpan hspec


--------------------------------------------------------------------------------
-- Text PosObjects



posText     :: InterpretUnit u => String -> PosGraphic u
posText     = addMargins . makeTextPO CAP_HEIGHT_PLUS_DESCENDER . escapeString

posEscText  :: InterpretUnit u => EscapedText -> PosGraphic u
posEscText  = addMargins . makeTextPO CAP_HEIGHT_PLUS_DESCENDER


posTextUpright      :: InterpretUnit u => String -> PosGraphic u
posTextUpright      = addMargins . makeTextPO JUST_CAP_HEIGHT . escapeString

posEscTextUpright   :: InterpretUnit u => EscapedText -> PosGraphic u
posEscTextUpright   = addMargins . makeTextPO JUST_CAP_HEIGHT

-- | Primtive builder that does not add margins.
--
posTextPrim         :: InterpretUnit u 
                    => Either String EscapedText -> PosGraphic u
posTextPrim = makeTextPO CAP_HEIGHT_PLUS_DESCENDER . either escapeString id


multilinePosText :: (Fractional u, InterpretUnit u)
                 => VAlign -> String -> PosGraphic u
multilinePosText vspec xs = 
    multilinePosEscText vspec $ map escapeString $ lines xs

multilinePosEscText :: (Fractional u, InterpretUnit u)
                    => VAlign -> [EscapedText] -> PosGraphic u
multilinePosEscText vspec xs = 
    addMargins $ PosObject $ \ctx pt -> 
      let sep    = runQuery textlineSpace ctx
      in getPosObject (body sep) ctx pt
  where
    body sp = alignColumnSep vspec sp $ 
                map (makeTextPO CAP_HEIGHT_PLUS_DESCENDER) xs



-- | Note - this does not add margins.
--
makeTextPO :: InterpretUnit u 
           => TextHeight -> EscapedText -> PosGraphic u
makeTextPO hspec esc = 
    makePosObject (textOrientationZero hspec esc) (dcEscapedlabel esc)


addMargins :: InterpretUnit u => PosObject u a -> PosObject u a
addMargins ma = 
   textMargin >>= \(xsep,ysep) -> extendPosObject xsep xsep ysep ysep ma

-- | Build the Orientation of a single line of EscapedText - 
-- writing direction zero (left-to-right).
-- 
-- The locus of the Orientation is baseline left - margins are 
-- added.
--
textOrientationZero :: (DrawingCtxM m, InterpretUnit u )
                    => TextHeight -> EscapedText -> m (Orientation u)
textOrientationZero hspec esc = 
    (\(V2 x _ ) (ymin,ymaj) -> Orientation 0 x ymin ymaj) 
      <$> escTextVector esc <*> heightSpan hspec


--------------------------------------------------------------------------------
-- Rotated text

-- | Note - for single line text.
--
rposText        :: (Real u, Floating u, InterpretUnit u) 
                => Radian -> String -> PosGraphic u
rposText ang    = addMargins . makeRotatedPO ang . escapeString

-- | Note - for single line text.
--
rposEscText     :: (Real u, Floating u, InterpretUnit u) 
                => Radian -> EscapedText -> PosGraphic u
rposEscText ang = addMargins . makeRotatedPO ang


makeRotatedPO :: (Real u, Floating u, InterpretUnit u) 
              => Radian -> EscapedText -> PosGraphic u
makeRotatedPO ang esc = makePosObject qry body
  where
    qry  = rotateOrientation ang <$> 
             textOrientationZero CAP_HEIGHT_PLUS_DESCENDER esc

    body = incline (dcREscapedlabel esc) ang


--------------------------------------------------------------------------------
-- Kerned text


posHKernText :: InterpretUnit u
            => [KernChar u] -> PosGraphic u
posHKernText xs = makePosObject (hkernOrientationZero xs) (hkernLine xs)

-- | The query should retrieve the width of one char.
--
monospaceText :: InterpretUnit u 
              => Query u u -> String -> PosGraphic u
monospaceText qry = monospaceEscText qry . escapeString


-- | The query should retrieve the width of one char.
--
monospaceEscText :: InterpretUnit u 
                 => Query u u -> EscapedText -> PosGraphic u
monospaceEscText qry esc = PosObject $ \ctx pt ->
    let upt    = dinterpF (dc_font_size ctx) pt
        uw     = runQuery qry ctx
        ks     = monos uw $ destrEscapedText id esc
        ortt   = runQuery (hkernOrientationZero  ks) ctx
        dort   = normalizeF (dc_font_size ctx) ortt
        (_,w1) = runLocImage (hkernLine ks) ctx upt
    in (UNil, dort, w1)





monos :: Num u => u -> [EscapedChar] -> [KernChar u]
monos w1 (c:cs) = (0,c) : map (\ch -> (w1,ch)) cs
monos _  []     = []



-- | Note - always CAP_HEIGHT_PLUS_DESCENDER for this one.
--
hkernOrientationZero :: (DrawingCtxM m, InterpretUnit u )
                     => [KernChar u] -> m (Orientation u)
hkernOrientationZero xs = 
    (\(V2 x _ ) (ymin,ymaj) -> Orientation 0 x ymin ymaj) 
      <$> hkernVector xs <*> heightSpan CAP_HEIGHT_PLUS_DESCENDER

 

--------------------------------------------------------------------------------
-- Combining PosObject


instance (Monoid a, InterpretUnit u) => ZConcat (PosObject u a) where
  superior = mappend
  anterior = flip mappend


instance Monoid a => Concat (PosObject u a) where
  hconcat = genMoveAlign spinemoveH spineRight
  vconcat = genMoveAlign spinemoveV spineBelow

instance (Monoid a, InterpretUnit u) => CatSpace (PosObject u a) where
  hspace = genMoveSepH spinemoveH spineRight
  vspace = genMoveSepV spinemoveV spineBelow



instance Monoid a => Align (PosObject u a) where
  halign HALIGN_TOP    = genMoveAlign binmoveHTop    halignTopO
  halign HALIGN_CENTER = genMoveAlign binmoveHCenter halignCenterO
  halign HALIGN_BASE   = genMoveAlign binmoveHBottom halignBottomO

  valign VALIGN_LEFT   = genMoveAlign binmoveVLeft   valignLeftO
  valign VALIGN_CENTER = genMoveAlign binmoveVCenter valignCenterO
  valign VALIGN_RIGHT  = genMoveAlign binmoveVRight  valignRightO



genMoveAlign :: Monoid a
             => (Orientation Double -> Orientation Double -> Vec2 Double) 
             -> (Orientation Double -> Orientation Double -> Orientation Double) 
             -> PosObject u a -> PosObject u a -> PosObject u a
genMoveAlign mkV mkO ma mb = PosObject $ \ctx pt -> 
    let (a,o1,w1) = getPosObject ma ctx pt
        (b,o2,w2) = getPosObject mb ctx pt
        v1        = mkV o1 o2
        ortt      = mkO o1 o2
        w2'       = cpmove v1 w2 
    in (a `mappend` b, ortt, w1 `mappend` w2')


--------------------------------------------------------------------------------
-- Sep

instance (Monoid a, InterpretUnit u) => AlignSpace (PosObject u a) where
  halignSpace HALIGN_TOP    = genMoveSepH binmoveHTop    halignTopO
  halignSpace HALIGN_CENTER = genMoveSepH binmoveHCenter halignCenterO
  halignSpace HALIGN_BASE   = genMoveSepH binmoveHBottom halignBottomO

  valignSpace VALIGN_LEFT   = genMoveSepV binmoveVLeft   valignLeftO
  valignSpace VALIGN_CENTER = genMoveSepV binmoveVCenter valignCenterO
  valignSpace VALIGN_RIGHT  = genMoveSepV binmoveVRight  valignRightO


genMoveSepH :: (Monoid a, InterpretUnit u) 
            => (Orientation Double -> Orientation Double -> Vec2 Double) 
            -> (Orientation Double -> Orientation Double -> Orientation Double) 
            -> u
            -> PosObject u a -> PosObject u a -> PosObject u a
genMoveSepH mkV mkO sep ma mb  = PosObject $ \ctx pt -> 
    let (a,o1,w1) = getPosObject ma ctx pt
        (b,o2,w2) = getPosObject mb ctx pt
        dsep      = normalize (dc_font_size ctx) sep
        v1        = hvec dsep ^+^ mkV o1 o2
        ortt      = extendORight dsep $ mkO o1 o2
        w2'       = cpmove v1 w2
    in (a `mappend` b, ortt, w1 `mappend` w2')



genMoveSepV :: (Monoid a, InterpretUnit u)
            => (Orientation Double -> Orientation Double -> Vec2 Double) 
            -> (Orientation Double -> Orientation Double -> Orientation Double) 
            -> u
            -> PosObject u a -> PosObject u a -> PosObject u a
genMoveSepV mkV mkO sep ma mb = PosObject $ \ctx pt -> 
    let (a,o1,w1) = getPosObject ma ctx pt
        (b,o2,w2) = getPosObject mb ctx pt
        dsep      = normalize (dc_font_size ctx) sep
        v1        = vvec (-dsep) ^+^ mkV o1 o2
        ortt      = extendODown dsep $ mkO o1 o2
        w2'       = cpmove v1 w2
    in (a `mappend` b, ortt, w1 `mappend` w2')


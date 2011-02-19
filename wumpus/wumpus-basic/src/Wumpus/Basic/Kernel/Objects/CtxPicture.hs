{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.CtxPicture
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- A Picture-with-implicit-context object. 
-- 
-- This is the corresponding type to Picture in the Wumpus-Core.
-- 
-- Note - many of the composition functions are in 
-- /destructor form/. As Wumpus cannot make a Picture from an 
-- empty list of Pictures, /destructor form/ decomposes the 
-- list into the @head@ and @rest@ as arguments in the function 
-- signature, rather than take a possibly empty list and have to 
-- throw an error.
-- 
-- TODO - PosImage no longer supports composition operators, so 
-- better names are up for grabs...
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.CtxPicture
  (

    CtxPicture
  , DCtxPicture
  , runCtxPicture
  , runCtxPictureU
  , drawTracing

  , mapCtxPicture

  -- * Composition
  , cxpBeneath

  , cxpUniteCenter
  , cxpRight
  , cxpDown
  
  , cxpCenteredAt


  , cxpRow 
  , cxpColumn


  , cxpRightSep
  , cxpDownSep
  , cxpRowSep
  , cxpColumnSep
 
  -- * Compose with alignment
  , cxpAlignH
  , cxpAlignV
  , cxpAlignSepH
  , cxpAlignSepV
  , cxpAlignRow
  , cxpAlignColumn
  , cxpAlignRowSep
  , cxpAlignColumnSep


  ) where

import Wumpus.Basic.Kernel.Base.Anchors
import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Objects.TraceDrawing

import Wumpus.Core                              -- package: wumpus-core

import Data.AdditiveGroup                       -- package: vector-space
import Data.AffineSpace

import Control.Applicative
import Data.List ( foldl' )


-- Note - PosGraphic should take priority for the good names.

-- | A /Contextual Picture/.
-- 
-- This type corresponds to the 'Picture' type in Wumpus-Core, but
-- it is embedded with a 'DrawingContext' (for font properties, 
-- fill colour etc.). So it is a function 
-- /from DrawingContext to Picture/.
--
-- Internally the result is actually a (Maybe Picture) and not a 
-- Picture, this is a trick to promote the extraction from 
-- possibly empty drawings (created by TraceDrawing) to the 
-- top-level of the type hierarchy where client code can deal 
-- with empty drawings explicitly (empty Pictures cannot be 
-- rendered by Wumpus-Core).
--
-- > a `oplus` b
--
-- The 'OPlus' (semigroup) instance for 'CtxPicture' draws picture 
-- a in front of picture b in the z-order, neither picture is 
-- moved. (Usually the picture composition operators in this 
-- module move the second picture aligning it somehow with the 
-- first).
--
newtype CtxPicture u = CtxPicture { getCtxPicture :: CF (Maybe (Picture u)) }

-- | Version of CtxPicture specialized to Double for the unit type.
--
type DCtxPicture = CtxPicture Double


type instance DUnit (CtxPicture u) = u



-- | 'runCtxPicture' : @ drawing_ctx * ctx_picture -> Maybe Picture @
--
-- Run a 'CtxPicture' with the supplied 'DrawingContext' 
-- producing a 'Picture'.
--
-- The resulting Picture may be empty. Wumpus-Core cannot 
-- generate empty pictures as they have no bounding box, so the 
-- result is wrapped within a Maybe. This delegates reponsibility 
-- for handling empty pictures to client code.
--
runCtxPicture :: DrawingContext -> CtxPicture u -> Maybe (Picture u)
runCtxPicture ctx drw = runCF ctx (getCtxPicture drw)  

-- | 'runCtxPictureU' : @ drawing_ctx * ctx_picture -> Picture @
--
-- /Unsafe/ version of 'runCtxPicture'.
--
-- This function throws a runtime error when supplied with an
-- empty CtxPicture.
--
runCtxPictureU :: DrawingContext -> CtxPicture u -> Picture u
runCtxPictureU ctx df = maybe fk id $ runCtxPicture ctx df
  where
    fk = error "runCtxPictureU - empty CtxPicture."   


-- | 'drawTracing' : @ trace_drawing  -> CtxPicture @
--
-- Transform a 'TraceDrawing' into a 'CtxPicture'.
--
drawTracing :: (Real u, Floating u, PtSize u) 
            => TraceDrawing u a -> CtxPicture u
drawTracing mf = CtxPicture $ 
    drawingCtx >>= \ctx -> return (liftToPictureMb $ execTraceDrawing ctx mf)


-- Note - cannot get an answer from a TraceDrawing with this 
-- CtxPicture type. There is nowhere to put the answer in the type.
--
-- If the type was extended:
--
-- > newtype CtxPicture u a = CtxPicture { getCtxPicture :: CF (a, Maybe (Picture u))) }
--
-- It would make things difficult for the drawing composition 
-- operators. @a@ could be monoidial but are there any types of 
-- a where this would be useful (rather than just making things 
-- more complicated)? 
--



-- | 'mapCtxPicture' : @ trafo * ctx_picture -> CtxPicture @
--
-- Apply a picture transformation function to the 'Picture'
-- warpped in a 'CtxPicture'.
--
mapCtxPicture :: (Picture u -> Picture u) -> CtxPicture u -> CtxPicture u
mapCtxPicture pf = CtxPicture . fmap (fmap pf) . getCtxPicture


--------------------------------------------------------------------------------




instance (Real u, Floating u) => Rotate (CtxPicture u) where 
  rotate ang = mapCtxPicture (rotate ang)

instance (Real u, Floating u) => RotateAbout (CtxPicture u) where
  rotateAbout r pt = mapCtxPicture (rotateAbout r pt)

instance (Num u, Ord u) => Scale (CtxPicture u) where
  scale sx sy = mapCtxPicture (scale sx sy)

instance (Num u, Ord u) => Translate (CtxPicture u) where
  translate dx dy = mapCtxPicture (translate dx dy)



--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Extract anchors

boundaryExtr :: (BoundingBox u -> a) -> Picture u -> a
boundaryExtr f = f . boundary

-- Operations on bounds

-- | The center of a picture.
--
boundaryCtr :: Fractional u => Picture u -> Point2 u
boundaryCtr = boundaryExtr center



-- | Extract the mid point of the top edge.
--
boundaryN :: Fractional u => Picture u -> Point2 u
boundaryN = boundaryExtr north

-- | Extract the mid point of the bottom edge.
--
boundaryS :: Fractional u => Picture u -> Point2 u
boundaryS = boundaryExtr south

-- | Extract the mid point of the left edge.
--
boundaryE :: Fractional u => Picture u -> Point2 u
boundaryE = boundaryExtr east

-- | Extract the mid point of the right edge.
--
boundaryW :: Fractional u => Picture u -> Point2 u
boundaryW = boundaryExtr west


-- | Extract the top-left corner.
--
boundaryNW :: Fractional u => Picture u -> Point2 u
boundaryNW = boundaryExtr northwest

-- | Extract the top-right corner.
--
boundaryNE :: Picture u -> Point2 u
boundaryNE = boundaryExtr ur_corner

-- | Extract the bottom-left corner.
--
boundarySW :: Picture u -> Point2 u
boundarySW = boundaryExtr ll_corner

-- | Extract the bottom-right corner.
--
boundarySE :: Fractional u => Picture u -> Point2 u
boundarySE = boundaryExtr southeast


boundaryLeftEdge :: Picture u -> u
boundaryLeftEdge = boundaryExtr (point_x . ll_corner)

boundaryRightEdge :: Picture u -> u
boundaryRightEdge = boundaryExtr (point_x . ur_corner)

boundaryBottomEdge :: Picture u -> u
boundaryBottomEdge = boundaryExtr (point_y . ll_corner)


boundaryTopEdge :: Picture u -> u
boundaryTopEdge = boundaryExtr (point_y . ur_corner)






--------------------------------------------------------------------------------
-- Composition operators

-- Naming convention - Wumpus-Core already prefixes operations
-- on Pictures with pic. As the picture operators here work on a
-- different type, they merit a different naming scheme.
--
-- Unfortunately the @cxp_@ prefix is rather ugly...
--
-- Directional names seem better than positional ones (less 
-- ambiguous as when used as binary operators).
--

cxpConcat :: (Picture u -> Picture u -> Picture u) 
          -> CtxPicture u -> CtxPicture u -> CtxPicture u
cxpConcat op a b = CtxPicture $ mbpostcomb op (getCtxPicture a) (getCtxPicture b)



mbpostcomb :: (a -> a -> a) -> CF (Maybe a) -> CF (Maybe a) -> CF (Maybe a)
mbpostcomb op = liftA2 fn 
  where
    fn (Just a) (Just b) = Just $ a `op` b
    fn a        Nothing  = a
    fn Nothing  b        = b


-- Note - the megaCombR operator is in some way an
-- /anti-combinator/. It seems easier to think about composing 
-- drawings if we do work on the result Pictures directly rather 
-- than build combinators to manipulate CtxPictures.
--
-- The idea of combining pre- and post- operating combinators
-- makes me worry about circular programs even though I know 
-- lazy evaluation allows me to write them (in some cicumstances).
--


-- Picture /mega-combiner/ - moves only the second argument aka the 
-- right picture.
--
megaCombR :: (Num u, Ord u)
          => (Picture u -> a) -> (Picture u -> a) 
          -> (a -> a -> Picture u -> Picture u) 
          -> CtxPicture u -> CtxPicture u
          -> CtxPicture u
megaCombR qL qR trafoR = cxpConcat fn
  where
    fn pic1 pic2 = let a    = qL pic1
                       b    = qR pic2
                       p2   = trafoR a b pic2
                   in pic1 `picOver` p2

-- | > a `oplus` b
-- 
-- Place \'drawing\' a over b. The idea of @over@ here is in 
-- terms z-ordering, nither picture a or b are actually moved.
--
instance (Num u, Ord u) => OPlus (CtxPicture u) where
  oplus = cxpConcat picOver


-- | 'cxpBeneath' : @ ctx_picture1 * ctx_picture2 -> CtxPicture @
-- 
-- > a `cxpBeneath` b
--
-- Similarly @beneath@ draws the first picture behind the second 
-- picture in the z-order, neither picture is moved.
--
cxpBeneath :: (Num u, Ord u) => CtxPicture u -> CtxPicture u -> CtxPicture u
cxpBeneath = flip oplus



--------------------------------------------------------------------------------
-- Composition

infixr 5 `cxpDown`
infixr 6 `cxpRight`, `cxpUniteCenter`




-- | Draw @a@, move @b@ so its center is at the same center as 
-- @a@, @b@ is drawn over underneath in the zorder.
--
-- > a `cxpUniteCenter` b 
--

cxpUniteCenter :: (Fractional u, Ord u) 
               => CtxPicture u -> CtxPicture u -> CtxPicture u
cxpUniteCenter = megaCombR boundaryCtr boundaryCtr moveFun
  where
    moveFun p1 p2 pic =  let v = p1 .-. p2 in pic `picMoveBy` v

--
-- Are combinator names less ambiguous if they name direction
-- rather than position?
--

-- | > a `cxpRight` b
-- 
-- Horizontal composition - position picture @b@ to the right of 
-- picture @a@.
-- 
cxpRight :: (Num u, Ord u) => CtxPicture u -> CtxPicture u -> CtxPicture u
cxpRight = megaCombR boundaryRightEdge boundaryLeftEdge moveFun
  where 
    moveFun a b pic = pic `picMoveBy` hvec (a - b)



-- | > a `cxpDown` b
--
-- Vertical composition - position picture @b@ /down/ from picture
-- @a@.
--
cxpDown :: (Num u, Ord u) => CtxPicture u -> CtxPicture u -> CtxPicture u
cxpDown = megaCombR boundaryBottomEdge boundaryTopEdge moveFun
  where 
    moveFun a b drw = drw `picMoveBy` vvec (a - b)


-- | Center the picture at the supplied point.
--
cxpCenteredAt :: (Fractional u, Ord u) => CtxPicture u -> Point2 u -> CtxPicture u
cxpCenteredAt d (P2 x y) = mapCtxPicture fn d
  where
    fn p = let bb = boundary p
               dx = x - (boundaryWidth  bb * 0.5)
               dy = y - (boundaryHeight bb * 0.5)
           in p `picMoveBy` vec dx dy



-- | 'cxpRow' : @ ctx_picture1 * [ctx_picture] -> CtxPicture @
-- 
-- Make a row of pictures concatenating them horizontally.
-- 
-- Note - this function is in /destructor form/. As Wumpus cannot
-- make a Picture from an empty list of Pictures, 
-- /destructor form/ decomposes the list into the @head@ and the 
-- @rest@ in the function signature, rather than take a possibly
-- empty list and have to throw an error.
-- 
cxpRow :: (Real u, Floating u, PtSize u) 
       => CtxPicture u -> [CtxPicture u] -> CtxPicture u
cxpRow = foldl' cxpRight


-- | 'cxpColumn' : @ ctx_picture1 * [ctx_picture] -> CtxPicture @
-- 
-- Make a column of pictures concatenating them vertically.
-- 
-- Note - this function is in /destructor form/.
--
cxpColumn :: (Real u, Floating u, PtSize u) 
          => CtxPicture u -> [CtxPicture u] -> CtxPicture u
cxpColumn = foldl' cxpDown




--------------------------------------------------------------------------------




-- | > cxpRightSep n a b
--
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ with a horizontal gap of @n@ separating the pictures.
--
cxpRightSep :: (Num u, Ord u) => u -> CtxPicture u -> CtxPicture u -> CtxPicture u
cxpRightSep n = megaCombR boundaryRightEdge boundaryLeftEdge moveFun
  where
    moveFun a b pic = pic `picMoveBy` hvec (n + a - b)

    



-- | > cxpDownSep n a b
--
-- Vertical composition - move @b@, placing it below @a@ with a
-- vertical gap of @n@ separating the pictures.
--
cxpDownSep :: (Num u, Ord u) 
           => u -> CtxPicture u -> CtxPicture u -> CtxPicture u
cxpDownSep n = megaCombR boundaryBottomEdge boundaryTopEdge moveFun
  where 
    moveFun a b pic = pic `picMoveBy`  vvec (a - b - n)



-- | > picRowSep n x xs
--
-- Concatenate the list of pictures @xs@ horizontally with 
-- @hspace@ starting at @x@. The pictures are interspersed with 
-- spaces of @n@ units.
--
cxpRowSep :: (Real u, Floating u, PtSize u) 
          => u -> CtxPicture u -> [CtxPicture u] -> CtxPicture u
cxpRowSep n = foldl' (cxpRightSep n)



-- | > vsepPic n xs
--
-- Concatenate the list of pictures @xs@ vertically with 
-- @vspace@ starting at @x@. The pictures are interspersed with 
-- spaces of @n@ units.
--
cxpColumnSep :: (Real u, Floating u, PtSize u) 
             => u -> CtxPicture u -> [CtxPicture u] -> CtxPicture u
cxpColumnSep n = foldl' (cxpDownSep n)


--------------------------------------------------------------------------------
-- Aligning pictures

alignMove :: (Num u, Ord u) => Point2 u -> Point2 u -> Picture u -> Picture u
alignMove p1 p2 pic = pic `picMoveBy` (p1 .-. p2)


-- Note - these don\'t conform to the naming convention, but using 
-- /Right/ in the names would be confusing with alignment.


-- | > cxpAlignH align a b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ and align it with the top, center or bottom of @a@.
-- 
cxpAlignH :: (Fractional u, Ord u) 
          =>  HAlign -> CtxPicture u -> CtxPicture u -> CtxPicture u
cxpAlignH HTop     = megaCombR boundaryNE boundaryNW  alignMove
cxpAlignH HCenter  = megaCombR boundaryE  boundaryW   alignMove
cxpAlignH HBottom  = megaCombR boundarySE boundarySW  alignMove


-- | > cxpAlignV align a b
-- 
-- Vertical composition - move @b@, placing it below @a@ 
-- and align it with the left, center or right of @a@.
-- 
cxpAlignV :: (Fractional u, Ord u) 
       => VAlign -> CtxPicture u -> CtxPicture u -> CtxPicture u
cxpAlignV VLeft    = megaCombR boundarySW boundaryNW alignMove
cxpAlignV VCenter  = megaCombR boundaryS  boundaryN  alignMove
cxpAlignV VRight   = megaCombR boundarySE boundaryNE  alignMove



alignMove2 :: (Num u, Ord u) 
           => Vec2 u ->  Point2 u -> Point2 u -> Picture u -> Picture u
alignMove2 v p1 p2 pic = pic `picMoveBy` (v ^+^ (p1 .-. p2))



-- | > cxpAlignSepH align sep a b
-- 
-- Spacing version of 'cxpAlignH' - move @b@ to the right of @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
cxpAlignSepH :: (Fractional u, Ord u) 
               => HAlign -> u -> CtxPicture u -> CtxPicture u -> CtxPicture u
cxpAlignSepH align dx = go align
  where
    go HTop    = megaCombR boundaryNE boundaryNW (alignMove2 (hvec dx))
    go HCenter = megaCombR boundaryE  boundaryW  (alignMove2 (hvec dx))
    go HBottom = megaCombR boundarySE boundarySW (alignMove2 (hvec dx))


-- | > cxpAlignSepV align sep a b
-- 
-- Spacing version of alignV - move @b@ below @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
cxpAlignSepV :: (Fractional u, Ord u) 
               => VAlign -> u -> CtxPicture u -> CtxPicture u -> CtxPicture u
cxpAlignSepV align dy = go align
  where
    go VLeft   = megaCombR boundarySW boundaryNW (alignMove2 $ vvec (-dy)) 
    go VCenter = megaCombR boundaryS  boundaryN  (alignMove2 $ vvec (-dy)) 
    go VRight  = megaCombR boundarySE boundaryNE (alignMove2 $ vvec (-dy))


-- | Variant of 'cxpRow' that aligns the pictures as well as
-- concatenating them.
--
cxpAlignRow :: (Real u, Floating u, PtSize u) 
            => HAlign -> CtxPicture u-> [CtxPicture u] -> CtxPicture u
cxpAlignRow ha = foldl' (cxpAlignH ha)



-- | Variant of 'cxpColumn' that aligns the pictures as well as
-- concatenating them.
--
cxpAlignColumn :: (Real u, Floating u, PtSize u) 
               => VAlign -> CtxPicture u -> [CtxPicture u] -> CtxPicture u
cxpAlignColumn va = foldl' (cxpAlignV va)


-- | Variant of 'cxpRow' that aligns the pictures as well as
-- concatenating and spacing them.
--
cxpAlignRowSep :: (Real u, Floating u, PtSize u) 
                 => HAlign -> u -> CtxPicture u -> [CtxPicture u] 
                 -> CtxPicture u
cxpAlignRowSep ha n = foldl' (cxpAlignSepH ha n)


-- | Variant of 'cxpColumn' that aligns the pictures as well as
-- concatenating and spacing them.
--
cxpAlignColumnSep :: (Real u, Floating u, PtSize u) 
                    => VAlign -> u -> CtxPicture u -> [CtxPicture u] 
                    -> CtxPicture u
cxpAlignColumnSep va n = foldl' (cxpAlignSepV va n) 




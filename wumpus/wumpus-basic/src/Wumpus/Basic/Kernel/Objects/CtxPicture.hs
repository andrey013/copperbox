{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.CtxPicture
-- Copyright   :  (c) Stephen Tetley 2010
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
-- CtxPicture is a function from the DrawingContext to a Picture.
-- Internally the result is actually a (Maybe Picture) and not a 
-- Picture, this is a trick to promote the extraction from 
-- possibly empty drawings (created by TraceDrawing) to the 
-- top-level of the type hierarchy where client code can deal 
-- with empty drawings explicitly (empty Pictures cannot be 
-- rendered by Wumpus-Core).
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.CtxPicture
  (

    CtxPicture
  , DCtxPicture
  , runCtxPicture
  , runCtxPictureU
  , drawTracing

  , clipCtxPicture
  , mapCtxPicture

  -- * Composition
  , over 
  , under

  , centric
  , nextToH
  , nextToV
  
  , atPoint 
  , centeredAt

  , zconcat

  , hcatPic 
  , vcatPic


  , hspacePic
  , vspacePic
  , hsepPic
  , vsepPic
 
  -- * Compose with alignment
  , alignH
  , alignV
  , alignHSep
  , alignVSep
  , hcatA
  , vcatA
  , hsepA
  , vsepA


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


newtype CtxPicture u = CtxPicture { getCtxPicture :: CF (Maybe (Picture u)) }

type DCtxPicture = CtxPicture Double


type instance DUnit (CtxPicture u) = u




runCtxPicture :: DrawingContext -> CtxPicture u -> Maybe (Picture u)
runCtxPicture ctx drw = runCF ctx (getCtxPicture drw)  


runCtxPictureU :: DrawingContext -> CtxPicture u -> Picture u
runCtxPictureU ctx df = maybe fk id $ runCtxPicture ctx df
  where
    fk = error "runCtxPictureU - empty CtxPicture."   



drawTracing :: (Real u, Floating u, FromPtSize u) 
            => TraceDrawing u a -> CtxPicture u
drawTracing mf = CtxPicture $ 
    drawingCtx >>= \ctx -> return (liftToPictureMb (execTraceDrawing ctx mf) )


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
--------------------------------------------------------------------------------

clipCtxPicture :: (Num u, Ord u) => (PrimPath u) -> CtxPicture u -> CtxPicture u
clipCtxPicture cpath = mapCtxPicture (clip cpath)


mapCtxPicture :: (Picture u -> Picture u) -> CtxPicture u -> CtxPicture u
mapCtxPicture pf = CtxPicture . fmap (fmap pf) . getCtxPicture

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




  
-- Note - do not export the empty drawing. It is easier to 
-- pretend it doesn't exist.
-- 
empty_drawing :: (Real u, Floating u, FromPtSize u) => CtxPicture u
empty_drawing = drawTracing $ return ()




--------------------------------------------------------------------------------
-- Composition operators


drawingConcat :: (Picture u -> Picture u -> Picture u) 
              -> CtxPicture u -> CtxPicture u -> CtxPicture u
drawingConcat op a b = CtxPicture $ mbpostcomb op (getCtxPicture a) (getCtxPicture b)



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
megaCombR qL qR trafoR = drawingConcat fn
  where
    fn pic1 pic2 = let a    = qL pic1
                       b    = qR pic2
                       p2   = trafoR a b pic2
                   in pic1 `picOver` p2




-- | > a `over` b
-- 
-- Place \'drawing\' a over b. The idea of @over@ here is in 
-- terms z-ordering, nither picture a or b are actually moved.
--
over    :: (Num u, Ord u) => CtxPicture u -> CtxPicture u -> CtxPicture u
over    = drawingConcat picOver



-- | > a `under` b
--
-- Similarly @under@ draws the first drawing behind 
-- the second but move neither.
--
under :: (Num u, Ord u) => CtxPicture u -> CtxPicture u -> CtxPicture u
under = flip over



-- | Move in both the horizontal and vertical.
--
move :: (Num u, Ord u) => Vec2 u -> CtxPicture u -> CtxPicture u
move v = mapCtxPicture (\p -> p `picMoveBy` v)




--------------------------------------------------------------------------------
-- Composition

infixr 5 `nextToV`
infixr 6 `nextToH`, `centric`




-- | Draw @a@, move @b@ so its center is at the same center as 
-- @a@, @b@ is drawn over underneath in the zorder.
--
-- > a `centeric` b 
--
--
centric :: (Fractional u, Ord u) => CtxPicture u -> CtxPicture u -> CtxPicture u
centric = megaCombR boundaryCtr boundaryCtr moveFun
  where
    moveFun p1 p2 pic =  let v = p1 .-. p2 in pic `picMoveBy` v



-- | > a `nextToH` b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@.
-- 
nextToH :: (Num u, Ord u) => CtxPicture u -> CtxPicture u -> CtxPicture u
nextToH = megaCombR boundaryRightEdge boundaryLeftEdge moveFun
  where 
    moveFun a b pic = pic `picMoveBy` hvec (a - b)



-- | > a `nextToV` b
--
-- Vertical composition - move @b@, placing it below @a@.
--
nextToV :: (Num u, Ord u) => CtxPicture u -> CtxPicture u -> CtxPicture u
nextToV = megaCombR boundaryBottomEdge boundaryTopEdge moveFun
  where 
    moveFun a b drw = drw `picMoveBy` vvec (a - b)


-- | Place the picture at the supplied point.
--
-- `atPoint` was previous the `at` operator.
-- 
atPoint :: (Num u, Ord u) => CtxPicture u -> Point2 u  -> CtxPicture u
p `atPoint` (P2 x y) = move (V2 x y) p



-- | Center the picture at the supplied point.
--
centeredAt :: (Fractional u, Ord u) => CtxPicture u -> Point2 u -> CtxPicture u
centeredAt d (P2 x y) = mapCtxPicture fn d
  where
    fn p = let bb = boundary p
               dx = x - (boundaryWidth  bb * 0.5)
               dy = y - (boundaryHeight bb * 0.5)
           in p `picMoveBy` vec dx dy


-- | Concatenate the list of drawings. 
--
-- No pictures are moved. 
--
zconcat :: (Real u, Floating u, FromPtSize u) => [CtxPicture u] -> CtxPicture u
zconcat []     = empty_drawing
zconcat (d:ds) = foldl' over d ds




-- | Concatenate the list pictures @xs@ horizontally.
-- 
hcatPic :: (Real u, Floating u, FromPtSize u) => [CtxPicture u] -> CtxPicture u
hcatPic []     = empty_drawing
hcatPic (d:ds) = foldl' nextToH d ds


-- | Concatenate the list of pictures @xs@ vertically.
--
vcatPic :: (Real u, Floating u, FromPtSize u) => [CtxPicture u] -> CtxPicture u
vcatPic []     = empty_drawing
vcatPic (d:ds) = foldl' nextToV d ds




--------------------------------------------------------------------------------




-- | > hspace n a b
--
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ with a horizontal gap of @n@ separating the pictures.
--
hspacePic :: (Num u, Ord u) => u -> CtxPicture u -> CtxPicture u -> CtxPicture u
hspacePic n = megaCombR boundaryRightEdge boundaryLeftEdge moveFun
  where
    moveFun a b pic = pic `picMoveBy` hvec (n + a - b)

    



-- | > vspace n a b
--
-- Vertical composition - move @b@, placing it below @a@ with a
-- vertical gap of @n@ separating the pictures.
--
vspacePic :: (Num u, Ord u) => u -> CtxPicture u -> CtxPicture u -> CtxPicture u
vspacePic n = megaCombR boundaryBottomEdge boundaryTopEdge moveFun
  where 
    moveFun a b pic = pic `picMoveBy`  vvec (a - b - n)



-- | > hsepPic n xs
--
-- Concatenate the list of pictures @xs@ horizontally with 
-- @hspace@ starting at @x@. The pictures are interspersed with 
-- spaces of @n@ units.
--
hsepPic :: (Real u, Floating u, FromPtSize u) => u -> [CtxPicture u] -> CtxPicture u
hsepPic _ []     = empty_drawing
hsepPic n (d:ds) = foldl' (hspacePic n) d ds



-- | > vsepPic n xs
--
-- Concatenate the list of pictures @xs@ vertically with 
-- @vspace@ starting at @x@. The pictures are interspersed with 
-- spaces of @n@ units.
--
vsepPic :: (Real u, Floating u, FromPtSize u) => u -> [CtxPicture u] -> CtxPicture u
vsepPic _ []     = empty_drawing
vsepPic n (d:ds) = foldl' (vspacePic n) d ds


--------------------------------------------------------------------------------
-- Aligning pictures

alignMove :: (Num u, Ord u) => Point2 u -> Point2 u -> Picture u -> Picture u
alignMove p1 p2 pic = pic `picMoveBy` (p1 .-. p2)



-- | > alignH align a b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ and align it with the top, center or bottom of @a@.
-- 
alignH :: (Fractional u, Ord u) 
       =>  HAlign -> CtxPicture u -> CtxPicture u -> CtxPicture u
alignH HTop     = megaCombR boundaryNE boundaryNW  alignMove
alignH HCenter  = megaCombR boundaryE  boundaryW   alignMove
alignH HBottom  = megaCombR boundarySE boundarySW  alignMove


-- | > alignV align a b
-- 
-- Vertical composition - move @b@, placing it below @a@ 
-- and align it with the left, center or right of @a@.
-- 
alignV :: (Fractional u, Ord u) 
       => VAlign -> CtxPicture u -> CtxPicture u -> CtxPicture u
alignV VLeft    = megaCombR boundarySW boundaryNW alignMove
alignV VCenter  = megaCombR boundaryS  boundaryN  alignMove
alignV VRight   = megaCombR boundarySE boundaryNE  alignMove



alignMove2 :: (Num u, Ord u) 
           => Vec2 u ->  Point2 u -> Point2 u -> Picture u -> Picture u
alignMove2 v p1 p2 pic = pic `picMoveBy` (v ^+^ (p1 .-. p2))



-- | > alignHSep align sep a b
-- 
-- Spacing version of alignH - move @b@ to the right of @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
alignHSep :: (Fractional u, Ord u) 
          => HAlign -> u -> CtxPicture u -> CtxPicture u -> CtxPicture u
alignHSep HTop    dx = megaCombR boundaryNE boundaryNW (alignMove2 (hvec dx))
alignHSep HCenter dx = megaCombR boundaryE  boundaryW  (alignMove2 (hvec dx))
alignHSep HBottom dx = megaCombR boundarySE boundarySW (alignMove2 (hvec dx))


-- | > alignVSep align sep a b
-- 
-- Spacing version of alignV - move @b@ below @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
alignVSep :: (Fractional u, Ord u) 
          => VAlign -> u -> CtxPicture u -> CtxPicture u -> CtxPicture u
alignVSep VLeft   dy = megaCombR boundarySW boundaryNW (alignMove2 $ vvec (-dy)) 
alignVSep VCenter dy = megaCombR boundaryS  boundaryN  (alignMove2 $ vvec (-dy)) 
alignVSep VRight  dy = megaCombR boundarySE boundaryNE (alignMove2 $ vvec (-dy))


-- | Variant of 'hcat' that aligns the pictures as well as
-- concatenating them.
--
hcatA :: (Real u, Floating u, FromPtSize u) 
      => HAlign -> [CtxPicture u] -> CtxPicture u
hcatA _  []     = empty_drawing
hcatA ha (d:ds) = foldl' (alignH ha) d ds



-- | Variant of 'vcat' that aligns the pictures as well as
-- concatenating them.
--
vcatA :: (Real u, Floating u, FromPtSize u) 
      => VAlign -> [CtxPicture u] -> CtxPicture u
vcatA _  []     = empty_drawing
vcatA va (d:ds) = foldl' (alignV va) d ds


-- | Variant of @hsep@ that aligns the pictures as well as
-- concatenating and spacing them.
--
hsepA :: (Real u, Floating u, FromPtSize u) 
      => HAlign -> u -> [CtxPicture u] -> CtxPicture u
hsepA _  _ []     = empty_drawing
hsepA ha n (d:ds) = foldl' op d ds
  where 
    a `op` b = alignHSep ha n a b 


-- | Variant of @vsep@ that aligns the pictures as well as
-- concatenating and spacing them.
--
vsepA :: (Real u, Floating u, FromPtSize u) 
      => VAlign -> u -> [CtxPicture u] -> CtxPicture u
vsepA _  _ []     = empty_drawing
vsepA va n (d:ds) = foldl' op d ds
  where 
    a `op` b = alignVSep va n a b 




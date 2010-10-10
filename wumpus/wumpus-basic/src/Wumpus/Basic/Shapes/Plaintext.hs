{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Shapes.Plaintext
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Plaintext is a bit like a shape but does not generate a path 
-- and cannot be scaled (it can be rotated or translated).
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Shapes.Plaintext
  (
    PlaintextAnchor
  , DPlaintextAnchor
  , Plaintext
  , DPlaintext

  , plaintext

  -- TEMP
  , baselineCenters
  , headIterate

  ) where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Graphic
import Wumpus.Basic.Shapes.Base
import Wumpus.Basic.Shapes.Derived

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid


--------------------------------------------------------------------------------
-- Free label

-- Free label is a rectangle that /is not drawn/, the 
-- constructor should always create some text.

newtype PlaintextAnchor u = PlaintextAnchor  { getPlaintext :: Rectangle u }


type DPlaintextAnchor = PlaintextAnchor Double

type instance DUnit (PlaintextAnchor u) = u


data Text1 u = Text1
       { text_xshift    :: !u
       , text_body      :: String
       }
  deriving (Eq,Ord,Show)


type instance DUnit (Text1 u) = u

data TextAlign =  AlignLeft | AlignCenter | AlignRight
  deriving (Eq,Ord,Show)

data Plaintext u = Plaintext
      { cached_dimensions     :: (u,u)
      , text_lines            :: [Text1 u]
      , text_align            :: TextAlign
      , text_x                :: !u
      , text_y                :: !u
      , padding_size          :: !u
      , trunc_right           :: !u 
      , text_ang              :: !Radian
      }
  deriving (Eq,Ord,Show)

-- Note - cached_dimensions makes Plaintext construction
-- /context-sensitive/. However curently all the Shapes have 
-- context-free constructors that don\'t care about the 
-- @DrawingCtx@.
--


type DPlaintext = Plaintext Double

type instance DUnit (Plaintext u) = u


--------------------------------------------------------------------------------

-- Finding the (maximum) width is dependent of the justification.


-- | Left aligned text adds the xshift - generally it will be 0 
-- unless the first line is indented.
-- 
-- Note - this over-calculates and includes the right-trail in 
-- the answer. 
--
alignLeftWidth :: (DrawingCtxM m, Num u, Ord u, FromPtSize u) 
              => [Text1 u] -> m u
alignLeftWidth = findMax (+)


-- | Center aligned text subtracts two times the (half) xshift 
-- to find the maximum.
-- 
-- It is assumed the user has moved the text by half the 
-- right-trail.
-- 
-- If the user has xshifted the text accurately this function
-- calculates accurately.
--
alignCenterWidth :: (DrawingCtxM m, Num u, Ord u, FromPtSize u) 
                => [Text1 u] -> m u
alignCenterWidth = findMax (\u len -> len - (2*u))


-- | Right aligned text subtracts the xshift to find the 
-- maximum.
-- 
-- It is assumed the user has moved the text by all the 
-- right-trail.
-- 
-- If the user has xshifted the text accurately this function
-- calculates accurately.
--
alignRightWidth :: (DrawingCtxM m, Num u, Ord u, FromPtSize u) 
               => [Text1 u] -> m u
alignRightWidth = findMax (\u len -> len - u)


findMax :: (DrawingCtxM m, Num u, Ord u, FromPtSize u) 
        => (u -> u -> u) -> [Text1 u] -> m u
findMax fn xs = maximum <$> mapM fnText1 xs
  where
    fnText1 (Text1 u ss) = (\len -> fn u len) <$> monoTextLength ss

--------------------------------------------------------------------------------

instance (Real u, Floating u) => CenterAnchor (PlaintextAnchor u) where
  center = center . getPlaintext


instance (Real u, Floating u) => CardinalAnchor (PlaintextAnchor u) where
  north = north . getPlaintext
  south = south . getPlaintext
  east  = east . getPlaintext
  west  = west . getPlaintext

instance (Real u, Floating u) => CardinalAnchor2 (PlaintextAnchor u) where
  northeast = northeast . getPlaintext
  southeast = southeast . getPlaintext
  southwest = southwest . getPlaintext
  northwest = northwest . getPlaintext

instance (Real u, Floating u) => RadialAnchor (PlaintextAnchor u) where
  radialAnchor theta = radialAnchor theta . getPlaintext



instance Rotate (Plaintext u) where
  rotate dr = (\s i -> s { text_ang = i+dr }) <*> text_ang

-- Note - cannot scale Plaintext


instance Num u => Translate (Plaintext u) where
  translate dx dy = (\s x y -> s { text_x = x+dx, text_y = y+dy }) 
                      <*> text_x <*> text_y


-- note other shapes are context-free (so far)

-- Don'\t want plaintext in a general DrawingCtxM monad because
-- we have to bind it at the construction site
--
-- >        ptext <- plaintext "Rect1"         -- this is why Ctx-free is good...
-- >        drawi_ $ drawText ptext
-- 

-- Is DrawingR better...

-- Its going to be very hard to rotate this...

plaintext :: (Real u, Floating u, FromPtSize u)
         => String -> Image u (PlaintextAnchor u)
plaintext ss = plaintextCons ss >>= \ptext -> 
               intoImage (pure $ anchorRectangle ptext) (plaintextDraw ptext)

plaintextCons :: (Fractional u, Ord u, FromPtSize u) 
          => String -> DrawingR (Plaintext u)
plaintextCons ss = let xs = [Text1 0 ss]; truncr = 0; align = AlignCenter in do
    pad   <- monoDefaultPadding 
    w     <- plaintextRectWidth pad truncr align xs
    h     <- plaintextRectHeight pad xs
    return $ Plaintext { cached_dimensions   = (w,h)
                       , text_lines          = xs
                       , text_align          = align
                       , text_x              = 0
                       , text_y              = 0
                       , padding_size        = pad
                       , trunc_right         = truncr 
                       , text_ang            = 0 }


plaintextDraw :: (Real u, Floating u, FromPtSize u) 
              => Plaintext u -> Graphic u
plaintextDraw (Plaintext { cached_dimensions = (_,h)
                         , text_x             = x0
                         , text_y             = y0
                         , text_ang           = ang
                         , text_lines         = xs
                         , padding_size       = pad })  = 
    baselineCenters h pad (length xs) (P2 x0 y0) >>= \ps ->  
    zipWithM (\(P2 x y) t1 -> drawOneLine x y ang (text_body t1)) ps xs >>= 
    return . mconcat


plaintextRectWidth :: (DrawingCtxM m, Num u, Ord u, FromPtSize u) 
                   => u -> u -> TextAlign -> [Text1 u] -> m u
plaintextRectWidth pad truncr align xs = case align of
    AlignLeft   -> bump <$> alignLeftWidth   xs
    AlignCenter -> bump <$> alignCenterWidth xs
    AlignRight  -> bump <$> alignRightWidth  xs
  where
    bump n = n + (2*pad) - truncr
                                                
plaintextRectHeight :: (DrawingCtxM m, Fractional u, FromPtSize u)
                    => u -> [Text1 u] -> m u
plaintextRectHeight pad xs = 
    (\h -> h + 2 * pad) <$> monoMultiLineTextHeight (length xs)
    



anchorRectangle :: (Fractional u, Ord u, FromPtSize u) 
                => Plaintext u -> PlaintextAnchor u
anchorRectangle ptext = PlaintextAnchor $ mkRectangle (0.5*w) (0.5*h) ctm
  where
    (w,h) = cached_dimensions ptext
    ctm   = ShapeCTM { ctm_trans_x    = text_x ptext
                     , ctm_trans_y    = text_y ptext
                     , ctm_scale_x    = 1
                     , ctm_scale_y    = 1
                     , ctm_rotation   = text_ang ptext
                     }


-- | It is easy to find the baseline of the bottom line:
--
-- > half the height down + descender_depth up + padding up
-- 
-- Once we have the bottom-baseline we can iterate upwards
-- 
baselineCenters :: (Fractional u, FromPtSize u)  
        => u -> u -> Int -> Point2 u -> DrawingR [Point2 u]
baselineCenters total_height pad n center_point = 
    (\spacing descender_depth -> 
        let dy = 0.5 * total_height + descender_depth + pad
        in headIterate n (.+^ vvec spacing) (center_point .-^ vvec dy))
      <$> baselineSpacing <*> monoDescenderDepth


headIterate :: Int -> (a -> a) -> a -> [a]
headIterate n f a = step 0 a []
  where
    step i x xs | i < n = step (i+1) (f x) (x:xs)
    step _ _ xs         = xs




drawOneLine :: (Real u, Floating u, FromPtSize u) 
            => u -> u -> Radian -> String -> Graphic u 
drawOneLine dx dy ang ss =
    monoVecToCenter ss >>= \v ->
    let ctr = P2 dx dy; bl = ctr .-^ v in 
    rotTextline ang ss (rotateAbout ang ctr bl)



rotTextline :: (Real u, Floating u) => Radian -> String -> LocGraphic u
rotTextline theta ss baseline_left = 
    withTextAttr $ \rgb attr -> 
        singleH $ rotatePrim theta $ textlabel rgb attr ss baseline_left
     


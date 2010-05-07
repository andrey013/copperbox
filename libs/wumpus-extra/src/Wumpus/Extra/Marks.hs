{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Marks
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Graphical attributes for /Marks/ - e.g. arrow heads or dots. 
--
-- Marks have colour (and sometimes background colour) and size
-- derived from the /current/ line width.  
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Marks
  ( 
    MarkAttr(..)
  , Mark(..)

  , lineWidth
  , lwX5
  , lwX10

  , mostroke
  , mcstroke
  , mfill
  , mfillSecond
  , mstrokePolygon
  , mellipse

  , dup

  ) where

import Wumpus.Core
import Wumpus.Core.Colour ( black )

import Wumpus.Geometry
import Wumpus.Geometry.Utils


import Control.Applicative

data MarkAttr = MarkAttr { 
        line_width         :: Double,
        mark_colour        :: RGB3 Double,
        mark_second_colour :: Maybe (RGB3 Double)
      }



class Mark t where
  mark :: t -> MarkAttr


constrLineWidth :: Double -> MarkAttr
constrLineWidth i = MarkAttr { line_width         = i
                             , mark_colour        = black
                             , mark_second_colour = Nothing }


constrColour :: PSColour c => c -> MarkAttr
constrColour c = MarkAttr { line_width          = 1
                          , mark_colour         = psColour c
                          , mark_second_colour  = Nothing }


instance Mark () where
  mark () = constrColour black


instance Mark (Gray Double) where
  mark = constrColour . psColour
 
instance Mark (HSB3 Double) where
  mark = constrColour . psColour

instance Mark (RGB3 Double) where
  mark = constrColour

instance Mark Double where
  mark = constrLineWidth

instance Mark Int where
  mark = constrLineWidth . fromIntegral

instance Mark (Gray Double, Double) where
  mark (c,n) = MarkAttr n (psColour c) Nothing
 
instance Mark (HSB3 Double, Double) where
  mark (c,n) = MarkAttr n (psColour c) Nothing

instance Mark (RGB3 Double, Double) where
  mark (c,n) = MarkAttr n c Nothing

instance Mark (Gray Double, Int) where
  mark (c,n) = MarkAttr (fromIntegral n) (psColour c) Nothing
 
instance Mark (HSB3 Double, Int) where
  mark (c,n) = MarkAttr (fromIntegral n) (psColour c) Nothing

instance Mark (RGB3 Double, Int) where
  mark (c,n) = MarkAttr (fromIntegral n) c Nothing

instance (PSColour c1, PSColour c2) => Mark (c1, Double, c2) where
  mark (c1,n,c2) = MarkAttr n (psColour c1) (Just $ psColour c2)
 
instance (PSColour c1, PSColour c2) => Mark (c1, Int, c2) where
  mark (c1,n,c2) = MarkAttr (fromIntegral n) (psColour c1) (Just $ psColour c2)


lineWidth :: (Fractional u, Mark t) => t -> u
lineWidth = realToFrac . line_width . mark

lwX5 :: (Fractional u, Mark t) => t -> u
lwX5 = (5*) . lineWidth

lwX10 :: (Fractional u, Mark t) => t -> u
lwX10 = (10*) . lineWidth


prodStroke :: Mark t => t -> (RGB3 Double, StrokeAttr)
prodStroke = liftA2 (,) strokeColour (LineWidth . lineWidth)

strokeColour :: Mark t => t -> RGB3 Double
strokeColour = mark_colour . mark

fillColour :: Mark t => t -> RGB3 Double
fillColour = mark_colour . mark

secondFillColour :: Mark t => t -> RGB3 Double
secondFillColour = maybe black id . mark_second_colour . mark

mostroke :: (Num u, Ord u, Mark t) => t -> Path u -> Primitive u
mostroke = ostroke `combfi` prodStroke


mcstroke :: (Num u, Ord u, Mark t) => t -> Path u -> Primitive u
mcstroke = cstroke `combfi` prodStroke



mfill :: (Num u, Ord u, Mark t) => t -> Path u -> Primitive u
mfill = fill `combfi` strokeColour
   
mfillSecond :: (Num u, Ord u, Mark t) => t -> Path u -> Primitive u
mfillSecond = fill `combfi` secondFillColour


mstrokePolygon :: (Mark t, Num u, Ord u) => t -> Polygon u -> Primitive u
mstrokePolygon =  strokePolygon `combfi` prodStroke 

-- | Always filled!
-- 
mellipse :: (Fractional u, Mark t) 
         => t -> u -> u -> Point2 u -> Primitive u
mellipse = ellipse `combfi` fillColour 


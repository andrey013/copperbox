{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PathCommands
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  deprecated
-- Portability :  GHC
--
--
-- \*\* This module is deprecated \*\*
--
-- Path Segment shorthand commands patterned after section 8.5.2.
--
-- This module is needs more thought - currently it is a sketch 
-- to help create paths. If it becomes useful in the future it 
-- is still be better off outside the OpenVG bindings package 
-- and presented in some higher-level package.
--

--------------------------------------------------------------------------------

module PathCommands (
  -- * Translate Path Commands
  pathData,
  
  -- * Path commands
  closePath,
  moveAbs, moveRel,
  lineAbs, lineRel,
  hlineAbs, hlineRel,
  vlineAbs, vlineRel,
  quadraticAbs, quadraticRel,
  cubicAbs, cubicRel,
  smoothQuadAbs, smoothQuadRel,
  smoothCubicAbs, smoothCubicRel,
  smallCcwArcAbs, smallCcwArcRel,
  smallCwArcAbs, smallCwArcRel,
  largeCcwArcAbs, largeCcwArcRel,
  largeCwArcAbs, largeCwArcRel,
  
  
) where

import Graphics.Rendering.OpenVG.VG.Paths

data SegmentCommand a = 
     CLOSE_PATH
   | MOVE_TO    PathAbsRel a a
   | LINE_TO    PathAbsRel a a
   | HLINE_TO   PathAbsRel a
   | VLINE_TO   PathAbsRel a
   | QUAD_TO    PathAbsRel a a a a
   | CUBIC_TO   PathAbsRel a a a a a a
   | SQUAD_TO   PathAbsRel a a
   | SCUBIC_TO  PathAbsRel a a a a
   | SCCWARC_TO PathAbsRel a a a a a
   | SCWARC_TO  PathAbsRel a a a a a
   | LCCWARC_TO PathAbsRel a a a a a
   | LCWARC_TO  PathAbsRel a a a a a
   deriving ( Eq, Show )

pathData :: StorablePathData a => [SegmentCommand a] -> ([PathCommand], [a])
pathData = foldr fn ([],[]) where
  fn :: SegmentCommand a -> ([PathCommand], [a]) -> ([PathCommand], [a])
  fn CLOSE_PATH                     (xs,ys) = (ClosePath:xs,ys) 
  fn (MOVE_TO t x y)                (xs,ys) 
          | t == Absolute                   = (MoveToAbs : xs, x:y:ys)
          | otherwise                       = (MoveToRel : xs, x:y:ys)
  fn (LINE_TO t x y)                (xs,ys) 
          | t == Absolute                   = (LineToAbs : xs, x:y:ys)
          | otherwise                       = (LineToRel : xs, x:y:ys)
  fn (HLINE_TO t x)                 (xs,ys)
          | t == Absolute                   = (HLineToAbs : xs, x:ys)
          | otherwise                       = (HLineToRel : xs, x:ys)
  fn (VLINE_TO t y)                 (xs,ys)
          | t == Absolute                   = (VLineToAbs : xs, y:ys)
          | otherwise                       = (VLineToRel : xs, y:ys)
  fn (QUAD_TO t x y x' y')          (xs,ys)
          | t == Absolute                   = (QuadToAbs : xs, x:y:x':y':ys)
          | otherwise                       = (QuadToRel : xs, x:y:x':y':ys)
  fn (CUBIC_TO t x y x' y' x'' y'') (xs,ys)
          | t == Absolute                   = (CubicToAbs : xs, x:y:x':y':x'':y'':ys)
          | otherwise                       = (CubicToRel : xs, x:y:x':y':x'':y'':ys)
  fn (SQUAD_TO t x y)               (xs,ys)
          | t == Absolute                   = (SQuadToAbs : xs, x:y:ys)
          | otherwise                       = (SQuadToRel : xs, x:y:ys)
  fn (SCUBIC_TO t x y x' y')        (xs,ys)
          | t == Absolute                   = (SCubicToAbs : xs, x:y:x':y':ys)
          | otherwise                       = (SCubicToRel : xs, x:y:x':y':ys)
  fn (SCCWARC_TO t rh rv rt x y)    (xs,ys)
          | t == Absolute                   = (SCCWArcToAbs : xs, rh:rv:rt:x:y:ys)
          | otherwise                       = (SCCWArcToRel : xs, rh:rv:rt:x:y:ys)
  fn (SCWARC_TO t rh rv rt x y)     (xs,ys)
          | t == Absolute                   = (SCWArcToAbs : xs, rh:rv:rt:x:y:ys)
          | otherwise                       = (SCWArcToRel : xs, rh:rv:rt:x:y:ys)
  fn (LCCWARC_TO t rh rv rt x y)    (xs,ys)
          | t == Absolute                   = (LCCWArcToAbs : xs, rh:rv:rt:x:y:ys)
          | otherwise                       = (LCCWArcToRel : xs, rh:rv:rt:x:y:ys)
  fn (LCWARC_TO t rh rv rt x y)     (xs,ys)
          | t == Absolute                   = (LCWArcToAbs : xs, rh:rv:rt:x:y:ys)
          | otherwise                       = (LCWArcToRel : xs, rh:rv:rt:x:y:ys)


closePath :: SegmentCommand a
closePath = CLOSE_PATH

moveAbs :: StorablePathData a => a -> a -> SegmentCommand a
moveAbs = MOVE_TO Absolute 

moveRel :: StorablePathData a => a -> a -> SegmentCommand a
moveRel = MOVE_TO Relative 

lineAbs :: StorablePathData a => a -> a -> SegmentCommand a
lineAbs = LINE_TO Absolute 

lineRel :: StorablePathData a => a -> a -> SegmentCommand a
lineRel = LINE_TO Relative 

hlineAbs :: StorablePathData a => a -> SegmentCommand a
hlineAbs = VLINE_TO Absolute 

hlineRel :: StorablePathData a => a -> SegmentCommand a
hlineRel = HLINE_TO Relative 

vlineAbs :: StorablePathData a => a -> SegmentCommand a
vlineAbs = VLINE_TO Absolute 

vlineRel :: StorablePathData a => a -> SegmentCommand a
vlineRel = VLINE_TO Relative 

quadraticAbs :: StorablePathData a => a -> a -> a -> a -> SegmentCommand a
quadraticAbs = QUAD_TO Absolute

quadraticRel :: StorablePathData a => a -> a -> a -> a -> SegmentCommand a
quadraticRel = QUAD_TO Relative

cubicAbs :: StorablePathData a => a -> a -> a -> a -> a -> a -> SegmentCommand a
cubicAbs = CUBIC_TO Absolute

cubicRel :: StorablePathData a => a -> a -> a -> a -> a -> a -> SegmentCommand a
cubicRel = CUBIC_TO Relative

smoothQuadAbs :: StorablePathData a => a -> a -> SegmentCommand a
smoothQuadAbs = SQUAD_TO Absolute

smoothQuadRel :: StorablePathData a => a -> a -> SegmentCommand a
smoothQuadRel = SQUAD_TO Relative

smoothCubicAbs :: StorablePathData a => a -> a -> a -> a -> SegmentCommand a
smoothCubicAbs = SCUBIC_TO Absolute

smoothCubicRel :: StorablePathData a => a -> a -> a -> a -> SegmentCommand a
smoothCubicRel = SCUBIC_TO Relative

smallCcwArcAbs :: StorablePathData a => a -> a -> a -> a -> a -> SegmentCommand a
smallCcwArcAbs = SCCWARC_TO Absolute

smallCcwArcRel :: StorablePathData a => a -> a -> a -> a -> a -> SegmentCommand a
smallCcwArcRel = SCCWARC_TO Relative

smallCwArcAbs :: StorablePathData a => a -> a -> a -> a -> a -> SegmentCommand a
smallCwArcAbs = SCWARC_TO Absolute

smallCwArcRel :: StorablePathData a => a -> a -> a -> a -> a -> SegmentCommand a
smallCwArcRel = SCWARC_TO Relative

largeCcwArcAbs :: StorablePathData a => a -> a -> a -> a -> a -> SegmentCommand a
largeCcwArcAbs = LCCWARC_TO Absolute

largeCcwArcRel :: StorablePathData a => a -> a -> a -> a -> a -> SegmentCommand a
largeCcwArcRel = LCCWARC_TO Relative

largeCwArcAbs :: StorablePathData a => a -> a -> a -> a -> a -> SegmentCommand a
largeCwArcAbs = LCWARC_TO Absolute

largeCwArcRel :: StorablePathData a => a -> a -> a -> a -> a -> SegmentCommand a
largeCwArcRel = LCWARC_TO Relative


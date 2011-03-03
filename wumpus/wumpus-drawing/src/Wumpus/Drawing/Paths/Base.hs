{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Base
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Extended path type - more amenable for complex drawings than
-- the type in Wumpus-Core.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Base
  ( 

    Path
  , DPath
  , length
  , append
  , pconcat
  , line
  , curve
  , pivot
  , traceLinePoints
  , traceCurvePoints
  , curveByAngles

  , toPrimPath 

  , tipL
  , tipR

  , shortenPath
  , shortenL
  , shortenR
  , directionL
  , directionR

  , midway
  , midway_
  , atstart
  , atstart_
  , atend
  , atend_

  , PathViewL(..)
  , DPathViewL
  , PathViewR(..)
  , DPathViewR
  , PathSegment(..)
  , DPathSegment
  , pathViewL
  , pathViewR


  , roundTrail
  , roundInterior

  ) where



import Wumpus.Basic.Geometry.Base               -- package: wumpus-basic
import Wumpus.Basic.Utils.JoinList ( JoinList, ViewL(..), viewl
                                   , ViewR(..), viewr, cons, snoc, join )
import qualified Wumpus.Basic.Utils.JoinList as JL

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace

import Data.List ( foldl' ) 

import Prelude hiding ( length )

data Path u = Path { _path_length   :: u 
                   , _path_start    :: Point2 u
                   , _path_elements :: JoinList (PathSeg u)
                   , _path_end      :: Point2 u
                   }
  deriving (Eq,Show)

type DPath = Path Double

-- Annotating each segment with length is \*\* good \*\*.
-- Makes it much more efficient to find the midway point.
--
-- But what do we do about the start point:
--
-- a) put it in the segment - too much info in the type, allows 
-- consistency problems vis-a-vis gaps in the path.
--
-- b) leave it out - too little info in the type, allows 
-- consistency problems with length.
--
-- Option (a) is probably most convenient espcially as the 
-- constructors won\'t be exported.

-- Annotation is length...
-- 
data PathSeg u = LineSeg  { _line_length  :: u 
                          , _line_start   :: Point2 u
                          , _line_end     :: Point2 u
                          }
               | CurveSeg { _curve_length :: u 
                          , _curve_start  :: Point2 u
                          , _ctrl_pt_one  :: Point2 u
                          , _ctrl_pt_two  :: Point2 u
                          , _curve_end    :: Point2 u
                          }
  deriving (Eq,Show)




--------------------------------------------------------------------------------

instance Functor Path where
  fmap f (Path u sp ls ep) = Path (f u) (fmap f sp) (fmap (fmap f) ls) (fmap f ep)

instance Functor PathSeg where
  fmap f (LineSeg u p0 p1)        = LineSeg (f u) (fmap f p0) (fmap f p1)  
  fmap f (CurveSeg u p0 p1 p2 p3) = 
      CurveSeg (f u) (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)

--------------------------------------------------------------------------------



infixr 1 `append`

length :: Num u => Path u -> u
length (Path u _ _ _) = u

append :: Floating u => Path u -> Path u -> Path u
append (Path len1 start1 se1 end1) (Path len2 start2 se2 end2) 
    | end1 == start2 = Path (len1+len2) start1 (se1 `join` se2) end2 
    | otherwise      = let joint     = lineSegment end1 start2
                           total_len = len1 + len2 + segmentLength joint
                       in Path total_len start1 (se1 `join` (cons joint se2)) end2 

-- CAUTION - @append@ is using Floating Point equality to see if
-- points are equal...


pconcat :: Floating u => Path u -> [Path u] -> Path u
pconcat p0 ps = foldl' append p0 ps

segmentLength :: PathSeg u -> u
segmentLength (LineSeg u _ _)       = u
segmentLength (CurveSeg u _ _ _ _)  = u


segmentStart :: PathSeg u -> Point2 u
segmentStart (LineSeg  _ p0 _)      = p0
segmentStart (CurveSeg _ p0 _ _ _)  = p0

segmentEnd :: PathSeg u -> Point2 u
segmentEnd (LineSeg  _ _ p1)        = p1
segmentEnd (CurveSeg _ _ _ _ p3)    = p3




lineSegment :: Floating u => Point2 u -> Point2 u -> PathSeg u 
lineSegment p0 p1 = let v = vlength $ pvec p0 p1 in LineSeg v p0 p1


pathZero :: Floating u => Point2 u -> Path u 
pathZero p0 = Path 0 p0 JL.empty p0
   

line :: Floating u => Point2 u -> Point2 u -> Path u 
line p0 p1 = let v = vlength $ pvec p0 p1 
             in Path v p0 (JL.one $ LineSeg v p0 p1) p1
   

curve :: (Floating u, Ord u, PtSize u)
      => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Path u 
curve p0 p1 p2 p3 = let v = bezierLength (BezierCurve p0 p1 p2 p3)
                    in Path v p0 (JL.one $ CurveSeg v p0 p1 p2 p3) p3

-- | A draw a /straight line/ of length 0 at the supplied point. 
--
-- This is /might/ be useful in concatenating curved paths
-- as it introduces and extra control point.
-- 
pivot :: Floating u => Point2 u -> Path u 
pivot p0 = Path 0 p0 (JL.one $ LineSeg 0 p0 p0) p0


-- | 'traceLinePoints' throws a runtime error if the supplied list
-- is empty. 
--
traceLinePoints :: Floating u => [Point2 u] -> Path u
traceLinePoints []       = error "traceLinePoints - empty point list."
traceLinePoints [a]      = line a a
traceLinePoints (a:b:xs) = step (line a b) b xs
  where
    step acc _ []     = acc
    step acc e (y:ys) = step (acc `append` line e y) y ys


-- | 'traceCurvePoints' consumes 4 points from the list on the 
-- intial step (start, control1, control2, end) then steps 
-- through the list taking 3 points at a time thereafter
-- (control1,control2, end). Leftover points are discarded.    
-- 
-- 'traceCurvePoints' throws a runtime error if the supplied list
-- is has less than 4 elements (start, control1, control2, end). 
--
traceCurvePoints :: (Floating u, Ord u, PtSize u) 
                 => [Point2 u] -> Path u
traceCurvePoints (a:b:c:d:xs) = step (curve a b c d) d xs
  where
    step acc p0 (x:y:z:zs) = step (acc `append` curve p0 x y z) z zs
    step acc _  _          = acc

traceCurvePoints _            = error "tracePointsCurve - less than 4 elems."


curveByAngles :: (Floating u, Ord u, PtSize u) 
              => Point2 u -> Radian -> Radian -> Point2 u -> Path u
curveByAngles start cin cout end = curve start (start .+^ v1) (end .+^ v2) end
  where
    sz     = 0.375 * (vlength $ pvec start end)
    v1     = avec cin  sz
    v2     = avec cout sz



-- | Turn a Path into an ordinary PrimPath.
--
-- Assumes path is properly formed - i.e. end point of one 
-- segment is the same point as the start point of the next
-- segment.
--
toPrimPath :: PtSize u => Path u -> PrimPath
toPrimPath (Path _ start segs _) = step1 $ viewl segs
  where
    step1 EmptyL                  = emptyPath start
    step1 (e :< se)               = let (p0,a) = seg1 e in 
                                    primPath p0 $ a : step2 (viewl se)

    step2 EmptyL                  = []
    step2 (e :< se)               = seg2 e : step2 (viewl se)
    
    seg1 (LineSeg  _ p0 p1)       = (p0, lineTo p1)
    seg1 (CurveSeg _ p0 p1 p2 p3) = (p0, curveTo p1 p2 p3)
 
    seg2 (LineSeg  _ _  p1)       = lineTo p1
    seg2 (CurveSeg _ _  p1 p2 p3) = curveTo p1 p2 p3




--------------------------------------------------------------------------------
-- tips 

tipL :: Path u -> Point2 u
tipL (Path _ sp _ _) = sp


tipR :: Path u -> Point2 u
tipR (Path _ _ _ ep) = ep




-- | 'sortenPath' : @ left_dist * right_dist * path -> Path @
--
shortenPath :: (Real u , Floating u) => u  -> u -> Path u -> Path u
shortenPath l r = shortenL l .  shortenR r 


--------------------------------------------------------------------------------
-- shorten from the left...

-- | Note - shortening a line from the left by 
-- greater-than-or-equal its length is operationally equivalent 
-- to making a zero-length line at the end point.
--
shortenL :: (Real u, Floating u) => u -> Path u -> Path u
shortenL n (Path u _ segs ep) 
    | n >= u                  = line ep ep
    | otherwise               = step n (viewl segs)
  where
    step _ EmptyL     = line ep ep      -- should be unreachable
    step d (e :< se)  = let z  = segmentLength e in
                        case compare d z of
                          GT -> step (d-z) (viewl se)
                          EQ -> makeLeftPath (u-n) se ep
                          LT -> let e1 = shortenSegL d e
                                in Path (u-n) (segmentStart e1) (e1 `cons` se) ep


makeLeftPath :: Floating u => u -> JoinList (PathSeg u) -> Point2 u -> Path u
makeLeftPath u se ep = 
    case viewl se of
      EmptyL   -> line ep ep
      (e :< _) -> Path u (segmentStart e) se ep


shortenSegL :: (Real u, Floating u) => u -> PathSeg u -> PathSeg u
shortenSegL n (LineSeg  u p0 p1)        = 
    LineSeg  (u-n) (shortenLineL n p0 p1) p1

shortenSegL n (CurveSeg u p0 p1 p2 p3)  = CurveSeg (u-n) q0 q1 q2 q3
  where
    (BezierCurve q0 q1 q2 q3) = snd $ subdividet (n/u) 
                                                 (BezierCurve p0 p1 p2 p3)
     


shortenLineL :: (Real u, Floating u) 
             => u -> Point2 u -> Point2 u -> Point2 u
shortenLineL n p0 p1 = p0 .+^ v
  where
    v0 = p1 .-. p0
    v  = avec (vdirection v0) n



--------------------------------------------------------------------------------
-- shorten from the right ...
 
-- | Note - shortening a line from the right by 
-- greater-than-or-equal its length is operationally equivalent 
-- to making a zero-length line at the start point.
--
shortenR :: (Real u, Floating u) => u -> Path u -> Path u
shortenR n (Path u sp segs _) 
    | n >= u                  = line sp sp
    | otherwise               = step n (viewr segs)
  where
    step _ EmptyR     = line sp sp      -- should be unreachable
    step d (se :> e)  = let z = segmentLength e in
                        case compare d z of
                          GT -> step (d-z) (viewr se)
                          EQ -> makeRightPath n sp se
                          LT -> let e1 = shortenSegR d e
                                in Path (u-n) sp (se `snoc` e1) (segmentEnd e1)
                         

makeRightPath :: Floating u => u -> Point2 u -> JoinList (PathSeg u) -> Path u
makeRightPath u sp se = 
    case viewr se of
      EmptyR   -> line sp sp
      (_ :> e) -> Path u sp se (segmentEnd e)



shortenSegR :: (Real u, Floating u) => u -> PathSeg u -> PathSeg u
shortenSegR n (LineSeg  u p0 p1)        = 
    LineSeg  (u-n) p0 (shortenLineR n p0 p1) 

shortenSegR n (CurveSeg u p0 p1 p2 p3)  = CurveSeg (u-n) q0 q1 q2 q3
  where
    (BezierCurve q0 q1 q2 q3) = fst $ subdividet ((u-n)/u) 
                                                 (BezierCurve p0 p1 p2 p3)
     


shortenLineR :: (Real u, Floating u) 
             => u -> Point2 u -> Point2 u -> Point2 u
shortenLineR n p0 p1 = p1 .+^ v
  where
    v0 = p0 .-. p1
    v  = avec (vdirection v0) n




--------------------------------------------------------------------------------
-- line direction

-- | Direction of empty path is considered to be 0.
--
directionL :: (Real u, Floating u) => Path u -> Radian
directionL (Path _ _ se _)  = step $ viewl se
  where
    step (LineSeg  _ p0 p1 :< _)      = lineDirection p1 p0  -- 1-to-0
    step (CurveSeg _ p0 p1 _ _ :< _)  = lineDirection p1 p0
    step _                            = 0       -- should be unreachable


-- | Direction of empty path is considered to be 0.
--
directionR :: (Real u, Floating u) => Path u -> Radian
directionR (Path _ _ se _) = step $ viewr se
  where
    step (_ :> LineSeg  _ p0 p1)      = lineDirection p0 p1
    step (_ :> CurveSeg _ _  _ p2 p3) = lineDirection p2 p3
    step _                            = 0       -- should be unreachable             




--------------------------------------------------------------------------------


-- Return direction as well because the calculation is expensive...
--
midway :: (Real u, Floating u) => Path u -> (Point2 u, Radian)
midway pa@(Path u sp _ _) 
    | u == 0    = (sp,0)
    | otherwise = let pa1 = shortenR (u/2) pa in (tipR pa1, directionR pa1)

-- Just the midway point.
--
midway_ :: (Real u, Floating u) => Path u -> Point2 u
midway_ = fst . midway


atstart :: (Real u, Floating u) => Path u -> (Point2 u, Radian)
atstart pa@(Path _ sp _ _) = (sp, directionL pa)

atstart_ :: Path u -> Point2 u
atstart_ (Path _ sp _ _) = sp


atend :: (Real u, Floating u) => Path u -> (Point2 u, Radian)
atend pa@(Path _ _ _ ep) = (ep, directionR pa)
 

atend_ :: Path u -> Point2 u
atend_ (Path _ _ _ ep) = ep


-- nearstart, nearend, verynear ...


--------------------------------------------------------------------------------

infixr 5 :<<
infixl 5 :>>

data PathViewL u = PathOneL (PathSegment u)
                 | PathSegment u :<< Path u
  deriving (Eq,Show) 

type DPathViewL = PathViewL Double

data PathViewR u = PathOneR (PathSegment u)
                 | Path u :>> PathSegment u
  deriving (Eq,Show) 

type DPathViewR = PathViewR Double


data PathSegment u = Line1  (Point2 u) (Point2 u)
                   | Curve1 (Point2 u) (Point2 u) (Point2 u) (Point2 u)
  deriving (Eq,Show) 

type DPathSegment = PathSegment Double


--------------------------------------------------------------------------------

instance Functor PathSegment where
  fmap f (Line1 p0 p1)        = Line1 (fmap f p0) (fmap f p1)
  fmap f (Curve1 p0 p1 p2 p3) = 
      Curve1 (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)

instance Functor PathViewL where
  fmap f (PathOneL a) = PathOneL (fmap f a)
  fmap f (a :<< as)   = fmap f a :<< fmap f as

instance Functor PathViewR where
  fmap f (PathOneR a) = PathOneR (fmap f a)
  fmap f (as :>> a)   = fmap f as :>> fmap f a

--------------------------------------------------------------------------------



pathViewL :: Num u => Path u -> PathViewL u
pathViewL (Path u _ segs ep) = go (viewl segs)
  where
    go EmptyL                   = error "pathViewL - (not) unreachable."
     
    go (LineSeg v p0 p1 :< se)
        | JL.null se            = PathOneL (Line1 p0 p1)
        | otherwise             = Line1 p0 p1 :<< Path (u-v) p1 se ep

    go (CurveSeg v p0 p1 p2 p3 :< se) 
        | JL.null se            = PathOneL (Curve1 p0 p1 p2 p3)
        | otherwise             = Curve1 p0 p1 p2 p3 :<< Path (u-v) p3 se ep


pathViewR :: Num u => Path u -> PathViewR u
pathViewR (Path u _ segs ep) = go (viewr segs)
  where
    go EmptyR                   = error "pathViewR - (not) unreachable."

    go (se :> LineSeg v p0 p1) 
        | JL.null se            = PathOneR (Line1 p0 p1)
        | otherwise             = Path (u-v) p1 se ep :>> Line1 p0 p1

    go (se :> CurveSeg v p0 p1 p2 p3) 
        | JL.null se            = PathOneR (Curve1 p0 p1 p2 p3)
        | otherwise             = Path (u-v) p3 se ep :>> Curve1 p0 p1 p2 p3




--------------------------------------------------------------------------------
-- Round corners

-- | The length of the control-point vector wants to be slighly 
-- longer than half of /d/ (d - being the distance between the 
-- /truncated/ points and the corner).
--
cornerCurve :: (Real u, Floating u, PtSize u) 
            => Point2 u -> Point2 u -> Point2 u -> Path u
cornerCurve p1 p2 p3 = curve p1 cp1 cp2 p3
  where
    len1 = 0.6 *  (vlength $ pvec p1 p2)
    len2 = 0.6 *  (vlength $ pvec p3 p2)
    cp1  = p1 .+^ (avec (lineAngle p1 p2) len1)
    cp2  = p3 .+^ (avec (lineAngle p3 p2) len2)


-- | 'roundTrail' : @ rounding_distance * [point] -> Path @
--
-- Build a path from the list of vertices, all the interior 
-- corners are rounded by the rounding distance \*and\* final 
-- round corner is created \"incorporating\" the start point (as 
-- the start point becomes a rounded corner the actual path will 
-- not intersect it). 
-- 
-- It is expected that this function will be used to create round 
-- cornered shapes.
-- 
-- 'roundTrail' throws a runtime error if the input list is empty. 
-- If the list has one element /the null path/ is built, if the 
-- list has two elements a straight line is built.
--
roundTrail :: (Real u, Floating u, PtSize u) 
           => u -> [Point2 u] -> Path u 
roundTrail _ []             = error "roundTrail - empty list."
roundTrail _ [a]            = pathZero a
roundTrail _ [a,b]          = line a b
roundTrail u (start:b:c:xs) = step (lineCurveTrail u start b c) (b:c:xs)
  where
    step acc (m:n:o:ps)     = step (acc `append` lineCurveTrail u m n o) (n:o:ps)
    step acc [n,o]          = acc `append` lineCurveTrail u n o start
                                  `append` lineCurveTrail u o start b 
    step acc _              = acc



-- | Two parts - line and corner curve...
--
-- Note - the starting point is moved, this function is for 
-- closed, rounded paths.
--
lineCurveTrail :: (Real u, Floating u, PtSize u) 
               => u -> Point2 u -> Point2 u -> Point2 u -> Path u
lineCurveTrail u a b c = line p1 p2 `append` cornerCurve p2 b p3
  where
    p1 = a .+^ (avec (vdirection $ pvec a b) u)
    p2 = b .+^ (avec (vdirection $ pvec b a) u)
    p3 = b .+^ (avec (vdirection $ pvec b c) u)


-- | 'roundInterior' : @ rounding_distance * [point] -> Path @
--
-- Build a path from the list of vertices, all the interior 
-- corners are rounded by the rounding distance. Unlike 
-- 'roundTrail' there is no /loop around/ to the start point,
-- and start path will begin exactly on the start point and end 
-- exactly on the end point. 
-- 
-- 'roundInterior' throws a runtime error if the input list is 
-- empty. If the list has one element /the null path/ is built, 
-- if the list has two elements a straight line is built.
--
roundInterior :: (Real u, Floating u, PtSize u) 
              => u -> [Point2 u] -> Path u 
roundInterior _ []             = error "roundEveryInterior - empty list."
roundInterior _ [a]            = pathZero a
roundInterior _ [a,b]          = line a b
roundInterior u (start:b:c:xs) = let (path1,p1) = lineCurveInter1 u start b c
                                 in step path1 (p1:c:xs)
  where
    step acc (m:n:o:ps)     = let (seg2,p1) = lineCurveInter1 u m n o
                              in step (acc `append` seg2) (p1:o:ps)
    step acc [n,o]          = acc `append` line n o
    step acc _              = acc



-- | Two parts - line and corner curve...
--
-- Note - draws a straight line from the starting point - this is 
-- the first step of an interior (non-closed) rounded path
--
lineCurveInter1 :: (Real u, Floating u, PtSize u) 
                => u -> Point2 u -> Point2 u -> Point2 u -> (Path u, Point2 u)
lineCurveInter1 u a b c = 
    (line a p2 `append` cornerCurve p2 b p3, p3)
  where
    p2 = b .+^ (avec (vdirection $ pvec b a) u)
    p3 = b .+^ (avec (vdirection $ pvec b c) u)
 

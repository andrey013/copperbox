{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Paths.Base
-- Copyright   :  (c) Stephen Tetley 2010
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

module Wumpus.Basic.Paths.Base
  ( 

    Path
  , DPath
  , length
  , append
  , pconcat
  , line
  , curve
  , traceLinePoints
  , traceCurvePoints
  , curveByAngles

  , toPrimPath 

  , tipL
  , tipR

  , shortenBoth
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

  ) where


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace
import Data.VectorSpace

import Data.List ( foldl' ) 
import Data.Sequence ( Seq, (><), ViewL(..), viewl
                     , ViewR(..), viewr, (<|) , (|>) )
import qualified Data.Sequence as S

import Prelude hiding ( length )

data Path u = Path { _path_length   :: u 
                   , _path_start    :: Point2 u
                   , _path_elements :: Seq (PathSeg u)
                   , _path_end      :: Point2 u
                   }
  deriving (Eq,Ord,Show)

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
  deriving (Eq,Ord,Show)


type instance DUnit (Path u)    = u
type instance DUnit (PathSeg u) = u



length :: Num u => Path u -> u
length (Path u _ _ _) = u

append :: Floating u => Path u -> Path u -> Path u
append (Path len1 start1 se1 end1) (Path len2 start2 se2 end2) 
    | end1 == start2 = Path (len1+len2) start1 (se1 >< se2) end2 
    | otherwise      = let join      = lineSegment end1 start2
                           total_len = len1 + len2 + segmentLength join
                       in Path total_len start1 (se1 >< (join <| se2)) end2 

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

line :: Floating u => Point2 u -> Point2 u -> Path u 
line p0 p1 = let v = vlength $ pvec p0 p1 
             in Path v p0 (S.singleton $ LineSeg v p0 p1) p1
   

curve :: (Floating u, Ord u)
      => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Path u 
curve p0 p1 p2 p3 = let v = curveLength p0 p1 p2 p3
                    in Path v p0 (S.singleton $ CurveSeg v p0 p1 p2 p3) p3

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
traceCurvePoints :: (Floating u, Ord u) => [Point2 u] -> Path u
traceCurvePoints (a:b:c:d:xs) = step (curve a b c d) d xs
  where
    step acc p0 (x:y:z:zs) = step (acc `append` curve p0 x y z) z zs
    step acc _  _          = acc

traceCurvePoints _            = error "tracePointsCurve - less than 4 elems."


curveByAngles :: (Floating u, Ord u) 
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
toPrimPath :: Path u -> PrimPath u
toPrimPath (Path _ _ segs _) = step1 $ viewl segs
  where
    step1 EmptyL                  = error "toPrimPath - (not) unreachable."
    step1 (e :< se)               = let (start,a) = seg1 e in 
                                    path start $ a : step2 (viewl se)

    step2 EmptyL                  = []
    step2 (e :< se)               = seg2 e : step2 (viewl se)
    
    seg1 (LineSeg  _ p0 p1)       = (p0, lineTo p1)
    seg1 (CurveSeg _ p0 p1 p2 p3) = (p0, curveTo p1 p2 p3)
 
    seg2 (LineSeg  _ _  p1)       = lineTo p1
    seg2 (CurveSeg _ _  p1 p2 p3) = curveTo p1 p2 p3



--------------------------------------------------------------------------------
-- Curve length

data StrictCurve u = Curve !(Point2 u) !(Point2 u) !(Point2 u) !(Point2 u)

curveLength :: (Floating u, Ord u)      
            => Point2 u -> Point2 u -> Point2 u -> Point2 u -> u
curveLength p0 p1 p2 p3 = gravesenLength 0.1 $ Curve p0 p1 p2 p3


-- | Jens Gravesen\'s bezier arc-length approximation. 
--
-- Note this implementation is parametrized on error tolerance.
--
gravesenLength :: (Floating u, Ord u) => u -> StrictCurve u -> u
gravesenLength err_tol crv = step crv where
  step c = let l1 = ctrlPolyLength c
               l0 = cordLength c
           in if   l1-l0 > err_tol
              then let (a,b) = subdivide c in step a + step b
              else 0.5*l0 + 0.5*l1


ctrlPolyLength :: Floating u => StrictCurve u -> u
ctrlPolyLength (Curve p0 p1 p2 p3) = len p0 p1 + len p1 p2 + len p2 p3
  where
    len pa pb = vlength $ pvec pa pb

cordLength :: Floating u => StrictCurve u -> u
cordLength (Curve p0 _ _ p3) = vlength $ pvec p0 p3


-- | mid-point between two points
--
pointMidpoint :: Fractional u => Point2 u -> Point2 u -> Point2 u
pointMidpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0


-- | Curve subdivision via de Casteljau\'s algorithm.
--
subdivide :: Fractional u 
          => StrictCurve u -> (StrictCurve u, StrictCurve u)
subdivide (Curve p0 p1 p2 p3) =
    (Curve p0 p01 p012 p0123, Curve p0123 p123 p23 p3)
  where
    p01   = pointMidpoint p0    p1
    p12   = pointMidpoint p1    p2
    p23   = pointMidpoint p2    p3
    p012  = pointMidpoint p01   p12
    p123  = pointMidpoint p12   p23
    p0123 = pointMidpoint p012  p123

-- | subdivide with an affine weight along the line...
--
subdividet :: Real u
           => u -> StrictCurve u -> (StrictCurve u, StrictCurve u)
subdividet t (Curve p0 p1 p2 p3) = 
    (Curve p0 p01 p012 p0123, Curve p0123 p123 p23 p3)
  where
    p01   = affineCombination t p0    p1
    p12   = affineCombination t p1    p2
    p23   = affineCombination t p2    p3
    p012  = affineCombination t p01   p12
    p123  = affineCombination t p12   p23
    p0123 = affineCombination t p012  p123

affineCombination :: Real u => u -> Point2 u -> Point2 u -> Point2 u
affineCombination a p1 p2 = p1 .+^ a *^ (p2 .-. p1)

--------------------------------------------------------------------------------
-- tips 

tipL :: Path u -> Point2 u
tipL (Path _ sp _ _) = sp


tipR :: Path u -> Point2 u
tipR (Path _ _ _ ep) = ep


-- | Shorten both ends...
--
-- u should be less-than half the path length
--
shortenBoth :: (Real u, Floating u) => u -> Path u -> Path u
shortenBoth u p = shortenL u $ shortenR u p

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
                                in Path (u-n) (segmentStart e1) (e1 <| se) ep


makeLeftPath :: Floating u => u -> Seq (PathSeg u) -> Point2 u -> Path u
makeLeftPath u se ep = 
    case viewl se of
      EmptyL   -> line ep ep
      (e :< _) -> Path u (segmentStart e) se ep


shortenSegL :: (Real u, Floating u) => u -> PathSeg u -> PathSeg u
shortenSegL n (LineSeg  u p0 p1)        = 
    LineSeg  (u-n) (shortenLineL n p0 p1) p1

shortenSegL n (CurveSeg u p0 p1 p2 p3)  = 
    let (Curve p0' p1' p2' p3') = snd $ subdividet (n/u) (Curve p0 p1 p2 p3)
    in CurveSeg (u-n) p0' p1' p2' p3'


shortenLineL :: (Real u, Floating u) 
             => u -> Point2 u -> Point2 u -> Point2 u
shortenLineL n p0 p1 = p0 .+^ v
  where
    v0 = p1 .-. p0
    v  = avec (direction v0) n



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
                                in Path (u-n) sp (se |> e1) (segmentEnd e1)
                         

makeRightPath :: Floating u => u -> Point2 u -> Seq (PathSeg u) -> Path u
makeRightPath u sp se = 
    case viewr se of
      EmptyR   -> line sp sp
      (_ :> e) -> Path u sp se (segmentEnd e)



shortenSegR :: (Real u, Floating u) => u -> PathSeg u -> PathSeg u
shortenSegR n (LineSeg  u p0 p1)        = 
    LineSeg  (u-n) p0 (shortenLineR n p0 p1) 

shortenSegR n (CurveSeg u p0 p1 p2 p3)  = 
    let (Curve p0' p1' p2' p3') = fst $ subdividet ((u-n)/u) (Curve p0 p1 p2 p3)
    in CurveSeg (u-n) p0' p1' p2' p3'


shortenLineR :: (Real u, Floating u) 
             => u -> Point2 u -> Point2 u -> Point2 u
shortenLineR n p0 p1 = p1 .+^ v
  where
    v0 = p0 .-. p1
    v  = avec (direction v0) n




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

data PathViewL u = PathOneL (PathSegment u)
                 | PathSegment u :<< Path u
  deriving (Eq,Ord,Show) 

type DPathViewL = PathViewL Double

data PathViewR u = PathOneR (PathSegment u)
                 | Path u :>> PathSegment u
  deriving (Eq,Ord,Show) 

type DPathViewR = PathViewR Double


data PathSegment u = Line1  (Point2 u) (Point2 u)
                   | Curve1 (Point2 u) (Point2 u) (Point2 u) (Point2 u)
  deriving (Eq,Ord,Show) 

type DPathSegment = PathSegment Double

type instance DUnit (PathViewL u)   = u
type instance DUnit (PathViewR u)   = u
type instance DUnit (PathSegment u) = u

pathViewL :: Num u => Path u -> PathViewL u
pathViewL (Path u _ segs ep) = go (viewl segs)
  where
    go EmptyL                   = error "pathViewL - (not) unreachable."
     
    go (LineSeg v p0 p1 :< se)
        | S.null se             = PathOneL (Line1 p0 p1)
        | otherwise             = Line1 p0 p1 :<< Path (u-v) p1 se ep

    go (CurveSeg v p0 p1 p2 p3 :< se) 
        | S.null se             = PathOneL (Curve1 p0 p1 p2 p3)
        | otherwise             = Curve1 p0 p1 p2 p3 :<< Path (u-v) p3 se ep


pathViewR :: Num u => Path u -> PathViewR u
pathViewR (Path u _ segs ep) = go (viewr segs)
  where
    go EmptyR                   = error "pathViewR - (not) unreachable."

    go (se :> LineSeg v p0 p1) 
        | S.null se             = PathOneR (Line1 p0 p1)
        | otherwise             = Path (u-v) p1 se ep :>> Line1 p0 p1

    go (se :> CurveSeg v p0 p1 p2 p3) 
        | S.null se             = PathOneR (Curve1 p0 p1 p2 p3)
        | otherwise             = Path (u-v) p3 se ep :>> Curve1 p0 p1 p2 p3


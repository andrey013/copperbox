{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Base.AbsPath
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

module Wumpus.Drawing.Paths.Base.AbsPath
  ( 

    AbsPath
  , DAbsPath

  , null
  , empty

  , length
  , append
  , consLineTo
  , snocLineTo

  , pconcat
  , line
  , curve
  , pivot
  , vertexPath
  , curvePath
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
import Wumpus.Basic.Kernel
import Wumpus.Basic.Utils.JoinList ( JoinList, ViewL(..), viewl
                                   , ViewR(..), viewr, cons, snoc, join )
import qualified Wumpus.Basic.Utils.JoinList as JL

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace

import Data.List ( foldl' ) 
import qualified Data.Traversable as T

import Prelude hiding ( null, length )




-- | Absolute path data type.

data AbsPath u = AbsPath 
      { _abs_path_length   :: u 
      , _abs_path_start    :: Point2 u
      , _abs_path_elements :: JoinList (AbsPathSeg u)
      , _abs_path_end      :: Point2 u
      }
  deriving (Eq,Show)

type instance DUnit (AbsPath u) = u


type DAbsPath = AbsPath Double

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
data AbsPathSeg u = AbsLineSeg  
                        { _abs_line_length  :: u 
                        , _abs_line_start   :: Point2 u
                        , _abs_line_end     :: Point2 u
                        }
                  | AbsCurveSeg 
                        { _abs_curve_length :: u 
                        , _abs_curve_start  :: Point2 u
                        , _abs_ctrl_pt_one  :: Point2 u
                        , _abs_ctrl_pt_two  :: Point2 u
                        , _abs_curve_end    :: Point2 u
                        }
  deriving (Eq,Show)


type instance DUnit (AbsPathSeg u) = u


--------------------------------------------------------------------------------

instance Functor AbsPath where
  fmap f (AbsPath u sp ls ep) = 
      AbsPath (f u) (fmap f sp) (fmap (fmap f) ls) (fmap f ep)

instance Functor AbsPathSeg where
  fmap f (AbsLineSeg u p0 p1)        = 
      AbsLineSeg (f u) (fmap f p0) (fmap f p1)  

  fmap f (AbsCurveSeg u p0 p1 p2 p3) = 
      AbsCurveSeg (f u) (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)


--------------------------------------------------------------------------------

-- TODO - must organize the Path datatype modules so they provide
-- Containers-like API functions when appropriate.

null :: AbsPath u -> Bool
null = JL.null . _abs_path_elements


-- | Note - an empty path always needs a start point.
--
empty :: Floating u => Point2 u -> AbsPath u
empty = zeroPath


length :: Num u => AbsPath u -> u
length (AbsPath u _ _ _) = u



infixr 1 `append`

-- CAUTION - @append@ is using Floating Point equality to see if
-- points are equal...

append :: Floating u => AbsPath u -> AbsPath u -> AbsPath u
append (AbsPath len1 start1 se1 end1) (AbsPath len2 start2 se2 end2) 
    | end1 == start2 = AbsPath (len1+len2) start1 (se1 `join` se2) end2 
    | otherwise      = let joint     = lineSegment end1 start2
                           total_len = len1 + len2 + segmentLength joint
                           segs      = se1 `join` (cons joint se2)
                       in AbsPath total_len start1 segs end2 


consLineTo :: Floating u => Point2 u -> AbsPath u -> AbsPath u
consLineTo p0 (AbsPath len1 sp se1 ep) = AbsPath (v+len1) p0 (cons s1 se1) ep
  where
    s1@(AbsLineSeg v _ _) = lineSegment p0 sp  


snocLineTo :: Floating u => AbsPath u -> Point2 u -> AbsPath u
snocLineTo (AbsPath len1 sp se1 ep) p1 = AbsPath (len1+v) sp (snoc se1 s1) p1
  where
    s1@(AbsLineSeg v _ _) = lineSegment ep p1


-- consCurveTo :: 


pconcat :: Floating u => AbsPath u -> [AbsPath u] -> AbsPath u
pconcat p0 ps = foldl' append p0 ps

segmentLength :: AbsPathSeg u -> u
segmentLength (AbsLineSeg u _ _)       = u
segmentLength (AbsCurveSeg u _ _ _ _)  = u


segmentStart :: AbsPathSeg u -> Point2 u
segmentStart (AbsLineSeg  _ p0 _)      = p0
segmentStart (AbsCurveSeg _ p0 _ _ _)  = p0

segmentEnd :: AbsPathSeg u -> Point2 u
segmentEnd (AbsLineSeg  _ _ p1)        = p1
segmentEnd (AbsCurveSeg _ _ _ _ p3)    = p3




lineSegment :: Floating u => Point2 u -> Point2 u -> AbsPathSeg u 
lineSegment p0 p1 = let v = vlength $ pvec p0 p1 in AbsLineSeg v p0 p1


zeroPath :: Floating u => Point2 u -> AbsPath u 
zeroPath p0 = AbsPath 0 p0 JL.empty p0
   

line :: Floating u => Point2 u -> Point2 u -> AbsPath u 
line p0 p1 = AbsPath v p0 (JL.one $ AbsLineSeg v p0 p1) p1
  where
    v = vlength $ pvec p0 p1 


curve :: (Floating u, Ord u, Tolerance u)
      => Point2 u -> Point2 u -> Point2 u -> Point2 u -> AbsPath u 
curve p0 p1 p2 p3 = AbsPath v p0 (JL.one $ AbsCurveSeg v p0 p1 p2 p3) p3
  where
    v = bezierLength (BezierCurve p0 p1 p2 p3)

-- | A draw a /straight line/ of length 0 at the supplied point. 
--
-- In som ecircumstances, this is may be useful for concatenating 
-- curved paths as it introduces and extra control point.
-- 
pivot :: Floating u => Point2 u -> AbsPath u 
pivot p0 = AbsPath 0 p0 (JL.one $ AbsLineSeg 0 p0 p0) p0


-- | 'vertexPath' throws a runtime error if the supplied list
-- is empty. 
--
vertexPath :: Floating u => [Point2 u] -> AbsPath u
vertexPath []       = error "traceLinePoints - empty point list."
vertexPath [a]      = line a a
vertexPath (a:b:xs) = step (line a b) b xs
  where
    step acc _ []     = acc
    step acc e (y:ys) = step (acc `append` line e y) y ys


-- | 'curvePath' consumes 4 points from the list on the 
-- intial step (start, control1, control2, end) then steps 
-- through the list taking 3 points at a time thereafter
-- (control1,control2, end). Leftover points are discarded.    
-- 
-- 'curvePath' throws a runtime error if the supplied list
-- is has less than 4 elements (start, control1, control2, end). 
--
curvePath :: (Floating u, Ord u, Tolerance u) 
          => [Point2 u] -> AbsPath u
curvePath (a:b:c:d:xs) = step (curve a b c d) d xs
  where
    step acc p0 (x:y:z:zs) = step (acc `append` curve p0 x y z) z zs
    step acc _  _          = acc

curvePath _            = error "curvePath - less than 4 elems."


curveByAngles :: (Floating u, Ord u, Tolerance u) 
              => Point2 u -> Radian -> Radian -> Point2 u -> AbsPath u
curveByAngles start cin cout end = 
    curve start (start .+^ v1) (end .+^ v2) end
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
toPrimPath :: InterpretUnit u => AbsPath u -> Query PrimPath
toPrimPath (AbsPath _ start segs _) = 
    uconvertCtxF start       >>= \dstart -> 
    T.mapM uconvertCtxF segs >>= \dsegs  ->
    return $ step1 dstart (viewl dsegs)
  where
    step1 p0 EmptyL                   = emptyPrimPath p0
    step1 _  (e :< se)                = let (p0,a) = seg1 e
                                            rest   = step2 (viewl se)
                                        in absPrimPath p0 (a:rest)

    step2 EmptyL                      = []
    step2 (e :< se)                   = seg2 e : step2 (viewl se)
    
    seg1 (AbsLineSeg  _ p0 p1)        = (p0, absLineTo p1)
    seg1 (AbsCurveSeg _ p0 p1 p2 p3)  = (p0, absCurveTo p1 p2 p3)
    
    seg2 (AbsLineSeg  _ _ p1)         = absLineTo  p1
    seg2 (AbsCurveSeg _ _ p1 p2 p3)   = absCurveTo p1 p2 p3



--------------------------------------------------------------------------------
-- tips 

tipL :: AbsPath u -> Point2 u
tipL (AbsPath _ sp _ _) = sp


tipR :: AbsPath u -> Point2 u
tipR (AbsPath _ _ _ ep) = ep




-- | 'sortenPath' : @ left_dist * right_dist * path -> Path @
--
shortenPath :: (Real u , Floating u) 
            => u  -> u -> AbsPath u -> AbsPath u
shortenPath l r = shortenL l .  shortenR r 


--------------------------------------------------------------------------------
-- shorten from the left...

-- | Note - shortening a line from the left by 
-- greater-than-or-equal its length is operationally equivalent 
-- to making a zero-length line at the end point.
--
shortenL :: (Real u, Floating u) => u -> AbsPath u -> AbsPath u
shortenL n (AbsPath u _ segs ep) 
    | n >= u                  = line ep ep
    | otherwise               = step n (viewl segs)
  where
    step _ EmptyL     = line ep ep      -- should be unreachable
    step d (e :< se)  = let z  = segmentLength e in
                        case compare d z of
                          GT -> step (d-z) (viewl se)
                          EQ -> makeLeftPath (u-n) se ep
                          LT -> let e1 = shortenSegL d e
                                    sp = segmentStart e1
                                in AbsPath (u-n) sp (e1 `cons` se) ep


makeLeftPath :: Floating u 
             => u -> JoinList (AbsPathSeg u) -> Point2 u -> AbsPath u
makeLeftPath u se ep = 
    case viewl se of
      EmptyL   -> line ep ep
      (e :< _) -> AbsPath u (segmentStart e) se ep


shortenSegL :: (Real u, Floating u) 
            => u -> AbsPathSeg u -> AbsPathSeg u
shortenSegL n (AbsLineSeg  u p0 p1)        = 
    AbsLineSeg  (u-n) (shortenLineL n p0 p1) p1

shortenSegL n (AbsCurveSeg u p0 p1 p2 p3)  = 
    AbsCurveSeg (u-n) q0 q1 q2 q3
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
shortenR :: (Real u, Floating u) => u -> AbsPath u -> AbsPath u
shortenR n (AbsPath u sp segs _) 
    | n >= u                  = line sp sp
    | otherwise               = step n (viewr segs)
  where
    step _ EmptyR     = line sp sp      -- should be unreachable
    step d (se :> e)  = let z = segmentLength e in
                        case compare d z of
                          GT -> step (d-z) (viewr se)
                          EQ -> makeRightPath n sp se
                          LT -> let e1 = shortenSegR d e
                                    ep = segmentEnd e1
                                in AbsPath (u-n) sp (se `snoc` e1) ep
                         

makeRightPath :: Floating u 
              => u -> Point2 u -> JoinList (AbsPathSeg u) -> AbsPath u
makeRightPath u sp se = 
    case viewr se of
      EmptyR   -> line sp sp
      (_ :> e) -> AbsPath u sp se (segmentEnd e)



shortenSegR :: (Real u, Floating u) 
            => u -> AbsPathSeg u -> AbsPathSeg u
shortenSegR n (AbsLineSeg  u p0 p1)        = 
    AbsLineSeg  (u-n) p0 (shortenLineR n p0 p1) 

shortenSegR n (AbsCurveSeg u p0 p1 p2 p3)  = AbsCurveSeg (u-n) q0 q1 q2 q3
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
directionL :: (Real u, Floating u) => AbsPath u -> Radian
directionL (AbsPath _ _ se _)  = step $ viewl se
  where
    step (AbsLineSeg  _ p0 p1 :< _)      = lineDirection p1 p0  -- 1-to-0
    step (AbsCurveSeg _ p0 p1 _ _ :< _)  = lineDirection p1 p0
    step _                               = 0       -- should be unreachable


-- | Direction of empty path is considered to be 0.
--
directionR :: (Real u, Floating u) => AbsPath u -> Radian
directionR (AbsPath _ _ se _) = step $ viewr se
  where
    step (_ :> AbsLineSeg  _ p0 p1)      = lineDirection p0 p1
    step (_ :> AbsCurveSeg _ _  _ p2 p3) = lineDirection p2 p3
    step _                               = 0       -- should be unreachable             
 



--------------------------------------------------------------------------------


-- Return direction as well because the calculation is expensive...
--
midway :: (Real u, Floating u) => AbsPath u -> (Point2 u, Radian)
midway pa@(AbsPath u sp _ _) 
    | u == 0    = (sp,0)
    | otherwise = let pa1 = shortenR (u/2) pa in (tipR pa1, directionR pa1)

-- Just the midway point.
--
midway_ :: (Real u, Floating u) => AbsPath u -> Point2 u
midway_ = fst . midway


atstart :: (Real u, Floating u) => AbsPath u -> (Point2 u, Radian)
atstart pa@(AbsPath _ sp _ _) = (sp, directionL pa)

atstart_ :: AbsPath u -> Point2 u
atstart_ (AbsPath _ sp _ _) = sp


atend :: (Real u, Floating u) => AbsPath u -> (Point2 u, Radian)
atend pa@(AbsPath _ _ _ ep) = (ep, directionR pa)
 

atend_ :: AbsPath u -> Point2 u
atend_ (AbsPath _ _ _ ep) = ep


-- nearstart, nearend, verynear ...


--------------------------------------------------------------------------------

infixr 5 :<<
infixl 5 :>>

data PathViewL u = PathOneL (PathSegment u)
                 | PathSegment u :<< AbsPath u
  deriving (Eq,Show) 

type DPathViewL = PathViewL Double

data PathViewR u = PathOneR (PathSegment u)
                 | AbsPath u :>> PathSegment u
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



pathViewL :: Num u => AbsPath u -> PathViewL u
pathViewL (AbsPath u _ segs ep) = go (viewl segs)
  where
    go EmptyL                   = error "pathViewL - (not) unreachable."
     
    go (AbsLineSeg v p0 p1 :< se)
        | JL.null se            = PathOneL (Line1 p0 p1)
        | otherwise             = Line1 p0 p1 :<< AbsPath (u-v) p1 se ep

    go (AbsCurveSeg v p0 p1 p2 p3 :< se) 
        | JL.null se            = PathOneL (Curve1 p0 p1 p2 p3)
        | otherwise             = Curve1 p0 p1 p2 p3 :<< AbsPath (u-v) p3 se ep


pathViewR :: Num u => AbsPath u -> PathViewR u
pathViewR (AbsPath u _ segs ep) = go (viewr segs)
  where
    go EmptyR                   = error "pathViewR - (not) unreachable."

    go (se :> AbsLineSeg v p0 p1) 
        | JL.null se            = PathOneR (Line1 p0 p1)
        | otherwise             = AbsPath (u-v) p1 se ep :>> Line1 p0 p1

    go (se :> AbsCurveSeg v p0 p1 p2 p3) 
        | JL.null se            = PathOneR (Curve1 p0 p1 p2 p3)
        | otherwise             = AbsPath (u-v) p3 se ep :>> Curve1 p0 p1 p2 p3




--------------------------------------------------------------------------------
-- Round corners

-- | The length of the control-point vector wants to be slighly 
-- longer than half of /d/ (d - being the distance between the 
-- /truncated/ points and the corner).
--
cornerCurve :: (Real u, Floating u, Tolerance u) 
            => Point2 u -> Point2 u -> Point2 u -> AbsPath u
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
roundTrail :: (Real u, Floating u, Tolerance u) 
           => u -> [Point2 u] -> AbsPath u 
roundTrail _ []             = error "roundTrail - empty list."
roundTrail _ [a]            = zeroPath a
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
lineCurveTrail :: (Real u, Floating u, Tolerance u) 
               => u -> Point2 u -> Point2 u -> Point2 u -> AbsPath u
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
roundInterior :: (Real u, Floating u, Tolerance u) 
              => u -> [Point2 u] -> AbsPath u 
roundInterior _ []             = error "roundEveryInterior - empty list."
roundInterior _ [a]            = zeroPath a
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
lineCurveInter1 :: (Real u, Floating u, Tolerance u) 
                => u -> Point2 u -> Point2 u -> Point2 u 
                -> (AbsPath u, Point2 u)
lineCurveInter1 u a b c = 
    (line a p2 `append` cornerCurve p2 b p3, p3)
  where
    p2 = b .+^ (avec (vdirection $ pvec b a) u)
    p3 = b .+^ (avec (vdirection $ pvec b c) u)
 

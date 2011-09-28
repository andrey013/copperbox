{-# LANGUAGE TypeFamilies               #-}
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
-- Absolute path type - this should be more amenable for building 
-- complex drawings than the PrimPath type in Wumpus-Core.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Base
  ( 

  -- * Absolute path type
    AbsPath
  , DAbsPath


  -- * Construction
  , emptyPath
  , line1
  , curve1
  , vertexPath
  , curvePath
  , controlCurve

  , vectorPath
  , vectorPathTheta

  , anaTrailPath
  , catTrailPath

  -- * Queries
  , null
  , length

  -- * Concat and extension
  , snocLine
  , snocLineTo
  , snocCurve
  , snocCurveTo


  -- * Conversion
  , toPrimPath

  , drawPath
  , drawPath_

  -- * Shortening
  , shortenPath
  , shortenL
  , shortenR

  -- * Tips and direction
  , tipL
  , tipR
  , directionL
  , directionR
  , isBezierL
  , isBezierR


  -- * Path anchors
  , midway
  , midway_
  , atstart
  , atstart_
  , atend
  , atend_


  -- * Views
  , PathViewL(..)
  , DPathViewL
  , PathViewR(..)
  , DPathViewR
  , PathSegment(..)
  , DPathSegment
  , pathViewL
  , pathViewR

  , optimizeLines

  , roundExterior
  , roundInterior

  -- * Path division
  , pathdiv


  ) where



import Wumpus.Basic.Geometry.Base               -- package: wumpus-basic
import Wumpus.Basic.Kernel
import Wumpus.Basic.Utils.JoinList ( JoinList, ViewL(..), viewl
                                   , ViewR(..), viewr, cons, snoc )
import qualified Wumpus.Basic.Utils.JoinList as JL

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Monoid
import qualified Data.Traversable as T

import Prelude hiding ( null, length )

--
-- Design Note
-- 
-- Wumpus has no relative path type. An AbsPath is expected to be 
-- an /answer/ from some function like an anchor point or a 
-- bounding box.
--
-- Wumpus didn\'t always work on this premise, some of the code 
-- below may contradict this...
--





-- | Absolute path data type.
--
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
data AbsPathSeg u = AbsLineSeg  u (Vec2 u)
                  | AbsCurveSeg u (Vec2 u) (Vec2 u) (Vec2 u)
  deriving (Eq,Show)


type instance DUnit (AbsPathSeg u) = u

-- 
-- DESIGN NOTE
--
-- No monoid instance. AbsPaths dont support empty or even concat 
-- naturally.
--
-- Concat is troublesome because AbsPaths are always located 
-- within a frame.
--
-- They can support:
--
-- a. Concat with a join between the end of the first path and the
-- start of the second.
-- 
-- b. Shape preserving concat - the second path is moved so it 
-- continues from the end point of the first path.
--
-- It is conceptually simpler to think about extension (adding to 
-- the path tip) rather than concatenation. 
--

--------------------------------------------------------------------------------

instance Functor AbsPath where
  fmap f (AbsPath u sp ls ep) = 
      AbsPath (f u) (fmap f sp) (fmap (fmap f) ls) (fmap f ep)

instance Functor AbsPathSeg where
  fmap f (AbsLineSeg u v1)        = 
      AbsLineSeg (f u) (fmap f v1)

  fmap f (AbsCurveSeg u v1 v2 v3) = 
      AbsCurveSeg (f u) (fmap f v1) (fmap f v2) (fmap f v3)



--------------------------------------------------------------------------------
-- Translate

-- Translate is cheap on AbsPath

instance Num u => Translate (AbsPath u) where
  translate x y (AbsPath len sp se ep) = 
      AbsPath len (translate x y sp) se (translate x y ep)



--------------------------------------------------------------------------------
-- Construction

-- | Create the empty path.
-- 
-- Note - an absolute path needs /locating/ and cannot be built 
-- without a start point. Figuratively, the empty path is a path
-- from the start point to the end point.
--
-- Thus AbsPath operates as a semigroup but not a monoid.
--
emptyPath :: Floating u => Point2 u -> AbsPath u
emptyPath = zeroPath


-- | Create an absolute path as a straight line between the 
-- supplied points.
--
line1 :: Floating u => Point2 u -> Point2 u -> AbsPath u 
line1 p0 p1 = AbsPath len p0 (JL.one $ AbsLineSeg len v1) p1
  where
    v1  = pvec p0 p1
    len = vlength v1

-- | Create an absolute path from a single cubic Bezier curve.
--
curve1 :: (Floating u, Ord u, Tolerance u)
      => Point2 u -> Point2 u -> Point2 u -> Point2 u -> AbsPath u 
curve1 p0 p1 p2 p3 = 
    AbsPath len p0 (JL.one $ AbsCurveSeg len v1 v2 v3) p3
  where
    v1  = pvec p0 p1
    v2  = pvec p1 p2
    v3  = pvec p2 p3
    len = bezierLength (BezierCurve p0 p1 p2 p3)


-- | 'vertexPath' throws a runtime error if the supplied list
-- is empty. 
--
vertexPath :: (Floating u, Ord u, Tolerance u) 
           => [Point2 u] -> AbsPath u
vertexPath []       = error "traceLinePoints - empty point list."
vertexPath [a]      = line1 a a
vertexPath (a:b:xs) = step (line1 a b) xs
  where
    step acc []     = acc
    step acc (y:ys) = step (snocLineTo acc y) ys



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
curvePath (a:b:c:d:xs) = step (curve1 a b c d) xs
  where
    step acc (x:y:z:zs) = step (snocCurveTo acc (x,y,z)) zs
    step acc _          = acc

curvePath _            = error "curvePath - less than 4 elems."



-- NOTE - need a proper arc path builder.



-- | This is not an arc...
-- 
controlCurve :: (Floating u, Ord u, Tolerance u) 
         => Point2 u -> Radian -> Radian -> Point2 u -> AbsPath u
controlCurve start cin cout end = 
    curve1 start (start .+^ v1) (end .+^ v2) end
  where
    sz     = 0.375 * (vlength $ pvec start end)
    v1     = avec cin  sz
    v2     = avec cout sz





vectorPath :: (Floating u, Ord u, Tolerance u) 
           => [Vec2 u] -> Point2 u -> AbsPath u
vectorPath vecs p0 = vertexPath $ p0 : step p0 vecs
  where
    step _ []       = []
    step pt (v1:vs) = let p1 = pt .+^ v1 in p1 : step p1 vs 


vectorPathTheta :: (Real u, Floating u, Tolerance u) 
                => [Vec2 u] -> Radian -> Point2 u -> AbsPath u 
vectorPathTheta vs ang = vectorPath $ map (rotate ang) vs


anaTrailPath :: (Floating u, Ord u, Tolerance u) 
             => Point2 u -> AnaTrail u -> AbsPath u
anaTrailPath pt trl = 
    let (v1,ss) = destrAnaTrail trl in step (emptyPath $ pt .+^ v1) ss
  where
    step ac []                   = ac
    step ac (TLine v1:xs)        = step (ac `snocLine` v1) xs
    step ac (TCurve v1 v2 v3:xs) = step (snocCurve ac (v1,v2,v3)) xs
  
catTrailPath :: (Floating u, Ord u, Tolerance u) 
             => Point2 u -> CatTrail u -> AbsPath u
catTrailPath pt trl = step (emptyPath pt) $ destrCatTrail trl
  where
    step ac []                   = ac
    step ac (TLine v1:xs)        = step (ac `snocLine` v1) xs
    step ac (TCurve v1 v2 v3:xs) = step (snocCurve ac (v1,v2,v3)) xs
  

--------------------------------------------------------------------------------
-- Queries

-- | Is the path empty?
--
null :: AbsPath u -> Bool
null = JL.null . _abs_path_elements

-- | Length of the Path.
--
-- Length is the length of the path as it is drawn, it is not a 
-- count of the number or path segments.
--
-- Length is cached so this operation is cheap - though this puts
-- a tax on the build operations. 
-- 
length :: Num u => AbsPath u -> u
length (AbsPath u _ _ _) = u


--------------------------------------------------------------------------------
-- Extension

infixl 5 `snocLine`


-- | Extend the path with a straight line segment from the 
-- end-point defined by the supplied vector.
--
-- > infixl 5 `snocLine`
--
snocLine :: Floating u => AbsPath u -> Vec2 u -> AbsPath u
snocLine (AbsPath u sp se ep) v1 = 
  let u1        = vlength v1 
      tail_line = AbsLineSeg u1 v1
  in AbsPath (u + u1) sp (JL.snoc se tail_line) (ep .+^ v1)



infixl 5 `snocLineTo`


-- | Extend the path with a straight line segment from the 
-- end-point to the supplied point.
--
-- > infixl 5 `snocLineTo`
--
snocLineTo :: Floating u => AbsPath u -> Point2 u -> AbsPath u
snocLineTo (AbsPath u sp se1 ep) p1 = AbsPath (u + len) sp (snoc se1 s1) p1
  where
    s1@(AbsLineSeg len _) = lineSegment ep p1


infixl 5 `snocCurve`

-- | Extend the path from the end-point with a Bezier curve 
-- segment formed by the supplied points.
--
-- > infixl 5 `snocCurve`
-- 
snocCurve :: (Floating u, Ord u, Tolerance u)
          => AbsPath u -> (Vec2 u, Vec2 u, Vec2 u) -> AbsPath u
snocCurve absp@(AbsPath _ _ _ ep) (v1,v2,v3) = snocCurveTo absp (p1,p2,p3)
  where
    p1 = ep .+^ v1
    p2 = p1 .+^ v2
    p3 = p2 .+^ v3
 

infixl 5 `snocCurveTo`


-- | Extend the path from the end-point with a Bezier curve 
-- segment formed by the supplied points.
--
-- > infixl 5 `snocCurveTo`
-- 
snocCurveTo :: (Floating u, Ord u, Tolerance u)
            => AbsPath u -> (Point2 u, Point2 u, Point2 u) -> AbsPath u
snocCurveTo (AbsPath u sp se1 ep) (p1,p2,p3) = 
    AbsPath (u + len) sp (snoc se1 s1) p3
  where
    s1@(AbsCurveSeg len _ _ _) = curveSegment ep p1 p2 p3
 

-------------------------------------------------------------------------------- 



segmentLength :: AbsPathSeg u -> u
segmentLength (AbsLineSeg u _)       = u
segmentLength (AbsCurveSeg u _ _ _)  = u


segmentVector :: Num u => AbsPathSeg u -> Vec2 u
segmentVector (AbsLineSeg  _ v1)       = v1
segmentVector (AbsCurveSeg _ v1 v2 v3) = v1 ^+^ v2 ^+^ v3



-- | Helper - construct a line segment.
-- 
lineSegment :: Floating u => Point2 u -> Point2 u -> AbsPathSeg u 
lineSegment p0 p1 = lineSegmentV $ pvec p0 p1

-- | Helper - construct a line segment.
-- 
lineSegmentV :: Floating u => Vec2 u -> AbsPathSeg u 
lineSegmentV v1 = AbsLineSeg (vlength v1) v1


-- | Helper - construct a curve segment.
-- 
curveSegment :: (Floating u, Ord u, Tolerance u) 
             => Point2 u -> Point2 u -> Point2 u -> Point2 u -> AbsPathSeg u 
curveSegment p0 p1 p2 p3 = AbsCurveSeg len v1 v2 v3
  where
    len = bezierLength (BezierCurve p0 p1 p2 p3)
    v1  = pvec p0 p1
    v2  = pvec p1 p2
    v3  = pvec p2 p3


-- | Helper - construct the /empty/ but located path.
-- 
zeroPath :: Floating u => Point2 u -> AbsPath u 
zeroPath p0 = AbsPath 0 p0 JL.empty p0
   


drawPath :: InterpretUnit u 
         => PathMode -> AbsPath u -> Image u (AbsPath u)
drawPath mode rp = replaceAns rp $ 
    liftQuery (toPrimPath rp) >>= dcPath mode


drawPath_ :: InterpretUnit u 
          => PathMode -> AbsPath u -> Graphic u
drawPath_ mode rp = liftQuery (toPrimPath rp) >>= dcPath mode


-- | Turn a Path into an ordinary PrimPath.
--
-- Assumes path is properly formed - i.e. end point of one 
-- segment is the same point as the start point of the next
-- segment.
--
toPrimPath :: InterpretUnit u => AbsPath u -> Query u PrimPath
toPrimPath (AbsPath _ start segs _) = 
    uconvertCtxF start       >>= \dstart -> 
    T.mapM uconvertCtxF segs >>= \dsegs  ->
    return $ step1 dstart dsegs
  where
    step1 p0 se | JL.null se          = emptyPrimPath p0
                | otherwise           = absPrimPath p0 $ step2 p0 (viewl se)

    step2 _  EmptyL                   = []
    step2 pt (e :< se)                = let (p1,s) = mkSeg pt e
                                        in s : step2 p1 (viewl se)
    
    mkSeg p0 (AbsLineSeg  _ v1)       = let p1 = p0 .+^ v1
                                        in (p1, absLineTo p1)
    mkSeg p0 (AbsCurveSeg _ v1 v2 v3) = let p1 = p0 .+^ v1
                                            p2 = p1 .+^ v2
                                            p3 = p2 .+^ v3
                                        in (p3, absCurveTo p1 p2 p3)






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
shortenL n path1@(AbsPath u startp segs ep) 
    | n <  0                  = path1
    | n >= u                  = AbsPath 0 ep mempty ep 
    | otherwise               = step n startp (viewl segs)
  where
    step _ _  EmptyL    = AbsPath 0 ep mempty ep 
    step d sp (e :< se) = let z     = segmentLength e
                              snext = sp .+^ segmentVector e
                          in case compare d z of
                             GT -> step (d-z) snext (viewl se)
                             EQ -> makeLeftPath (u-n) snext se ep
                             LT -> let (spart,e1) = shortenSegL d sp e
                                   in AbsPath (u-n) spart (e1 `cons` se) ep


makeLeftPath :: Floating u 
             => u -> Point2 u -> JoinList (AbsPathSeg u) -> Point2 u 
             -> AbsPath u
makeLeftPath u sp se ep | JL.null se = line1 sp ep
                        | otherwise  = AbsPath u sp se ep



shortenSegL :: (Real u, Floating u) 
            => u -> Point2 u -> AbsPathSeg u -> (Point2 u, AbsPathSeg u)
shortenSegL n sp (AbsLineSeg  u v1)        = 
    let v2  = shortenVec n v1 
        sp' = sp .+^ (v1 ^-^ v2)
    in (sp', AbsLineSeg  (u-n) v2)

shortenSegL n sp (AbsCurveSeg u v1 v2 v3)  = 
    (q0, AbsCurveSeg (u-n) (pvec q0 q1) (pvec q1 q2) (pvec q2 q3))
  where
    (BezierCurve q0 q1 q2 q3) = let p1 = sp .+^ v1
                                    p2 = p1 .+^ v2
                                    p3 = p2 .+^ v3
                                in snd $ subdividet (n/u) 
                                                    (BezierCurve sp p1 p2 p3)
     


shortenVec :: (Real u, Floating u) => u -> Vec2 u -> Vec2 u
shortenVec n v0 = v0 ^-^ v
  where
    v  = avec (vdirection v0) n


--------------------------------------------------------------------------------
-- shorten from the right ...
 
-- | Note - shortening a line from the right by 
-- greater-than-or-equal its length is operationally equivalent 
-- to making a zero-length line at the start point.
--
shortenR :: (Real u, Floating u) => u -> AbsPath u -> AbsPath u
shortenR n path1@(AbsPath u sp segs endpt) 
    | n < 0                   = path1
    | n >= u                  = AbsPath 0 sp mempty sp 
    | otherwise               = step n (viewr segs) endpt
  where
    step _ EmptyR    _  = AbsPath 0 sp mempty sp 
    step d (se :> e) ep = let z   = segmentLength e 
                              enext = ep .-^ segmentVector e
                          in case compare d z of
                              GT -> step (d-z) (viewr se) enext
                              EQ -> makeRightPath n sp se enext
                              LT -> let (e1,epart) = shortenSegR d e ep
                                    in AbsPath (u-n) sp (se `snoc` e1) epart
                         

makeRightPath :: Floating u 
              => u -> Point2 u -> JoinList (AbsPathSeg u) -> Point2 u 
              -> AbsPath u
makeRightPath u sp se ep | JL.null se = line1 sp ep
                         | otherwise  = AbsPath u sp se ep


shortenSegR :: (Real u, Floating u) 
            => u -> AbsPathSeg u -> Point2 u -> (AbsPathSeg u, Point2 u)
shortenSegR n (AbsLineSeg u v1)        ep = 
    let v2  = shortenVec n v1 
        ep' = ep .-^ (v1 ^+^ v2)
    in (AbsLineSeg (u-n) v2, ep')

shortenSegR n (AbsCurveSeg u v1 v2 v3) ep = 
    (AbsCurveSeg (u-n) (pvec q0 q1) (pvec q1 q2) (pvec q2 q3), q3)
  where
    (BezierCurve q0 q1 q2 q3) = let p2 = ep .-^ v3
                                    p1 = p2 .-^ v2
                                    p0 = p1 .-^ v1
                                in fst $ subdividet ((u-n)/u) 
                                                    (BezierCurve p0 p1 p2 ep)
     




--------------------------------------------------------------------------------
-- tips 

tipL :: AbsPath u -> Point2 u
tipL (AbsPath _ sp _ _) = sp


tipR :: AbsPath u -> Point2 u
tipR (AbsPath _ _ _ ep) = ep


--------------------------------------------------------------------------------
-- line direction

-- | Direction of empty path is considered to be 0.
--
directionL :: (Real u, Floating u) => AbsPath u -> Radian
directionL (AbsPath _ _ se _)  = step $ viewl se
  where
    step (AbsLineSeg  _ v1 :< _)       = vdirection $ negateV v1
    step (AbsCurveSeg _ v1 _  _  :< _) = vdirection $ negateV v1
    step _                             = 0


-- | Direction of empty path is considered to be 0.
--
directionR :: (Real u, Floating u) => AbsPath u -> Radian
directionR (AbsPath _ _ se _) = step $ viewr se
  where
    step (_ :> AbsLineSeg  _ v1)       = vdirection v1
    step (_ :> AbsCurveSeg _ _  _  v3) = vdirection v3
    step _                             = 0
 
-- | Is the left tip a Bezier curve?
--
isBezierL :: AbsPath u -> Bool
isBezierL (AbsPath _ _ se _) = step $ viewl se
  where
    step (AbsCurveSeg _ _ _ _ :< _) = True 
    step _                          = False


 
-- | Is the right tip a Bezier curve?
-- 
isBezierR :: AbsPath u -> Bool
isBezierR (AbsPath _ _ se _) = step $ viewr se
  where
    step (_ :> AbsCurveSeg _ _ _ _) = True 
    step _                          = False


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
-- Path Views cf. Data.Sequence


infixr 5 :<<
infixl 5 :>>

data PathViewL u = EmptyPathL
                 | PathSegment u :<< AbsPath u
  deriving (Eq,Show) 

type instance DUnit (PathViewL u) = u 

type DPathViewL = PathViewL Double

data PathViewR u = EmptyPathR
                 | AbsPath u :>> PathSegment u
  deriving (Eq,Show) 

type instance DUnit (PathViewR u) = u 

type DPathViewR = PathViewR Double



-- | PathSegments are annotated with length.
--
--
data PathSegment u = LineSeg  u (Point2 u) (Point2 u)
                   | CurveSeg u (Point2 u) (Point2 u) (Point2 u) (Point2 u)
  deriving (Eq,Show) 

type instance DUnit (PathSegment u) = u 


type DPathSegment = PathSegment Double


--------------------------------------------------------------------------------

instance Functor PathSegment where
  fmap f (LineSeg d p0 p1)        = LineSeg (f d) (fmap f p0) (fmap f p1)
  fmap f (CurveSeg d p0 p1 p2 p3) = 
      CurveSeg (f d) (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)

instance Functor PathViewL where
  fmap _ EmptyPathL   = EmptyPathL
  fmap f (a :<< as)   = fmap f a :<< fmap f as

instance Functor PathViewR where
  fmap _ EmptyPathR   = EmptyPathR
  fmap f (as :>> a)   = fmap f as :>> fmap f a

--------------------------------------------------------------------------------



pathViewL :: Num u => AbsPath u -> PathViewL u
pathViewL (AbsPath len sp segs ep) = go (viewl segs)
  where
    go EmptyL                           = EmptyPathL
     
    go (AbsLineSeg d v1 :< se)        =
        let p1 = sp .+^ v1 in LineSeg d sp p1 :<< AbsPath (len - d) p1 se ep

    go (AbsCurveSeg d v1 v2 v3 :< se) =
        let p1 = sp .+^ v1
            p2 = p1 .+^ v2
            p3 = p2 .+^ v3
        in CurveSeg d sp p1 p2 p3 :<< AbsPath (len - d) p3 se ep


pathViewR :: Num u => AbsPath u -> PathViewR u
pathViewR (AbsPath len sp segs ep) = go (viewr segs)
  where
    go EmptyR                           = EmptyPathR 

    go (se :> AbsLineSeg d v1)        = 
       let p0 = ep .-^ v1 in AbsPath (len - d) sp se p0 :>> LineSeg d p0 ep

    go (se :> AbsCurveSeg d v1 v2 v3) =  
       let p2 = ep .-^ v3
           p1 = p2 .-^ v2
           p0 = p1 .-^ v1
       in AbsPath (len - d) sp se p0 :>> CurveSeg d p0 p1 p2 ep



--------------------------------------------------------------------------------


-- Path should be same length afterwards.

optimizeLines :: (Real u, Floating u, Ord u, Tolerance u) 
              => AbsPath u -> AbsPath u
optimizeLines (AbsPath _ sp0 segs _) =
    outer (zeroPath sp0) (viewl segs)
  where
    outer acc (AbsLineSeg _ v1 :< se)              = 
        inner acc (vdirection v1) v1 (viewl se)

    outer acc (AbsCurveSeg u v1 v2 v3  :< se)      = 
        outer (snocC acc u v1 v2 v3) (viewl se)

    outer acc EmptyL                               = acc

    inner acc d0 v0 (AbsLineSeg _ v1 :< se)        =
        let d1 = vdirection v1
        in if (d0 == vdirection v1) 
          then inner acc d1 (v0 ^+^ v1) (viewl se)
          else inner (snocV acc v0) d1 v1 (viewl se)

    inner acc _  v0 (AbsCurveSeg u v1 v2 v3 :< se) = 
        let acc1 = snocC (snocV acc v0) u v1 v2 v3
        in outer acc1 (viewl se)

    inner acc _  v0 EmptyL                         = snocV acc v0

    snocC (AbsPath u sp se ep) u1 v1 v2 v3         = 
        let tail_curve = AbsCurveSeg u1 v1 v2 v3 
            vtotal     = v1 ^+^ v2 ^+^ v3 
        in AbsPath (u+u1) sp (JL.snoc se tail_curve) (ep .+^ vtotal)

    snocV (AbsPath u sp se ep) v1                  =
        let u1        = vlength v1
            tail_line = AbsLineSeg u1 v1
        in AbsPath (u+u1) sp (JL.snoc se tail_line) (ep .+^ v1)


--------------------------------------------------------------------------------
-- Round corners


segToCatTrail :: AbsPathSeg u -> CatTrail u
segToCatTrail (AbsLineSeg _ v1)        = catline v1
segToCatTrail (AbsCurveSeg _ v1 v2 v3) = catcurve v1 v2 v3

-- | Because lookahead of 2 makes the rounding algos easier we
-- decons to a list...
--
segmentListL :: AbsPath u -> (Point2 u, [AbsPathSeg u])
segmentListL (AbsPath _ sp se _) = (sp, JL.toList se)

-- | Round interior corners of a Path.
--
-- The path is treated as open - the start of the initial and end
-- of the final segments are not rounded. Only straight line to 
-- straight line joins are rounded, joins to or from Beczier 
-- curves are not rounded.
-- 
-- Caution - all path segments are expected to be longer than
-- 2x the round corner length, though this is not checked..
--
roundInterior :: (Real u, Floating u,Tolerance u) 
              => u -> AbsPath u -> AbsPath u
roundInterior du pth0 = catTrailPath pt ctrail
  where
    (pt,ss) = segmentListL pth0
    ctrail  = roundInteriorCat du ss


roundInteriorCat :: (Real u, Floating u) => u -> [AbsPathSeg u] -> CatTrail u
roundInteriorCat _  []     = mempty
roundInteriorCat du (z:zs) = step mempty z zs
  where
    step ac (AbsLineSeg _ v1) (AbsLineSeg _ v2:xs) = 
      let (x,k) = roundAB du v1 v2 in step (ac `mappend` x) k xs

    step ac prev            (x:xs)             = 
       let tprev = segToCatTrail prev in step (ac `mappend` tprev) x xs

    step ac prev            []                 = ac `mappend` segToCatTrail prev

roundAB :: (Real u, Floating u) 
        => u -> Vec2 u -> Vec2 u -> (CatTrail u, AbsPathSeg u)
roundAB du v1 v2 = 
    (catline v1' `mappend` tcurve, lineSegmentV v2')
  where
    v1'      = shortenVec du v1
    v2'      = shortenVec du v2

    tv1      = avec (vdirection v1) du
    tv2      = avec (vdirection v2) du
    base_vec = tv1 ^+^ tv2
    bw       = vlength base_vec
    h        = sqrt $ pow2 du - (pow2 $ 0.5 * bw)
    clockd   = clockDirection v1 v2
    tcurve   = tricurve clockd bw (-h) (vdirection base_vec)
    
    -- note the (-h) in tricurve is wrong, we need to account 
    -- for CCW or CW properly...

pow2 :: Num a => a -> a
pow2 x = x ^ (2::Int)

-- Form corners inside a /triangle/ Bezier.


-- | Round a \"closed\" path. 
--
-- Caution - all path sgements are expected to be longer than
-- 2x the round corner length, though this is not checked..
--
roundExterior :: (Real u, Floating u, Tolerance u) 
              => u -> AbsPath u -> AbsPath u
roundExterior du pth0 
   | isBezierR pth0 || isBezierL pth0 = roundInterior du pth0
   | otherwise                        = catTrailPath pt ctrail
  where
   (pt,ss) = segmentListL pth0
   ctrail  = roundExteriorCat du ss


roundExteriorCat :: (Real u, Floating u) => u -> [AbsPathSeg u] -> CatTrail u
roundExteriorCat _  []                        = mempty

roundExteriorCat _  (AbsCurveSeg _ _ _ _ : _) = 
    error "runExteriorCat - unreachable."

roundExteriorCat du (z@(AbsLineSeg _ v0):zs)  = step0 z zs
  where
    step0 (AbsLineSeg _ v1) xs = 
      let seg0 = lineSegmentV $ shortenVec du v1 in step1 mempty seg0 xs

    step0 _                 _  = error "roundExteriorCat - unreachable 1."
    
    step1 ac (AbsLineSeg _ v1) (AbsLineSeg _ v2:xs) = 
      let (x,k) = roundAB du v1 v2 in step1 (ac `mappend` x) k xs

    step1 ac prev              (x:xs)             = 
      let tprev = segToCatTrail prev in step1 (ac `mappend` tprev) x xs

    step1 ac (AbsLineSeg _ v1) []                 = 
      let (t,_) = roundAB du v1 v0 in ac `mappend` t

    step1 _  _                 []                 = 
      error "roundExteriorCat - unreachable 2."

--------------------------------------------------------------------------------
-- Path division


-- | Divide a path returning intermediate points and direction
--
-- Args are initial-prefix, division size, trailing size.
-- 
-- Generation is stopped if the remainder of the path is shorter
-- than the trailing size.
--
pathdiv :: (Real u, Floating u) 
        => u -> u -> u -> AbsPath u -> [(Point2 u, Radian)]
pathdiv ana sz end = step . shortenL ana 
  where
    step pth | length pth < end = []
             | otherwise         = atstart pth : step (shortenL sz pth)
                          

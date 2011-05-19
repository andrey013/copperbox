{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Base.RelPath
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Relative path type - this should be more amenable for building 
-- complex drawings than the PrimPath type in Wumpus-Core.
-- 
-- Note - RelPath is not directly equivalent to AbsPath.
-- AbsPath is more powerful - as it is expected to have more 
-- demanding use-cases (e.g. connector paths).
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Base.RelPath
  ( 


  -- * Relative path type

    RelPath
  , DRelPath

  -- * Construction
  , empty
  , line1
  , curve1
  , vertexPath
  , curvedPath
  
  , circular

  -- * Queries
  , null
  , length

  -- * Concat
  , append
  , consLineTo
  , snocLineTo
  , consCurveTo
  , snocCurveTo


  -- * Conversion
  , fromPathAlgVertices
  , fromPathAlgCurves

  , toPrimPath
  , toAbsPath

  , openRelPath
  , closedRelPath

  ) where


import Wumpus.Drawing.Paths.Base.AbsPath ( AbsPath )
import qualified Wumpus.Drawing.Paths.Base.AbsPath as Abs

import Wumpus.Basic.Geometry
import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.JoinList ( JoinList, ViewL(..), viewl, join )
import qualified Wumpus.Basic.Utils.JoinList as JL

import Wumpus.Core                              -- package: wumpus-core


import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace


import qualified Data.Foldable          as F
import Data.Monoid
import qualified Data.Traversable       as T
import Prelude hiding ( null, length )





-- | Relative Path data type.
-- 
-- Note this type is more limited than AbsPath, it does not
-- support /introspective/ operations like @length@ or anchors.
--
data RelPath u = RelPath 
      { rel_path_len    :: u
      , rel_path_segs   :: JoinList (RelPathSeg u) }
  deriving (Eq,Show)

type instance DUnit (RelPath u) = u


type DRelPath = RelPath Double

-- No annotations...
-- 
data RelPathSeg u = RelLineSeg  
                      { rel_line_len    :: u 
                      , rel_line_to     :: Vec2 u
                      }
                  | RelCurveSeg 
                      { rel_curve_len   :: u
                      , rel_curve_cp1   :: Vec2 u
                      , rel_curve_cp2   :: Vec2 u
                      , rel_curve_cp3   :: Vec2 u
                      }
  deriving (Eq,Show)


type instance DUnit (RelPathSeg u) = u



--------------------------------------------------------------------------------

instance Functor RelPath where
  fmap f (RelPath len xs) = RelPath (f len) (fmap (fmap f) xs)

instance Functor RelPathSeg where
  fmap f (RelLineSeg len v1)        = 
      RelLineSeg (f len) (fmap f v1)

  fmap f (RelCurveSeg len v1 v2 v3) = 
      RelCurveSeg (f len) (fmap f v1) (fmap f v2) (fmap f v3)


instance Num u => Monoid (RelPath u) where
  mempty  = empty
  mappend = append


--------------------------------------------------------------------------------
-- Construction


-- | Helper - construct a straight line segment.
-- 
lineSegment :: Floating u => Vec2 u -> (u, RelPathSeg u)
lineSegment v1 = 
    (len, RelLineSeg { rel_line_len = len, rel_line_to = v1 })
  where
    len = vlength v1


-- | Helper - construct a curve segment.
-- 
curveSegment :: (Floating u, Ord u, Tolerance u) 
             => Vec2 u -> Vec2 u -> Vec2 u -> (u, RelPathSeg u)
curveSegment v1 v2 v3 = (len, cseg)
  where
    p0    = zeroPt
    p1    = p0 .+^ v1
    p2    = p1 .+^ v2
    p3    = p2 .+^ v3
    
    len   = bezierLength (BezierCurve p0 p1 p2 p3)
    cseg  = RelCurveSeg { rel_curve_len = len
                        , rel_curve_cp1 = v1
                        , rel_curve_cp2 = v2
                        , rel_curve_cp3 = v3
                        }    




-- | An empty relative path is acceptible to Wumpus because 
-- it is always drawn as a LocGraphic.
--
empty :: Num u => RelPath u 
empty = RelPath { rel_path_len = 0, rel_path_segs = mempty }

-- | Create a relative path from a single straight line.
--
line1 :: Floating u => Vec2 u -> RelPath u
line1 v = RelPath len (JL.one $ RelLineSeg len v)
  where
    len = vlength v
   


-- | Create a relative path from a single Bezier curve.
--
curve1 :: Floating u 
       => Vec2 u -> Vec2 u -> Vec2 u -> RelPath u
curve1 v1 v2 v3 = RelPath len (JL.one $ RelCurveSeg len v1 v2 v3)
  where
    len = vlength $ v1 ^+^ v2 ^+^ v3


vertexPath :: Floating u => [Vec2 u] -> RelPath u
vertexPath [] = empty
vertexPath (x:xs) = go (line1 x) xs
  where
    go acc []     = acc
    go acc (v:vs) = go (acc `snocLineTo` v) vs



curvedPath :: Floating u => [Vec2 u] -> RelPath u
curvedPath xs = case xs of 
    (v1:v2:v3:vs) -> go (curve1 v1 v2 v3) vs
    _             -> empty
  where
    go acc (v1:v2:v3:vs) = go (acc `append` curve1 v1 v2 v3) vs
    go acc _             = acc



circular :: Floating u => u -> RelPath u
circular = snd . fromPathAlgCurves . circlePathAlg 

--------------------------------------------------------------------------------
-- Queries

null :: RelPath u -> Bool
null = JL.null . rel_path_segs

-- | Length of the Path.
--
-- Length is the length of the path as it is drawn, it is not a 
-- count of the number or path segments.
--
-- Length is cached so this operation is cheap - though this puts 
-- a tax on the build operations. 
-- 
length :: RelPath u -> u
length = rel_path_len


--------------------------------------------------------------------------------
-- Concat 

infixr 1 `append`



append :: Num u => RelPath u -> RelPath u -> RelPath u
append (RelPath la ssa) (RelPath lb ssb) = RelPath (la + lb)  $ ssa `join` ssb


consLineTo :: Floating u 
           => Vec2 u -> RelPath u -> RelPath u 
consLineTo v1 (RelPath len se) = RelPath (len + vl) $ JL.cons s se
  where
    (vl,s) = lineSegment v1 

snocLineTo :: Floating u 
           => RelPath u -> Vec2 u -> RelPath u
snocLineTo (RelPath len se) v1 = RelPath (len + vl) $ JL.snoc se s
  where
    (vl,s) = lineSegment v1 



consCurveTo :: (Floating u, Ord u, Tolerance u) 
            => Vec2 u -> Vec2 u -> Vec2 u -> RelPath u -> RelPath u 
consCurveTo v1 v2 v3 (RelPath len se) = RelPath (len + cl) $ JL.cons s se
  where
    (cl,s) = curveSegment v1 v2 v3

snocCurveTo :: (Floating u, Ord u, Tolerance u) 
            => RelPath u -> Vec2 u -> Vec2 u -> Vec2 u -> RelPath u
snocCurveTo (RelPath len se) v1 v2 v3 = RelPath (len + cl) $ JL.snoc se s
  where
    (cl,s) = curveSegment v1 v2 v3





--------------------------------------------------------------------------------
-- Conversion

fromPathAlgVertices :: Floating u => PathAlg u -> (Vec2 u, RelPath u)
fromPathAlgVertices = step . runPathAlgVec
  where
    step (Nothing, xs) = (zeroVec, vertexPath xs)
    step (Just v1, xs) = (v1, vertexPath xs)

fromPathAlgCurves :: Floating u => PathAlg u -> (Vec2 u, RelPath u)
fromPathAlgCurves = step . runPathAlgVec
  where
    step (Nothing, xs) = (zeroVec, curvedPath xs)
    step (Just v1, xs) = (v1, curvedPath xs)


toPrimPath :: InterpretUnit u => Point2 u -> RelPath u -> Query u PrimPath
toPrimPath start (RelPath _ segs) = 
    uconvertCtxF start       >>= \dstart -> 
    T.mapM uconvertCtxF segs >>= \dsegs  ->
    return $ relPrimPath dstart $ F.foldr fn [] dsegs
  where
    fn (RelLineSeg _ v1)        ac = relLineTo v1 : ac
    fn (RelCurveSeg _ v1 v2 v3) ac = relCurveTo v1 v2 v3 : ac



toAbsPath :: (Floating u, Ord u, Tolerance u) 
          => Point2 u -> RelPath u -> AbsPath u
toAbsPath start (RelPath _ segs) = step1 start $ viewl segs
  where
    step1 p0 EmptyL                           = Abs.empty p0

    step1 p0 (RelLineSeg _ v1 :< se)            = 
        let (pth,end) = aline p0 v1 in step2 end pth (viewl se)

    step1 p0 (RelCurveSeg _ v1 v2 v3 :< se)     = 
        let (pth,end) = acurve p0 v1 v2 v3 in step2 end pth (viewl se)

    step2 _  acc EmptyL                       = acc
    step2 p0 acc (RelLineSeg _ v1 :< se)        = 
        let (s1,end) = aline p0 v1 
        in step2 end (acc `Abs.append` s1) (viewl se)

    step2 p0 acc (RelCurveSeg _ v1 v2 v3 :< se) = 
        let (s1,end) = acurve p0 v1 v2 v3 
        in step2 end (acc `Abs.append` s1) (viewl se)

    aline p0 v1                               = 
        let p1 = p0 .+^ v1 in (Abs.line1 p0 p1, p1)

    acurve p0 v1 v2 v3                        = 
        let p1 = p0 .+^ v1
            p2 = p1 .+^ v2
            p3 = p2 .+^ v3
        in (Abs.curve1 p0 p1 p2 p3, p3)



openRelPath :: InterpretUnit u 
            => RelPath u -> LocImage u (RelPath u)
openRelPath rp = replaceAns rp $ 
    promoteLoc $ \start -> zapQuery (toPrimPath start rp) >>= dcOpenPath


closedRelPath :: InterpretUnit u 
              => DrawStyle -> RelPath u -> LocImage u (RelPath u)
closedRelPath sty rp = replaceAns rp $ 
    promoteLoc $ \start -> zapQuery (toPrimPath start rp) >>= dcClosedPath sty


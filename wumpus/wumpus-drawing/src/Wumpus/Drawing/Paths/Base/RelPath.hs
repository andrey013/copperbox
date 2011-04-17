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

  -- * Queries
  , null


  -- * Concat
  , append
  , consLineTo
  , snocLineTo
  , consCurveTo
  , snocCurveTo


  -- * Conversion
  , toPrimPath
  , toAbsPath
  , strokeRelPath

  ) where


import Wumpus.Drawing.Paths.Base.AbsPath ( AbsPath )
import qualified Wumpus.Drawing.Paths.Base.AbsPath as Abs

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.JoinList ( JoinList, ViewL(..), viewl, join )
import qualified Wumpus.Basic.Utils.JoinList as JL

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace

import qualified Data.Foldable          as F
import Data.Monoid
import qualified Data.Traversable       as T
import Prelude hiding ( null )





-- | Relative Path data type.
-- 
-- Note this type is more limited than AbsPath, it does not
-- support /introspective/ operations like @length@ or anchors.
--
newtype RelPath u = RelPath { getRelPath :: JoinList (RelPathSeg u) }
  deriving (Eq,Show)

type instance DUnit (RelPath u) = u


type DRelPath = RelPath Double

-- No annotations...
-- 
data RelPathSeg u = RelLineSeg  (Vec2 u)
                  | RelCurveSeg (Vec2 u) (Vec2 u) (Vec2 u)
  deriving (Eq,Show)


type instance DUnit (RelPathSeg u) = u



--------------------------------------------------------------------------------

instance Functor RelPath where
  fmap f = RelPath . fmap (fmap f) . getRelPath

instance Functor RelPathSeg where
  fmap f (RelLineSeg v1)        = 
      RelLineSeg (fmap f v1)

  fmap f (RelCurveSeg v1 v2 v3) = 
      RelCurveSeg (fmap f v1) (fmap f v2) (fmap f v3)


instance Monoid (RelPath u) where
  mempty  = empty
  mappend = append


--------------------------------------------------------------------------------
-- Construction


-- | An empty relative path is acceptible to Wumpus because 
-- it is always drawn as a LocGraphic.
--
empty :: RelPath u 
empty = RelPath mempty

-- | Create a relative path from a single straight line.
--
line1 :: Vec2 u -> RelPath u
line1 = RelPath . JL.one . RelLineSeg


-- | Create a relative path from a single Bezier curve.
--
curve1 :: Vec2 u -> Vec2 u -> Vec2 u -> RelPath u
curve1 v1 v2 v3 = RelPath $ JL.one $ RelCurveSeg v1 v2 v3


vertexPath :: [Vec2 u] -> RelPath u
vertexPath [] = empty
vertexPath (x:xs) = go (line1 x) xs
  where
    go acc []     = acc
    go acc (v:vs) = go (acc `snocLineTo` v) vs


circle :: u -> Path u
circle rad = 

--------------------------------------------------------------------------------
-- Queries

null :: RelPath u -> Bool
null = JL.null . getRelPath



--------------------------------------------------------------------------------
-- Concat 

infixr 1 `append`



append :: RelPath u -> RelPath u -> RelPath u
append (RelPath se0) (RelPath se1) = RelPath $ se0 `join` se1


consLineTo :: Vec2 u -> RelPath u -> RelPath u 
consLineTo v1 (RelPath se) = RelPath $ JL.cons (RelLineSeg v1) se

snocLineTo :: RelPath u -> Vec2 u -> RelPath u
snocLineTo (RelPath se) v1 = RelPath $ JL.snoc se (RelLineSeg v1)



consCurveTo :: Vec2 u -> Vec2 u -> Vec2 u -> RelPath u -> RelPath u 
consCurveTo v1 v2 v3 (RelPath se) = RelPath $ JL.cons (RelCurveSeg v1 v2 v3) se

snocCurveTo :: RelPath u -> Vec2 u -> Vec2 u -> Vec2 u -> RelPath u
snocCurveTo (RelPath se) v1 v2 v3 = RelPath $ JL.snoc se (RelCurveSeg v1 v2 v3)





--------------------------------------------------------------------------------
-- Conversion

toPrimPath :: InterpretUnit u => Point2 u -> RelPath u -> Query PrimPath
toPrimPath start (RelPath segs) = 
    uconvertCtxF start       >>= \dstart -> 
    T.mapM uconvertCtxF segs >>= \dsegs  ->
    return $ relPrimPath dstart $ F.foldr fn [] dsegs
  where
    fn (RelLineSeg v1)        ac = relLineTo v1 : ac
    fn (RelCurveSeg v1 v2 v3) ac = relCurveTo v1 v2 v3 : ac


toAbsPath :: (Floating u, Ord u, Tolerance u) 
          => Point2 u -> RelPath u -> AbsPath u
toAbsPath start (RelPath segs) = step1 start $ viewl segs
  where
    step1 p0 EmptyL                           = Abs.empty p0

    step1 p0 (RelLineSeg v1 :< se)            = 
        let (pth,end) = aline p0 v1 in step2 end pth (viewl se)

    step1 p0 (RelCurveSeg v1 v2 v3 :< se)     = 
        let (pth,end) = acurve p0 v1 v2 v3 in step2 end pth (viewl se)

    step2 _  acc EmptyL                       = acc
    step2 p0 acc (RelLineSeg v1 :< se)        = 
        let (s1,end) = aline p0 v1 
        in step2 end (acc `Abs.append` s1) (viewl se)

    step2 p0 acc (RelCurveSeg v1 v2 v3 :< se) = 
        let (s1,end) = acurve p0 v1 v2 v3 
        in step2 end (acc `Abs.append` s1) (viewl se)

    aline p0 v1                               = 
        let p1 = p0 .+^ v1 in (Abs.line1 p0 p1, p1)

    acurve p0 v1 v2 v3                        = 
        let p1 = p0 .+^ v1
            p2 = p1 .+^ v2
            p3 = p2 .+^ v3
        in (Abs.curve1 p0 p1 p2 p3, p3)



strokeRelPath :: InterpretUnit u => RelPath u -> LocGraphic u
strokeRelPath rp = 
    promoteR1 $ \start -> toPrimPath start rp >>= openStroke



{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Relative
-- Copyright   :  (c) Stephen Tetley 2011
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

module Wumpus.Drawing.Paths.Relative
  ( 
    RelPath
  , DRelPath

  , null
  , emptyRelPath
  , append
  
  , lineTo
  , curveTo

  , strokeRelPath

  , toPrimPath
  , toAbsPath

  ) where


import Wumpus.Drawing.Paths.Absolute ( AbsPath )
import qualified Wumpus.Drawing.Paths.Absolute as Abs

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

-- No annotation...
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
  mempty = emptyRelPath
  mappend = append


--------------------------------------------------------------------------------

type LocRelPath u = LocQuery u (RelPath u)

null :: RelPath u -> Bool
null = JL.null . getRelPath

infixr 1 `append`


-- | An empty relative path is acceptible to Wumpus because 
-- it is always drawn as a LocGraphic.
--
emptyRelPath :: RelPath u 
emptyRelPath = RelPath mempty

append :: RelPath u -> RelPath u -> RelPath u
append (RelPath se0) (RelPath se1) = RelPath $ se0 `join` se1


lineTo :: Vec2 u -> RelPath u
lineTo = RelPath . JL.one . RelLineSeg


curveTo :: Vec2 u -> Vec2 u -> Vec2 u -> RelPath u
curveTo v1 v2 v3 = RelPath $ JL.one $ RelCurveSeg v1 v2 v3

strokeRelPath :: InterpretUnit u => RelPath u -> LocGraphic u
strokeRelPath rp = 
    let qry = toPrimPath rp
    in promoteR1 $ \start -> (qry `at` start) >>= openStroke


toPrimPath :: InterpretUnit u => RelPath u -> LocQuery u PrimPath
toPrimPath (RelPath segs) = promoteR1 $ \start -> 
    uconvertCtxF start       >>= \dstart -> 
    T.mapM uconvertCtxF segs >>= \dsegs  ->
    return $ relPrimPath dstart $ F.foldr fn [] dsegs
  where
    fn (RelLineSeg v1)        ac = relLineTo v1 : ac
    fn (RelCurveSeg v1 v2 v3) ac = relCurveTo v1 v2 v3 : ac


toAbsPath :: (Floating u, Ord u, LengthTolerance u) 
          => Point2 u -> RelPath u -> AbsPath u
toAbsPath start (RelPath segs) = step1 start $ viewl segs
  where
    step1 p0 EmptyL                           = Abs.pathZero p0

    step1 p0 (RelLineSeg v1 :< se)            = 
        let (pth,end) = line p0 v1 in step2 end pth (viewl se)

    step1 p0 (RelCurveSeg v1 v2 v3 :< se)     = 
        let (pth,end) = curve p0 v1 v2 v3 in step2 end pth (viewl se)

    step2 _  acc EmptyL                       = acc
    step2 p0 acc (RelLineSeg v1 :< se)        = 
        let (s1,end) = line p0 v1 
        in step2 end (acc `Abs.append` s1) (viewl se)

    step2 p0 acc (RelCurveSeg v1 v2 v3 :< se) = 
        let (s1,end) = curve p0 v1 v2 v3 
        in step2 end (acc `Abs.append` s1) (viewl se)

    line p0 v1                                = 
        let p1 = p0 .+^ v1 in (Abs.line p0 p1, p1)

    curve p0 v1 v2 v3                         = 
        let p1 = p0 .+^ v1
            p2 = p1 .+^ v2
            p3 = p2 .+^ v3
        in (Abs.curve p0 p1 p2 p3, p3)

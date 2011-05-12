{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.RefTrace
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Writer monad with imperative /turtle/ style movement to build 
-- LocGraphics and /references/ allowing connectors between 
-- objects.
--
-- Note - references are not /feedback/. Subsequent nodes cannot
-- be place at anchors of previous nodes - anchors only allow
-- connectors to be drawn between located nodes.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.RefTrace
  (

    -- * Re-exports
    LocTraceM(..)

  , RefTrace
  , RefTraceT

  , Ref
  , RefTraceM(..)

  , runRefTrace
  , runRefTraceT

  , unaryLink
  , binaryLink
  , multiwayLink
  )

  where

import Wumpus.Drawing.Basis.LocTrace

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import qualified Wumpus.Basic.Utils.JoinList as JL

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Monoid




newtype RefTrace u z a = RefTrace { 
          getRefTrace :: RefSt u z -> (a, RefSt u z) }


type instance MonUnit (RefTrace u z a) = u

newtype RefTraceT u z m a = RefTraceT { 
          getRefTraceT :: RefSt u z -> m (a, RefSt u z) }


type instance MonUnit (RefTraceT u z m a) = u


newtype Ref = Ref { getRefUid :: Int }


-- GRAPHIC or LOC_GRAPHIC?   cf. connectors...
--
-- TODO - make this an newtype and only export an arity family of 
-- constructors.
-- 
-- Maybe we only support the arity2 (connector) and list (path) cases?
--
-- type Elaboration u ans = IntMap.IntMap ans -> Graphic u

data LinkRef u ans = 
      Unary { refU    :: Ref 
            , ancrU   :: ans -> Point2 u
            , drawU   :: LocGraphic u 
            }
    | Binary { refB1  :: Ref 
             , refB2  :: Ref 
             , ancrB1 :: ans -> Point2 u
             , ancrB2 :: ans -> Point2 u
             , drawB  :: ConnectorGraphic u
             }
    | Multiway { refLs :: [Ref]
               , ancrM :: ans -> Point2 u
               , drawM :: [Point2 u] -> Graphic u
               }

data RefSt u z = RefSt 
      { uid_count       :: Int
      , current_tip     :: Vec2 u
      , ref_acc         :: LocImage u (IntMap.IntMap z)
      , ref_links       :: JL.JoinList (LinkRef u z)
      }



type instance DUnit (RefSt u z) = u

type RefStF u z = RefSt u z -> RefSt u z 


zeroRefSt :: Num u => RefSt u z
zeroRefSt = RefSt { uid_count   = 0
                  , current_tip = V2 0 0
                  , ref_acc     = mempty
                  , ref_links   = mempty      
                  }




-- Functor

instance Functor (RefTrace u z) where
  fmap f ma = RefTrace $ \s0 -> let (a,s1) = getRefTrace ma s0 in (f a, s1)

instance Monad m => Functor (RefTraceT u z m) where
  fmap f ma = RefTraceT $ \s0 -> getRefTraceT ma s0 >>= \(a,s1) ->
                                 return (f a, s1)



-- Applicative

instance Applicative (RefTrace u z) where
  pure a    = RefTrace $ \s0 -> (a, s0)
  mf <*> ma = RefTrace $ \s0 -> 
                let (f,s1) = getRefTrace mf s0
                    (a,s2) = getRefTrace ma s1
                in (f a, s2)



instance Monad m => Applicative (RefTraceT u z m) where
  pure a    = RefTraceT $ \s0 -> return (a, s0)
  mf <*> ma = RefTraceT $ \s0 -> getRefTraceT mf s0 >>= \(f,s1) -> 
                                 getRefTraceT ma s1 >>= \(a,s2) ->
                                 return (f a, s2)



-- Monad

instance Monad (RefTrace u z) where
  return a  = RefTrace $ \s0 -> (a, s0)
  ma >>= k  = RefTrace $ \s0 -> 
                let (a,s1) = getRefTrace ma s0
                in (getRefTrace . k) a s1


instance Monad m => Monad (RefTraceT u z m) where
  return a  = RefTraceT $ \s0 -> return (a, s0)
  ma >>= k  = RefTraceT $ \s0 -> getRefTraceT ma s0 >>= \(a,s1) ->
                                 (getRefTraceT . k) a s1
                



-- LocTraceM

instance Num u => LocTraceM (RefTrace u z) where
  insertl gf  = RefTrace $ \s0 -> ((), insertSt gf s0)
  moveBy v    = RefTrace $ \s0 -> ((), moveSt v s0)
  location    = RefTrace $ \s0 -> (current_tip s0, s0)


instance (Monad m, Num u) => LocTraceM (RefTraceT u z m) where
  insertl gf  = RefTraceT $ \s0 -> return ((), insertSt gf s0)
  moveBy v    = RefTraceT $ \s0 -> return ((), moveSt v s0)
  location    = RefTraceT $ \s0 -> return (current_tip s0, s0)



-- Run functions

runRefTrace :: Num u => RefTrace u ans a -> LocImage u a
runRefTrace mf = post $ getRefTrace mf zeroRefSt
  where
    post (a,st) = pushR1 (replaceAns a) $ reconcileRefSt st


runRefTraceT :: (Monad m, Num u) => RefTraceT u ans m a -> m (LocImage u a)
runRefTraceT mf = liftM post $ getRefTraceT mf zeroRefSt
  where
    post (a,st) = pushR1 (replaceAns a) $ reconcileRefSt st



-- Note we have to drop the vector

reconcileRefSt :: RefSt u z -> LocGraphic u
reconcileRefSt st = 
    step (ref_acc st) (JL.toList $ ref_links st)
  where
    step img xs = locGraphic_ $ elaborateR1 img (\a -> mconcat $ map (fn a) xs)
    
    fn im (Unary r1 ar1 gf) = 
      maybe mempty (\pt -> promoteR1 $ \_ -> apply1R1 gf pt) (projectRef r1 ar1 im)
   
    fn im (Binary r1 r2 ar1 ar2 conn) = 
      case (projectRef r1 ar1 im, projectRef r2 ar2 im) of
        (Just p1, Just p2) -> promoteR1 $ \_ -> apply2R2 conn p1 p2
        _                  -> mempty


    fn im (Multiway rs ar1 gf) = 
        let ps = catMaybes $ map (\a -> projectRef a ar1 im) rs
        in promoteR1 $ \_ -> gf ps
                                 


projectRef :: Ref -> (ans -> Point2 u) -> IntMap.IntMap ans -> Maybe (Point2 u)
projectRef r ancr im = ancr <$> IntMap.lookup (getRefUid r) im

-- Note - probably this supports Tree which is not a Trace monad...

class Monad m => RefTraceM (m :: * -> *) where
  type MonRef m :: *
  insertRef   :: (MonRef m ~ a, MonUnit (m ()) ~ u) => LocImage u a -> m Ref
  linkRef     :: (MonRef m ~ a, MonUnit (m ()) ~ u) => LinkRef u a -> m ()

instance Num u => RefTraceM (RefTrace u z) where
  type MonRef (RefTrace u z) = z
  insertRef img = RefTrace $ \s0 -> let (ix,s1) = incrementSt img s0
                                    in (Ref ix, s1)

  linkRef fn    = RefTrace $ \s0 -> ((), snocLink fn s0)  


moveSt :: Num u => Vec2 u -> RefStF u z 
moveSt v = (\s i -> s { current_tip = i ^+^ v }) 
             <*> current_tip

insertSt :: Num u => LocImage u z2 -> RefStF u ans
insertSt gf = (\s ac v -> let g1 = locGraphic_ $ moveStart (dispVec v) gf
                          in s { ref_acc = decorateR1 ac g1 }) 
                <*> ref_acc <*> current_tip

snocLink :: LinkRef u ans -> RefStF u ans
snocLink fn = (\s i -> s { ref_links = JL.snoc i fn }) 
                <*> ref_links


incrementSt :: Num u 
            => LocImage u ans -> RefSt u ans -> (Int, RefSt u ans)
incrementSt img s0 = (uid_count s0, upd s0)
  where
    upd = (\s ac v ix -> let img1 = moveStart (dispVec v) img
                         in s { ref_acc   = fn ix ac img1
                              , uid_count = ix+1 }) 
                <*> ref_acc <*> current_tip <*> uid_count 

    fn ix ac gf = pushR1 (mapAns (\(a,b) -> IntMap.insert ix b a)) $ 
                    bothLocImage ac gf



unaryLink :: (ans -> Point2 u) -> LocGraphic u -> Ref -> LinkRef u ans
unaryLink f gf = \r1 -> Unary { refU    = r1
                              , ancrU   = f
                              , drawU   = gf
                              }

binaryLink :: (ans -> Point2 u) -> (ans -> Point2 u) 
           -> ConnectorGraphic u -> Ref -> Ref 
           -> LinkRef u ans
binaryLink f g conn = \r1 r2 -> Binary { refB1  = r1
                                       , refB2  = r2 
                                       , ancrB1 = f
                                       , ancrB2 = g
                                       , drawB  = conn
                                       }


multiwayLink :: (ans -> Point2 u) -> ([Point2 u] -> Graphic u) -> [Ref] 
             -> LinkRef u ans
multiwayLink f gf = \rs -> Multiway { refLs = rs
                                    , ancrM = f
                                    , drawM = gf
                                    }
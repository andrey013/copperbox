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

  , Lookup
  , RefGraphic
  , lookup
  , unary
  , binary
  , multiway
  , oneToMany

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
import Prelude hiding ( lookup )




newtype RefTrace u z a = RefTrace { 
          getRefTrace :: RefSt u z -> (a, RefSt u z) }


type instance MonUnit (RefTrace u z a) = u

newtype RefTraceT u z m a = RefTraceT { 
          getRefTraceT :: RefSt u z -> m (a, RefSt u z) }


type instance MonUnit (RefTraceT u z m a) = u


newtype Ref = Ref { getRefUid :: Int }


type Lookup u ans = IntMap.IntMap ans -> Query u (Maybe ans)
type RefGraphic u ans = IntMap.IntMap ans -> Graphic u


lookup :: Ref -> Lookup u ans
lookup ref = \im -> return $ IntMap.lookup (getRefUid ref) im


unary :: InterpretUnit u 
      => (ans -> Graphic u) 
      -> Ref
      -> RefGraphic u ans
unary gf r1 = \im -> 
  zapQuery (lookup r1 im) >>= maybe mempty gf



binary :: InterpretUnit u 
       => (ans -> ans -> Graphic u) 
       -> Ref -> Ref
       -> RefGraphic u ans
binary gf r1 r2 = \im -> 
  zapQuery (both (lookup r1 im) (lookup r2 im)) >>= \(ma,mb) -> 
  case (ma,mb) of
    (Just a, Just b) -> gf a b
    _                -> mempty


multiway :: InterpretUnit u 
         => ([ans] -> Graphic u) 
         -> [Ref]
         -> RefGraphic u ans
multiway gf rs = \im -> 
  zapQuery (fmap catMaybes $ mapM (\r -> lookup r im) rs) >>= gf


oneToMany :: InterpretUnit u 
         => (ans -> [ans] -> Graphic u) 
         -> Ref -> [Ref]
         -> RefGraphic u ans
oneToMany gf r1 rs = \im -> 
    zapQuery (both (lookup r1 im) (allrefs im rs)) >>= \(ma,xs) ->
    maybe mempty (\a -> gf a xs) ma
  where
    allrefs im = fmap catMaybes . mapM (\r -> lookup r im)




data RefSt u z = RefSt 
      { uid_count       :: Int
      , current_tip     :: Vec2 u
      , ref_acc         :: LocImage u (IntMap.IntMap z)
      , ref_links       :: JL.JoinList (RefGraphic u z)
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

instance InterpretUnit u => LocTraceM (RefTrace u z) where
  insertl gf  = RefTrace $ \s0 -> ((), insertSt gf s0)
  moveBy v    = RefTrace $ \s0 -> ((), moveSt v s0)
  location    = RefTrace $ \s0 -> (current_tip s0, s0)


instance (Monad m, InterpretUnit u) => LocTraceM (RefTraceT u z m) where
  insertl gf  = RefTraceT $ \s0 -> return ((), insertSt gf s0)
  moveBy v    = RefTraceT $ \s0 -> return ((), moveSt v s0)
  location    = RefTraceT $ \s0 -> return (current_tip s0, s0)



-- Run functions

runRefTrace :: InterpretUnit u => RefTrace u ans a -> LocImage u a
runRefTrace mf = post $ getRefTrace mf zeroRefSt
  where
    post (a,st) = replaceAns a $ reconcileRefSt st


runRefTraceT :: (Monad m, InterpretUnit u) 
             => RefTraceT u ans m a -> m (LocImage u a)
runRefTraceT mf = liftM post $ getRefTraceT mf zeroRefSt
  where
    post (a,st) = replaceAns a $ reconcileRefSt st



-- Note we have to drop the vector

reconcileRefSt :: InterpretUnit u => RefSt u z -> LocGraphic u
reconcileRefSt st = 
    ignoreAns $ selaborate (ref_acc st) 
                           (\a -> mconcat $ map (fn a) $ JL.toList $ ref_links st)
  where
    fn im g = promoteLoc $ \_ -> g im


-- Note - probably this supports Tree which is not a Trace monad...

class Monad m => RefTraceM (m :: * -> *) where
  type MonRef m :: *
  insertRef   :: (MonRef m ~ a, MonUnit (m ()) ~ u) => LocImage u a -> m Ref
  linkRef     :: (MonRef m ~ a, MonUnit (m ()) ~ u) => RefGraphic u a -> m ()

instance InterpretUnit u => RefTraceM (RefTrace u z) where
  type MonRef (RefTrace u z) = z
  insertRef img = RefTrace $ \s0 -> let (ix,s1) = incrementSt img s0
                                    in (Ref ix, s1)

  linkRef fn    = RefTrace $ \s0 -> ((), snocLink fn s0)  




moveSt :: Num u => Vec2 u -> RefStF u z 
moveSt v = (\s i -> s { current_tip = i ^+^ v }) 
             <*> current_tip

insertSt :: InterpretUnit u => LocImage u z2 -> RefStF u ans
insertSt gf = (\s ac v1 -> let g1 = ignoreAns $ moveStart v1 gf
                           in s { ref_acc = sdecorate ac g1 }) 
                <*> ref_acc <*> current_tip

snocLink :: RefGraphic u ans -> RefStF u ans
snocLink fn = (\s i -> s { ref_links = JL.snoc i fn }) 
                <*> ref_links


incrementSt :: InterpretUnit u 
            => LocImage u ans -> RefSt u ans -> (Int, RefSt u ans)
incrementSt img s0 = (uid_count s0, upd s0)
  where
    upd = (\s ac v1 ix -> let img1 = moveStart v1 img
                          in s { ref_acc   = fn ix ac img1
                               , uid_count = ix+1 }) 
                <*> ref_acc <*> current_tip <*> uid_count 

    fn ix ac gf = fmap (\(a,b) -> IntMap.insert ix b a) $ both ac gf





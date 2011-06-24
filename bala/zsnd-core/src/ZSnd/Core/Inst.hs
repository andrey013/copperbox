{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Inst
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Monadic instrument definition patterned after the Click modular 
-- router language.
--
--------------------------------------------------------------------------------

module ZSnd.Core.Inst
  (

    Inst
  , runInst

  , Element
  , getElementI
  , getElementK
  , getElementA
  , mkElement

  , ilet
  , klet
  , alet
  , out
  , (->-)
  , (=>=)
  , (=>-)
  , (->=)

  , Rate
  , dataRate
  , KA_Rate
  , IRate
  , KRate
  , ARate
  
  
  ) where

import ZSnd.Core.Inst.Click
import ZSnd.Core.Inst.Prim
import ZSnd.Core.Utils.HList


import Control.Applicative


type LetCount = Int
type W = H CStmt


newtype Inst a = Inst { getInst :: LetCount -> (a,LetCount,W) }

instance Functor Inst where
  fmap f ma = Inst $ \s -> let (a,s1,w) = getInst ma s in (f a, s1,w)

instance Applicative Inst where
  pure a    = Inst $ \s -> (a,s,id)
  mf <*> ma = Inst $ \s -> let (f,s1,w1) = getInst mf s
                               (a,s2,w2) = getInst ma s1
                           in (f a, s2, w1 . w2)

instance Monad Inst where
  return a = Inst $ \s -> (a,s,id)
  m >>= k  = Inst $ \s -> let (a,s1,w1) = getInst m s
                              (b,s2,w2) = (getInst . k) a s1
                          in (b, s2, w1 . w2)



runInst :: Int -> Inst a -> Either FailMsg PrimInst
runInst n ma = post $ getInst ma 0
  where
    post (_,_,f) = translateDesc n $ toListH f


newtype Element rate = Element { getElement :: UElement }
  deriving (Eq,Ord,Show)

getElementI :: Element IRate -> UElement
getElementI = getElement

getElementK :: Element KRate -> UElement
getElementK = getElement

getElementA :: Element ARate -> UElement
getElementA = getElement

mkElement :: UElement -> Element rate
mkElement = Element

-- | configuration let
--
clet :: Element rate -> Inst DeclRef
clet elt = Inst $ \s -> 
    let v1 = fromIntegral s
    in (v1, s+1, wrapH $ Decl (fromIntegral s) (getElement elt))


ilet :: Element IRate -> Inst DeclRef
ilet = clet

klet :: Element KRate -> Inst DeclRef
klet = clet

alet :: Element ARate -> Inst DeclRef
alet = clet



out :: DeclRef -> Inst ()
out v = Inst $ \s -> ((),s, wrapH $ Out v)


(->-) :: (DeclRef,Int) -> (DeclRef,Int) -> Inst ()
(->-) p1 p2 = Inst $ \s -> ((),s, wrapH $ Conn p1 p2)

(=>=) :: DeclRef -> DeclRef -> Inst ()
(=>=) v1 v2 = (v1,0) ->- (v2,0)

(=>-) :: DeclRef -> (DeclRef,Int) -> Inst ()
(=>-) v1 p2 = (v1,0) ->- p2 

(->=) :: (DeclRef,Int) -> DeclRef -> Inst ()
(->=) p1 v2 = p1 ->- (v2,0)




class Rate rate where
  dataRate :: rate -> DataRate

data IRate 
data KRate
data ARate

instance Rate IRate where
  dataRate _ = I

instance Rate KRate where
  dataRate _ = K

instance Rate ARate where
  dataRate _ = A


class Rate rate => KA_Rate rate

instance KA_Rate KRate
instance KA_Rate ARate


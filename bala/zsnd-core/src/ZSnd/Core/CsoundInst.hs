{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CsoundInst
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

module ZSnd.Core.CsoundInst
  (

    Orch(..)
  , OrchHeader(..)
  , default_mono_header
  , default_stereo_header

  , Inst
  , PrimInst    -- * re-export
  , runInst
  , runInstU

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
  , IK_Rate
  , IRate
  , KRate
  , ARate

  , Conf
  , getConfI
  , getConfK
  , getConfA
  , getConfUniv
  , mkConf
 
  , cast

  , pfield
  , port  

  , funcall
  
  ) where

import ZSnd.Core.Inst.Click
import ZSnd.Core.Inst.Prim
import ZSnd.Core.Utils.FormatCombinators
import ZSnd.Core.Utils.HList


import Control.Applicative hiding ( empty )



--------------------------------------------------------------------------------
-- Orchestra - collections of Inst


data Orch = Orch
      { orch_header     :: OrchHeader
      , orch_insts      :: [PrimInst]
      }
  deriving (Eq,Ord,Show)

-- | Orchestra file header values.
--
-- The following fields are not accessible to ZScore:
--
-- > ctrlinit   
-- > massign
-- > pgmassign
-- > pset
-- > strset
--
data OrchHeader = OrchHeader 
      { zero_dbfs               :: Int     -- default 32767
      , kr_ctrl_rate            :: Int     -- default 1000
      , sr_aud_sample_rate      :: Int     -- default 44100
      , ksmps_ctrl_period_samps :: Int     -- default 10
      , nchnls_num_chans        :: Int     -- default 1 (mono)
      , seed_gbl_random         :: Maybe Int  
      }
  deriving (Eq,Ord,Show)

      
default_mono_header :: OrchHeader 
default_mono_header =  
    OrchHeader { zero_dbfs                = 32767
               , kr_ctrl_rate             = 4410
               , sr_aud_sample_rate       = 44100
               , ksmps_ctrl_period_samps  = 32
               , nchnls_num_chans         = 1
               , seed_gbl_random          = Nothing
               }

default_stereo_header :: OrchHeader 
default_stereo_header =  
    OrchHeader { zero_dbfs                = 32767
               , kr_ctrl_rate             = 4410
               , sr_aud_sample_rate       = 44100
               , ksmps_ctrl_period_samps  = 32
               , nchnls_num_chans         = 2
               , seed_gbl_random          = Nothing
               }


--------------------------------------------------------------------------------
-- Inst

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

-- | Unsafe version of @runInst@ - throws runtime error on failure.
--
runInstU :: Int -> Inst a -> PrimInst
runInstU n ma = case runInst n ma of
    Left err -> error err
    Right ans -> ans


newtype Element rate = Element { getElement :: UElement }
  deriving (Eq,Ord,Show)

getElementI :: Element IRate -> UElement
getElementI = getElement

getElementK :: Element KRate -> UElement
getElementK = getElement

getElementA :: Element ARate -> UElement
getElementA = getElement

mkElement :: String -> [InConf] -> OutConf -> Element rate
mkElement ss ins outspec = Element $ UElement ss ins outspec

-- | configuration let
--
clet :: Element rate -> Inst ElemRef
clet elt = Inst $ \s -> 
    let v1 = mkElemRef s
    in (v1, s+1, wrapH $ Decl v1 (getElement elt))


ilet :: Element IRate -> Inst ElemRef
ilet = clet

klet :: Element KRate -> Inst ElemRef
klet = clet

alet :: Element ARate -> Inst ElemRef
alet = clet



out :: ElemRef -> Inst ()
out v = Inst $ \s -> ((),s, wrapH $ Out v)


(->-) :: (ElemRef,Int) -> (ElemRef,Int) -> Inst ()
(->-) p1 p2 = Inst $ \s -> ((),s, wrapH $ Conn p1 p2)

(=>=) :: ElemRef -> ElemRef -> Inst ()
(=>=) v1 v2 = (v1,0) ->- (v2,0)

(=>-) :: ElemRef -> (ElemRef,Int) -> Inst ()
(=>-) v1 p2 = (v1,0) ->- p2 

(->=) :: (ElemRef,Int) -> ElemRef -> Inst ()
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


class Rate rate => IK_Rate rate

instance IK_Rate IRate
instance IK_Rate KRate


newtype Conf rate = Conf { getConf :: InConf }
  deriving (Eq,Ord)

instance Show (Conf rate) where
  showsPrec p = showsPrec p . getConf

mkConf :: InConf -> Conf rate
mkConf = Conf

getConfI :: Conf IRate -> InConf
getConfI = getConf

getConfK :: Conf KRate -> InConf
getConfK = getConf

getConfA :: Conf ARate -> InConf
getConfA = getConf

-- | Extract Conf at a universal type (rate).
--
getConfUniv :: Conf rate -> InConf
getConfUniv = getConf

cast :: Conf r1 -> Conf r2
cast a = Conf (getConf a)


binop :: (InConf -> InConf -> InConf) -> Conf rate -> Conf rate -> Conf rate
binop op a b = Conf $ (getConf a) `op` (getConf b)

unop :: (InConf -> InConf) -> Conf rate -> Conf rate
unop op a = Conf $ op $ getConf a


instance Num (Conf rate) where
  (+)     = binop (+)
  (-)     = binop (-)
  (*)     = binop (*)
  abs     = unop abs
  negate  = unop negate 
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = Conf $ fromInteger i


instance Fractional (Conf rate) where
  (/)     = binop (/)
  recip _ = error "recip - no interpretation of recip in Csound."  
  fromRational d = Conf $ fromRational d


pfield  :: Int -> Conf rate
pfield  = Conf . CPfield

port    :: Int -> Conf rate
port    = Conf . ClkPort


funcall :: String -> Conf rate -> Conf rate
funcall s a = Conf $ CFuncall s (getConf a)


--------------------------------------------------------------------------------
-- Format instances

instance Format Orch where
  format (Orch hdr xs) = vspace (format hdr : map format xs)

instance Format OrchHeader where
  format orch =        lhs "0dbfs"  <+> int (zero_dbfs orch)
            `vconcat`  lhs "sr"     <+> int (sr_aud_sample_rate orch)
--            `vconcat`  lhs "kr"     <+> int (kr_ctrl_rate orch)
            `vconcat`  lhs "ksmps"  <+> int (ksmps_ctrl_period_samps orch)
            `vconcat`  lhs "nchnls" <+> int (nchnls_num_chans orch)
            `vconcat`  opt_seed
    where
      lhs s = text s <+> char '='
      opt_seed = case seed_gbl_random orch of 
                   Nothing -> empty
                   Just i  -> lhs "seed" <+> int i

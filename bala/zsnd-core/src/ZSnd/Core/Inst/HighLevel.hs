{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Inst.HighLevel
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- High-level monadic layer for definition Click instruments.
-- Some typing improvements via phantom types.
--
--------------------------------------------------------------------------------

module ZSnd.Core.Inst.HighLevel
  (

  -- * Rate placeholder types and classes
    IRate
  , KRate  
  , ARate

  , Rate
  , dataRate
  , KA_Rate
  , IK_Rate

  -- * Phantom typed UElement
  , Element
  , mkElement
  , getElementI 
  , getElementK
  , getElementA
  , getElementUniv 

  -- * Phantom typed Expr
  , Conf
  , mkConf
  , getConfI
  , getConfK
  , getConfA
  , getConfUniv
  
  , cast
  , pfield
  , funcall  


  ) where

import ZSnd.Core.Inst.Click
import ZSnd.Core.Inst.Prim


data IRate 
data KRate
data ARate


-- Note - the classes are exported opaquely. This is because there
-- is a fixed, closed set of instances.


class Rate rate where
  dataRate :: rate -> DataRate

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

--------------------------------------------------------------------------------
-- Element

-- | Typed version of 'UElement' - rate is a phantom param.
--
newtype Element rate = Element { getElement :: UElement }

mkElement :: String -> InputConfig -> OutConf -> Element rate
mkElement ss ins outspec = Element $ UElement ss ins outspec

getElementI :: Element IRate -> UElement
getElementI = getElement

getElementK :: Element KRate -> UElement
getElementK = getElement

getElementA :: Element ARate -> UElement
getElementA = getElement

getElementUniv :: Element rate -> UElement
getElementUniv = getElement

--------------------------------------------------------------------------------
-- Conf

-- | Typed version of 'Expr' - rate is a phantom param.
--
newtype Conf rate = Conf { getConf :: Expr }
  deriving (Eq,Ord)

instance Show (Conf rate) where
  showsPrec p = showsPrec p . getConf


-- | Note - not sure this is necessary,  it may be removed...
--
mkConf :: Expr -> Conf rate
mkConf = Conf

getConfI :: Conf IRate -> Expr
getConfI = getConf

getConfK :: Conf KRate -> Expr
getConfK = getConf

getConfA :: Conf ARate -> Expr
getConfA = getConf

getConfUniv :: Conf rate -> Expr
getConfUniv = getConf


-- Constructors ( also num instances for literals)


cast :: Conf r1 -> Conf r2
cast a = Conf (getConf a)


pfield  :: Int -> Conf rate
pfield  = Conf . PField


funcall :: String -> Conf rate -> Conf rate
funcall s a = Conf $ Funcall s (getConf a)



binop :: Rator -> Conf rate -> Conf rate -> Conf rate
binop op a b = Conf $ BinOp op (getConf a) (getConf b)

unop :: Rator -> Conf rate -> Conf rate
unop op a = Conf $ UnOp op (getConf a)


instance Num (Conf rate) where
  (+)     = binop (infixL 6 "+")
  (-)     = binop (infixL 6 "-")
  (*)     = binop (infixL 7 "*")
  abs     = funcall "abs"
  negate  = unop (prefix 9 "-")
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = Conf $ Literal $ CsInt $ fromInteger i


instance Fractional (Conf rate) where
  (/)     = binop (infixL 7 "/")
  recip _ = error "recip - no interpretation of recip in Csound."  
  fromRational d = Conf $ Literal $ CsDouble (fromRational d)

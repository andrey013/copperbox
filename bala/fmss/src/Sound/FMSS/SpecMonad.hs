{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.SpecMonad
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Low-level abstract syntax
--
--------------------------------------------------------------------------------

module Sound.FMSS.SpecMonad
  (

    Spec
  , ErrMsg
  , Params(..)

  , execSpec

  , globals
  , variable
  , envelope
  , config  

  ) where


import Sound.FMSS.AbstractSyntax
import Sound.FMSS.ConfigMonad
import Sound.FMSS.Envelopes
import Sound.FMSS.Utils.HList

import Control.Applicative
import Data.Monoid




data Params = Params { instr_num :: Int, sine_table :: Int } 


-- data SpecAns = SpecAns { ans_body :: L2Body, ans_out :: Stmt }

-- Note - potentially this can be implemented as an inner
-- /connection spec/. With sharing for envelopes in an outer
-- monad.


type ErrMsg = String

newtype W = W { getW :: H Stmt }
newtype St = St { st_varnames :: [String] }

stAddName :: String -> St -> St 
stAddName s (St xs) = St (s:xs)

st_zero :: St
st_zero = St { st_varnames = [] }



instance Monoid W where
  mempty            = W emptyH
  W a `mappend` W b = W $ a `appendH` b


bimapE :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimapE f _ (Left a)  = Left (f a)
bimapE _ g (Right b) = Right (g b)

-- | Spec monad.
--
newtype Spec a = Spec { getSpec :: St -> Either ErrMsg (a, St, W) }

instance Functor Spec where
  fmap f mf = Spec $ \s -> bimapE id fn $ getSpec mf s
    where
      fn (a,s1,w) = (f a,s1,w)

instance Applicative Spec where
  pure a    = Spec $ \s -> return (a,s,mempty)
  mf <*> ma = Spec $ \s -> getSpec mf s  >>= \(f,s1,w1) -> 
                           getSpec ma s1 >>= \(a,s2,w2) -> 
                           return (f a, s2, w1 `mappend` w2) 

instance Monad Spec where
  return a  = Spec $ \s -> return (a,s,mempty)
  ma >>= k  = Spec $ \s -> getSpec ma s     >>= \(a,s1,w1) -> 
                           getSpec (k a) s1 >>= \(b,s2,w2) -> 
                           return (b, s2, w1 `mappend` w2)


execSpec :: Params -> Spec a -> Either ErrMsg Instr
execSpec (Params {instr_num = i, sine_table = tnum }) mf = 
    bimapE id post $ getSpec (global_decls tnum >> mf) st_zero
  where
    post (_,_,w) = Instr i $ toListH $ getW w


global_decls :: Int -> Spec ()
global_decls tnum = 
       variable "isinetbl" (fromIntegral tnum)
    >> variable "idur"     (PField 3)
    >> variable "iamp"     (PField 4)
    >> variable "ifreq"    (PField 5)
    >> return ()


-- | returns - (idur, iamp, ifreq)
--
globals :: Spec (Expr,Expr,Expr)
globals = Spec $ \s -> Right (ans, s, W $ emptyH)
  where
    ans  = (VarE "idur", VarE "iamp", VarE "ifreq")



variable :: String -> Expr -> Spec Expr
variable name expr = Spec $ \s -> 
    if name `elem` st_varnames s 
      then Left err_msg
      else let s1 = stAddName name s
           in Right (VarE name, s1, W $ wrapH $ DeclStmt $ Decl name expr)
  where
    err_msg = "variable - var name '" ++ name ++ "' is already used."


                         

envelope :: DeconsEnvelope env => String -> env -> Spec Expr
envelope name env = Spec $ \s -> 
    if name `elem` st_varnames s 
      then Left err_msg
      else let (opcode,body) = deconsEnvelope env
               s1            = stAddName name s
               decl          = Envelope name opcode body
           in Right (VarE name, s1, W $ wrapH $ DeclStmt decl)
  where
    err_msg = "envelope - variable name '" ++ name ++ "' is already used."



config :: Config a -> Spec ()
config cf = Spec $ \s -> let ans = runConfig cf in Right ((),s, W ans)

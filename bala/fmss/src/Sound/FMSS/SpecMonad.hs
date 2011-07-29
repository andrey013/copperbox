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
  , SpecAns  
  , Params(..)

  , execSpec


  , globals
  , variable
  , envelope
  , config  

  ) where


import Sound.FMSS.AbstractSyntax
import Sound.FMSS.ConfigMonad
import Sound.FMSS.Datatypes
import Sound.FMSS.Envelopes
import Sound.FMSS.Utils.HList

import Control.Applicative
import Data.Monoid




data Params = Params { instr_num :: Int, sine_table :: Int } 


data SpecAns = SpecAns { ans_body :: SynthBody, ans_out :: Stmt }

-- Note - potentially this can be implemented as an inner
-- /connection spec/. With sharing for envelopes in an outer
-- monad.


type ErrMsg = String

newtype W = W { w_decls :: H Decl }
newtype St = St { st_varnames :: [String] }

stAddName :: String -> St -> St 
stAddName s (St xs) = St (s:xs)

st_zero :: St
st_zero = St { st_varnames = ["idur", "iamp", "ifreq" ] }



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


execSpec :: Params -> Spec SpecAns -> Either ErrMsg FMSynth
execSpec (Params {instr_num = i, sine_table = tnum }) mf = 
    bimapE id post $ getSpec mf st_zero
  where
    post (SpecAns {ans_body = body, ans_out = out},_ , w) = 
        FMSynth { fm_instr_num  = i
                , fm_sinetbl    = tnum
                , fm_decls      = toListH $ w_decls w
                , fm_synth_body = body 
                , fm_out        = out
                }


throwErr :: ErrMsg -> Spec a
throwErr msg = Spec $ \_ -> Left msg


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
           in Right (VarE name, s1, W $ wrapH $ Decl name expr)
  where
    err_msg = "variable - var name '" ++ name ++ "' is already used."


                         

envelope :: DeconsEnvelope env => String -> env -> Spec Expr
envelope name env = Spec $ \s -> 
    if name `elem` st_varnames s 
      then Left err_msg
      else let (opcode,body) = deconsEnvelope env
               s1            = stAddName name s
           in Right (VarE name, s1, W $ wrapH $ Envelope name opcode body)
  where
    err_msg = "envelope - variable name '" ++ name ++ "' is already used."

config :: (Config o) -> (o -> Expr) -> Spec SpecAns
config cf k = Spec $ \s -> 
    let (o,body) = runConfig cf
        outexpr   = k o
    in Right (SpecAns {ans_body = body, ans_out = Out outexpr}, s, mempty)

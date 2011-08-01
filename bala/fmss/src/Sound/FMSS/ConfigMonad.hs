{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.ConfigMonad
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Configuration of carriers and modulators
--
--------------------------------------------------------------------------------

module Sound.FMSS.ConfigMonad
  (

   
    
    Config
  , ConfigAns
  , runConfig
  , tell1

  , Var

  , postpro

  , modulator
  , carrier
  , out

  , noise

  , vexpr
  , expr1
  , expr2
  , expr3

  ) where


import Sound.FMSS.AbstractSyntax
import Sound.FMSS.Utils.HList

import Control.Applicative hiding ( Const )

-- Note - potentially this can be implemented as an inner
-- /connection spec/. With sharing for envelopes in an outer
-- monad.



data St = St 
      { mod_count    :: !Int
      , car_count    :: !Int
      , noise_count  :: !Int
      }

type W = H Stmt


--------------------------------------------------------------------------------
-- The monad


newtype Config a = Config { getConfig :: St -> (a, St, W) }

type ConfigAns = H Stmt

instance Functor Config where
  fmap f mf = Config $ \s -> let (a,s1,w) = getConfig mf s in (f a, s1, w)

instance Applicative Config where
  pure a    = Config $ \s -> (a,s,emptyH)
  mf <*> ma = Config $ \s -> let (f,s1,w1) = getConfig mf s
                                 (a,s2,w2) = getConfig ma s1
                             in (f a, s2, w1 `appendH` w2) 

instance Monad Config where
  return a  = Config $ \s -> (a,s,emptyH)
  ma >>= k  = Config $ \s -> let (a,s1,w1) = getConfig ma s
                                 (b,s2,w2) = getConfig (k a) s1
                             in (b, s2, w1 `appendH` w2)

runConfig :: Config a -> ConfigAns
runConfig mf = post $ getConfig mf s0
  where
    post (_,_,w) = w
    s0           = St { mod_count = 1, car_count = 1, noise_count = 1 }



tell1 :: Stmt -> Config ()
tell1 stmt = Config $ \s -> ((),s, wrapH stmt )


postpro :: (Expr -> Expr) -> Config Var -> Config Var
postpro fn mf = do 
    v1 <- mf
    tell1 $ assignExpr v1 (fn $ vexpr v1)
    return v1

modulator :: Expr -> Config Var
modulator e1 = Config $ \s@(St {mod_count=i}) ->
    let vsig  = "amod" ++ show i ++ "sig"
        vphs  = "amod" ++ show i ++ "phs"
        s0    = wrapH $ CommentS $ "modulator " ++ show i
        s1    = wrapH $ AssignOpcode vphs "phasor" [e1]
        s2    = wrapH $ AssignOpcode vsig "tablei" (tableiArgs vphs)
    in (mkVar vsig, s { mod_count = i+1}, s0 . s1 . s2)

    
                         
tableiArgs :: VarId -> [Expr]
tableiArgs v1 = [ VarE v1, VarE "isinetbl", Const $ CsInt 1
                , Const $ CsInt 0, Const $ CsInt 1 ]

carrier :: Expr -> Config Var
carrier e1 = Config $ \s@(St {car_count=i}) ->
    let vsig  = "acar" ++ show i ++ "sig"
        vphs  = "acar" ++ show i ++ "phs"
        s0    = wrapH $ CommentS $ "carrier " ++ show i
        s1    = wrapH $ AssignOpcode vphs "phasor" [e1]
        s2    = wrapH $ AssignOpcode vsig "tablei" (tableiArgs vphs)
    in (mkVar vsig, s { car_count = i+1}, s0 . s1 . s2)


out :: Expr -> Config ()
out e1 = tell1 $ Out e1

-- Note - example for @noise@ in Csound docs uses @line@....
--
noise :: Expr -> Expr -> Config Var
noise e1 e2 = Config $ \s@(St {noise_count=i}) ->
    let vsig  = "anoise" ++ show i ++ "sig"
        s0    = wrapH $ CommentS $ "noise " ++ show i
        s1    = wrapH $ AssignOpcode vsig "noise" [e1, e2]
    in (mkVar vsig, s { noise_count = i+1}, s0 . s1)



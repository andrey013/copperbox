{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.OrchMonad
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Low-level abstract syntax
--
--------------------------------------------------------------------------------

module Orchsyn.OrchMonad
  (


    -- * MakeVar - closed class, no new instances
    MakeVar
  , mkvar

  -- * Monadic building

  , Instr
  , runInstr
  , execInstr

  , instrument

  , opcodeStmt0
  , opcodeStmt1
  , opcodeStmt2
  , opcodeStmt3
  , opcodeStmt4

  , assignStmt


  ) where

import Orchsyn.Language.Expr
import Orchsyn.Language.PrimAst ( InstDef(..) )
import Orchsyn.Language.Stmt
import Orchsyn.Utils.HList

import Control.Applicative
import Data.Monoid



newtype InstrBody = InstrBody { getInstrBody :: H Stmt }

instance Monoid InstrBody where
  mempty = InstrBody emptyH
  InstrBody a `mappend` InstrBody b = InstrBody $ a `appendH` b

data Fresh = Fresh 
      { fresh_i :: Int
      , fresh_k :: Int
      , fresh_a :: Int
      }

-- | Idiomatically, Csound variable naming starts from 1.
--
init_fresh :: Fresh
init_fresh = Fresh 1 1 1

nextI :: Fresh -> (Int,Fresh)
nextI a = let i = fresh_i a in (i, a { fresh_i = i+1})

nextK :: Fresh -> (Int,Fresh)
nextK a = let i = fresh_k a in (i, a { fresh_k = i+1})

nextA :: Fresh -> (Int,Fresh)
nextA a = let i = fresh_a a in (i, a { fresh_a = i+1})



-- | Instrument is a State-Writer monad.
-- 
newtype Instr a = Instr { getInstr :: Fresh -> (a, Fresh, InstrBody) }


instance Functor Instr where
  fmap f ma = Instr $ \s -> let (a,s1,w1) = getInstr ma s in (f a, s1, w1)


instance Applicative Instr where
  pure a    = Instr $ \s -> (a,s,mempty)
  mf <*> ma = Instr $ \s -> let (f,s1,w1) = getInstr mf s 
                                (a,s2,w2) = getInstr ma s1
                            in (f a, s2, w1 `mappend` w2) 


instance Monad Instr where
  return  = pure
  m >>= k = Instr $ \s -> let (a,s1,w1) = getInstr m s
                              (b,s2,w2) = getInstr (k a) s1
                          in (b, s2, w1 `mappend` w2)



runInstr :: Instr a -> (a, [Stmt])
runInstr ma = post $ getInstr ma init_fresh
  where
    post (a,_,w) = (a, toListH $ getInstrBody w)

execInstr :: Instr a -> [Stmt]
execInstr = snd . runInstr


instrument :: Int -> Instr a -> InstDef
instrument n ma = InstDef
    { inst_num    = n
    , arg_defs    = []
    , var_defs    = []
    , body_stmts  = stmts
    }
  where
    stmts = map stmtToPrimStmt $ execInstr ma

sets :: (Fresh -> (a,Fresh)) -> Instr a
sets fn = Instr $ \s -> let (a,s1) = fn s in (a, s1, mempty)

tell :: Stmt -> Instr ()
tell a = Instr $ \s -> ((),s, InstrBody $ wrapH a)



class Rate rate => MakeVar rate where
  mkvar :: rate -> Instr Var

instance MakeVar IInit where
  mkvar _ = fmap (Var I) $ sets nextI

instance MakeVar KRate where
  mkvar _ = fmap (Var K) $ sets nextK

instance MakeVar ARate where
  mkvar _ = fmap (Var A) $ sets nextA


-- | Only model single assignment. 
-- 
-- At some point the compiler should be able to optimize
-- variable use.
--
--

-- | Opcode /generator/ returning nothing.
--
opcodeStmt0 :: String -> [DExpr] -> Instr ()
opcodeStmt0 opco args = tell $ OpcodeS [] opco args

-- | Opcode /generator/ returning one variable.
--
opcodeStmt1 :: (MakeVar rate, TypeRate rate) 
           => String -> rate -> [DExpr] -> Instr (Expr rate)
opcodeStmt1 opco rate args = do 
    v <- mkvar rate
    tell $ OpcodeS [v] opco args
    return $ typeRate $ VarE v




-- | Opcode /generator/ returning two variables.
--
opcodeStmt2 :: (MakeVar rate, TypeRate rate) 
            => String -> rate -> [DExpr] -> Instr (Expr rate, Expr rate)
opcodeStmt2 opco rate args = do 
    v1 <- mkvar rate
    v2 <- mkvar rate
    tell $ OpcodeS [v1, v2] opco args
    return (typeRate $ VarE v1, typeRate $ VarE v2)

-- | Opcode /generator/ returning three variables.
--
opcodeStmt3 :: (MakeVar rate, TypeRate rate) 
            => String -> rate -> [DExpr] 
            -> Instr (Expr rate, Expr rate, Expr rate)
opcodeStmt3 opco rate args = do 
    v1 <- mkvar rate
    v2 <- mkvar rate
    v3 <- mkvar rate
    tell $ OpcodeS [v1, v2, v3] opco args
    return (typeRate $ VarE v1, typeRate $ VarE v2, typeRate $ VarE v3)


-- | Opcode /generator/ returning four variables.
--
opcodeStmt4 :: (MakeVar rate, TypeRate rate) 
            => String -> rate -> [DExpr] 
            -> Instr (Expr rate, Expr rate, Expr rate, Expr rate)
opcodeStmt4 opco rate args = do 
    v1 <- mkvar rate
    v2 <- mkvar rate
    v3 <- mkvar rate
    v4 <- mkvar rate
    tell $ OpcodeS [v1, v2, v3, v4] opco args
    return ( typeRate $ VarE v1, typeRate $ VarE v2
           , typeRate $ VarE v3, typeRate $ VarE v4)



assignStmt :: (MakeVar rate, TypeRate rate) 
           => rate -> DExpr -> Instr (Expr rate)
assignStmt rate rhs = do 
    v <- mkvar rate 
    tell $ AssignS v rhs
    return $ typeRate $ VarE v



pfieldDecl :: String -> Int -> Instr ()
pfieldDecl iname num = tell $ AssignS (INamed iname) (PfieldE num)
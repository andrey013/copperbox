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


  -- * Monadic building

    Instr
  , runInstr
  , execInstr

  , instrument


  , Opcode0
  , Opcode1
  , Opcode2
  , Opcode3
  , Opcode4

  , ivar
  , kvar
  , avar

  , newIVarE
  , newIVar
  , newIVar2
  , newIVar3
  , newIVar4

  , newKVarE
  , newKVar
  , newKVar2
  , newKVar3
  , newKVar4

  , newAVarE
  , newAVar
  , newAVar2
  , newAVar3
  , newAVar4

  , (#=)
  , ($=)

  
  , opcodeStmt0
  , opcodeStmt1
  , opcodeStmt2
  , opcodeStmt3
  , opcodeStmt4

  , assignStmt

  , pfieldDecl

  ) where

import Orchsyn.Language.Expr
import Orchsyn.Language.PrimAst ( InstDef(..) )
import Orchsyn.Language.Stmt
import Orchsyn.Utils.HList

import Control.Applicative
import Control.Monad
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



--------------------------------------------------------------------------------
-- The instrument building \"API\".


ivar :: Instr (LVal IInit)
ivar = fmap (LVal . Var I) $ sets nextI


kvar :: Instr (LVal KRate)
kvar = fmap (LVal . Var K) $ sets nextK

avar :: Instr (LVal ARate)
avar = fmap (LVal . Var A) $ sets nextA


-- | Only model single assignment. 
-- 
-- At some point the compiler should be able to optimize
-- variable use.
--
--



--
-- NOTE - Opcode statements shouldn\'t generate fresh variables 
-- as the can be used for re-assignment.
--


-- From Cascone - this is what we want:
-- 
-- > do a1 <- var >>= randi i4 p9
-- >    a1 #= oscil a1 i3 10        -- wrong - a1 is LVar not expr
-- >    a1 #= oscil a1 3000 11      -- so it cannot appear on the right
--
-- > do a1 <- var >>= randi i4 p9
-- >    a1 #= oscil a1 i3 10 #= oscil a1 3000 11
-- 

-- UGLY:
--
-- > do a1 <- newAVar $ randi i4 p9
-- >    a1 #= oscil (rval a1) i3 10
-- >    a1 #= oscil (rval a1) 3000 11

-- Introduces possibility of pattern match failure:
-- where data LVal = Var DExpr:
-- 
-- 
-- > do a1@(Var a1') <- var >>= randi i4 p9
-- >    a1 #= oscil a1' i3 10
-- >    a1 #= oscil a1' 3000 11



-- (#=) :: LVal rate -> Opcode1 rate -> Instr ()

-- Csound assignment:
-- 
-- > aosc2     =         asig1*asig2
--
-- Haskell:
-- 
-- > asoc2 <- var
-- > asoc2 $= asig1 * asig2
-- 
-- > asoc2@(Var a) <- withVar $ asig1 * asig
-- > asoc2         $= a * kenv


type Opcode0 rate = Instr ()
type Opcode1 rate = LVal rate -> Instr ()
type Opcode2 rate = (LVal rate, LVal rate) -> Instr ()
type Opcode3 rate = (LVal rate, LVal rate, LVal rate) -> Instr ()
type Opcode4 rate = (LVal rate, LVal rate, LVal rate, LVal rate) -> Instr ()



newIVarE :: Expr IInit -> Instr (LVal IInit)
newIVarE e1 = ivar >>= \v1 -> assignStmt (uniRate e1) v1 >> return v1

newIVar :: Opcode1 IInit -> Instr (LVal IInit)
newIVar op = ivar >>= \v1 -> op v1 >> return v1

newIVar2 :: Opcode2 IInit -> Instr (LVal IInit, LVal IInit)
newIVar2 op = liftM2 (,) ivar ivar >>= \ans -> op ans  >> return ans

newIVar3 :: Opcode3 IInit -> Instr (LVal IInit, LVal IInit, LVal IInit)
newIVar3 op = 
    liftM3 (,,) ivar ivar ivar >>= \ans -> op ans  >> return ans

newIVar4 :: Opcode4 IInit 
         -> Instr (LVal IInit, LVal IInit, LVal IInit, LVal IInit)
newIVar4 op = 
    liftM4 (,,,) ivar ivar ivar ivar >>= \ans -> op ans  >> return ans



newKVarE :: Expr ARate -> Instr (LVal ARate)
newKVarE e1 = avar >>= \v1 -> assignStmt (uniRate e1) v1 >> return v1

newKVar :: Opcode1 ARate -> Instr (LVal ARate)
newKVar op = avar >>= \v1 -> op v1 >> return v1

newKVar2 :: Opcode2 ARate -> Instr (LVal ARate, LVal ARate)
newKVar2 op = liftM2 (,) avar avar >>= \ans -> op ans  >> return ans

newKVar3 :: Opcode3 ARate -> Instr (LVal ARate, LVal ARate, LVal ARate)
newKVar3 op = 
    liftM3 (,,) avar avar avar >>= \ans -> op ans  >> return ans

newKVar4 :: Opcode4 ARate 
         -> Instr (LVal ARate, LVal ARate, LVal ARate, LVal ARate)
newKVar4 op = 
    liftM4 (,,,) avar avar avar avar >>= \ans -> op ans  >> return ans


newAVarE :: Expr ARate -> Instr (LVal ARate)
newAVarE e1 = avar >>= \v1 -> assignStmt (uniRate e1) v1 >> return v1

newAVar :: Opcode1 ARate -> Instr (LVal ARate)
newAVar op = avar >>= \v1 -> op v1 >> return v1

newAVar2 :: Opcode2 ARate -> Instr (LVal ARate, LVal ARate)
newAVar2 op = liftM2 (,) avar avar >>= \ans -> op ans  >> return ans

newAVar3 :: Opcode3 ARate -> Instr (LVal ARate, LVal ARate, LVal ARate)
newAVar3 op = 
    liftM3 (,,) avar avar avar >>= \ans -> op ans  >> return ans

newAVar4 :: Opcode4 ARate 
         -> Instr (LVal ARate, LVal ARate, LVal ARate, LVal ARate)
newAVar4 op = 
    liftM4 (,,,) avar avar avar avar >>= \ans -> op ans  >> return ans



infixl 1 #=, $=

-- | Reassign a variable with the result of calling an opcode.
--
(#=) :: LVal rate -> Opcode1 rate -> Instr ()
(#=) l op = op l 

-- | Reassign a variable to the value of the expression.
--
($=) :: Rate rate => LVal rate -> Expr rate -> Instr ()
($=) l e = assignStmt (uniRate e) l


                                    


-- | Opcode /generator/ returning nothing.
--
opcodeStmt0 :: String -> [DExpr] -> Opcode0 rate
opcodeStmt0 opco args = tell $ OpcodeS [] opco args



-- | Opcode /generator/ assigning to 1 variable.
--
opcodeStmt1 :: String -> [DExpr] -> Opcode1 rate
opcodeStmt1 opco args = \(LVal v) -> do 
    tell $ OpcodeS [v] opco args
    return ()

-- | Opcode /generator/ assigning to 2 variables.
--
opcodeStmt2 :: String -> [DExpr] -> Opcode2 rate
opcodeStmt2 opco args = \(LVal v1, LVal v2) -> do 
    tell $ OpcodeS [v1, v2] opco args
    return ()


-- | Opcode /generator/ assigning to 3 variables.
--
opcodeStmt3 :: String -> [DExpr] -> Opcode3 rate
opcodeStmt3 opco args = \(LVal v1, LVal v2, LVal v3) -> 
    tell $ OpcodeS [v1, v2, v3] opco args


-- | Opcode /generator/ assiging to 4 variables.
--
opcodeStmt4 :: String -> [DExpr] -> Opcode4 rate
opcodeStmt4 opco args = \(LVal v1, LVal v2, LVal v3, LVal v4) -> 
    tell $ OpcodeS [v1, v2, v3, v4] opco args


assignStmt :: DExpr -> LVal rate -> Instr ()
assignStmt rhs (LVal v) = tell $ AssignS v rhs




-- | Re-assignment - transforming with a expression is so common
-- that we should implement it rather than try to recover it in 
-- the compiler.
-- 
-- This is complicated by the fact that we bind opcode statements
-- to an expression rather than a var / lvalue.
--
-- Both opcodes and exprs can re-asssign.


pfieldDecl :: String -> Int -> Instr ()
pfieldDecl iname num = tell $ AssignS (INamed iname) (PfieldE num)
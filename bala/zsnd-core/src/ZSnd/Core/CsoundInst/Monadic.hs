{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Monadic
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- New inst langauge...
--
--------------------------------------------------------------------------------

module ZSnd.Core.CsoundInst.Monadic
  (

    BuildOrch
  , BuildInst

  , runOrch
  , runOrchU

  , instr
  , gaInit

  , newLabel 

  , label

  , out
  , out2

  , (=<=)
  , writeSignalInst
  , writeSignalOrch

  , idef
  , kdef
  , adef

  , ilet
  , klet
  , alet
  , ilet2
  , klet2
  , alet2
  , ilet3
  , klet3
  , alet3
  , ilet4
  , klet4
  , alet4

  , goto
  , kgoto
  , igoto
  , (.>.)
  , ifgoto

  ) where

import ZSnd.Core.CsoundInst.Prim
import ZSnd.Core.CsoundInst.Typed
import ZSnd.Core.Utils.HList

import Control.Applicative
import Data.Either

type LetIx   = Int
type FailMsg = String


bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimap f _ (Left a)  = Left (f a)
bimap _ g (Right b) = Right (g b)


-- | Build Monad - state-writer-error.
--
newtype BuildMonad elt a = BM { 
          getBM :: LetIx -> Either FailMsg (a, LetIx, H elt ) }

type BuildOrch a  = BuildMonad (Either UStmt PrimInst) a
type BuildInst a  = BuildMonad UStmt a


instance Functor (BuildMonad elt) where
  fmap f ma = BM $ \s -> bimap id sk $ getBM ma s 
    where
      sk (a,s1,w) = (f a, s1, w)

instance Applicative (BuildMonad elt) where
  pure a    = BM $ \s -> Right (a,s,emptyH)
  mf <*> ma = BM $ \s -> 
                case getBM mf s of
                  Left err -> Left err
                  Right (f,s1,w1) -> case getBM ma s1 of
                    Left err -> Left err
                    Right (a,s2,w2) -> Right (f a,s2, w1 `appendH` w2)

instance Monad (BuildMonad elt) where
  return a = BM $ \s -> Right (a,s,emptyH)
  ma >>= k = BM $ \s -> 
                case getBM ma s of
                  Left err -> Left err
                  Right (a,s1,w1) -> case getBM (k a) s1 of
                    Left err -> Left err
                    Right (b,s2,w2) -> Right (b,s2, w1 `appendH` w2)


runOrch :: OrchHeader -> BuildOrch a -> Either FailMsg Orch
runOrch hdr ma = bimap id sk $ getBM ma 1
  where
    sk (_,_,w1) = let (xs,ys) = partitionEithers $ toListH w1 
                  in Orch hdr xs ys


runOrchU :: OrchHeader -> BuildOrch a -> Orch
runOrchU hdr ma = either error sk $ getBM ma 1
  where
    sk (_,_,w1) = let (xs,ys) = partitionEithers $ toListH w1 
                  in Orch hdr xs ys



tellElt :: elt -> BuildMonad elt ()
tellElt elt = BM $ \s -> Right ((),s, wrapH elt)


newLocVar :: Rate rate 
          => rate -> BuildMonad elt (Var rate)
newLocVar a = BM $ \s -> let rt = dataRate a 
                         in Right (mkVar $ LocVar rt s , s+1, emptyH)


gaInit :: String -> Expr rate -> BuildOrch (Var ARate)
gaInit name e = 
    tellElt (Left (OpcodeAssign [getVarA v1] "init" [getExprUniv e])) >> return v1
  where
    v1 = mkVar $ UserVar name

-- is gainit special in that it isn\'t a @galet@ ?


instr :: Int -> BuildInst a -> BuildOrch ()
instr n ma = BM $ \s -> bimap id (sk s) $ getBM ma s
  where
    sk s0 (_,_,w1) = let inst = PrimInst n (toListH w1)
                     in ((),s0, wrapH $ Right inst)


newLabel :: String -> BuildInst Label
newLabel s = return (mkLabel s)

label :: Label -> BuildInst ()
label lbl = tellElt (Label lbl)

out :: Expr ARate -> BuildInst ()
out e1 = tellElt (Outs [getExprA e1])

out2 :: Expr ARate -> Expr ARate -> BuildInst ()
out2 e1 e2 = tellElt (Outs [getExprA e1, getExprA e2])


(=<=) :: Var rate -> Opcode1 rate -> BuildInst ()
v1 =<= opc = tellElt (assignStmt1 v1 opc)


-- | Write a signal via a zero-output opcode.
--
writeSignalOrch :: Opcode0 rate -> BuildOrch ()
writeSignalOrch opc = tellElt (Left (assignStmt0 opc))


-- | Write a signal via a zero-output opcode.
--
writeSignalInst :: Opcode0 rate -> BuildInst ()
writeSignalInst opc = tellElt (assignStmt0 opc)


--------------------------------------------------------------------------------
-- Binding

idef :: Expr IInit -> BuildInst (Var IInit)
idef expr = do 
    varid <- newLocVar (undefined :: IInit)
    tellElt (VarAssign (getVarI varid) (getExprI expr))
    return varid

kdef :: Expr KRate -> BuildInst (Var KRate)
kdef expr = do 
    varid <- newLocVar (undefined :: KRate)
    tellElt (VarAssign (getVarK varid) (getExprK expr))
    return varid

adef :: Expr ARate -> BuildInst (Var ARate)
adef expr = do 
    varid <- newLocVar (undefined :: ARate)
    tellElt (VarAssign (getVarA varid) (getExprA expr))
    return varid


ilet :: Opcode1 IInit -> BuildInst (Var IInit)
ilet opc = do 
    varid <- newLocVar (undefined :: IInit)
    tellElt (assignStmt1 varid opc)
    return varid


klet :: Opcode1 KRate -> BuildInst (Var KRate)
klet opc = do 
    varid <- newLocVar (undefined :: KRate)
    tellElt (assignStmt1 varid opc)
    return varid

alet :: Opcode1 ARate -> BuildInst (Var ARate)
alet opc = do 
    varid <- newLocVar (undefined :: ARate)
    tellElt (assignStmt1 varid opc)
    return varid


ilet2 :: Opcode2 IInit -> BuildInst (Var IInit, Var IInit)
ilet2 opc = do 
    v1 <- newLocVar (undefined :: IInit)
    v2 <- newLocVar (undefined :: IInit)
    tellElt (assignStmt2 v1 v2 opc)
    return (v1,v2)


klet2 :: Opcode2 KRate -> BuildInst (Var KRate, Var KRate)
klet2 opc = do 
    v1 <- newLocVar (undefined :: KRate)
    v2 <- newLocVar (undefined :: KRate)
    tellElt (assignStmt2 v1 v2 opc)
    return (v1,v2)

alet2 :: Opcode2 ARate -> BuildInst (Var ARate, Var ARate)
alet2 opc = do 
    v1 <- newLocVar (undefined :: ARate)
    v2 <- newLocVar (undefined :: ARate)
    tellElt (assignStmt2 v1 v2 opc)
    return (v1,v2)


ilet3 :: Opcode3 IInit -> BuildInst (Var IInit, Var IInit, Var IInit)
ilet3 opc = do 
    v1 <- newLocVar (undefined :: IInit)
    v2 <- newLocVar (undefined :: IInit)
    v3 <- newLocVar (undefined :: IInit)
    tellElt (assignStmt3 v1 v2 v3 opc)
    return (v1,v2,v3)


klet3 :: Opcode3 KRate -> BuildInst (Var KRate, Var KRate, Var KRate)
klet3 opc = do 
    v1 <- newLocVar (undefined :: KRate)
    v2 <- newLocVar (undefined :: KRate)
    v3 <- newLocVar (undefined :: KRate)
    tellElt (assignStmt3 v1 v2 v3 opc)
    return (v1,v2,v3)

alet3 :: Opcode3 ARate -> BuildInst (Var ARate, Var ARate, Var ARate)
alet3 opc = do 
    v1 <- newLocVar (undefined :: ARate)
    v2 <- newLocVar (undefined :: ARate)
    v3 <- newLocVar (undefined :: ARate)
    tellElt (assignStmt3 v1 v2 v3 opc)
    return (v1,v2,v3)


ilet4 :: Opcode4 IInit 
      -> BuildInst (Var IInit, Var IInit, Var IInit, Var IInit)
ilet4 opc = do 
    v1 <- newLocVar (undefined :: IInit)
    v2 <- newLocVar (undefined :: IInit)
    v3 <- newLocVar (undefined :: IInit)
    v4 <- newLocVar (undefined :: IInit)
    tellElt (assignStmt4 v1 v2 v3 v4 opc)
    return (v1,v2,v3,v4)


klet4 :: Opcode4 KRate 
      -> BuildInst (Var KRate, Var KRate, Var KRate, Var KRate)
klet4 opc = do 
    v1 <- newLocVar (undefined :: KRate)
    v2 <- newLocVar (undefined :: KRate)
    v3 <- newLocVar (undefined :: KRate)
    v4 <- newLocVar (undefined :: KRate)
    tellElt (assignStmt4 v1 v2 v3 v4 opc)
    return (v1,v2,v3,v4)

alet4 :: Opcode4 ARate 
      -> BuildInst (Var ARate, Var ARate, Var ARate, Var ARate)
alet4 opc = do 
    v1 <- newLocVar (undefined :: ARate)
    v2 <- newLocVar (undefined :: ARate)
    v3 <- newLocVar (undefined :: ARate)
    v4 <- newLocVar (undefined :: ARate)
    tellElt (assignStmt4 v1 v2 v3 v4 opc)
    return (v1,v2,v3,v4)


--------------------------------------------------------------------------------

goto :: Label -> BuildInst ()
goto lbl = tellElt (Goto GOTO lbl)

kgoto :: Label -> BuildInst ()
kgoto lbl = tellElt (Goto KGOTO lbl)

igoto :: Label -> BuildInst ()
igoto lbl = tellElt (Goto IGOTO lbl)


infix 4 .>.

(.>.) :: Expr rate -> Expr rate -> BinRel
(.>.) e1 e2 = BinRel greater_than (getExprUniv e1) (getExprUniv e2)

ifgoto :: BinRel -> Label -> BuildInst ()
ifgoto brel lbl = tellElt (IfGoto brel GOTO lbl)
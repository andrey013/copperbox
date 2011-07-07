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
  , (=<=)

  , alet

  , (.>.)
  , ifgoto

  ) where

import ZSnd.Core.CsoundInst.Prim
import ZSnd.Core.CsoundInst.Typed
import ZSnd.Core.Utils.HList

import Control.Applicative

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
    sk (_,_,w1) = Orch hdr $ toListH w1 

runOrchU :: OrchHeader -> BuildOrch a -> Orch
runOrchU hdr ma = either error sk $ getBM ma 1
  where
    sk (_,_,w1) = Orch hdr $ toListH w1 



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


(=<=) :: Var rate -> Opcode1 rate -> BuildInst ()
v1 =<= opc = tellElt (assignStmt1 v1 opc)

alet :: Opcode1 ARate -> BuildInst (Var ARate)
alet opc = do 
    varid <- newLocVar (undefined :: ARate)
    tellElt (assignStmt1 varid opc)
    return varid

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
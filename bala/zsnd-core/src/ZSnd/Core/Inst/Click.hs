{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Inst.Click
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Instrument defs patterned after the Click modular router language.
--
--------------------------------------------------------------------------------

module ZSnd.Core.Inst.Click
  (

    CExpr(..)
  , Element(..)
  , InConf(..)
  , OutConf(..)
  , TagVar(..)
  , DataRate(..)

  , FailMsg
  , translateDesc

  ) where

import ZSnd.Core.Inst.Prim
import ZSnd.Core.Utils.FormatCombinators


import Control.Applicative
import qualified Data.IntMap as IM
import Data.Monoid

import Prelude hiding ( lookup )

--
-- We want to keep the /Prim/ language and generate output by 
-- pretty printing it.
--
-- The Click language should be transformed to the Prim language.
--


newtype VarName = VarName { getVarName :: Int }
  deriving (Enum,Eq,Ord,Integral,Num,Real)

instance Show VarName where
  showsPrec p d = showsPrec p (getVarName d)



type Port  = Int


data CExpr = LetE VarName Element CExpr
           | (VarName, Port) :-> (VarName, Port)
           | Sequ [CExpr]
           | Out VarName
  deriving (Eq,Ord,Show)




-- | Element is a UGen.
--
data Element = Element
       { elt_name   :: String
       , elt_inputs :: [InConf]
       , elt_out    :: OutConf
       }
  deriving (Eq,Ord,Show)


-- Conf looks as though it will need to handle expressions 
-- (functions, (+),(*), etc.)

data InConf = SLiteral String              -- file names
            | DLiteral Double
            | ILiteral Int
            | CPfield  Int
            | CVar     VarName
            | CBinOp   String InConf InConf
  deriving (Eq,Ord,Show)


-- | It is assumed no opcodes actually generate multiple
-- output at different data rates...
--
data OutConf = Out0 
             | Out1 DataRate 
             | Out2 DataRate
             | OutN DataRate Int
  deriving (Eq,Ord,Show)



--------------------------------------------------------------------------------
-- Translation


-- | Translate a Click instrument.
--
translateDesc :: Int -> CExpr -> Either FailMsg PrimInst
translateDesc i cexp = case runTransMonad $ transStep cexp of
  Left err -> Left err
  Right (_,xs) -> Right $ Inst i xs




newtype TransMonad a = TM { 
          getTM :: Env -> St -> Either FailMsg (a, St, W) }

type FailMsg = String
type W       = StmtAcc
type Env     = DeclEnv

data St = St
      { i_num  :: Int
      , k_num  :: Int
      , a_num  :: Int
      }

instance Functor TransMonad where
  fmap f ma = TM $ \r s -> fmap (fAns f) $ getTM ma r s
                              
fAns :: (a -> d) -> (a,b,c) -> (d,b,c)
fAns f (a,b,c) = (f a, b, c)

instance Applicative TransMonad where
  pure a    = TM $ \_ s -> Right (a,s, mempty)
  mf <*> ma = TM $ \r s -> case getTM mf r s of
      Left err -> Left err 
      Right (f,s1,w1) -> case getTM ma r s1 of
        Left err -> Left err
        Right (a,s2,w2) -> Right (f a, s2, w1 `mappend` w2)



instance Monad TransMonad where
  return a  = TM $ \_ s -> Right (a, s, mempty)
  ma >>= k  = TM $ \r s -> case getTM ma r s of 
      Left err -> Left err
      Right (a,s1,w1) -> case (getTM . k) a r s1 of
        Left err -> Left err  
        Right (b,s2,w2) -> Right (b,s2, w1 `mappend` w2)


runTransMonad :: TransMonad a -> Either FailMsg (a,[Stmt])
runTransMonad ma = fmap post $ getTM ma envZero (St 0 0 0)
  where
    post (a,_,w) = (a, map snd $ getStmtAcc w)


reportFail :: String -> TransMonad a
reportFail msg = TM $ \_ _ -> Left msg

newLocVar :: DataRate -> TransMonad TagVar
newLocVar rt = TM $ \_ s -> Right $ step rt s
  where
    step I s = let i = i_num s in (LocVar I (i+1), s { i_num = i + 1 }, mempty)
    step K s = let i = k_num s in (LocVar K (i+1), s { k_num = i + 1 }, mempty)
    step A s = let i = a_num s in (LocVar A (i+1), s { a_num = i + 1 }, mempty)

-- | This is local of the reader monad.
--
localBinding :: VarName -> Element -> TransMonad a -> TransMonad a
localBinding vn elt ma = TM $ \r s -> 
    case insert vn elt r of
      Left err -> Left err
      Right r1 -> getTM ma r1 s


lookupElem :: VarName -> TransMonad Element
lookupElem vn = TM $ \r s -> 
    case lookup vn r of
      Nothing -> Left $ "error - missing declaration."
      Just elt -> Right (elt, s, mempty)

tellStmt :: VarName -> Stmt -> TransMonad ()
tellStmt vn stmt = TM $ \_ s -> Right ((), s, wrapSA vn stmt )



-- | This is wrong. opcodes should be generated for @a@ of (:->) 
-- if it is fresh...
--
transStep :: CExpr -> TransMonad ()
transStep (LetE vn elt body)  = 
    localBinding vn elt (transStep body)

transStep ((vn,_) :-> _b)     = do
    elt <- lookupElem vn
    invokeOpcode vn elt    

transStep (Sequ es)           = mapM_ transStep es

transStep (Out vn)            = do
    elt <- lookupElem vn
    invokeOpcode vn elt    


invokeOpcode :: VarName -> Element -> TransMonad ()
invokeOpcode vn (Element name inspec outspec) = do
    inps  <- mapM inpExpr inspec
    ovars <- outVars outspec
    tellStmt vn (Opcode ovars name inps) 



inpExpr ::InConf -> TransMonad Expr
inpExpr (SLiteral s)      = pure $ Literal $ CsString s
inpExpr (DLiteral d)      = pure $ Literal $ CsDouble d
inpExpr (ILiteral i)      = pure $ Literal $ CsInt i
inpExpr (CPfield i)       = pure $ PField i
inpExpr (CVar _v)         = pure $ Literal $ CsString "varFAIL"
inpExpr (CBinOp op e1 e2) = 
    (\a b -> BinOp op a b) <$> inpExpr e1 <*> inpExpr e2

outVars :: OutConf -> TransMonad [TagVar]
outVars Out0        = pure []
outVars (Out1 rt)   = (\a -> [a]) <$> newLocVar rt
outVars (Out2 rt)   = (\a b -> [a,b]) <$> newLocVar rt <*> newLocVar rt
outVars (OutN rt n) = countA n (newLocVar rt)


countA :: Applicative f => Int -> f a -> f [a]
countA i _  | i <= 0 = pure []
countA i ma          = (:) <$> ma <*> countA (i-1) ma


--------------------------------------------------------------------------------
-- Env for decls

newtype DeclEnv = DeclEnv { getDeclEnv :: IM.IntMap Element }

envZero :: DeclEnv
envZero = DeclEnv mempty

lookup :: VarName -> DeclEnv -> Maybe Element
lookup vn  = IM.lookup (getVarName vn) . getDeclEnv


-- | Note - insert of the same name causes failure.
--
-- This seems in keeping with Click, although the current 
-- in-Haskell implementation could avoid this at it gets scoping
-- for free. 
-- 
insert :: VarName -> Element -> DeclEnv -> Either FailMsg DeclEnv
insert vn elt env = case lookup vn env of
   Nothing -> Right $ DeclEnv $ IM.insert (getVarName vn) elt (getDeclEnv env)
   Just _  -> Left  $ "error - mulitple variable declaration."


-- Note - we well need an operation to @fill-in@ ports...





--------------------------------------------------------------------------------
-- Accumulator for Stmts

-- Generating a list of statements is a bit convoluted:
--
-- We want to generate a Stmt (opcode invocation) only the first 
-- time we see the opcode result invoked on the RHS of a 
-- connection.
-- 
-- We don\'t want to generate a Stmt for a declaration as we might
-- have unknown ports at this point.
--

newtype StmtAcc = StmtAcc { getStmtAcc :: [(VarName, Stmt)] }

wrapSA :: VarName -> Stmt -> StmtAcc 
wrapSA nm stmt = StmtAcc { getStmtAcc = [(nm, stmt)] }

instance Monoid StmtAcc where
  mempty = StmtAcc []
  a `mappend` b = StmtAcc $ merge (getStmtAcc a) (getStmtAcc b)
    where
      merge []            ys = ys
      merge xs            [] = xs
      merge (x@(vn,_):xs) ys = x : merge xs (elim vn ys) 

      elim vn (z@(vn1,_):zs) | vn == vn1 = zs
                             | otherwise = z : elim vn zs

      elim _  []             = []


--------------------------------------------------------------------------------
-- Format instances (useful for debugging)

instance Format CExpr where
   format (LetE name elt expr)  = 
      format name <+> text "::" <+> format elt <> char ';'
        `vconcat` format expr

   format ((v1,i1) :-> (v2,i2))           = 
     format v1 <> brackets (int i1) <+> text "->" 
               <+> brackets (int i2) <> format v2 <> char ';'

   format (Sequ xs)             = vcat (map format xs)
   format (Out v1)              = text "==>" <+>  format v1 <> char ';'
     

instance Format Element where
  format (Element name _ins _outs) = text name

instance Format InConf where
  format (SLiteral s)           = dquotes $ text s
  format (DLiteral d)           = dtrunc d
  format (ILiteral i)           = int i
  format (CPfield i)            = char 'p' <> int i
  format (CVar v)               = format v
  format (CBinOp ss a b)        = format a <> text ss <> format b

instance Format VarName where
  format (VarName i) = text "elt" <> int i
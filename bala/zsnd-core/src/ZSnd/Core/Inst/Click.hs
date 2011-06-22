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
import qualified Data.IntMap    as IM
import qualified Data.Map       as M
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


-- Connection is actually another Let...

data CExpr = LetD VarName Element CExpr
           | LetC (VarName, Port) (VarName, Port) CExpr
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
            | CBinOp   String InConf InConf
            | ClkPort  Int
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


newLocVar :: DataRate -> TransMonad TagVar
newLocVar rt = TM $ \_ s -> Right $ step rt s
  where
    step I s = let i = i_num s in (LocVar I (i+1), s { i_num = i + 1 }, mempty)
    step K s = let i = k_num s in (LocVar K (i+1), s { k_num = i + 1 }, mempty)
    step A s = let i = a_num s in (LocVar A (i+1), s { a_num = i + 1 }, mempty)

-- | This is local of the reader monad.
--
localBindDecl :: VarName -> Element -> TransMonad a -> TransMonad a
localBindDecl vn elt ma = TM $ \r s -> 
    case insertDecl vn elt r of
      Left err -> Left err
      Right r1 -> getTM ma r1 s


localBindPorts :: VarName -> [TagVar]  -> TransMonad a -> TransMonad a
localBindPorts vn vs ma = TM $ \r s -> step r s $ zip [0..] vs
  where
    step r s [] = getTM ma r s
    step r s ((i,tv):zs) = case insertPortAssignment (vn,i) tv r of
                             Left err -> Left err
                             Right r1 -> step r1 s zs


localBindConn :: (VarName,Int) -> (VarName,Int) -> TransMonad a -> TransMonad a
localBindConn keyfr keyto ma = TM $ \r s ->
    case insertPortConn keyfr keyto r of
      Left err -> Left err
      Right r1 -> getTM ma r1 s


lookupElem :: VarName -> TransMonad Element
lookupElem vn = TM $ \r s -> 
    case lookupDecl vn r of
      Nothing -> Left $ "error - missing declaration."
      Just elt -> Right (elt, s, mempty)


lookupPort :: VarName -> Int -> TransMonad TagVar
lookupPort vn i = TM $ \r s -> 
    case lookupPortRef (vn,i) r of
      Nothing -> Left $ "error - uninstantiated port."
      Just tv -> Right (tv, s, mempty)
 


tellStmt :: VarName -> Stmt -> TransMonad ()
tellStmt vn stmt = TM $ \_ s -> Right ((), s, wrapSA vn stmt )



-- | This is wrong. opcodes should be generated for @a@ of (:->) 
-- if it is fresh...
--
transStep :: CExpr -> TransMonad ()
transStep (LetD vn elt body)  = 
    localBindDecl vn elt (transStep body)

transStep (LetC a@(vno,_) b body)     = do
    elt  <- lookupElem vno
    outs <- invokeOpcode vno elt
    localBindPorts vno outs $ localBindConn a b $ transStep body
  
transStep (Out vn)            = do
    elt  <- lookupElem vn
    _    <- invokeOpcode vn elt
    return ()
    

invokeOpcode :: VarName -> Element -> TransMonad [TagVar]
invokeOpcode vn (Element name inspec outspec) = do
    inps  <- mapM (inpExpr vn) inspec
    ovars <- outVars outspec
    tellStmt vn (Opcode ovars name inps) 
    return ovars


inpExpr :: VarName -> InConf -> TransMonad Expr
inpExpr _  (SLiteral s)      = pure $ Literal $ CsString s
inpExpr _  (DLiteral d)      = pure $ Literal $ CsDouble d
inpExpr _  (ILiteral i)      = pure $ Literal $ CsInt i
inpExpr _  (CPfield i)       = pure $ PField i
inpExpr vn (CBinOp op e1 e2) = 
    (\a b -> BinOp op a b) <$> inpExpr vn e1 <*> inpExpr vn e2

inpExpr vn (ClkPort i)       = (\tv -> VarE tv) <$> lookupPort vn i

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

data Env = Env 
      { _env_decls       :: IM.IntMap Element 
      , _env_port_assigs :: M.Map (VarName,Int) TagVar
      , _env_port_conns  :: M.Map (VarName,Int) (VarName,Int)
      }

envZero :: Env
envZero = Env mempty mempty mempty

lookupDecl :: VarName -> Env -> Maybe Element
lookupDecl vn  = IM.lookup (getVarName vn) . _env_decls


-- | Note - insert of the same name causes failure.
--
-- This seems in keeping with Click, although the current 
-- in-Haskell implementation could avoid this at it gets scoping
-- for free. 
-- 
insertDecl :: VarName -> Element -> Env -> Either FailMsg Env
insertDecl vn elt env@(Env decls passns pconns) = 
    case lookupDecl vn env of
      Nothing -> let decls1 = IM.insert (getVarName vn) elt decls
                 in Right $ Env decls1 passns pconns
      Just _  -> Left  $ "error - multiple variable declaration."


lookupPortRef :: (VarName,Int) -> Env -> Maybe TagVar
lookupPortRef key (Env _ passns pconns) = 
    case M.lookup key pconns of
      Nothing -> Nothing 
      Just pkey -> M.lookup pkey passns

insertPortAssignment :: (VarName,Int) -> TagVar -> Env -> Either FailMsg Env
insertPortAssignment key var (Env decls passns pconns) = 
    case M.lookup key passns of 
      Nothing -> let passns1 = M.insert key var passns
                 in Right $ Env decls passns1 pconns
      Just _  -> Left  $ "error - mulitple port assignment."



-- | Implementation note - map is @ to => from @ so keys are flipped.
--
insertPortConn :: (VarName,Int) -> (VarName,Int) -> Env -> Either FailMsg Env
insertPortConn keyfrom keyto (Env decls passns pconns) = 
    case M.lookup keyto pconns of 
      Nothing -> let pconns1 = M.insert keyto keyfrom pconns
                 in Right $ Env decls passns pconns1
      Just _  -> Left  $ "error - mulitple port assignment."





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
   format (LetD name elt expr)  = 
      format name <+> text "::" <+> format elt <> char ';'
        `vconcat` format expr

   format (LetC (v1,i1) (v2,i2) expr)           = 
     format v1 <> brackets (int i1) <+> text "->" 
               <+> brackets (int i2) <> format v2 <> char ';'
               `vconcat` format expr

   format (Out v1)              = text "==>" <+>  format v1 <> char ';'
     

instance Format Element where
  format (Element name _ins _outs) = text name

instance Format InConf where
  format (SLiteral s)           = dquotes $ text s
  format (DLiteral d)           = dtrunc d
  format (ILiteral i)           = int i
  format (CPfield i)            = char 'p' <> int i
  format (ClkPort v)            = format v
  format (CBinOp ss a b)        = format a <> text ss <> format b

instance Format VarName where
  format (VarName i) = text "elt" <> int i
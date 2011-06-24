{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
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

    DeclRef
  , CStmt(..)
  , UElement(..)
  , InConf(..)
  , OutConf(..)
  , TagVar(..)
  , DataRate(..)

  , FailMsg
  , translateDesc

  ) where

import ZSnd.Core.Inst.Prim
import ZSnd.Core.Utils.FormatCombinators
import ZSnd.Core.Utils.FormatExpr


import Control.Applicative
import Data.List ( sortBy )
import qualified Data.Map       as M
import Data.Monoid

import Prelude hiding ( lookup )

--
-- We want to keep the /Prim/ language and generate output by 
-- pretty printing it.
--
-- The Click language should be transformed to the Prim language.
--


newtype DeclRef = DeclRef { getDeclRef :: Int }
  deriving (Enum,Eq,Integral,Num,Ord,Real)

instance Show DeclRef where
  showsPrec p = showsPrec p . getDeclRef

type Port  = Int




data CStmt = Decl DeclRef UElement
           | Conn (DeclRef, Port) (DeclRef, Port)
           | Out  DeclRef
  deriving (Eq,Ord,Show)


--
-- data Final = Out TagVar
--            | GAssign TagVar 
--

-- If we add in assignment to global vars, we lose the sense of
-- an expression language:
-- 
--         | Assign TagVar CExpr
--
-- Needs an analogue type to InConf:
--
--         | Assign TagVar GConf
--
-- data GExpr = GVar TagVar | GUnOp String GExpr | ..
--

-- newtype Element rate = Element { getElement :: UElement }
--   deriving (Eq,Ord,Show)

-- | UElement - universally typed element. This is a UGen.
--
data UElement = UElement
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
            | CFuncall String InConf
            | CUnOp    Rator InConf
            | CBinOp   Rator InConf InConf
            | ClkPort  Int
            | SatPort  TagVar
  deriving (Eq,Ord,Show)



-- | It is assumed no opcodes actually generate multiple
-- output at different data rates...
--
data OutConf = Out0 
             | Out1 DataRate 
             | Out2 DataRate
             | OutN DataRate Int
  deriving (Eq,Ord,Show)



instance Num InConf where
  (+)    = CBinOp $ infixL 6 "+"
  (-)    = CBinOp $ infixL 6 "-"
  (*)    = CBinOp $ infixL 7 "*"
  abs    = CFuncall "abs"
  negate = CUnOp  $ prefix 9 "-"
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = ILiteral (fromInteger i)


instance Fractional InConf where
  (/)            = CBinOp $ infixL 7 "/"  
  recip _        = error "recip - no interpretation of recip in Csound."  
  fromRational d = DLiteral $ fromRational d

--------------------------------------------------------------------------------
-- Translation


-- | Translate a Click instrument.
--
translateDesc :: Int -> [CStmt] -> Either FailMsg PrimInst
translateDesc i stmts = case runTransMonad $ mapM transStmt stmts of
  Left err -> Left err
  Right xs -> Right $ PrimInst i xs




newtype TransMonad a = TM { 
          getTM :: St -> AccDecls -> Either FailMsg (a, St, AccDecls) }

type FailMsg = String


data St = St
      { i_num    :: Int
      , k_num    :: Int
      , a_num    :: Int
      , pa_count :: Int         -- count the port assignments
      }

instance Functor TransMonad where
  fmap f ma = TM $ \s ac -> fmap post $ getTM ma s ac
    where
      post (a,s1,ac1) = (f a, s1,ac1)
                              

instance Applicative TransMonad where
  pure a    = TM $ \s ac -> Right (a,s,ac)
  mf <*> ma = TM $ \s ac -> case getTM mf s ac of
      Left err -> Left err 
      Right (f,s1,ac1) -> case getTM ma s1 ac1 of
        Left err2 -> Left err2
        Right (a,s2,ac2) -> Right (f a, s2, ac2)


instance Monad TransMonad where
  return a  = TM $ \s ac -> Right (a, s, ac)
  ma >>= k  = TM $ \s ac -> case getTM ma s ac of 
      Left err -> Left err
      Right (a,s1,ac1) -> (getTM . k) a s1 ac1


runTransMonad :: TransMonad a -> Either FailMsg [Stmt]
runTransMonad ma = getTM ma (St 0 0 0 0) accdZero >>= sk
  where
    sk (_,_,ac) = mapM transElement $ extractDecls ac


newLocVar :: DataRate -> TransMonad TagVar
newLocVar rt = TM $ \s ac -> let (a,s1) = step rt s in Right (a,s1,ac)
  where
    step I s = let i = i_num s in (LocVar I (i+1), s { i_num = i + 1 })
    step K s = let i = k_num s in (LocVar K (i+1), s { k_num = i + 1 })
    step A s = let i = a_num s in (LocVar A (i+1), s { a_num = i + 1 })




bindDecl :: DeclRef -> UElement -> [TagVar] -> TransMonad ()
bindDecl dref elt outs = TM $ \s ac -> 
    let ac1 = updateDecl dref (EStmt (pa_count s) elt outs) ac
    in Right ((),s,ac1)


bindPorts :: DeclRef -> [TagVar] -> TransMonad ()
bindPorts dref vs = TM $ \s ac -> step s ac $ zip [0..] vs
  where
    step s ac []          = Right ((),s,ac)
    step s ac ((i,tv):zs) = 
      addPortAssignment (dref,i) tv ac >>= \ac1 -> step s ac1 zs


assignPort :: (DeclRef,Int) -> (DeclRef,Int) -> TransMonad ()
assignPort pfrom (dref,pnum) = TM $ \s ac -> 
    case M.lookup pfrom (acc_port_assigns ac) of
      Nothing -> Left "error - missing port assignment."
      Just tv -> case M.lookup dref (acc_decls ac) of 
        Nothing -> Left "error - missing declaration ref."
        Just estmt -> sk s ac tv estmt
  where
    sk s ac tv (EStmt _ elt outs) = updatePort pnum tv elt >>= \elt1 -> 
          let pa  = pa_count s 
              ac1 = updateDecl dref (EStmt pa elt1 outs) ac
          in Right ((),s { pa_count = pa + 1 }, ac1) 


-- This is not good - there is a better correspondence (1-1) 
-- between decls (Click) and opcode stmts (Csound).



-- Decl generates an opcode Stmt - it may have unassigned ports
-- which need filling in.
--

transStmt :: CStmt -> TransMonad ()
transStmt (Decl dref elt)  = do
    ovars <- outVars $ elt_out elt
    bindDecl dref elt ovars 
    bindPorts dref ovars

transStmt (Conn a b)       = assignPort a b  

transStmt (Out _)          = return ()
    

outVars :: OutConf -> TransMonad [TagVar]
outVars Out0        = pure []
outVars (Out1 rt)   = (\a -> [a]) <$> newLocVar rt
outVars (Out2 rt)   = (\a b -> [a,b]) <$> newLocVar rt <*> newLocVar rt
outVars (OutN rt n) = countA n (newLocVar rt)


countA :: Applicative f => Int -> f a -> f [a]
countA i _  | i <= 0 = pure []
countA i ma          = (:) <$> ma <*> countA (i-1) ma


transElement :: (UElement,[TagVar]) -> Either FailMsg Stmt
transElement (UElement name ins _, outs) = fmap sk $ mapM fn ins
  where
    sk xs                = Opcode outs name xs
    fn (SLiteral s)      = Right $ Literal $ CsString s
    fn (DLiteral d)      = Right $ Literal $ CsDouble d
    fn (ILiteral i)      = Right $ Literal $ CsInt i
    fn (CFuncall s e1)   = (\a -> Funcall s a) <$> fn e1
    fn (CPfield i)       = Right $ PField i
    fn (CUnOp op e1)     = (\a -> UnOp op a) <$> fn e1
    fn (CBinOp op e1 e2) = (BinOp op) <$> fn e1 <*> fn e2
    fn (SatPort tv)      = Right $ VarE tv
    fn (ClkPort _)       = Left $ "error - unassigned port."


--------------------------------------------------------------------------------
-- Acummulate declarations

-- Maybe this is state - mutated rather than forked by @local@...

data AccDecls = AccDecls 
      { acc_decls           :: M.Map DeclRef EStmt
      , acc_port_assigns    :: M.Map (DeclRef,Int) TagVar
      }

-- | Statement under evaluation...
--
data EStmt = EStmt 
      { _last_port_assignment    :: Int 
      , _estmt_element           :: UElement
      , _estmt_outs              :: [TagVar]
      }

accdZero :: AccDecls
accdZero = AccDecls mempty mempty

-- This actually needs [TagVar] as well...
--
extractDecls :: AccDecls -> [(UElement, [TagVar])]
extractDecls = 
    map unPa . sortBy cmp . M.elems . acc_decls
  where
    cmp (EStmt pa1 _ _) (EStmt pa2 _ _) = compare pa1 pa2
    unPa (EStmt _ elt outs)             = (elt,outs)

addPortAssignment :: (DeclRef,Int) -> TagVar -> AccDecls 
                  -> Either FailMsg AccDecls
addPortAssignment key cstag (AccDecls decls passns) = 
    case M.lookup key passns of
      Nothing -> let passns1 = M.insert key cstag passns
                 in Right $ AccDecls decls passns1
      Just _  -> Left  $ "error - multiple port assignment."


-- | Note unlike port assignement, fail case should never happen.
--
updateDecl :: DeclRef -> EStmt -> AccDecls -> AccDecls
updateDecl key estmt (AccDecls decls passns) = AccDecls decls1 passns
  where
    decls1 = M.insert key estmt decls
     
    
-- This doesn\'t work on a ClkPort deeply embedded in an expr...
--
updatePort :: Int -> TagVar -> UElement -> Either FailMsg UElement
updatePort pnum tv (UElement name cfgs out) = 
    step id cfgs
  where
    step _  []              = Left $ "error - update port, missing port - "
                                      ++ name ++ show [pnum]
    step ac (x:xs)  = case single x of
                       (_,False) -> step (ac . (x:)) xs
                       (e,True) -> let cfgs' = ac $ e : xs
                                   in Right $ UElement name cfgs' out


    single (ClkPort i) 
        | i == pnum          = (SatPort tv,True)
        | otherwise          = (ClkPort i, False)

    single (CFuncall s e1)   = let (e2,bl) = single e1 in (CFuncall s e2,bl)
    single (CUnOp op e1)     = let (e2,bl) = single e1 in (CUnOp op e2,bl)
    single (CBinOp op e1 e2) = let (e3,b1) = single e1 
                                   (e4,b2) = single e2
                               in (CBinOp op e3 e4, b1 || b2)
    single e1                = (e1, False)


--------------------------------------------------------------------------------
-- Format instances (useful for debugging)

instance Format CStmt where
  format (Decl name elt)        = 
      format name <+> text "::" <+> format elt <> char ';'
    

  format (Conn (v1,i1) (v2,i2)) = 
      format v1 <> brackets (int i1) <+> text "->" 
               <+> brackets (int i2) <> format v2 <> char ';'
               

  format (Out v1)               = text "==>" <+> format v1 <> char ';'
     

instance Format UElement where
  format (UElement name _ins _outs) = text name

instance Format InConf where
  format = unparse . buildExpr

buildExpr :: InConf -> DocExpr
buildExpr (SLiteral s)          = Atom $ dquotes $ text s
buildExpr (DLiteral d)          = Atom $ dtrunc d
buildExpr (ILiteral i)          = Atom $ int i
buildExpr (CFuncall ss a)       = Atom $ text ss <> parens (format a)
buildExpr (CPfield i)           = Atom $ char 'p' <> int i
buildExpr (CUnOp op a)          = Unary op (buildExpr a)
buildExpr (CBinOp op a b)       = Binary (buildExpr a) op (buildExpr b)
buildExpr (ClkPort v)           = Atom $ format v
buildExpr (SatPort v)           = Atom $ format v

instance Format DeclRef where
  format (DeclRef i) = text "var" <> int i 

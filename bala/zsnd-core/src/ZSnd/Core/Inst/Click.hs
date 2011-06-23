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

    DeclRef
  , CExpr(..)
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


-- Connection is actually another Let...
-- 
-- Remember the TagVar of out is the name of the /element/ -
-- it is not the name of the out port.


data CExpr = LetD DeclRef Element CExpr
           | LetC (DeclRef, Port) (DeclRef, Port) CExpr
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
            | CFuncall String InConf
            | CUnOp    String InConf
            | CBinOp   String InConf InConf
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
  (+)    = CBinOp "+"
  (-)    = CBinOp "-"
  (*)    = CBinOp "*"
  abs    = CUnOp "abs"
  negate = CUnOp "-"
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = ILiteral (fromInteger i)


instance Fractional InConf where
  (/)            = CBinOp "/"  
  recip _        = error "recip - no interpretation of recip in Csound."  
  fromRational d = DLiteral $ fromRational d

--------------------------------------------------------------------------------
-- Translation


-- | Translate a Click instrument.
--
translateDesc :: Int -> CExpr -> Either FailMsg PrimInst
translateDesc i cexp = case runTransMonad $ transStep cexp of
  Left err -> Left err
  Right xs -> Right $ Inst i xs




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




bindDecl :: DeclRef -> Element -> [TagVar] -> TransMonad ()
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



transStep :: CExpr -> TransMonad ()

-- LetD generates an opcode Stmt - it may have unassigned ports
-- which need filling in.
--
transStep (LetD dref elt body)        = do
    ovars <- outVars $ elt_out elt
    bindDecl dref elt ovars 
    bindPorts dref ovars
    transStep body

transStep (LetC a b body)   = do
    assignPort a b
    transStep body
  
transStep (Out vn)                  = return ()
    

outVars :: OutConf -> TransMonad [TagVar]
outVars Out0        = pure []
outVars (Out1 rt)   = (\a -> [a]) <$> newLocVar rt
outVars (Out2 rt)   = (\a b -> [a,b]) <$> newLocVar rt <*> newLocVar rt
outVars (OutN rt n) = countA n (newLocVar rt)


countA :: Applicative f => Int -> f a -> f [a]
countA i _  | i <= 0 = pure []
countA i ma          = (:) <$> ma <*> countA (i-1) ma


transElement :: (Element,[TagVar]) -> Either FailMsg Stmt
transElement (Element name ins _, outs) = fmap sk $ mapM fn ins
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
    fn (ClkPort i)       = Left $ "error - unassigned port."


--------------------------------------------------------------------------------
-- Acummulate declarations

-- Maybe this is state - mutated rather than forked by @local@...

data AccDecls = AccDecls 
      { acc_decls           :: M.Map DeclRef EStmt
      , acc_port_assigns    :: M.Map (DeclRef,Int) TagVar
      }

-- | Statement under evaluation...
--
-- 
--
data EStmt = EStmt 
      { last_port_assignment    :: Int 
      , estmt_element           :: Element
      , estmt_outs              :: [TagVar]
      }

accdZero :: AccDecls
accdZero = AccDecls mempty mempty

-- This actually needs [TagVar] as well...
--
extractDecls :: AccDecls -> [(Element, [TagVar])]
extractDecls = 
    map unPa . sortBy cmp . M.elems . acc_decls
  where
    cmp (EStmt pa1 _ _) (EStmt pa2 _ _) = compare pa1 pa2
    unPa (EStmt _ elem outs)            = (elem,outs)

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
updatePort :: Int -> TagVar -> Element -> Either FailMsg Element
updatePort pnum tv (Element name cfgs out) = 
    step id cfgs
  where
    step _  []              = Left $ "error - update port, missing port - "
                                      ++ name ++ show [pnum]
    step ac (ClkPort i:xs) 
        | i == pnum         = let cfgs' = ac $ (SatPort tv) : xs
                              in Right $ Element name cfgs' out
    step ac (x:xs)          = step (ac . (x:)) xs



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

   format (Out v1)               = text "==>" <+> format v1 <> char ';'
     

instance Format Element where
  format (Element name _ins _outs) = text name

instance Format InConf where
  format (SLiteral s)           = dquotes $ text s
  format (DLiteral d)           = dtrunc d
  format (ILiteral i)           = int i
  format (CFuncall ss a)        = text ss <> parens (format a)
  format (CPfield i)            = char 'p' <> int i
  format (CUnOp ss a)           = text ss <> format a
  format (CBinOp ss a b)        = format a <> text ss <> format b
  format (ClkPort v)            = format v
  format (SatPort v)            = format v

instance Format DeclRef where
  format (DeclRef i) = text "var" <> int i 

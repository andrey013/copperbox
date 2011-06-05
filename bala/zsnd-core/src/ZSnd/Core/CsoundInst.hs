{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CsoundInst
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Csound instrument
--
--------------------------------------------------------------------------------

module ZSnd.Core.CsoundInst
  (

    Orch(..)
  , OrchHeader(..)
  , Inst
  , default_orch_header

  , InstBuilder
  , Stmt
  , Expr(..)
  , DExpr(..)

  , IRate
  , KRate
  , ARate

  , runInstBuilder

  , ivar
  , iopcode
  , kvar
  , kopcode
  , avar
  , aopcode


  , out
  , outs


  ) where


import ZSnd.Core.Utils.FormatCombinators
import ZSnd.Core.Utils.HList

import Control.Applicative hiding ( Const, empty )


data Orch = Orch
      { orch_header     :: OrchHeader
      , orch_insts      :: [Inst]
      }
  deriving (Eq,Ord,Show)

-- | Orchestra file header values.
--
-- The following fields are not accessible to ZScore:
--
-- > ctrlinit   
-- > massign
-- > pgmassign
-- > pset
-- > strset
--
data OrchHeader = OrchHeader 
      { zero_dbfs               :: Int     -- default 32767
      , kr_ctrl_rate            :: Int     -- default 1000
      , sr_aud_sample_rate      :: Int     -- default 44100
      , ksmps_ctrl_period_samps :: Int     -- default 10
      , nchnls_num_chans        :: Int     -- default 1 (mono)
      , seed_gbl_random         :: Maybe Int  
      }
  deriving (Eq,Ord,Show)

      
default_orch_header :: OrchHeader 
default_orch_header =  
    OrchHeader { zero_dbfs                = 32767
               , kr_ctrl_rate             = 4410
               , sr_aud_sample_rate       = 44100
               , ksmps_ctrl_period_samps  = 32
               , nchnls_num_chans         = 1
               , seed_gbl_random          = Nothing
               }


data Inst = Inst { inst_num :: Int, inst_body :: [Stmt] }
  deriving (Eq,Ord,Show)


newtype InstBuilder a = Build { 
          getBuild :: IntSupply -> (a, H Stmt, IntSupply) }


data IntSupply = IntSupply
      { i_int   :: Int
      , k_int   :: Int
      , a_int   :: Int
      }


data CsValue = CsInt Int
             | CsDouble Double
  deriving (Eq,Ord,Show)

data Stmt = Vardef  SVar DExpr
          | Opcode  SVar String [DExpr]
          | Outs    [DExpr]
  deriving (Eq,Ord,Show)


-- | Synthesized variable
--
data SVar = SVar RateVar Int
  deriving (Eq,Ord,Show)


-- | Dynamically typed expr
-- 
data DExpr = Var     SVar
           | PField  Int
           | Const   CsValue
           | UnOp    String DExpr
           | BinOp   DExpr String DExpr
           | Funcall String DExpr
  deriving (Eq,Ord,Show)


newtype Expr rate = Expr { getExpr :: DExpr }
  deriving (Eq,Ord,Show)


data IRate
data KRate
data ARate

-- | CSB page22.
--
data RateVar = I | K | A 
  deriving (Eq,Ord,Show)


binop :: String -> Expr a -> Expr a -> Expr a
binop name a b = Expr $ BinOp (getExpr a) name (getExpr b)

instance Num (Expr a) where
  (+) = binop "+"
  (-) = binop "-"
  (*) = binop "*"
  abs    = Expr . UnOp "abs" . getExpr
  negate = Expr . UnOp "-"   . getExpr
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = Expr $ Const $ CsInt $ fromInteger i

instance Fractional (Expr a) where
  (/) = binop "/"  
  recip _        = error "recip - no interpretation of recip in Csound."  
  fromRational i = Expr $ Const $ CsDouble $ fromRational i



instance Functor InstBuilder where
  fmap f ma = Build $ \s -> let (a,w,s1) = getBuild ma s in (f a, w, s1)

instance Applicative InstBuilder where
  pure a    = Build $ \s -> (a, emptyH, s)
  mf <*> ma = Build $ \s -> let (f,w1,s1) = getBuild mf s
                                (a,w2,s2) = getBuild ma s1
                            in (f a, w1 `appendH` w2, s2)

instance Monad InstBuilder where
  return a  = Build $ \s -> (a, emptyH, s)
  ma >>= k  = Build $ \s -> let (a,w1,s1) = getBuild ma s
                                (b,w2,s2) = (getBuild . k) a s1
                            in (b, w1 `appendH` w2, s2)

runInstBuilder :: Int -> InstBuilder a -> Inst
runInstBuilder n ma = post $ getBuild ma (IntSupply 1 1 1)
  where
    post (_,w,_) = Inst { inst_num = n, inst_body = toListH w }



tellStmt :: Stmt -> InstBuilder ()
tellStmt e = Build $ \s -> ((), wrapH e, s)

freshIVar :: InstBuilder SVar
freshIVar = Build $ \s -> 
    let n = i_int s in (SVar I n, emptyH, s { i_int = n + 1 })

freshKVar :: InstBuilder SVar
freshKVar = Build $ \s -> 
    let n = k_int s in (SVar K n, emptyH, s { k_int = n + 1 })

freshAVar :: InstBuilder SVar
freshAVar = Build $ \s -> 
    let n = a_int s in (SVar A n, emptyH, s { a_int = n + 1 })


mkVar :: SVar -> DExpr -> InstBuilder DExpr
mkVar v expr = tellStmt (Vardef v expr) >> return (Var v)


-- | Note some opcodes return pairs...
--
mkOpcode :: SVar -> String -> [DExpr] -> InstBuilder DExpr
mkOpcode v opcd es = tellStmt (Opcode v opcd es) >> return (Var v)




ivar :: Expr IRate -> InstBuilder (Expr IRate)
ivar expr = freshIVar >>= \v -> fmap Expr $ mkVar v (getExpr expr)

iopcode :: String -> [DExpr] -> InstBuilder (Expr IRate)
iopcode opcd es = freshIVar >>= \v -> fmap Expr $ mkOpcode v opcd es

kvar :: DExpr -> InstBuilder (Expr KRate)
kvar expr = freshKVar >>= \v -> fmap Expr $ mkVar v expr

kopcode :: String -> [DExpr] -> InstBuilder (Expr KRate)
kopcode opcd es = freshKVar >>= \v -> fmap Expr $ mkOpcode v opcd es

avar :: DExpr -> InstBuilder (Expr ARate)
avar expr = freshAVar >>= \v -> fmap Expr $ mkVar v expr

aopcode :: String -> [DExpr] -> InstBuilder (Expr ARate)
aopcode opcd es = freshAVar >>= \v -> fmap Expr $ mkOpcode v opcd es


out :: Expr ARate -> InstBuilder ()
out e = tellStmt $ Outs [getExpr e]

outs :: [Expr ARate] -> InstBuilder ()
outs = tellStmt . Outs . map getExpr



--------------------------------------------------------------------------------
-- Format instances


instance Format Orch where
  format (Orch hdr xs) = vspace (format hdr : map format xs)

instance Format OrchHeader where
  format orch =        lhs "0dbfs"  <+> int (zero_dbfs orch)
            `vconcat`  lhs "sr"     <+> int (sr_aud_sample_rate orch)
--            `vconcat`  lhs "kr"     <+> int (kr_ctrl_rate orch)
            `vconcat`  lhs "ksmps"  <+> int (ksmps_ctrl_period_samps orch)
            `vconcat`  lhs "nchnls" <+> int (nchnls_num_chans orch)
            `vconcat`  opt_seed
    where
      lhs s = text s <+> char '='
      opt_seed = case seed_gbl_random orch of 
                   Nothing -> empty
                   Just i  -> lhs "seed" <+> int i


instance Format Inst where
  format (Inst n xs) = vcat [ text "instr" <+> int n 
                            , vcat $ map format xs
                            , text "endin"
                            ]


instance Format Stmt where
  format (Vardef var val)    = 
    indent 8 (padr 11 $ format var) <+> char '=' <+> format val

  format (Opcode var name xs) = 
    padr 7 (format var) <+> padr 11 (text name) 
                        <+> punctuate (text ", ") (map format xs)
  
  format (Outs [x])           = 
    indent 8 (padr 11 (text "out") <+> format x)
  
  format (Outs xs)            = 
    indent 8 (padr 11 (text "outs") <+> punctuate (text ", ") (map format xs))

instance Format (Expr a) where
  format = format . getExpr

instance Format DExpr where
  format (Var s)        = format s
  format (PField i)     = char 'p' <> int i 
  format (Const val)    = format val
  format (BinOp a ss b) = format a <> text ss <> format b
  format (UnOp ss a)    = text ss <> format a
  format (Funcall ss a) = text ss <> parens (format a)

instance Format SVar where
  format (SVar rv i) = format rv <> int i


instance Format CsValue where
  format (CsInt i)    = int i
  format (CsDouble d) = dtrunc d



instance Format RateVar where
  format I = char 'i'
  format K = char 'k'
  format A = char 'a'

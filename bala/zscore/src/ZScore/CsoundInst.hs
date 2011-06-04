{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZScore.CsoundInst
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

module ZScore.CsoundInst
  (

    Inst
  , InstBuilder
  , Stmt
  , Expr(..)

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


import ZScore.Utils.FormatCombinators
import ZScore.Utils.HList

import Control.Applicative hiding ( Const )


data IntSupply = IntSupply
      { i_int   :: Int
      , k_int   :: Int
      , a_int   :: Int
      }

data Inst = Inst { inst_num :: Int, inst_body :: [Stmt] }
  deriving (Eq,Ord,Show)

newtype InstBuilder a = Build { 
          getBuild :: IntSupply -> (a, H Stmt, IntSupply) }


data CsValue = CsInt Int
             | CsDouble Double
  deriving (Eq,Ord,Show)

data Stmt = Vardef  SVar Expr
          | Opcode  SVar String [Expr]
          | Outs    [Expr]
  deriving (Eq,Ord,Show)

-- TODO - maybe Expr should have a rate phantom argument.

-- | Synthesized variable
--
data SVar = SVar RateVar Int
  deriving (Eq,Ord,Show)



data Expr = Var     SVar    
          | PField  Int
          | Const   CsValue
          | UnOp    String Expr
          | BinOp   Expr String Expr
          | Funcall String Expr
  deriving (Eq,Ord,Show)

-- | CSB page22.
--
data RateVar = I | K | A 
  deriving (Eq,Ord,Show)


binop :: String -> Expr -> Expr -> Expr
binop name a b = BinOp a name b

instance Num Expr where
  (+) = binop "+"
  (-) = binop "-"
  (*) = binop "*"
  abs    = UnOp "abs"
  negate = UnOp "-"
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = Const $ CsInt $ fromInteger i

instance Fractional Expr where
  (/) = binop "/"  
  recip _        = error "recip - no interpretation of recip in Csound."  
  fromRational i = Const $ CsDouble $ fromRational i



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


mkVar :: SVar -> Expr -> InstBuilder Expr
mkVar v expr = tellStmt (Vardef v expr) >> return (Var v)


-- | Note some opcodes return pairs...
--
mkOpcode :: SVar -> String -> [Expr] -> InstBuilder Expr
mkOpcode v opcd es = tellStmt (Opcode v opcd es) >> return (Var v)




ivar :: Expr -> InstBuilder Expr
ivar expr = freshIVar >>= \v -> mkVar v expr

iopcode :: String -> [Expr] -> InstBuilder Expr
iopcode opcd es = freshIVar >>= \v -> mkOpcode v opcd es

kvar :: Expr -> InstBuilder Expr
kvar expr = freshKVar >>= \v -> mkVar v expr

kopcode :: String -> [Expr] -> InstBuilder Expr
kopcode opcd es = freshKVar >>= \v -> mkOpcode v opcd es

avar :: Expr -> InstBuilder Expr
avar expr = freshAVar >>= \v -> mkVar v expr

aopcode :: String -> [Expr] -> InstBuilder Expr
aopcode opcd es = freshAVar >>= \v -> mkOpcode v opcd es


out :: Expr -> InstBuilder ()
out e = tellStmt $ Outs [e]

outs :: [Expr] -> InstBuilder ()
outs = tellStmt . Outs 





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



instance Format Expr where
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

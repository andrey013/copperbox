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
    InstBuilder
  , Expr
  , Stmt

  , runInstBuilder

  , var   
  , opcode
  , out
  , outs

 
  , pfield
  , cpspch
 
  ) where


import ZScore.FormatCombinators
import ZScore.HList

import Control.Applicative hiding ( Const )




newtype InstBuilder a = Inst { 
          getInst :: Int -> (a, H Stmt, Int) }


data CsValue = CsInt Int
             | CsDouble Double
  deriving (Eq,Ord,Show)

data Stmt = Vardef  String Expr
          | Opcode  Expr String [Expr]
          | Outs    [Expr]
  deriving (Eq,Ord,Show)

data Expr = Var     String
          | GenVar  RateVar Int    
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
    abs = UnOp "abs"
    fromInteger i = Const $ CsInt $ fromInteger i



instance Functor InstBuilder where
  fmap f ma = Inst $ \s -> let (a,w,s1) = getInst ma s in (f a, w, s1)

instance Applicative InstBuilder where
  pure a    = Inst $ \s -> (a, emptyH, s)
  mf <*> ma = Inst $ \s -> let (f,w1,s1) = getInst mf s
                               (a,w2,s2) = getInst ma s1
                           in (f a, w1 `appendH` w2, s2)

instance Monad InstBuilder where
  return a  = Inst $ \s -> (a, emptyH, s)
  ma >>= k  = Inst $ \s -> let (a,w1,s1) = getInst ma s
                               (b,w2,s2) = (getInst . k) a s1
                           in (b, w1 `appendH` w2, s2)

runInstBuilder :: Int -> InstBuilder a -> Doc
runInstBuilder n ma = post $ getInst ma 1
  where
    post (_,w,_) = output n $ toListH w


output :: Int -> [Stmt] -> Doc
output n xs = vcat $
    [ text "instr" <+> int n 
    , vcat $ map format xs
    , text "endin"
    ]



tellStmt :: Stmt -> InstBuilder ()
tellStmt e = Inst $ \s -> ((), wrapH e, s)

var :: String -> Expr -> InstBuilder Expr
var name expr = tellStmt (Vardef name expr) >> return (Var name)

opcode :: String -> String -> [Expr] -> InstBuilder Expr
opcode name opcd es = 
    let sid = Var name in tellStmt (Opcode sid opcd es) >> return sid

out :: Expr -> InstBuilder ()
out e = tellStmt $ Outs [e]

outs :: [Expr] -> InstBuilder ()
outs = tellStmt . Outs 


pfield :: Int -> Expr 
pfield i = PField i

cpspch :: Expr -> Expr
cpspch = Funcall "cpspch"


-- Like Node in Text.Dot
--
-- opcode :: RateVar -> InstBuilder StmtId

instance Format Stmt where
  format (Vardef name val)    = 
    indent 8 (padr 11 $ text name) <+> char '=' <+> format val

  format (Opcode oid name xs) = 
    padr 7 (format oid) <+> padr 11 (text name) 
                        <+> punctuate (text ", ") (map format xs)
  
  format (Outs [x])           = 
    indent 8 (padr 11 (text "out") <+> format x)
  
  format (Outs xs)            = 
    indent 8 (padr 11 (text "outs") <+> punctuate (text ", ") (map format xs))



instance Format Expr where
  format (Var s)        = text s
  format (GenVar rv i)  = format rv <> int i
  format (PField i)     = char 'p' <> int i 
  format (Const val)    = format val
  format (BinOp a ss b) = format a <> text ss <> format b
  format (UnOp ss a)    = text ss <> format a
  format (Funcall ss a) = text ss <> parens (format a)


instance Format CsValue where
  format (CsInt i)    = int i
  format (CsDouble d) = dtrunc d



instance Format RateVar where
  format I = char 'i'
  format K = char 'k'
  format A = char 'a'

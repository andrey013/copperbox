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
  , default_mono_header
  , default_stereo_header

  , InstBuilder
  , Stmt
  , Expr(..)
  , DExpr(..)

  , IR
  , KR
  , AR

  , Ir
  , Kr
  , Ar

  , runInstBuilder

  , Opcode
  , var
  , opcode
  , opcode2
  , opcode4
  , opcode0

  , KA_Rate     -- opaque typeclass
  , IK_Rate     -- opaque typeclass

  , castIR
  , castKR
  , castAR

  , ivar
  , kvar
  , avar

  , pfield
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

      
default_mono_header :: OrchHeader 
default_mono_header =  
    OrchHeader { zero_dbfs                = 32767
               , kr_ctrl_rate             = 4410
               , sr_aud_sample_rate       = 44100
               , ksmps_ctrl_period_samps  = 32
               , nchnls_num_chans         = 1
               , seed_gbl_random          = Nothing
               }

default_stereo_header :: OrchHeader 
default_stereo_header =  
    OrchHeader { zero_dbfs                = 32767
               , kr_ctrl_rate             = 4410
               , sr_aud_sample_rate       = 44100
               , ksmps_ctrl_period_samps  = 32
               , nchnls_num_chans         = 2
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


-- | Note with this formulation all arities of opcode result -
-- mono, stereo (quad?) - need to be enumerated /ahead of time/.
--
-- Csound itself just has an implicit list.
--
data Stmt = Vardef      TagVar DExpr
          | Opcode      [TagVar] String [DExpr]
          | Outs        [DExpr]
  deriving (Eq,Ord,Show)


-- | Type tagged variable
-- 
-- The type is tagged in the constructor (not at the type level)
-- this is to allow DExpr to account for all types.
--
data TagVar = TagVar RateVar Int
  deriving (Eq,Ord,Show)

-- newtype Var rate = Var { getVar :: TagVar }

-- | Dynamically typed expr
-- 
data DExpr = VarE    TagVar
           | ParenE  DExpr
           | PField  Int
           | Const   CsValue
           | ZeroOp  String
           | UnOp    String DExpr
           | BinOp   String DExpr DExpr
           | Funcall String DExpr
  deriving (Eq,Ord,Show)


newtype Expr rate = Expr { getExpr :: DExpr }
  deriving (Eq,Ord,Show)


data IR
data KR
data AR

-- | Synonyms - useful for asserting types in do-notation.
type Ir = Expr IR
type Kr = Expr KR
type Ar = Expr AR



-- | CSB page22.
--
data RateVar = I | K | A 
  deriving (Eq,Ord,Show)


binop :: String -> Expr a -> Expr a -> Expr a
binop name a b = Expr $ BinOp name (getExpr a) (getExpr b)

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



freshIVar :: InstBuilder TagVar
freshIVar = Build $ \s -> let n = i_int s 
                          in (TagVar I n, emptyH, s { i_int = n + 1 })

freshKVar :: InstBuilder TagVar 
freshKVar = Build $ \s -> let n = k_int s 
                          in (TagVar K n, emptyH, s { k_int = n + 1 })

freshAVar :: InstBuilder TagVar
freshAVar = Build $ \s -> let n = a_int s 
                          in (TagVar A n, emptyH, s { a_int = n + 1 })




class Opcode rate where
  var     :: DExpr -> InstBuilder (Expr rate)
  opcode  :: String -> [DExpr] -> InstBuilder (Expr rate)
  opcode2 :: String -> [DExpr] -> InstBuilder (Expr rate, Expr rate)
  opcode4 :: String -> [DExpr] 
                    -> InstBuilder (Expr rate, Expr rate, Expr rate, Expr rate)

opcode0  :: String -> [DExpr] -> InstBuilder ()
opcode0 opcd es = tellStmt (Opcode [] opcd es) >> return ()


instance Opcode IR where
  var expr       = freshIVar >>= \v -> 
                   tellStmt (Vardef v expr) >> 
                   return (Expr $ VarE v)

  opcode opcd es = freshIVar >>= \v -> 
                   tellStmt (Opcode [v] opcd es) >>
                   return (Expr $ VarE v)


  opcode2 opcd es = freshIVar >>= \v1 -> 
                    freshIVar >>= \v2 -> 
                    tellStmt (Opcode [v1, v2] opcd es) >>
                    return (Expr $ VarE v1, Expr $ VarE v2)

  opcode4 opcd es = freshIVar >>= \v1 -> 
                    freshIVar >>= \v2 -> 
                    freshIVar >>= \v3 ->
                    freshIVar >>= \v4 ->
                    tellStmt (Opcode [v1, v2, v3, v4] opcd es) >>
                    return ( Expr $ VarE v1, Expr $ VarE v2
                           , Expr $ VarE v3, Expr $ VarE v4 )


instance Opcode KR where
  var expr       = freshKVar >>= \v -> 
                   tellStmt (Vardef v expr) >> 
                   return (Expr $ VarE v)

  opcode opcd es = freshKVar >>= \v -> 
                   tellStmt (Opcode [v] opcd es) >>
                   return (Expr $ VarE v)

  opcode2 opcd es = freshKVar >>= \v1 -> 
                    freshKVar >>= \v2 -> 
                    tellStmt (Opcode [v1, v2] opcd es) >>
                    return (Expr $ VarE v1, Expr $ VarE v2)

  opcode4 opcd es = freshKVar >>= \v1 -> 
                    freshKVar >>= \v2 -> 
                    freshKVar >>= \v3 ->
                    freshKVar >>= \v4 ->
                    tellStmt (Opcode [v1, v2, v3, v4] opcd es) >>
                    return ( Expr $ VarE v1, Expr $ VarE v2
                           , Expr $ VarE v3, Expr $ VarE v4 )


instance Opcode AR where
  var expr       = freshAVar >>= \v -> 
                   tellStmt (Vardef v expr) >> 
                   return (Expr $ VarE v)

  opcode opcd es = freshAVar >>= \v -> 
                   tellStmt (Opcode [v] opcd es) >>
                   return (Expr $ VarE v)


  opcode2 opcd es = freshAVar >>= \v1 -> 
                    freshAVar >>= \v2 -> 
                    tellStmt (Opcode [v1, v2] opcd es) >>
                    return (Expr $ VarE v1, Expr $ VarE v2)

  opcode4 opcd es = freshAVar >>= \v1 -> 
                    freshAVar >>= \v2 -> 
                    freshAVar >>= \v3 ->
                    freshAVar >>= \v4 ->
                    tellStmt (Opcode [v1, v2, v3, v4] opcd es) >>
                    return ( Expr $ VarE v1, Expr $ VarE v2
                           , Expr $ VarE v3, Expr $ VarE v4 )



class Opcode rate => KA_Rate rate 

instance KA_Rate KR

instance KA_Rate AR


class Opcode rate => IK_Rate rate 

instance IK_Rate IR

instance IK_Rate KR


-- | Dynamic typecast - no residual Csound code is generated.
--
castIR :: Expr rate -> Expr IR
castIR = Expr . getExpr

-- | Dynamic typecast - no residual Csound code is generated.
--
castKR :: Expr rate -> Expr KR
castKR = Expr . getExpr

-- | Dynamic typecast - no residual Csound code is generated.
--
castAR :: Expr rate -> Expr AR
castAR = Expr . getExpr


-- Explicitly typed versions
                   
ivar :: Expr IR -> InstBuilder (Expr IR)
ivar = var . getExpr

kvar :: Expr KR -> InstBuilder (Expr KR)
kvar = var . getExpr

avar :: Expr AR -> InstBuilder (Expr AR)
avar = var . getExpr


-- | Declare a p-field.
--
pfield    :: Int -> Expr a
pfield i  = Expr $ PField i


out :: Expr AR -> InstBuilder ()
out e = tellStmt $ Outs [getExpr e]

outs :: [Expr AR] -> InstBuilder ()
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
  format (Vardef v1 val)    = 
    padr 7 (format v1) <+> char '=' <+> format val

  format (Opcode vs name xs) = 
    padr 7 (dlist $ map format vs) <+> padr 11 (text name) 
                                   <+> dlist (map format xs)
      where
        dlist = punctuate (text ", ")


  format (Outs [x])           = 
    indent 8 (padr 11 (text "out") <+> format x)
  
  format (Outs xs)            = 
    indent 8 (padr 11 (text "outs") <+> punctuate (text ", ") (map format xs))

instance Format (Expr a) where
  format = format . getExpr

instance Format DExpr where
  format (VarE s)       = format s
  format (ParenE e)     = parens $ format e
  format (PField i)     = char 'p' <> int i 
  format (Const val)    = format val
  format (ZeroOp ss)    = text ss
  format (UnOp ss a)    = text ss <> format a
  format (BinOp ss a b) = format a <> text ss <> format b
  format (Funcall ss a) = text ss <> parens (format a)

instance Format TagVar where
  format (TagVar rv i) = format rv <> int i


instance Format CsValue where
  format (CsInt i)    = int i
  format (CsDouble d) = dtrunc d



instance Format RateVar where
  format I = char 'i'
  format K = char 'k'
  format A = char 'a'

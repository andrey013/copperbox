{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CsoundInst.Prim
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

module ZSnd.Core.CsoundInst.Prim
  (

  -- * Datatypes
    Orch(..)
  , OrchHeader(..)
  , PrimInst(..)
  , Goto(..)
  , UStmt(..)
  , UExpr(..)
  , Label               -- opaque
  , mkLabel
  , BinRel(..)
  , RelOp               -- opaque
  , CsValue(..)
  , VarId(..)
  , DataRate(..)

  -- * Operators
  , logical_and
  , logical_or
  , power_of
  , modulus_op
  , plus_op
  , minus_op
  , mult_op
  , divide_op
  , unary_negate

  , greater_than
  , less_than
  , greater_than_eq
  , less_than_eq
  , equal_rel
  , not_equal_rel

  ) where

import ZSnd.Core.Utils.FormatCombinators
import ZSnd.Core.Utils.FormatExpr





data Orch = Orch OrchHeader [Either UStmt PrimInst]
  deriving (Eq,Ord,Show)



-- | Orchestra file header values.
--
-- The following fields are not accessible to ZSnd:
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



data PrimInst = PrimInst Int [UStmt]
  deriving (Eq,Ord,Show)

 
-- | Note - in the astract syntax there is no difference between
-- variable assignment to a fresh var and variable assignment to 
-- a new var.
--
data UStmt = VarAssign     VarId UExpr 
           | OpcodeAssign  [VarId] String [UExpr]
           | Outs          [UExpr]
           | Label         Label
           | Goto          Goto Label
           | IfGoto        BinRel Goto Label
           | Timout        UExpr UExpr Label
           | Reinit        Label
           | Rireturn      
  deriving (Eq,Ord,Show)

data Goto = GOTO | IGOTO | KGOTO | TIGOTO | RIGOTO
  deriving (Eq,Ord,Show)


-- | Untyped expr
-- 
data UExpr = VarE      VarId
           | PField    Int
           | TableRef  Int
           | Literal   CsValue
           | ZeroOp    String
           | UnOp      Rator UExpr
           | BinOp     Rator UExpr UExpr
           | Funcall   String UExpr
           | Cond      BinRel UExpr UExpr
  deriving (Eq,Ord,Show)

newtype Label = LabelId { getLabelId :: String }
  deriving (Eq,Ord,Show)


mkLabel :: String -> Label
mkLabel = LabelId

-- Note this cannot hadle conjunction, dijunction...
--
data BinRel = BinRel RelOp UExpr UExpr
  deriving (Eq,Ord,Show)

newtype RelOp = RelOp { getRelOp :: String }
  deriving (Eq,Ord,Show)

data CsValue = CsInt    Int
             | CsDouble Double
             | CsString String          -- for file names
  deriving (Eq,Ord,Show)



data VarId = UserVar String
           | LocVar DataRate Int
  deriving (Eq,Ord,Show)


-- | Data rate - strictly speaking @I@ is a initialization time
-- rather than a rate.
--
data DataRate = I | K | A 
  deriving (Eq,Ord,Show)



--------------------------------------------------------------------------------
-- Operators

-- @_op@ suffix used for disambiguation...

logical_and     :: Rator
logical_and     = infixL 3 "&&"

logical_or      :: Rator
logical_or      = infixL 2 "||"

power_of        :: Rator
power_of        = infixL 4 "^"

modulus_op      :: Rator
modulus_op      = infixL 7 "%"

plus_op         :: Rator
plus_op         = infixL 6 "+"

minus_op        :: Rator
minus_op        = infixL 6 "-"

mult_op         :: Rator
mult_op         = infixL 7 "*"

divide_op       :: Rator
divide_op       = infixL 7 "/"

unary_negate    :: Rator
unary_negate    = prefix 9 "-"



greater_than        :: RelOp
greater_than        = RelOp ">"

less_than           :: RelOp
less_than           = RelOp "<"

greater_than_eq     :: RelOp
greater_than_eq     = RelOp ">="

less_than_eq        :: RelOp
less_than_eq        = RelOp "<="

equal_rel           :: RelOp
equal_rel           = RelOp "=="

not_equal_rel       :: RelOp
not_equal_rel       = RelOp "!="

--------------------------------------------------------------------------------
-- Format instances


instance Format Orch where
  format (Orch hdr stmts) = vspace $ (format hdr) : map (either format format) stmts

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


instance Format PrimInst where
  format (PrimInst n stmts) = 
      (text "instr" <+> int n) `vconcat` body `vconcat` text "endin"
    where
      body = vcat $ map format stmts



instance Format UStmt where
  format (VarAssign v1 val)         = 
    padr 8 (format v1) <> char '=' <+> format val

  format (OpcodeAssign vs name xs)  = 
    padr 8 (dlist $ map format vs) <> padr 11 (text name) 
                                   <+> dlist (map format xs)
      where
        dlist = punctuate (text ", ")

  format (Outs [x])                 = 
    indent 8 (padr 11 (text "out") <+> format x)
  
  format (Outs xs)                  = 
    indent 8 (padr 11 (text "outs") <+> punctuate (text ", ") 
                                                  (map format xs))

  format (Label lbl)                = format lbl <> char ':'

  format (Goto gospec lbl)          = format gospec <+> format lbl

  format (IfGoto rel gospec lbl)    =
      text "if" <+> format rel <+> format gospec <+> format lbl

  format (Timout istrt idur lbl)    = 
    indent 8 (padr 11 (text "timout") <+> punctuate (text ", ") ds)
      where
        ds = [format istrt, format idur, format lbl]


  format (Reinit lbl)         = indent 8 (text "reinit" <+> format lbl)

  format Rireturn             = indent 8 (text "rireturn")

instance Format Goto where
  format GOTO   = text "goto"
  format IGOTO  = text "igoto"
  format KGOTO  = text "kgoto"
  format TIGOTO = text "tigoto"
  format RIGOTO = text "rigoto"


instance Format UExpr where
  format = unparse . buildExpr

buildExpr :: UExpr -> DocExpr
buildExpr (VarE s)       = Atom $ format s
buildExpr (PField i)     = Atom $ char 'p' <> int i 
buildExpr (TableRef i)   = Atom $ int i 
buildExpr (Literal val)  = Atom $ format val
buildExpr (ZeroOp ss)    = Atom $ text ss
buildExpr (UnOp op a)    = Unary op (buildExpr a)
buildExpr (BinOp op a b) = Binary (buildExpr a) op (buildExpr b)
buildExpr (Funcall ss a) = Atom $ text ss <> parens (format a)
buildExpr (Cond rel t f) = 
    Atom $ parens (format rel) <+> char '?' <+> format t 
                               <+> char ':' <+> format f


instance Format Label where
  format = text . getLabelId
                                    
instance Format BinRel where
  format (BinRel op a b) = format a <+> format op <+> format b

instance Format RelOp where
  format = text . getRelOp

instance Format CsValue where
  format (CsInt i)    = int i
  format (CsDouble d) = dtrunc d
  format (CsString s) = dquotes $ text s

instance Format VarId where
  format (UserVar s)    = text s
  format (LocVar rt i)  = format rt <> int i

instance Format DataRate where
  format I = char 'i'
  format K = char 'k'
  format A = char 'a'

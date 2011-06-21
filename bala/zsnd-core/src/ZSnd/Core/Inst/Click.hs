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

newtype TransMonad a = TM { 
          getTM :: IntSupply -> Either FailMsg (a, IntSupply) }

type FailMsg = String


data IntSupply = IntSupply
      { i_int   :: Int
      , k_int   :: Int
      , a_int   :: Int
      }

instance Functor TransMonad where
  fmap f ma = TM $ \s -> fmap (flippy f) $ getTM ma s
                              
flippy :: (a -> c) -> (a,b) -> (c,b)
flippy f (a,b) = (f a, b)

instance Applicative TransMonad where
  pure a    = TM $ \s -> Right (a,s)
  mf <*> ma = TM $ \s -> case getTM mf s of
                           Left err -> Left err 
                           Right (f,s1) -> case getTM ma s1 of
                                             Left err -> Left err
                                             Right (a,s2) -> Right (f a, s2)



instance Monad TransMonad where
  return a  = TM $ \s -> Right (a, s)
  ma >>= k  = TM $ \s -> case getTM ma s of 
                           Left err -> Left err
                           Right (a,s1) -> (getTM . k) a s1


runTransMonad :: TransMonad a -> Either FailMsg a
runTransMonad ma = fmap fst $ getTM ma (IntSupply 0 0 0)


newLocVar :: DataRate -> TransMonad TagVar
newLocVar rt = TM $ \s -> Right $ step rt s
  where
    step I s = let i = i_int s in (LocVar I (i+1), s { i_int = i + 1 })
    step K s = let i = k_int s in (LocVar K (i+1), s { k_int = i + 1 })
    step A s = let i = a_int s in (LocVar A (i+1), s { a_int = i + 1 })


translateDesc :: Int -> CExpr -> Either FailMsg PrimInst
translateDesc i cexp = fmap (Inst i) $ runTransMonad $ transStep cexp

-- | This is wrong. opcodes should be generated for @a@ of (:->) 
-- if it is fresh...
--
transStep :: CExpr -> TransMonad [Stmt]
transStep (LetE _ elt body) = (:) <$> invokeOpcode elt <*> transStep body
transStep (_a :-> _b)       = pure []
transStep (Sequ es)         = concat <$> mapM transStep es


invokeOpcode :: Element -> TransMonad Stmt
invokeOpcode (Element name inspec outspec) = 
    (\inps ovars -> Opcode ovars name inps) 
      <$> mapM inpExpr inspec <*> outVars outspec


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
-- Format instances (useful for debugging)

instance Format CExpr where
   format (LetE name elt expr)  = 
      format name <+> text "::" <+> format elt <> char ';'
        `vconcat` format expr

   format ((v1,i1) :-> (v2,i2))           = 
     format v1 <> brackets (int i1) <+> text "->" 
               <+> brackets (int i2) <> format v2 <> char ';'

   format (Sequ xs)             = vcat (map format xs)
     

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
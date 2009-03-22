{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}

module S1.SyntaxCPS where

import Language.KURE

type Var = Int 

newtype CVar = CVar Int
  deriving (Eq,Ord,Show,Read,Num)

data Value = Unit 
           | Bool Bool
           | Real Double
           | Pair Var Var 
           | Lam  CVar Var CTerm
  deriving (Show)
  
data CTerm = Letval  Var  Value CTerm
        --  | Let Var Int Var CTm
           | Letcont CVar Var  CTerm CTerm
           | Appcont CVar Var
           | App     Var  CVar Var
  deriving (Show)
  
  
--------------------------------------------------------------------------------
-- KURE

data CPSGeneric = GValue Value
                | GCTerm CTerm

                
instance Term Value where
  type Generic Value = CPSGeneric
  inject             = GValue
  select (GValue a)  = Just a
  select _           = Nothing
  
instance Term CTerm where
  type Generic CTerm = CPSGeneric
  inject             = GCTerm
  select (GCTerm a)  = Just a
  select _           = Nothing 

instance Term CPSGeneric where
  type Generic CPSGeneric = CPSGeneric  -- 'generic root'
  inject    = id
  select e  = Just e  
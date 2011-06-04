{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZScore.Opcodes
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

module ZScore.Opcodes
  (
 
    pfield

  , dbamp
  , ampdb

  , octpch
  , cpspch
  , pchoct
  , octcps
  , cpsoct

  , oscil

  ) where


import ZScore.CsoundInst


--------------------------------------------------------------------------------

pfield    :: Int -> Expr 
pfield i  = PField i


dbamp     :: Expr -> Expr
dbamp     = Funcall "dbamp"

ampdb     :: Expr -> Expr
ampdb     = Funcall "ampdb"


octpch    :: Expr -> Expr
octpch    = Funcall "octpch"

cpspch    :: Expr -> Expr
cpspch    = Funcall "cpspch"

pchoct    :: Expr -> Expr
pchoct    = Funcall "pchoct"

octcps    :: Expr -> Expr
octcps    = Funcall "octcps"

cpsoct    :: Expr -> Expr
cpsoct    = Funcall "cpsoct"



oscil :: Expr -> Expr -> Expr -> InstBuilder Expr
oscil amp cps ifn = aopcode "oscil" [amp, cps, ifn]

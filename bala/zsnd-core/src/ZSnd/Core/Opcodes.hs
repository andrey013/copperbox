{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes
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

module ZSnd.Core.Opcodes
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


import ZSnd.Core.CsoundInst


--------------------------------------------------------------------------------

pfield    :: Int -> Expr a
pfield i  = Expr $ PField i

class CdbAmp rate where
  -- | I or K rate only.
  dbamp     :: Expr rate -> Expr rate

instance CdbAmp IRate where
  dbamp     = Expr . Funcall "dbamp" . getExpr

instance CdbAmp KRate where
  dbamp     = Expr . Funcall "dbamp" . getExpr


ampdb     :: Expr a -> Expr a
ampdb     = Expr . Funcall "ampdb" . getExpr


--------------------------------------------------------------------------------
-- Pitch conversion

class CPitchConvert rate where
  -- | I or K rate only.
  octpch :: Expr rate -> Expr rate
  -- | I or K rate only.
  cpspch :: Expr rate -> Expr rate
  -- | I or K rate only.
  pchoct :: Expr rate -> Expr rate
  -- | I or K rate only.
  octcps :: Expr rate -> Expr rate



instance CPitchConvert IRate where
  octpch    = Expr . Funcall "octpch" . getExpr
  cpspch    = Expr . Funcall "cpspch" . getExpr
  pchoct    = Expr . Funcall "pchoct" . getExpr
  octcps    = Expr . Funcall "octcps" . getExpr

instance CPitchConvert KRate where
  octpch    = Expr . Funcall "octpch" . getExpr
  cpspch    = Expr . Funcall "cpspch" . getExpr
  pchoct    = Expr . Funcall "pchoct" . getExpr
  octcps    = Expr . Funcall "octcps" . getExpr


-- | No rate restriction.
--
cpsoct    :: Expr a -> Expr a
cpsoct    = Expr . Funcall "cpsoct" . getExpr



oscil :: Expr a -> Expr a -> Expr a -> InstBuilder (Expr ARate)
oscil amp cps ifn = aopcode "oscil" [getExpr amp, getExpr cps, getExpr ifn]

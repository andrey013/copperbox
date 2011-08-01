{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Top-level shim module.
--
--------------------------------------------------------------------------------

module Sound.FMSS
  (
   
  -- * Re-exported modules
    module Sound.FMSS.ConfigMonad
  , module Sound.FMSS.Envelopes
  , module Sound.FMSS.SpecMonad

  -- * Re-exported types
  , Expr

  , writeSynth

  -- * Expressions
  , pfield

  , cond
  , (.==.)

  , (.<.)
  , (.>.)
  , (.<=.)
  , (.>=.)

  ) where

import Sound.FMSS.AbstractSyntax
import Sound.FMSS.ConfigMonad
import Sound.FMSS.Envelopes
import Sound.FMSS.SpecMonad
import Sound.FMSS.Utils.FormatCombinators
import Sound.FMSS.Utils.FormatExpr


writeSynth :: FilePath -> Params -> Spec a -> IO ()
writeSynth outpath params mf = case execSpec params mf of
   Left err -> putStrLn err
   Right ans -> writeFile outpath (show $ format ans)


--------------------------------------------------------------------------------
-- Expressions


pfield :: Int -> Expr
pfield = PField

-- mult :: Expr -> ExprF 
-- mult a = (a *)

-- add  :: Expr -> ExprF 
-- add a = (a +)


cond :: Expr -> Expr -> Expr -> Expr 
cond = Cond

infix 4 .==.

(.==.)          :: Expr -> Expr -> Expr
(.==.)          = BinOp equal

equal           :: Rator 
equal           = infixNone 4 "=="

infix 4 .<., .>.

(.<.)           :: Expr -> Expr -> Expr
(.<.)           = BinOp lessthan


lessthan        :: Rator
lessthan        = infixNone 4 "<"

(.>.)           :: Expr -> Expr -> Expr
(.>.)           = BinOp greaterthan


greaterthan     :: Rator
greaterthan     = infixNone 4 ">"


infix 4 .<=., .>=.

(.<=.)          :: Expr -> Expr -> Expr
(.<=.)          = BinOp lessthaneq


lessthaneq      :: Rator
lessthaneq      = infixNone 4 "<="

(.>=.)          :: Expr -> Expr -> Expr
(.>=.)          = BinOp greaterthaneq


greaterthaneq   :: Rator
greaterthaneq   = infixNone 4 ">="


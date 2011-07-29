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

  , mult
  , add

  ) where

import Sound.FMSS.AbstractSyntax
import Sound.FMSS.ConfigMonad
import Sound.FMSS.Envelopes
import Sound.FMSS.SpecMonad
import Sound.FMSS.Translate
import Sound.FMSS.Utils.FormatCombinators


writeSynth :: FilePath -> Params -> Spec SpecAns -> IO ()
writeSynth outpath params mf = case execSpec params mf of
   Left err -> putStrLn err
   Right ans -> case translate ans of 
                  Left err -> putStrLn err
                  Right code -> writeFile outpath (show $ format code)


--------------------------------------------------------------------------------
-- Expressions


pfield :: Int -> Expr
pfield = PField

mult :: Expr -> ExprF 
mult a = (a *)

add  :: Expr -> ExprF 
add a = (a +)

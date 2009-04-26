{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcDoc
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Document combinators for Abc
--
--------------------------------------------------------------------------------


module Mullein.AbcDoc where

import Mullein.AbcOutput ( AbcOutput(..), field, keyField, meterField ) 
import Mullein.CoreTypes
import Mullein.Duration

import Text.PrettyPrint.Leijen 



newtype P a = P { unP :: Doc }

data CtxField

keyinfo :: Key -> P CtxField
keyinfo = P . keyField  

meterinfo :: Meter -> P CtxField
meterinfo = P . meterField

tunenum :: Int -> P CtxField
tunenum = P . field 'X' . int

title :: String -> P CtxField
title = P . field 'T' . text

tune :: AbcOutput -> P CtxField
tune = P . getAbcOutput

infixl 5 +++
class Concat ctx ctx' where
  (+++)  :: P ctx -> P ctx' -> P ctx'

instance Concat CtxField CtxField where
  (+++) a b = P $ unP a <$> unP b
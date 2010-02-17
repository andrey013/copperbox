{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.HsSrcUtils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------


module Precis.HsSrcUtils
  (
    extractQName
  , extractCName
  , extractModuleName
  , extractName
  , extractSpecialCon

  ) where

import Language.Haskell.Exts hiding ( name )


extractQName :: QName -> String
extractQName (Qual mname name)  = extractModuleName mname ++ extractName name
extractQName (UnQual name)      = extractName name
extractQName (Special spc)      = extractSpecialCon spc

extractCName :: CName -> String
extractCName (VarName name)     = extractName name
extractCName (ConName name)     = extractName name

extractModuleName :: ModuleName -> String 
extractModuleName (ModuleName name) = name

extractName :: Name -> String
extractName (Ident name)  = name
extractName (Symbol name) = name

extractSpecialCon :: SpecialCon -> String
extractSpecialCon UnitCon	= "()"

{-
ListCon	list type constructor []
FunCon	function type constructor ->
TupleCon Boxed Int	n-ary tuple type and data constructors (,) etc, possibly boxed (#,#)
Cons	list data constructor (:)
UnboxedSingleCon	unboxed singleton tuple constructor (# #)
-}

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

  , namedDecls

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
extractSpecialCon = prettyPrint

{-
extractSpecialCon UnitCon           = "()"
extractSpecialCon ListCon           = "[]"
extractSpecialCon FunCon            = "->"
extractSpecialCon (TupleCon _ _)    = "(,)"
extractSpecialCon Cons              = "(:)"
extractSpecialCon UnboxedSingleCon  = "(# #)"
-}


--------------------------------------------------------------------------------

--
namedDecls :: Decl -> [(String,Decl)]
namedDecls t@(TypeDecl    _ n _ _)                     = [(extractName n,t)]
namedDecls t@(TypeFamDecl _ n _ _)                     = [(extractName n,t)]
namedDecls t@(DataDecl    _ _ _ n _ _ _)               = [(extractName n,t)]
namedDecls t@(GDataDecl   _ _ _ n _ _ _ _)             = [(extractName n,t)]
namedDecls t@(DataFamDecl _ _ n _ _)                   = [(extractName n,t)]
namedDecls t@(ClassDecl   _ _ n _ _ _)                 = [(extractName n,t)]
namedDecls t@(InstDecl    _ _ q _ _)                   = [(extractQName q,t)]
namedDecls t@(DerivDecl   _ _ q _)                     = [(extractQName q,t)]
namedDecls t@(TypeSig     _ ns _)                      = map fn ns
   where fn n = (extractName n, t)

namedDecls _                                           = []
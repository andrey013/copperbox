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

    readModule 
  , parseModuleWithExts
  , extractQName
  , extractCName
  , extractModuleName
  , extractName
  , extractSpecialCon

  , getModuleName

  , namedDecls

  , hsppList

  ) where

import Precis.Datatypes

import Language.Haskell.Exts hiding ( name )      -- package: haskell-src-exts


readModule :: MacroExpandedSrcFile -> Either ModuleParseError Module
readModule (MacroExpandedSrcFile filename mx_src) = 
    case parseModuleWithExts knownExtensions filename mx_src of
      ParseFailed loc msg -> Left $ ERR_MODULE_FILE_PARSE $ mkMsg msg loc
      ParseOk a           -> Right $ a
  where
    mkMsg msg loc        = msg ++ " - " ++ ppLoc loc
    ppLoc (SrcLoc _ l c) = unwords ["line:", show l ++ ",", "column:", show c]

parseModuleWithExts :: [Extension] -> FilePath -> String -> ParseResult Module
parseModuleWithExts exts file_name txt = parseModuleWithMode pmode txt
  where
    pmode = defaultParseMode { extensions            = exts
                             , parseFilename         = file_name
                             , ignoreLinePragmas     = False      }


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

--------------------------------------------------------------------------------

getModuleName :: Module -> ModuleName 
getModuleName (Module _ mn _ _ _ _ _) = mn 


--------------------------------------------------------------------------------

--
namedDecls :: Decl -> [(StrName,TextRep)]
namedDecls t@(TypeDecl    _ n _ _)            = [(extractName n, prettyPrint t)]
namedDecls t@(TypeFamDecl _ n _ _)            = [(extractName n, prettyPrint t)]
namedDecls t@(DataDecl    _ _ _ n _ _ _)      = [(extractName n, prettyPrint t)]
namedDecls t@(GDataDecl   _ _ _ n _ _ _ _)    = [(extractName n, prettyPrint t)]
namedDecls t@(DataFamDecl _ _ n _ _)          = [(extractName n, prettyPrint t)]
namedDecls t@(ClassDecl   _ _ n _ _ _)        = [(extractName n, prettyPrint t)]
namedDecls t@(InstDecl    _ _ q _ _)          = [(extractQName q, prettyPrint t)]
namedDecls t@(DerivDecl   _ _ q _)            = [(extractQName q, prettyPrint t)]
namedDecls t@(TypeSig     _ ns _)             = map fn ns
   where fn n = (extractName n, prettyPrint t)
namedDecls _                                  = []


hsppList :: Pretty a => [a] -> String
hsppList = unwords . map prettyPrint
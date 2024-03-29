-- Extra PPrint combinators for Haskell output

module Ext.Haskell.PPHaskell 
  where

import Gen.Ext.Haskell.HaskellAbsSyn
import Util.PPExt

import PPrint

typeDecl :: String -> [String] -> Doc -> Doc
typeDecl name [] tydoc = text "type" <+> text name <+> equals <+> tydoc
typeDecl name ts tydoc = text "type" <+> hsep (map text ts) <+> equals <+> tydoc

dataDecl :: String -> [String] -> [Doc] -> [String] -> Doc
dataDecl name ts consdocs ds = text "data" <+> text name <+> 
                   align ((constrsep consdocs) <$> deriv_line)
  where deriv_line =  text "deriving" <+> encloseSepAlt lparen rparen comma (map text ds)
        constrsep = encloseSep (text "= ") empty (text "| ")
        
        
conDecl :: String -> [Doc] -> Doc
conDecl name [] = text name
conDecl name xs = text name <+> hsep xs

recDecl :: String -> [Doc] -> Doc
recDecl name [] = text name 
recDecl name xs = text name <+> (encloseSep lbrace rbrace comma xs)

optWhere [] = empty
optWhere xs = indent 2 (text "where" <+> align (vsep xs))

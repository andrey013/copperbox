

module Util.PPExt where

import PPrint

infixr 6 <.>
infixr 6 <=>

matchWith :: Doc -> [Doc] -> Doc
matchWith match_exp pat_cases
  = line <> indent 2 (text "match" <+> match_exp <+> text "with"
                        <$> encloseSep (text "  ") 
                                       empty 
                                       (text "| ") 
                                       pat_cases)
                                       
indentedLines :: [Doc] -> Doc -> Doc
indentedLines ls sep = encloseSep empty empty (sep <> line) ls 
  
                                       

x <.> y         = x <> dot <> y
x <=> y         = x <+> equals <+> y

-- encloseSep iff has contents
encloseSepO :: Doc -> Doc -> Doc -> [Doc] -> Doc 
encloseSepO l r s []  = empty
encloseSepO l r s xs  = encloseSep l r s xs

tupledO [] = empty
tupledO xs = tupled xs

tups = encloseSep lparen rparen commaspace

commaspace :: Doc
commaspace = text ", " 


linespace = encloseSepO line line line

encloseSepAlt l r s [] = l <> r
encloseSepAlt l r s ds
  = l <> hcat (punctuate s ds) <> r 
  
starSep = encloseSep empty empty (text " * ")

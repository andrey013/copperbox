

module PPExt where

import PPrint

commaSpace :: Doc
commaSpace = text ", "

vcat2 :: [Doc] -> Doc
vcat2 = vcat . (punctuate line)

postPunctuate :: Doc -> [Doc] -> [Doc]
postPunctuate _ []      = []
postPunctuate p [d]     = [d <> p]
postPunctuate p (d:ds)  = (d <> p) : postPunctuate p ds




sepWith :: Doc -> [Doc] -> Doc
sepWith sep = foldr (\x acc -> x <> sep <> acc) empty


encloseSep1 :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep1 _ _ _ []  = empty
encloseSep1 l r s xs  = encloseSep l r s xs

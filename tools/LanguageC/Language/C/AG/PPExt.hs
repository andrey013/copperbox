

module PPExt where

import PPrint
import Data.Maybe


infixl 7 <?>,<+?>

x <?> Nothing       = x 
x <?> (Just y)      = x <> y

x <+?> Nothing       = x 
x <+?> (Just y)      = x <+> y

optEmpty :: Maybe Doc -> Doc
optEmpty = fromMaybe empty

prefixJust :: Doc -> Maybe Doc -> Doc
prefixJust p (Just d)   = p <> d
prefixJust _ Nothing    = empty

commaSpace :: Doc
commaSpace = text ", "

dparens d = (text "((") <> d <> (text "))")

subscript e e' = e <> char '[' <> e' <> char ']'


vcat2 :: [Doc] -> Doc
vcat2 = vcat . (punctuate line)

postPunctuate :: Doc -> [Doc] -> [Doc]
postPunctuate _ []      = []
postPunctuate p [d]     = [d <> p]
postPunctuate p (d:ds)  = (d <> p) : postPunctuate p ds

nestParens []     = empty
nestParens [x]    = parens x
nestParens (x:xs) = parens (x <> nestParens xs)  


spaceSep = encloseSep empty empty space
commaSep = encloseSep empty empty comma
semiSep  = encloseSep empty empty semi
colonSep = encloseSep empty empty colon


parensSpace :: [Doc] -> Doc
parensSpace xs = encloseSep lparen rparen space xs

parensSpace1 :: [Doc] -> Doc
parensSpace1 [] = empty
parensSpace1 xs = encloseSep lparen rparen space xs

parenNoSpace :: [Doc] -> Doc
parenNoSpace xs = encloseSep lparen rparen empty xs

parenNoSpace1 :: [Doc] -> Doc
parenNoSpace1 [] = empty
parenNoSpace1 xs = encloseSep lparen rparen empty xs


parensComma xs = encloseSep lparen rparen comma xs


parensSemi xs = encloseSep lparen rparen semi xs

bracesSpace  = encloseSep lbrace rbrace space



bracesSemi = encloseSep lbrace rbrace semi

bracesSemi1 [] = empty
bracesSemi1 xs = encloseSep lbrace rbrace semi xs


-- | A version of encloseSep that doesn't group
encloseSepNG l r s [] = l <> r
encloseSepNG l r s ds
  = l <> hcat (punctuate s ds) <> r 

encloseSepLines l r s [] = l <> r
encloseSepLines l r s ds = l <$> vcat (punctuate s ds) <$> r 

encloseSepLinesIndent l r s i [] = l <> r
encloseSepLinesIndent l r s i ds = l <$> indent i (vcat (punctuate s ds)) <$> r 

-- encloseSepLinesIndent i l r s = encloseSepNG (l <> line)  (line <> r) (s <> line)

  
bracesSemiLines = encloseSepLines lbrace rbrace semi
bracesSemiLinesIndent = encloseSepLinesIndent lbrace rbrace semi
bracesEmptyLinesIndent = encloseSepLinesIndent lbrace rbrace empty

encloseSep1 :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep1 _ _ _ []  = empty
encloseSep1 l r s xs  = encloseSep l r s xs


optBracesSemi []  = Nothing 
optBracesSemi xs  = Just $ encloseSep lbrace rbrace semi xs

optBracesComma []  = Nothing 
optBracesComma xs  = Just $ encloseSep lbrace rbrace comma xs

optBracesSemiLinesIndent i []   = Nothing
optBracesSemiLinesIndent i xs  = Just $ encloseSepLinesIndent lbrace rbrace semi i xs

optBracesCommaLinesIndent i []  = Nothing
optBracesCommaLinesIndent i xs  = Just $ encloseSepLinesIndent lbrace rbrace comma i xs

-- probably something like the interface of RecLib's traversal class good here
-- for encloseSep variations 

structSep []   = Nothing
structSep ds  = Just $ line <> lbrace <$> indent 3 (vcat (postPunctuate semi ds)) <$> rbrace


enumSep []   = Nothing
enumSep ds  = Just $ line <> lbrace <$> indent 3 (vcat (punctuate comma ds)) <$> rbrace

attribSep []  = Nothing
attribSep ds  = Just $ encloseSep lparen rparen comma ds 

optSpaceSep [] = Nothing
optSpaceSep ds = Just $ encloseSep empty empty space ds
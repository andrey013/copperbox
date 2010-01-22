{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.JoinPrint.Core
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Printing with /join-strings/.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.JoinPrint.Core
  ( 
    Doc
  , empty
  , null
  , length

  , (<>)
  , (<+>)
  , (<%>)
  , vcat
  , hcat
  , hsep

  , text
  , char
  , int
  , integer

  , sglspace
  , dblspace
  , comma
  , semicolon


  , punctuate
  , enclose
  , squotes
  , dquotes
  , parens
  , brackets
  , braces
  , angles

  , lparen
  , rparen
  , lbracket
  , rbracket
  , lbrace
  , rbrace
  , langle
  , rangle


  , replicateChar
  , spacer


  , padl
  , padr
  , column
  , column_truncr_padl
  , column_truncr_padr
  , column_truncl_padl
  , column_truncl_padr
  
  , center
  , center_truncr
  , center_truncl
  
  , render
  
  ) where


import Text.PrettyPrint.JoinPrint.JoinString ( JoinString, (++) )
import qualified Text.PrettyPrint.JoinPrint.JoinString as JS

import Prelude hiding ( (++), null, length )

newtype Doc = Doc { getDoc :: JoinString }

instance Show Doc where
  show = render
        
infixr 5 <%>
infixr 6 <>, <+>


empty :: Doc
empty = Doc $ JS.empty

null :: Doc -> Bool
null = JS.null . getDoc 


-- | length on a Doc is O(1).
length :: Doc -> Int
length = JS.length . getDoc


(<>) :: Doc -> Doc -> Doc
Doc a <> Doc b = Doc $ a ++ b

(<+>) :: Doc -> Doc -> Doc
Doc a <+> Doc b = Doc (a ++ JS.cons1 ' ' b)

(<%>) :: Doc -> Doc -> Doc
Doc a <%> Doc b = Doc (a ++ JS.cons1 '\n' b)

vcat :: [Doc] -> Doc
vcat = foldr (<%>) empty

hcat :: [Doc] -> Doc
hcat = foldr (<>) empty

hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty



text :: String -> Doc
text = Doc . (JS.text)

char :: Char -> Doc
char = Doc . (JS.text) . return

int :: Int -> Doc
int  = text . show

integer :: Integer -> Doc
integer = text . show


sglspace :: Doc
sglspace = char ' '

dblspace :: Doc
dblspace = text "  "

comma :: Doc
comma = char ','

semicolon :: Doc
semicolon = char ';'


--------------------------------------------------------------------------------

punctuate :: Doc -> [Doc] -> Doc
punctuate _ []     = empty
punctuate _ [x]    = x
punctuate s (x:xs) = x <> s <> punctuate s xs


enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l <> d <> r

squotes :: Doc -> Doc
squotes = enclose (char '\'') (char '\'')

dquotes :: Doc -> Doc
dquotes = enclose (char '"') (char '"')


parens :: Doc -> Doc
parens = enclose lparen rparen

brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

braces :: Doc -> Doc
braces = enclose lbrace rbrace

angles :: Doc -> Doc
angles = enclose langle rangle



lparen :: Doc
lparen = char '('

rparen :: Doc
rparen = char ')'

lbracket :: Doc
lbracket = char '['

rbracket :: Doc
rbracket = char ']'

lbrace :: Doc
lbrace = char '{'

rbrace :: Doc
rbrace = char '}'

langle :: Doc
langle = char '<'

rangle :: Doc
rangle = char '>'


--------------------------------------------------------------------------------

replicateChar :: Int -> Char -> Doc
replicateChar i = Doc . (JS.text) . replicate i

spacer :: Int -> Doc
spacer = replicateChar `flip` ' '


padl :: Int -> Char -> Doc -> Doc
padl i c d = step (length d) where
  step dl | dl >= i   = d
          | otherwise = replicateChar (i-dl) c <> d 

padr :: Int -> Char -> Doc -> Doc
padr i c d = step (length d) where
  step dl | dl >= i   = d
          | otherwise = d <> replicateChar (i-dl) c

-- | column
column :: Int -> Char -> Doc -> Doc
column = column_truncr_padl


column_truncr_padl :: Int -> Char -> Doc -> Doc
column_truncr_padl i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeLeft i (getDoc d)
        | dl < i    = replicateChar (i-dl) c <> d
        | otherwise = d

column_truncr_padr :: Int -> Char -> Doc -> Doc
column_truncr_padr i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeLeft i (getDoc d)
        | dl < i    = d <> replicateChar (i-dl) c
        | otherwise = d



column_truncl_padl :: Int -> Char -> Doc -> Doc
column_truncl_padl i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeRight i (getDoc d)
        | dl < i    = replicateChar (i-dl) c <> d
        | otherwise = d

column_truncl_padr :: Int -> Char -> Doc -> Doc
column_truncl_padr i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeRight i (getDoc d)
        | dl < i    = d <> replicateChar (i-dl) c
        | otherwise = d

center :: Int -> Char -> Doc -> Doc
center = center_truncr


center_truncr :: Int -> Char -> Doc -> Doc
center_truncr i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeLeft i (getDoc d)
        | dl < i    = let (spl,spr) = centerSpacings (i-dl) c in
                      spl <> d <> spr
        | otherwise = d

center_truncl :: Int -> Char -> Doc -> Doc
center_truncl i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeRight i (getDoc d)
        | dl < i    = let (spl,spr) = centerSpacings (i-dl) c in
                      spl <> d <> spr
        | otherwise = d


centerSpacings :: Int -> Char -> (Doc,Doc)
centerSpacings i c = let (d,m) = i `divMod` 2 in
                     if m == 0 then dup $ replicateChar d c
                               else (replicateChar d c, replicateChar (d+1) c)
  where
    dup a = (a,a)


-- | Rendering is simple because there is no notion of fitting.
--
render :: Doc -> String
render = JS.toString . getDoc




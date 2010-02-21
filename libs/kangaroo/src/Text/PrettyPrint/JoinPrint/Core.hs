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
  , VDoc
  , empty
  , null
  , length

  , (<>)
  , (<+>)
  , hcat
  , hsep

  , vcat
  , vcons
  , vsnoc

  , text
  , char
  , int
  , integer
  , integral

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
  , truncl 
  , truncr
  
  , render
  , renderIO
  
  ) where


import Text.PrettyPrint.JoinPrint.JoinString ( JoinString, (++) )
import qualified Text.PrettyPrint.JoinPrint.JoinString as JS

import Data.List ( foldl' )
import Prelude hiding ( (++), null, length )

newtype Doc = Doc { getDoc :: JoinString }


newtype VDoc = VDoc { getVDoc :: ShowS }

instance Show Doc where
  show = render

instance Show VDoc where
  show = renderV
        
infixr 6 <>, <+>

-- | Create an empty, zero length document.
--
empty :: Doc
empty = Doc $ JS.empty

-- | Test if the doc is empty.
--
null :: Doc -> Bool
null = JS.null . getDoc 


-- | Get the length of the Doc. 
-- 
-- Length is cached in the document\'s  data type so this 
-- operation is O(1).
--
length :: Doc -> Int
length = JS.length . getDoc

-- | Horizontally concatenate two documents with no space 
-- between them.
-- 
(<>) :: Doc -> Doc -> Doc
Doc a <> Doc b = Doc $ a ++ b

-- | Horizontally concatenate two documents with a single space 
-- between them.
-- 
(<+>) :: Doc -> Doc -> Doc
Doc a <+> Doc b = Doc (a ++ JS.cons1 ' ' b)


hcat :: [Doc] -> Doc
hcat = foldr (<>) empty

hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty


-- Vertically concatenation is different to PPrint or Hughes-PJ.
-- Because the Doc type tracks (horizontal) length, vertically
-- concat cannot use the same type as there is no reasonable
-- horizontal length (max length, length of first, length of 
-- last?)
--

vsnocS :: ShowS -> Doc -> ShowS
vsnocS f a = f . showChar '\n' . renderS a

vcons :: Doc -> VDoc -> VDoc
vcons d (VDoc f) = VDoc (renderS d . showChar '\n' . f)

vsnoc :: VDoc -> Doc -> VDoc
vsnoc (VDoc f) d = VDoc (vsnocS f d)

vcat :: [Doc] -> VDoc
vcat []     = VDoc id
vcat [a]    = VDoc (renderS a)
vcat (a:as) = VDoc $ foldl' vsnocS (renderS a) as



text :: String -> Doc
text = Doc . (JS.text)

char :: Char -> Doc
char = Doc . (JS.text) . return

int :: Int -> Doc
int  = text . show

integer :: Integer -> Doc
integer = text . show

integral :: Integral a => a -> Doc
integral = integer . fromIntegral

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


truncl :: Int -> Doc -> Doc
truncl i d = step (length d) where
    step dl | dl > i    = Doc $ JS.dropLeft i (getDoc d)
            | otherwise = d

truncr :: Int -> Doc -> Doc
truncr i d = step (length d) where
    step dl | dl > i    = Doc $ JS.dropRight i (getDoc d)
            | otherwise = d


-- | Rendering is simple because there is no notion of fitting.
--
render :: Doc -> String
render = JS.toString . getDoc


renderS :: Doc -> ShowS
renderS = showString . render

renderV :: VDoc -> String
renderV = ($ "") . getVDoc

renderIO :: Doc -> IO ()
renderIO = putStrLn . JS.toString . getDoc
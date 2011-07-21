{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.Utils.FormatCombinators
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Formatting combinators - pretty printers without the fitting.
--
--------------------------------------------------------------------------------

module Sound.FMSS.Utils.FormatCombinators
  (
    Doc
  , DocS
  , Format(..)
  , empty
  , showsDoc
  , (<>)
  , (<+>)  
  , vconcat
  , separate
  , hcat
  , hsep
  , vcat
  , vspace

  , text
  , char
  , int
  , integer
  , integral
  , float
  , double
  , dtrunc

  , space
  , spaces
  , comma
  , semicolon
  , line

  , padl
  , padr

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

  , list
  , tupled
  , semiBraces
  , commaSep

  , indent
    
  ) where

import Data.Monoid
import Numeric

-- | Doc is a Join List ...
--
data Doc = Doc1   ShowS 
         | Join   Doc   Doc
         | Line
         | Indent !Int  Doc 


type DocS = Doc -> Doc


-- Join could be improved...
--
unDoc :: Doc -> ShowS
unDoc = step 0 id
  where
   step _ acc (Doc1 sf)    = acc . sf
   step n acc (Join a b)   = let acc' = step n acc a in step n acc' b
   step n acc Line         = acc . showChar '\n' . indentS n
   step n acc (Indent i d) = step (n+i) (acc . (indentS i)) d


indentS :: Int -> ShowS
indentS i | i < 1     = id
          | otherwise = showString $ replicate i ' '

runDoc :: Doc -> String
runDoc = ($ "") . unDoc


instance Show Doc where
  show = runDoc

-- Eq is horrible but it is required for Primitive

instance Eq Doc where
  (==) (Doc1 f)     (Doc1 g)      = (f []) == (g [])    -- horribly expensive!
  (==) (Join a b)   (Join x y)    = a == x && b == y
  (==) Line         Line          = True
  (==) (Indent i a) (Indent j x)  = i == j && a == x
  (==) _            _             = False

instance Monoid Doc where
  mempty = empty
  mappend = (<>)


--------------------------------------------------------------------------------

class Format a where format :: a -> Doc

instance Format Int where
  format = int

instance Format Integer where
  format = integer

instance Format Double where
  format = double


--------------------------------------------------------------------------------
        
infixr 6 <>, <+>



-- | Create an empty, zero length document.
--
empty :: Doc
empty = Doc1 id

-- | Create a document from a ShowS function.
--
showsDoc :: ShowS -> Doc
showsDoc = Doc1


-- | Horizontally concatenate two documents with no space 
-- between them.
-- 
(<>) :: Doc -> Doc -> Doc
a <> b = Join a b 


-- | Horizontally concatenate two documents with a single space 
-- between them.
-- 
(<+>) :: Doc -> Doc -> Doc
a <+> b = Join a (Join space b)

-- | Vertical concatenate two documents with a line break.
-- 
vconcat :: Doc -> Doc -> Doc
vconcat a b = a <> Line <> b



separate :: Doc -> [Doc] -> Doc
separate _   []     = empty
separate sep (a:as) = step a as
  where
    step acc []     = acc
    step acc (x:xs) = step (acc <> sep <> x) xs

-- | Horizontally concatenate a list of documents with @(\<\>)@.
--
hcat :: [Doc] -> Doc
hcat = foldr (<>) empty

-- | Horizontally concatenate a list of documents with @(\<+\>)@.
--
hsep :: [Doc] -> Doc
hsep = separate space

-- | Vertically concatenate a list of documents, with a line 
-- break between each doc.
--
vcat :: [Doc] -> Doc
vcat []     = empty
vcat (x:xs) = step x xs 
  where
    step acc (z:zs) = step (acc `vconcat` z) zs
    step acc []     = acc

-- | Vertically concatenate a list of documents, with a blank line 
-- between each doc.
--
vspace :: [Doc] -> Doc
vspace []     = empty
vspace (x:xs) = step x xs 
  where
    step acc (z:zs) = step (acc `vconcat` empty `vconcat` z) zs
    step acc []     = acc


-- | Create a document from a literal string.
-- 
-- The string should not contain newlines (though this is not 
-- enforced). 
--
text :: String -> Doc
text = Doc1 . showString


-- | Create a document from a literal character.
--
-- The char should not be a tab or newline. 
--
char :: Char -> Doc
char = Doc1 . showChar

-- | Show the Int as a Doc.
--
-- > int  = text . show
--
int :: Int -> Doc
int i | i < 0  = Doc1 $ ('-':) . showInt (abs i)
int i          = Doc1 $ showInt i 

-- | Show the Integer as a Doc.
--
integer :: Integer -> Doc
integer = Doc1 . showInt

-- | Show an \"integral value\" as a Doc via 'fromIntegral'.
--
integral :: Integral a => a -> Doc
integral = Doc1 . showInt

-- | Show the Float as a Doc.
--
float :: Double -> Doc
float = Doc1 . showFloat

-- | Show the Double as a Doc.
--
double :: Double -> Doc
double = Doc1 . showFFloat Nothing



dtrunc :: Double -> Doc
dtrunc = text . truncateDouble


-- | Truncate the printed decimal representation of a Double.
-- The is prefered to 'showFFloat' from Numeric as it produces
-- shorter representations where appropriate.
-- 
-- 0.000000000 becomes 0 rather than however many digs are 
-- specified.
--  
truncateDouble :: Double -> String
truncateDouble d | abs d < 0.0001  = "0"
                 | d < 0.0         = '-' :  fn (abs tx)
                 | otherwise       = fn tx
  where
    tx :: Double
    tx = (realToFrac (roundi (d*1000000.0))) / 1000000.0

    fn v = let (a,b) = properFractioni v 
           in if b == 0.0 then show a else ($ "") $ showFFloat Nothing v

roundi :: RealFrac a => a -> Integer
roundi = round


properFractioni :: RealFrac a => a -> (Integer,a)
properFractioni = properFraction

 
-- | Create a Doc containing a single space character.
--
space :: Doc
space = char ' '


-- | Create a Doc containing @n@ space characters.
--
spaces :: Int -> Doc
spaces i = text $ replicate i ' '


-- | Create a Doc containing a comma, \",\".
--
comma :: Doc
comma = char ','

-- | Create a Doc containing a semi colon, \";\".
--
semicolon :: Doc
semicolon = char ';'

-- | Create a Doc containing newline, \"\\n\".
--
line :: Doc 
line = char '\n'


--------------------------------------------------------------------------------

-- | Fill a doc to the supplied length, padding the right-hand
-- side with spaces.
--
-- Note - this function is expensive - it unrolls the functional
-- representation of the String. 
-- 
-- Also it should only be used for single line Doc\'s.
-- 
padr :: Int -> Doc -> Doc
padr i d = rightpad i ' ' $ runDoc d

padl :: Int -> Doc -> Doc
padl i d = leftpad i ' ' $ runDoc d


leftpad :: Int -> Char -> String -> Doc
leftpad i c ss = step (length ss) 
  where
    step len | len >= i  = text ss
             | otherwise = text (replicate (i-len) c) <> text ss


rightpad :: Int -> Char -> String -> Doc
rightpad i c ss = step (length ss)
  where
    step len | len >= i  = text ss
             | otherwise = text ss <> text (replicate (i-len) c)



--------------------------------------------------------------------------------

-- | Punctuate the Doc list with the separator, producing a Doc. 
--
punctuate :: Doc -> [Doc] -> Doc
punctuate _ []     = empty
punctuate _ [x]    = x
punctuate s (x:xs) = x <> s <> punctuate s xs


-- | Enclose the final Doc within the first two.
--
-- There are no spaces between the documents:
--
-- > enclose l r d = l <> d <> r
--
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l <> d <> r



-- | Enclose the Doc within single quotes.
--
squotes :: Doc -> Doc
squotes = enclose (char '\'') (char '\'')

-- | Enclose the Doc within double quotes.
--
dquotes :: Doc -> Doc
dquotes = enclose (char '"') (char '"')

-- | Enclose the Doc within parens @()@.
--
parens :: Doc -> Doc
parens = enclose lparen rparen

-- | Enclose the Doc within square brackets @[]@.
--
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

-- | Enclose the Doc within curly braces @{}@.
--
braces :: Doc -> Doc
braces = enclose lbrace rbrace

-- | Enclose the Doc within angle brackets @\<\>@.
--
angles :: Doc -> Doc
angles = enclose langle rangle



-- | Create a Doc containing a left paren, \'(\'.
--
lparen :: Doc
lparen = char '('

-- | Create a Doc containing a right paren, \')\'.
--
rparen :: Doc
rparen = char ')'

-- | Create a Doc containing a left square bracket, \'[\'.
--
lbracket :: Doc
lbracket = char '['

-- | Create a Doc containing a right square bracket, \']\'.
--
rbracket :: Doc
rbracket = char ']'

-- | Create a Doc containing a left curly brace, \'{\'.
--
lbrace :: Doc
lbrace = char '{'

-- | Create a Doc containing a right curly brace, \'}\'.
--
rbrace :: Doc
rbrace = char '}'

-- | Create a Doc containing a left angle bracket, \'\<\'.
--
langle :: Doc
langle = char '<'

-- | Create a Doc containing a right angle bracket, \'\>\'.
--
rangle :: Doc
rangle = char '>'

-- | Comma separate the list of documents and enclose in square
-- brackets.
--
list :: [Doc] -> Doc
list = brackets . punctuate comma

-- | Comma separate the list of documents and enclose in parens.
--
tupled :: [Doc] -> Doc
tupled = parens . punctuate comma

-- | Separate the list with a semicolon and enclose in curly 
-- braces.
--
semiBraces :: [Doc] -> Doc
semiBraces = braces . punctuate semicolon



-- | Comma separate the list of documents.
--
commaSep :: [Doc] -> Doc
commaSep = punctuate (comma <> space)


-- | Horizontally indent a Doc.
--
-- Note - this space-prefixes the Doc on /the current line/. It
-- does not indent subsequent lines if the Doc spans multiple 
-- lines.
--
indent :: Int -> Doc -> Doc
indent i d | i < 1     = d
           | otherwise = Indent i d



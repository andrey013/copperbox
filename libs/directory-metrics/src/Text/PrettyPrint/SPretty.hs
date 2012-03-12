{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.SPretty
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Strict pretty printer.
--
-- Acknowledgment - based on Christian Lindig\'s work - Strictly 
-- Pretty.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.SPretty
  (
    Doc

  , testFits

  -- * Rendering
  , pretty

  -- * Primitive combinators
  , (<>)
  , (<+>)
  , ($+$)
  , empty
  , null

  , char
  , text
  , nest 
  , break
  , breakWith
  , group

  -- * Padded from @show@
  , padShowL
  , padStringL
  , padShowR
  , padStringR

  -- * Numbers
  , int
  , integer
  , float
  , double
  , word8
  , word16
  , word32

  , nat2
  , nat4
  

  -- * Punctuation
  , lparen
  , rparen
  , lbracket
  , rbracket
  , lbrace
  , rbrace
  , langle
  , rangle
  
  , space
  , comma
  , dot
  , colon
  , semicolon

  -- * Enclose
  , enclose
  , encloseLoose
  , intersperse
  , intersperseLoose

  , squotes
  , dquotes
  , parens
  , brackets
  , braces
  , angles
  , bananas

  -- * Concatenation
  , docfold
  , hcat
  , hsep
  , vsep

  ) where

import Data.Word
import Data.Monoid

import qualified Numeric as Num
import Prelude hiding ( break, null )



data Doc = Nil 
         | Cat Doc Doc
         | Char Char
         | Text String          -- no newlines
         | Nest Int Doc
         | Break String
         | Group Doc

instance Show Doc where
  show = pretty 80

instance Monoid Doc where
  mempty  = Nil
  mappend = Cat


--------------------------------------------------------------------------------
-- Rendering

testFits :: Doc -> Bool
testFits d = fits 80 [F 0 Flat (Group d)]

pretty :: Int -> Doc -> String
pretty w doc = sdocToString $ format w 0 [F 0 Flat (Group doc)]


data SDoc = SNil
          | SText String SDoc
          | SLine Int SDoc      -- int is indent level of next line


type HString = String -> String

emptyH :: HString
emptyH = id

repeatChar :: Int -> Char -> HString
repeatChar ntimes ch = go ntimes
  where
     go i | i < 1     = emptyH
          | otherwise = (ch :) . go (i-1) 

sdocToString :: SDoc -> String
sdocToString = ($ "") . go
  where
    go             :: SDoc -> HString
    go SNil        = emptyH
    go (SText s d) = (s++) . go d
    go (SLine i d) = ('\n':) . repeatChar i ' ' . go d


data Mode = Flat | Broken
  deriving (Eq,Show)


-- Add extra strictness...
data Fits = F !Int !Mode !Doc


fits :: Int -> [Fits] -> Bool
fits w _                    | w < 0 = False
fits _ []                           = True
fits w ((F _ _ Nil)           :zs)  = fits w zs
fits w ((F i m (Cat x y))     :zs)  = fits w (F i m x : F i m y : zs)
fits w ((F i m (Nest j x))    :zs)  = fits w (F (i+j) m x : zs)
fits w ((F _ _ (Char _))      :zs)  = fits (w - 1) zs
fits w ((F _ _ (Text s))      :zs)  = fits (w - length s) zs
fits w ((F _ Flat (Break s))  :zs)  = fits (w - length s) zs
fits _ ((F _ Broken (Break _)):_ )  = True    -- unreachable
fits w ((F i _ (Group x))     :zs)  = fits w (F i Flat x : zs)
 


format :: Int -> Int -> [Fits] -> SDoc
format _ _ []                           = SNil
format w k ((F _ _ Nil)           :zs)  = format w k zs
format w k ((F i m (Cat x y))     :zs)  = format w k (F i m x : F i m y : zs)
format w k ((F i m (Nest j x))    :zs)  = format w k (F (i+j) m x : zs)
format w k ((F _ _ (Char c))      :zs)  = let d = format w (k + 1) zs
                                          in SText [c] d
format w k ((F _ _ (Text s))      :zs)  = let d = format w (k + length s) zs
                                          in SText s d
format w k ((F _ Flat (Break s))  :zs)  = let d = format w (k + length s) zs
                                          in SText s d
format w _ ((F i Broken (Break _)):zs)  = SLine i (format w i zs)
format w k ((F i _ (Group x))     :zs)  =
    if fits (w-k) (F i Flat x : zs) then format w k (F i Flat x : zs)
                                    else format w k (F i Broken x : zs)




infixr 6 <>, <+>

(<>)            :: Doc -> Doc -> Doc
(<>)            = Cat


(<+>)           :: Doc -> Doc -> Doc
(<+>) a b       = a <> space <> b


($+$)           :: Doc -> Doc -> Doc
($+$) a b       = a <> line <> b


empty           :: Doc 
empty           = Nil

-- | This is the main /innovation/ of SPretty - it allows 
-- documents to incorporate conditional spacing.
--
-- Note:
--
-- > text "" /= null
--
null            :: Doc -> Bool
null Nil        = True
null _          = False


char            :: Char -> Doc
char            = Char

text            :: String -> Doc
text            = Text

textS           :: ShowS -> Doc
textS           = Text . ($ "")

nest            :: Int -> Doc -> Doc
nest            = Nest

break           :: Doc 
break           = Break " "

breakWith       :: String -> Doc
breakWith       = Break

group           :: Doc -> Doc
group           = Group


line            :: Doc
line            = breakWith "\n"



-- | Convert the element to a String and optionally left-pad with
--  the supplied Char to the supplied minimum length. 
--
-- There is no truncation if the string representation of the 
-- showable object is longer than the minimum length.
--
padShowL :: Show a => Int -> Char -> a -> Doc
padShowL i c a = padStringL i c (show a) 



-- | Left-pad the String with the supplied Char to the minimum 
-- length. 
--
-- There is no truncation if the String is longer than the 
-- minimum length.
--
padStringL :: Int -> Char -> String -> Doc
padStringL i c ss = text $ prefix ++ ss
  where
    prefix = replicate (i - length ss) c


-- | Convert the element to a String and optionally right-pad with
--  the supplied Char to the supplied minimum length. 
--
-- There is no truncation if the string representation of the 
-- showable object is longer than the minimum length.
--
padShowR :: Show a => Int -> Char -> a -> Doc
padShowR i c a = padStringR i c (show a) 


-- | Right-pad the String with the supplied Char to the minimum 
-- length. 
--
-- There is no truncation if the String is longer than the 
-- minimum length.
--
padStringR :: Int -> Char -> String -> Doc
padStringR i c ss = text $ ss ++ suffix
  where
    suffix = replicate (i - length ss) c



--------------------------------------------------------------------------------
-- Numbers

int             :: Int -> Doc
int             = text . show

integer         :: Integer -> Doc
integer         = text . show

-- | The implementation is:
--
-- > text . show
--
float           :: Float -> Doc
float           = text . show 

-- | The implementation is:
--
-- > text . show
--
double          :: Double -> Doc
double          = text . show 



-- | Padded to two chars, i.e.
--
-- > word8 12 == "0c"
--
word8           :: Word8 -> Doc
word8 i | i < 16    = textS $ ('0':) . Num.showHex i
        | otherwise = textS $ Num.showHex i




-- | Padded to four chars, i.e.
--
-- > word16 12 == "000c"
--
word16          :: Word16 -> Doc
word16 i | i < 16    = textS $ (repeatChar 3 '0') . Num.showHex i
         | i < 256   = textS $ (repeatChar 2 '0') . Num.showHex i
         | i < 4096  = textS $ ('0':)             . Num.showHex i
         | otherwise = textS $ Num.showHex i


-- | Padded to 32 chars, i.e.
--
-- > word132 12 == "0000000c"
--
word32          :: Word32 -> Doc
word32 i | i < 16     = textS $ (repeatChar 7 '0') . Num.showHex i
         | i < 256    = textS $ (repeatChar 6 '0') . Num.showHex i
         | i < 4096   = textS $ (repeatChar 5 '0') . Num.showHex i
         | i < (16*4) = textS $ (repeatChar 4 '0') . Num.showHex i
         | i < (16*5) = textS $ (repeatChar 3 '0') . Num.showHex i
         | i < (16*6) = textS $ (repeatChar 2 '0') . Num.showHex i
         | i < (16*7) = textS $ ('0':)             . Num.showHex i
         | otherwise  = textS $ Num.showHex i





-- | Padded to two chars, i.e.
--
-- > nat2 6 == "06"
--
-- Negative numbers are printed as-is.
-- 
nat2            :: Int -> Doc
nat2 i | i < 10 && i >= 0 = textS $ ('0':) . Num.showInt i
       | otherwise        = textS $ Num.showInt i

-- | Padded to four chars, i.e.
--
-- > nat4 6 == "0006"
-- 
-- Negative numbers are printed as-is.
--
nat4            :: Int -> Doc
nat4 i | i < 10 && i >= 0 = textS $ (repeatChar 3 '0') . Num.showInt i
       | i < 100          = textS $ (repeatChar 2 '0') . Num.showInt i
       | i < 1000         = textS $ ('0':) . Num.showInt i
       | otherwise        = textS $ Num.showInt i


--------------------------------------------------------------------------------
-- Punctuation

-- | Create a Doc containing a left paren, \'(\'.
--
lparen          :: Doc
lparen          = char '('

-- | Create a Doc containing a right paren, \')\'.
--
rparen          :: Doc
rparen          = char ')'

-- | Create a Doc containing a left square bracket, \'[\'.
--
lbracket        :: Doc
lbracket        = char '['

-- | Create a Doc containing a right square bracket, \']\'.
--
rbracket        :: Doc
rbracket        = char ']'

-- | Create a Doc containing a left curly brace, \'{\'.
--
lbrace          :: Doc
lbrace          = char '{'

-- | Create a Doc containing a right curly brace, \'}\'.
--
rbrace          :: Doc
rbrace          = char '}'

-- | Create a Doc containing a left angle bracket, \'\<\'.
--
langle          :: Doc
langle          = char '<'

-- | Create a Doc containing a right angle bracket, \'\>\'.
--
rangle          :: Doc
rangle          = char '>'


-- | Create a Doc containing a single space character.
--
space           :: Doc
space           = char ' '

-- | Create a Doc containing a comma, \",\".
--
comma           :: Doc
comma           = char ','

-- | Create a Doc containing a dot, \".\".
--
dot             :: Doc
dot             = char '.'

-- | Create a Doc containing a colon, \":\".
--
colon           :: Doc
colon           = char ':'

-- | Create a Doc containing a semi colon, \";\".
--
semicolon       :: Doc
semicolon       = char ';'


--------------------------------------------------------------------------------
-- Enclose


-- | Enclose the final Doc within the first two.
--
-- There are no spaces between the documents:
--
-- > enclose l r d = l <> d <> r
--
enclose         :: Doc -> Doc -> Doc -> Doc
enclose l r d   = l <> d <> r


-- | Enclose the final Doc within the first two.
--
-- There are no spaces between the documents:
--
-- > enclose l r d = l <+> d <+> r
--
encloseLoose    :: Doc -> Doc -> Doc -> Doc
encloseLoose l r d = l <+> d <+> r



intersperse :: Doc -> [Doc] -> Doc
intersperse sep = docfold (\a b -> a <> sep <> b)

intersperseLoose :: Doc -> [Doc] -> Doc
intersperseLoose sep = docfold (\a b -> a <+> sep <+> b)



-- | Enclose the Doc within single quotes.
--
squotes         :: Doc -> Doc
squotes         = enclose (char '\'') (char '\'')

-- | Enclose the Doc within double quotes.
--
dquotes         :: Doc -> Doc
dquotes         = enclose (char '"') (char '"')

-- | Enclose the Doc within parens @()@.
--
parens          :: Doc -> Doc
parens          = enclose lparen rparen

-- | Enclose the Doc within square brackets @[]@.
--
brackets        :: Doc -> Doc
brackets        = enclose lbracket rbracket

-- | Enclose the Doc within curly braces @{}@.
--
braces          :: Doc -> Doc
braces          = enclose lbrace rbrace

-- | Enclose the Doc within angle brackets @\<\>@.
--
angles          :: Doc -> Doc
angles          = enclose langle rangle


-- | Enclose the doc within banana brackets @(\| \|)@.
-- 
-- Bananas use /loose/ spacing between the brackets and enclosed
-- document.
--
bananas         :: Doc -> Doc
bananas         = encloseLoose (text "(|") (text "|)")



--------------------------------------------------------------------------------
-- Concatenation

docfold         :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
docfold sp      = go 
  where
    go []     = empty
    go [a]    = a
    go (a:as) = a `sp` (go as)

hcat            :: [Doc] -> Doc
hcat            = docfold (<>)

hsep            :: [Doc] -> Doc
hsep            = docfold (<+>)


vsep            :: [Doc] -> Doc
vsep            = docfold (\a b -> a <> line <> b)



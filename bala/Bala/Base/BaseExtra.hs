{-# OPTIONS_GHC -XEmptyDataDecls #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.BaseExtra
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Useful functions
--
--------------------------------------------------------------------------------


module Bala.Base.BaseExtra where

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad (ap)

import Data.Char (ord)
import Data.List (mapAccumL, sortBy)

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language


--------------------------------------------------------------------------------
-- Counting may need to consider its direction
--------------------------------------------------------------------------------

data Direction = Upwards | Downwards
  deriving (Eq,Read,Show)
  
  
--------------------------------------------------------------------------------
-- Affi(cher) & Deco(uper) -- alternatives to Read & Show for prettier representations
--------------------------------------------------------------------------------

-- | An alternative to Show - to be used for pretty formatting keeping Show
-- for outputting Haskell readable constructors 
class Affi a where
  affi :: a -> ShowS

-- | An alternative to Read - to be used for reading 'Affi' output 
class Deco a where
  deco :: Parser a

-- | Pretty print an 'Affi' instance  
afficher :: Affi a => a -> String
afficher a = affi a []

-- | Pretty print a list seperated by spaces when the element type is 
-- an instance of 'Affi'.
afficherL :: Affi a => [a] -> String
afficherL as = (hsepS $ map affi as) []

-- | Parse a string when the target datatype is an instance of 'Deco'.
decouper :: Deco a => String -> a
decouper s = case parse deco "" s of
                Left err -> error $ "parse error" ++ show err
                Right a -> a 

-- | Parse a list of elements seperated by whitespace. The target element 
-- must be an instance of 'Deco'.
decouperL :: Deco a => String -> [a]
decouperL s = case parse (many1 $ lexeme deco) "" s of
                     Left err -> error $ "parse error" ++ show err
                     Right a -> a                     

--------------------------------------------------------------------------------
-- Unwrap
--------------------------------------------------------------------------------

-- | Extract an element from a constructor by a type. This only works if the 
-- types carried by the constructor are disjoint.
class Extract a b where extract :: a -> b

                    
--------------------------------------------------------------------------------

-- | An apologia - the pitch arithmetic in Bala has been a source of many 
-- errors, probably because my maths has got rusty. Many arithmetic operations
-- are easy to think of as counting, rather than addition, subtraction... etc 
-- with quixotic number bases. Counting is of course much slower...
-- The first argument to countUntil is a limit incase the count diverges, 
-- otherwise the it is essentially until from the Prelude except it returns a 
-- count for how many times f has been applied rather than the result of 
-- repeatedly applying f.
countUntil :: (Eq a) => Int -> (a -> Bool) -> (a -> a) -> a -> Int
countUntil lim p f a = snd $ until p' f' (a,0)
  where p' (a,i) = if (i < lim) 
                     then (p a) 
                     else (error $ "countUtil diverges at " ++ show lim)
        f' (a,i) = (f a, i+1)

countTo :: (Eq a) => (a -> a) -> a -> a -> Int
countTo fn x y = countUntil 1000 (== y) fn x 

retroCountTo :: (Eq a) => (a -> a) -> a -> a -> Int
retroCountTo fn x y = 1 + countUntil 1000 (== y) fn x 

-- | Repeat succ i times.
successor :: (Enum a) => a -> Int -> a
successor a i = applyi f a i
  where f = if (i >= 0) then succ else pred

-- | Repeat pred i times.
predecessor :: (Enum a) => a -> Int -> a  
predecessor a i = applyi f a i
  where f = if (i >= 0) then pred else succ
  
              
applyi :: (a -> a) -> a -> Int -> a
applyi f a i | i <= 0    = a
             | otherwise = applyi f (f a) (i-1)
             
              
explode12, explode100  :: (Integral a) => a -> (a, a)
explode12 i       = i `divMod` 12
explode100 i      = i `divMod` 100

collapse12, collapse100  :: (Integral a) => (a, a) -> a
collapse12 (o,d)  = d + 12 * o
collapse100 (o,d) = d + 100 * o

normalize12, normalize100 :: (Integral a) => (a, a) -> (a, a) 
normalize12 (o,d)  = let (c, d') = explode12 d in (o + c, d')
normalize100 (o,d) = let (c, d') = explode100 d in (o + c, d')

--------------------------------------------------------------------------------
-- shifty arithmetic numbers for counting intervals
--------------------------------------------------------------------------------

-- "Counting" includes the current position and there is no zero
  
shiftyPlus :: (Num a, Ord a) => a -> a -> a
shiftyPlus a b = a + shiftyStep b

shiftyMinus :: (Num a, Ord a) => a -> a -> a
shiftyMinus a b = a - shiftyStep b
      
shiftyStep a | a < 0     = a + 1
             | otherwise = a - 1

--------------------------------------------------------------------------------
-- alternatively, counting in various ways
--------------------------------------------------------------------------------

-- | Wrap up integers that have /funny/ counting semantics with 
-- a phantom type.
newtype Count a = Count { unCount :: Int } 
  deriving (Eq,Ord)

instance Show (Count a) where
  showsPrec _ (Count i) = shows i 
  
class Countable a where 
  forward  :: a -> Int -> a
  backward :: a -> Int -> a
  
  backward a i =  a `forward` (negate i)

-- | Phantom placeholder for non-negative numbers.
data NonNeg

-- | Smart constructor for non-negative numbers.
nonneg :: Int -> Count NonNeg
nonneg = Count . abs

instance Countable (Count NonNeg) where
  forward (Count i) j  = nonneg $ i + j

-- | Phantom placeholder for non-negative, non-zero numbers.
data NnNz

-- | Smart constructor for non-negative, non-zero numbers.
nnnz :: Int -> Count NnNz
nnnz 0 = error "Cannot create a nonzero from zero"
nnnz i = Count $ abs i

instance Countable (Count NnNz) where
  forward (Count i) j  | signum j == 1        = nnnz $ i + j
                       | i        >  (abs j)  = nnnz $ i + j
                       | otherwise            = nnnz $ i + (j - 2)

-- | Phantom placeholder for /retrograde/ non-negative, non-zero numbers.
-- Retrograde in this instances means that counting includes the current 
-- position.
data RNnNz

-- | Smart constructor for retrograde non-negative, non-zero numbers.
rnnnz :: Int -> Count RNnNz
rnnnz 0 = error "Cannot create a nonzero from zero"
rnnnz i = Count $ abs i


instance Countable (Count RNnNz) where
  forward (Count i) j  | j == 0 || abs j == 1 = Count i
                       | signum j == 1        = rnnnz $ i + (j - 1)
                       | i        >  (abs j)  = rnnnz $ i + (j + 1)
                       | otherwise            = rnnnz $ i + (j - 1)


-- | Addition by counting.                       
countPlus :: (Countable (Count a)) => Count a -> Count a -> Count a 
countPlus a b = a `forward` (unCount b)

-- | Subtraction by counting.  
countMinus :: (Countable (Count a)) => Count a -> Count a -> Count a 
countMinus a b = a `backward` (unCount b)
                           
--------------------------------------------------------------------------------
-- functions
--------------------------------------------------------------------------------

-- | zam - zippy map
zam :: (a -> a -> b) -> [a] -> [b]
zam f (x:y:ys) = f x y : zam f (y:ys)
zam f _        = []

mod12 :: (Integral a) => a -> a
mod12 i = i `mod` 12

mod7 :: (Integral a) => a -> a
mod7  i = i `mod` 7  


sub1 :: Integral a => a -> a
sub1 = flip (-) 1


-- | pointsfree or
ora :: (a -> Bool) -> (a -> Bool) -> a -> Bool
ora f g a = f a || g a

-- | pointsfree and
anda :: (a -> Bool) -> (a -> Bool) -> a -> Bool
anda f g a = f a && g a

-- | dyadic apply
dyap :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dyap f g a b = f (g a b) 

-- | triadic apply
triap :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
triap f g a b c = f (g a b c) 

  
--------------------------------------------------------------------------------
-- Parsec helpers
--------------------------------------------------------------------------------


instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap
  
  
readsParsec :: (Parser a) -> String -> [(a,String)]
readsParsec p s = case parse pfn "" s of
                    Left _ -> []
                    Right a -> [a] 
  where pfn = (,) <$> p <*> getInput

longestString :: [String] -> Parser String
longestString = choice . map string . reverse . sortBy longer
  where longer a b = (length a) `compare` (length b)

  
optOneOf :: [Char] -> Parser (Maybe Char)    
optOneOf cs = optparse $ oneOf cs

optparse :: Parser a -> Parser (Maybe a)
optparse p = option Nothing (Just <$> p)

counting, counting1 :: Parser a -> Parser Int
counting  p = length <$> many p
counting1 p = length <$> many1 p


positiveInt :: Parser Int
positiveInt = read <$> many1 digit

signedInt :: Parser Int
signedInt   = (\ a b -> read (a:b)) <$> sign <*> many1 digit
  where sign        = oneOf "+-"

lexString :: String -> Parser String
lexString   = lexeme . string

lexChar :: Char -> Parser Char
lexChar   = lexeme . char

    
baseLex           = P.makeTokenParser emptyDef


whiteSpace        = P.whiteSpace baseLex
lexeme            = P.lexeme baseLex 
parens            = P.parens baseLex
brackets          = P.brackets baseLex
angles            = P.angles baseLex
integer           = P.integer baseLex
double            = P.float baseLex
stringLiteral     = P.stringLiteral baseLex



float             :: Parser Float
float             = fromRational . toRational <$> double

int               :: Parser Int
int               = fromIntegral <$> integer 

digiti            :: Parser Int
digiti            = (flip (-) 48) . ord  <$> digit


-- | more general type than stringLiteral
doubleQuoted :: Parser a -> Parser a
doubleQuoted p = char '"' *> p <* char '"'

-- | island parsing (if it works...)
water :: Parser a -> Parser a
water p = p <|> (glob *> water p)
  where
    glob = manyTill (noneOf " \t\n") (oneOf " \t\n")

-- | collect the water as a string until p parses     
collectWater :: Parser a -> Parser (String,a)
collectWater p = colwater []
  where
    colwater acc  =  (p >>= \a -> return (unlines $ reverse acc,a) )
                 <|> (restOfLine >>= \s -> colwater (s:acc))
    restOfLine    = manyTill anyChar newline

--------------------------------------------------------------------------------
-- Show helpers 
-- acknowledgement - Daan Leijen's pprint combinators recast for ShowS 
--------------------------------------------------------------------------------

optS :: (Show a) => Maybe a -> ShowS
optS Nothing = id
optS (Just a) = shows a

punctuateS s []      = []
punctuateS s [x]     = [x]
punctuateS s (x:xs)  = (x . s) : punctuateS s xs

encloseSepS l r s []  = l . r
encloseSepS l r s [x] = l . x . r
encloseSepS l r s xs  = l . hcatS (punctuateS s xs) . r

listS           = encloseSepS lbracketS rbracketS commaS
tupledS         = encloseSepS lparenS   rparenS   commaS
semiBraceS      = encloseSepS lbraceS   rbraceS   semiS


hcatS           = foldS (.)
hsepS           = foldS sepS
vsepS           = foldS lineS

foldS f []      = id
foldS f xs      = foldr1 f xs


x `sepS`  y     = x . spaceS . y  
x `lineS` y     = x . newlineS . y

squoteS         = encloseS sglquoteS sglquoteS
dquoteS         = encloseS dblquoteS dblquoteS
braceS          = encloseS lbraceS rbraceS
parenS          = encloseS lparenS rparenS
angleS          = encloseS langleS rangleS
bracketS        = encloseS lbracketS rbracketS
encloseS l r x  = l . x . r

lparenS         = showChar '('
rparenS         = showChar ')'
langleS         = showChar '<'
rangleS         = showChar '>'
lbraceS         = showChar '{'
rbraceS         = showChar '}'
lbracketS       = showChar '['
rbracketS       = showChar ']'     

sglquoteS       = showChar '\''
dblquoteS       = showChar '"'
semiS           = showChar ':'
colonS          = showChar ';'
commaS          = showChar ','
spaceS          = showChar ' '
dotS            = showChar '.'
equalS          = showChar '='
backslashS      = showChar '\\'
newlineS        = showChar '\n'

replicateS :: Int -> ShowS -> ShowS
replicateS i x = hcatS $ replicate i x



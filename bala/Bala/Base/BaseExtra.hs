{-# OPTIONS_GHC -XEmptyDataDecls #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

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
-- |
--------------------------------------------------------------------------------


module Bala.Base.BaseExtra where

import Control.Applicative hiding (many, optional)
import Control.Monad (ap)

import Data.Char (ord)
import Data.List (mapAccumL)

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

--------------------------------------------------------------------------------
-- Affi(cher) & Deco(uper) -- alternatives to Read & Show for prettier representations
--------------------------------------------------------------------------------

class Affi a where
  affi :: a -> ShowS

class Deco a where
  deco :: Parser a
  
afficher :: Affi a => a -> String
afficher a = affi a []

decouper :: Deco a => String -> a
decouper s = case parse deco "" s of
                Left err -> error $ "parse error" ++ show err
                Right a -> a 


spacedElements :: Deco a => String -> [a]
spacedElements s = case parse (many1 $ lexeme deco) "" s of
                     Left err -> error $ "parse error" ++ show err
                     Right a -> a                     

--------------------------------------------------------------------------------
-- semitones is the basis for Pitch arithmetic
--------------------------------------------------------------------------------

class Semitones a where semitones :: a -> Int

                    
--------------------------------------------------------------------------------

successor, predecessor :: (Enum a) => a -> Int -> a

successor a i = applyi f a i
  where f = if (i >= 0) then succ else pred



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

newtype Count a = Count { unCount :: Int } 
  deriving (Eq,Ord)

instance Show (Count a) where
  showsPrec _ (Count i) = shows i 
  
class Countable a where 
  forward  :: a -> Int -> a
  backward :: a -> Int -> a
  
  backward a i =  a `forward` (negate i)


data NonNeg

nonneg :: Int -> Count NonNeg
nonneg = Count . abs

instance Countable (Count NonNeg) where
  forward (Count i) j  = nonneg $ i + j

-- non negative & non zero  
data NnNz

nnnz :: Int -> Count NnNz
nnnz 0 = error "Cannot create a nonzero from zero"
nnnz i = Count $ abs i

instance Countable (Count NnNz) where
  forward (Count i) j  | signum j == 1        = nnnz $ i + j
                       | i        >  (abs j)  = nnnz $ i + j
                       | otherwise            = nnnz $ i + (j - 2)

-- 'Retrograde' non negative & non zero 
-- counting includes the current position
data RNnNz

rnnnz :: Int -> Count RNnNz
rnnnz 0 = error "Cannot create a nonzero from zero"
rnnnz i = Count $ abs i


instance Countable (Count RNnNz) where
  forward (Count i) j  | j == 0 || abs j == 1 = Count i
                       | signum j == 1        = rnnnz $ i + (j - 1)
                       | i        >  (abs j)  = rnnnz $ i + (j + 1)
                       | otherwise            = rnnnz $ i + (j - 1)
                       

countPlus a b = a `forward` (unCount b)

countMinus a b = a `backward` (unCount b)
                           
--------------------------------------------------------------------------------
-- functions
--------------------------------------------------------------------------------

-- zam - zippy map
zam :: (a -> a -> b) -> [a] -> [b]
zam f (x:y:ys) = f x y : zam f (y:ys)
zam f _        = []


mod12 i = i `mod` 12
mod7  i = i `mod` 7  


  
  
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

lexeme :: Parser a -> Parser a
lexeme p = p <* many (oneOf " \t\n")

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


baseLex             = P.makeTokenParser emptyDef


whiteSpace        = P.whiteSpace baseLex 
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



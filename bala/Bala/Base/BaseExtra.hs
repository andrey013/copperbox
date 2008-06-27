{-# LANGUAGE EmptyDataDecls, FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses #-}


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
-- * Affi(cher) & Deco(uper) 
-- ** Alternatives to Read & Show for prettier representations
--------------------------------------------------------------------------------

-- | Affi - alternative to 'Show' - to be used for pretty formatting keeping Show
-- for outputting Haskell readable constructors. 
class Affi a where
  affi :: a -> ShowS

-- | Deco - alternative to Read - to be used for reading 'Affi' output 
class Deco a where
  deco :: Parser a

-- | Pretty print an 'Affi' instance  
afficher :: Affi a => a -> String
afficher a = affi a []

-- | Pretty print a list seperated by spaces when the element type is 
-- an instance of 'Affi'.
afficherL :: Affi a => [a] -> String
afficherL as = (hsepS $ map affi as) []

-- | Pretty print a pair when left and right are both instances of 'Affi'.
afficherP :: (Affi a, Affi b) => (a,b) -> String
afficherP (a,b) = (parenS $ hcatS [affi a, commaS , affi b]) []


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

--------------------------------------------------------------------------------
-- * Counting 
--------------------------------------------------------------------------------


-- | Direction - counting may need to consider its direction
data Direction = Upwards | Downwards
  deriving (Eq,Read,Show)
  
  
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

-- | Repeat succ n times.
successor :: (Enum a) => a -> Int -> a
successor a n = applyi f a n
  where f = if (n >= 0) then succ else pred

-- | Repeat pred n times.
predecessor :: (Enum a) => a -> Int -> a  
predecessor a n = applyi f a n
  where f = if (n >= 0) then pred else succ
  
-- | Apply a function i times.              
applyi :: (a -> a) -> a -> Int -> a
applyi f a i | i <= 0    = a
             | otherwise = applyi f (f a) (i-1)
             


--------------------------------------------------------------------------------
-- * Shifty arithmetic numbers for counting intervals
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
-- * Utility unctions
--------------------------------------------------------------------------------

-- | zam - zippy map.
zam :: (a -> a -> b) -> [a] -> [b]
zam f (x:y:ys) = f x y : zam f (y:ys)
zam f _        = []

-- | mod12 - modulus 12.
mod12 :: (Integral a) => a -> a
mod12 i = i `mod` 12

-- | mod12 - modulus 7.
mod7 :: (Integral a) => a -> a
mod7  i = i `mod` 7  

-- | sub - flipped (-) 
sub :: Num a => a -> (a -> a)
sub = flip (-)

-- | sub1 - subtract 1. 
sub1 :: Num a => a -> a 
sub1 = sub 1


-- | (||) with 'apply' - test a with f, if it fails test it with g.
ora :: (a -> Bool) -> (a -> Bool) -> a -> Bool
ora f g a = f a || g a

-- | (&&) with 'apply' - test a with both f and g.
anda :: (a -> Bool) -> (a -> Bool) -> a -> Bool
anda f g a = f a && g a

-- | Dyadic apply \/ compose - apply the binary function g to a and b, 
-- then apply the unary function f to the result.
dyap :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dyap f g a b = f (g a b) 

-- | Triadic apply \/ compose - apply the ternary function g to a, b and c, 
-- then apply the unary function f to the result.
triap :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
triap f g a b c = f (g a b c) 

--------------------------------------------------------------------------------
-- * Helpers for modulo 12 and modulo 100.
-- $explodedoc
--------------------------------------------------------------------------------

-- $explodedoc
-- Useful for semitone arithmetic (base 12) and cent arithmetic (base 100).
-- The divMod pair makes a /fraction/. 

-- | divMod for base 12.              
explode12 :: (Integral a) => a -> (a, a) 
explode12 i       = i `divMod` 12

-- | divMod for base 100. 
explode100  :: (Integral a) => a -> (a, a)
explode100 i      = i `divMod` 100

-- | Collapse an 'explode12' pair back to an integral.
collapse12  :: (Integral a) => (a, a) -> a
collapse12 (o,d)  = d + 12 * o

-- | Collapse an 'collapse100' pair back to an integral.
collapse100  :: (Integral a) => (a, a) -> a
collapse100 (o,d) = d + 100 * o

-- | Normalize an 'explode12' pair so that the right hand size is less than 12.
normalize12 :: (Integral a) => (a, a) -> (a, a) 
normalize12 (o,d)  = let (c, d') = explode12 d in (o + c, d')

-- | Normalize an 'explode100' pair so that the right hand size is less than 100.
normalize100 :: (Integral a) => (a, a) -> (a, a) 
normalize100 (o,d) = let (c, d') = explode100 d in (o + c, d')

  
--------------------------------------------------------------------------------
-- * Parsec helpers
--------------------------------------------------------------------------------

-- | An applicative instance 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap
  
-- | Use a Parsec parser like a ReadS parser 
readsParsec :: (Parser a) -> String -> [(a,String)]
readsParsec p s = case parse pfn "" s of
                    Left _ -> []
                    Right a -> [a] 
  where pfn = (,) <$> p <*> getInput

-- | Match the longest string
longestString :: [String] -> Parser String
longestString = choice . map (try . string) . reverse . sortBy longer
  where longer a b = (length a) `compare` (length b)


withLongestString :: (String -> Parser b) ->  [(String,a)] -> Parser a
withLongestString f = choice . map (try . interp f) . reverse . sortBy longer
  where 
    longer (a,_) (b,_) = (length a) `compare` (length b)
    interp f (a,b) = b <$ f a
    
    
-- | Wrap Parsec's oneOf with a Maybe to handle failure. 
optOneOf :: [Char] -> Parser (Maybe Char)    
optOneOf cs = optparse $ oneOf cs

-- | An alternative to Parsec's option parser. Whereas option returns a default
-- value if the parse fails, optparse wraps success and failure in a Maybe.
optparse :: Parser a -> Parser (Maybe a)
optparse p = option Nothing (try $ Just <$> p)

-- | Wrap Parser's alterative (\<|\>) combinator with the Either type to 
-- get different types for the left and right parse.
eitherparse :: Parser a -> Parser b -> Parser (Either a b)
eitherparse p p' = (Left <$> p) <|> (Right <$> p')

-- | Return the count of the number of parses, rather than a list of elements.
-- (Note the count combinator in Parsec works differently, it will parse a 
-- element n times).
counting :: Parser a -> Parser Int
counting  p = length <$> many p

-- | Version of counting that must succeed at least once.
counting1 :: Parser a -> Parser Int
counting1 p = length <$> many1 p

-- | A digit seuence, returning Int.
positiveInt :: Parser Int
positiveInt = read <$> many1 digit

-- | A signed digit sequence, returning Int.
signedInt :: Parser Int
signedInt   = (\ a b -> read (a:b)) <$> sign <*> many1 digit
  where sign        = oneOf "+-"

-- | Wrap Parsec's string parser to consume trailing whitespace.
lexString :: String -> Parser String
lexString   = lexeme . string

-- | Wrap Parsec's char parser to consume trailing whitespace.
lexChar :: Char -> Parser Char
lexChar   = lexeme . char

    
baseLex           = P.makeTokenParser emptyDef


whiteSpace        = P.whiteSpace baseLex
lexeme            = P.lexeme baseLex
symbol            = P.symbol baseLex  
parens            = P.parens baseLex
brackets          = P.brackets baseLex
angles            = P.angles baseLex
braces            = P.braces baseLex
integer           = P.integer baseLex
double            = P.float baseLex
stringLiteral     = P.stringLiteral baseLex


-- | Parsec's double parser, coerced to return float.
float             :: Parser Float
float             = fromRational . toRational <$> double

-- | Parsec's integer parser, coerced to return Int.
int               :: Parser Int
int               = fromIntegral <$> integer 

digiti            :: Parser Int
digiti            = (flip (-) 48) . ord  <$> digit


-- | Like Parsec's stringLiteral but with a  more general type.
doubleQuoted :: Parser a -> Parser a
doubleQuoted p = char '"' *> p <* char '"'

-- | Island parsing (does it work?)
-- It generates a parse error if we get to eof without parsing p
-- which is what we want.
water :: Parser a -> Parser a
water p = do
    a <- optparse p    
    case a of
      Just a -> return $ a
      Nothing -> anyChar >> water p 


-- | Collect the water as a string until p parses.     
collectWater :: Parser a -> Parser (String,a)
collectWater p = colwater []
  where
    colwater cs  =  do
      a <- optparse p
      case a of 
        Just a -> return (reverse cs, a)
        Nothing -> anyChar >>= \c -> colwater (c:cs)
        
      

--------------------------------------------------------------------------------
-- * Show helpers 
-- ** Acknowledgement - Daan Leijen's pprint combinators recast for ShowS 
--------------------------------------------------------------------------------

optS :: (Show a) => Maybe a -> ShowS
optS Nothing = id
optS (Just a) = shows a

punctuateS :: ShowS -> [ShowS] -> [ShowS]
punctuateS s []      = []
punctuateS s [x]     = [x]
punctuateS s (x:xs)  = (x . s) : punctuateS s xs

encloseSepS :: ShowS -> ShowS -> ShowS -> [ShowS] -> ShowS
encloseSepS l r s []  = l . r
encloseSepS l r s [x] = l . x . r
encloseSepS l r s xs  = l . hcatS (punctuateS s xs) . r

listS, tupledS, semiBraceS :: [ShowS] -> ShowS
listS           = encloseSepS lbracketS rbracketS commaS
tupledS         = encloseSepS lparenS   rparenS   commaS
semiBraceS      = encloseSepS lbraceS   rbraceS   semiS

hcatS, hsepS, vsepS :: [ShowS] -> ShowS
hcatS           = foldS (.)
hsepS           = foldS sepS
vsepS           = foldS lineS

foldS :: (ShowS -> ShowS -> ShowS) -> [ShowS] -> ShowS
foldS f []      = id
foldS f xs      = foldr1 f xs

sepS, lineS :: ShowS -> ShowS -> ShowS
x `sepS`  y     = x . spaceS . y  
x `lineS` y     = x . newlineS . y

squoteS, dquoteS, braceS, parenS, angleS, bracketS :: ShowS -> ShowS
squoteS         = encloseS sglquoteS sglquoteS
dquoteS         = encloseS dblquoteS dblquoteS
braceS          = encloseS lbraceS rbraceS
parenS          = encloseS lparenS rparenS
angleS          = encloseS langleS rangleS
bracketS        = encloseS lbracketS rbracketS
encloseS l r x  = l . x . r

lparenS, rparenS, langleS, rangleS, lbraceS, rbraceS, lbracketS, rbracketS
  :: ShowS
lparenS         = showChar '('
rparenS         = showChar ')'
langleS         = showChar '<'
rangleS         = showChar '>'
lbraceS         = showChar '{'
rbraceS         = showChar '}'
lbracketS       = showChar '['
rbracketS       = showChar ']'     

sglquoteS, dblquoteS, semiS, colonS, commaS, spaceS, dotS, equalS, 
  backslashS, newlineS, barS 
  :: ShowS
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
barS            = showChar '|'

replicateS :: Int -> ShowS -> ShowS
replicateS i x = hcatS $ replicate i x



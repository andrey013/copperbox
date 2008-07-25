{-# LANGUAGE EmptyDataDecls, FlexibleInstances, FlexibleContexts #-}


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


module Bala.Base.BaseExtra (
  -- * Typeclasses - Affi & Deco
  -- $affidoc 
  Affi(..), Deco(..),
  
  -- * Helpers for Affi & Deco 
  afficher, afficherL, afficherP, decouper, decouperL,

  
  -- * Counting
  -- $countingdoc 
  Direction(..), countUntil, countTo, retroCountTo,
  successor, predecessor, shiftyPlus, shiftyMinus,
  Count(..), Countable(..), 
  NonNeg, nonneg, NnNz, nnnz, RNnNz, rnnnz,
  countPlus, countMinus,
  base2bases, 
  
  -- * Utility functions 
  applyi, zam, 
  mod12, mod7,
  sub, sub1, 
  andthen, ora, anda,
  dyap, triap, 
  hexStr, 
  
  -- ** Helpers for modulo 12 and modulo 100.
  -- $explodedoc 
  explode12, explode100, collapse12, collapse100,
  normalize12, normalize100, 
  
  -- * Parsec helpers
  readsParsec, longestString, withLongestString,
  optOneOf, optparse, eitherparse, counting, counting1,
  whiteSpace, lexeme, symbol, stringLiteral,
  parens, brackets, angles, braces, integer, double, 
  positiveInt, signedInt, float, int, digiti, 
  doubleQuoted, lexString, lexChar,  
  water, collectWater,

  -- * Show helpers 
  -- $showsdoc  
  optS, punctuateS, encloseSepS, listS,
  tupledS, semiBraceS, hcatS, hsepS, vsepS,
  foldS, sepS, lineS,
  squoteS, dquoteS, braceS, parenS, angleS, bracketS,
  lparenS, rparenS, langleS, rangleS, lbraceS, rbraceS,
  lbracketS, rbracketS, sglquoteS, dblquoteS, 
  semiS, colonS, commaS, spaceS, dotS, equalS, 
  backslashS, newlineS, barS, replicateS

  ) where

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad (ap)

import Data.Char (ord)
import Data.List (mapAccumL, sortBy, unfoldr)
import Numeric (showHex)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language



  
  
--------------------------------------------------------------------------------
-- Affi(cher) & Deco(uper) 

-- $affidoc 
-- Alternatives to Read & Show for prettier representations.

-- | Affi - alternative to 'Show' - to be used for pretty formatting keeping Show
-- for outputting Haskell readable constructors. 
class Affi a where
  affi :: a -> ShowS

-- | Deco - alternative to Read - to be used for reading 'Affi' output 
class Deco a where
  -- | @Parser@ is the a Parsec parser, rather than a ReadS one. 
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

--------------------------------------------------------------------------------
-- Counting 

-- $countingdoc
-- An apologia - the pitch arithmetic in Bala has been a source of many 
-- errors, probably because my maths has got rusty. Many arithmetic operations
-- are easy to think of as counting, rather than addition, subtraction... etc 
-- with quixotic number bases. Counting is of course much slower...
-- The first argument to countUntil is a limit in case the count diverges, 
-- otherwise the it is essentially until from the Prelude except it returns a 
-- count for how many times f has been applied rather than the result of 
-- repeatedly applying f.

-- | Direction - counting may need to consider its direction
data Direction = Upwards | Downwards
  deriving (Eq,Read,Show)
  
  

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
successor :: (Enum a) => Int -> a -> a
successor n a = applyi n f a
  where f = if (n >= 0) then succ else pred

-- | Repeat pred n times.
predecessor :: (Enum a) => Int -> a -> a  
predecessor n a = applyi n f a
  where f = if (n >= 0) then pred else succ
  

-- | generate an infinite list of base2 bases (?) 
-- (terminological to do on this one)
base2bases :: [Integer]
base2bases = unfoldr (\x -> Just (x, x * 2)) 1 

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
-- Retrograde in this context means that counting includes the current 
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
-- Utility functions

-- | Apply a function i times.              
applyi :: Int -> (a -> a) -> a -> a
applyi i f a | i <= 0    = a
             | otherwise = applyi (i-1) f (f a) 
             
             
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

-- | Apply @f@ then @g@ to @a@ combining the results with @op@.
-- 
-- > andthen op f g a = f a `op` g a 
andthen :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
andthen op f g a = f a `op` g a 

-- | (||) with 'apply' - test a with f, if it fails test it with g.
ora :: (a -> Bool) -> (a -> Bool) -> a -> Bool
ora = andthen (||) 

-- | (&&) with 'apply' - test a with both f and g.
anda :: (a -> Bool) -> (a -> Bool) -> a -> Bool
anda = andthen (&&)


-- | Dyadic apply \/ compose - apply the binary function g to a and b, 
-- then apply the unary function f to the result.
dyap :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dyap f g a b = f (g a b) 

-- | Triadic apply \/ compose - apply the ternary function g to a, b and c, 
-- then apply the unary function f to the result.
triap :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
triap f g a b c = f (g a b c) 

-- | Show as a hexadecimal string, prefixed with @0x@.
hexStr :: Integral a => a -> String
hexStr a = (showString "0x" . showHex a) []

 
--------------------------------------------------------------------------------
-- Helpers for modulo 12 and modulo 100.

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
-- Parsec helpers

-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap
  
-- | Use a Parsec parser like a ReadS parser. 
readsParsec :: (GenParser Char () a) -> String -> [(a,String)]
readsParsec p s = case parse pfn "" s of
                    Left _ -> []
                    Right a -> [a] 
  where pfn = (,) <$> p <*> getInput

-- | Match the longest string.
longestString :: [String] -> GenParser Char st String
longestString = choice . map (try . string) . reverse . sortBy longer
  where longer a b = (length a) `compare` (length b)

-- | Match the longest string and apply f to interpret it.
withLongestString :: (String -> GenParser Char st b) 
                  -> [(String,a)] 
                  -> GenParser Char st a
withLongestString f = choice . map (try . interp f) . reverse . sortBy longer
  where 
    longer (a,_) (b,_) = (length a) `compare` (length b)
    interp f (a,b) = b <$ f a
    
    
-- | Wrap Parsec's @oneOf@ with a Maybe to handle failure. 
optOneOf :: [Char] -> GenParser Char st (Maybe Char)    
optOneOf cs = optparse $ oneOf cs

-- | An alternative to Parsec's @option@ parser. Whereas option returns a 
-- default value if the parse fails, optparse wraps success and failure in
-- a Maybe.
optparse :: GenParser Char st a -> GenParser Char st (Maybe a)
optparse p = option Nothing (try $ Just <$> p)

-- | Wrap Parser's alterative (\<|\>) combinator with the Either type to 
-- get different types for the left and right parse.
eitherparse :: GenParser Char st a 
            -> GenParser Char st b 
            -> GenParser Char st (Either a b)
eitherparse p p' = (Left <$> p) <|> (Right <$> p')

-- | Return the count of the number of parses, rather than a list of elements.
-- (Note the @count@ combinator in Parsec works differently, it will parse a 
-- element n times).
counting :: GenParser Char st a -> GenParser Char st Int
counting  p = length <$> many p

-- | Version of @counting@ that must succeed at least once.
counting1 :: GenParser Char st a -> GenParser Char st Int
counting1 p = length <$> many1 p

-- | A digit seuence, returning Int.
positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

-- | A signed digit sequence, returning Int.
signedInt :: GenParser Char st Int
signedInt   = (\ a b -> read (a:b)) <$> sign <*> many1 digit
  where sign        = oneOf "+-"



-- | Parsec's @double@ parser, coerced to return float.
float             :: GenParser Char st Float
float             = fromRational . toRational <$> double

-- | Parsec's @integer@ parser, coerced to return Int.
int               :: GenParser Char st Int
int               = fromIntegral <$> integer 

-- | Parsec's @digit@ parser, coerced to return Int rather than Char.
digiti            :: GenParser Char st Int
digiti            = (flip (-) 48) . ord  <$> digit


-- | Like Parsec's @stringLiteral@ but with a more general type.
doubleQuoted :: GenParser Char st a -> GenParser Char st a
doubleQuoted p = char '"' *> p <* char '"'

-- | Wrap Parsec's @string@ parser to consume trailing whitespace.
lexString :: String -> GenParser Char st String
lexString   = lexeme . string

-- | Wrap Parsec's @char@ parser to consume trailing whitespace.
lexChar :: Char -> GenParser Char st Char
lexChar   = lexeme . char

-- | Island parsing (does it work?)
-- It generates a parse error if we get to eof without parsing p
-- which is what we want.
water :: GenParser Char st a -> GenParser Char st a
water p = do
    a <- optparse p    
    case a of
      Just a -> return $ a
      Nothing -> anyChar >> water p 


-- | Collect the water as a string until p parses.     
collectWater :: GenParser Char st a -> GenParser Char st (String,a)
collectWater p = colwater []
  where
    colwater cs  =  do
      a <- optparse p
      case a of 
        Just a -> return (reverse cs, a)
        Nothing -> anyChar >>= \c -> colwater (c:cs)
        



baseLex           = P.makeTokenParser emptyDef

-- | @whiteSpace@ from ParsecChar.
whiteSpace        :: CharParser st ()
whiteSpace        = P.whiteSpace baseLex

-- | @lexeme@ from ParsecChar.
lexeme            :: CharParser st a -> CharParser st a
lexeme            = P.lexeme baseLex

-- | @symbol@ from ParsecChar.
symbol            :: String -> CharParser st String
symbol            = P.symbol baseLex

-- | @stringLiteral@ from ParsecChar.
stringLiteral     :: CharParser st String
stringLiteral     = P.stringLiteral baseLex

-- | @parens@ from ParsecChar.
parens            :: CharParser st a -> CharParser st a  
parens            = P.parens baseLex

-- | @brackets@ from ParsecChar.
brackets          :: CharParser st a -> CharParser st a  
brackets          = P.brackets baseLex

-- | @angles@ from ParsecChar.
angles            :: CharParser st a -> CharParser st a  
angles            = P.angles baseLex

-- | @braces@ from ParsecChar.
braces            :: CharParser st a -> CharParser st a  
braces            = P.braces baseLex

-- | @integer@ from ParsecChar.
integer           :: CharParser st Integer  
integer           = P.integer baseLex

-- | @double@ from ParsecChar.
double            :: CharParser st Double  
double            = P.float baseLex


      

--------------------------------------------------------------------------------
-- Show helpers 

-- $showsdoc
-- Acknowledgement - Daan Leijen's pprint combinators recast for ShowS 

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



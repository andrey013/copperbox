

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


explode12 i       = i `divMod` 12
collapse12 (o,d)  = d + 12 * o
normalize12 (o,d) = let (c, d') = explode12 d in (o + c, d')


explode100 i       = i `divMod` 100
collapse100 (o,d)  = d + 100 * o
normalize100 (o,d) = let (c, d') = explode100 d in (o + c, d')


  
shiftyPlus :: (Num a) => a -> a -> a
shiftyPlus a b = a - 1 + b  
  

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




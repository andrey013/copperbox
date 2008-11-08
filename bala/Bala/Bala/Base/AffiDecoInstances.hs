
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.AffiDecoInstances
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Alternatives to Read & Show for prettier representations.
--
--------------------------------------------------------------------------------

module Bala.Base.AffiDecoInstances where

import Bala.Base.BaseExtra
import Bala.Base.Pitch
import Bala.Base.PitchConversion
import Bala.Base.Scale


import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad
import Data.Char (ord)
import Data.List (sortBy)


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
-- Show (affi) helpers 

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


--------------------------------------------------------------------------------
-- Parsec (deco) helpers

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
optOneOf cs = optionMaybe $ oneOf cs


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
    a <- optionMaybe p    
    case a of
      Just a -> return $ a
      Nothing -> anyChar >> water p 


-- | Collect the water as a string until p parses.     
collectWater :: GenParser Char st a -> GenParser Char st (String,a)
collectWater p = colwater []
  where
    colwater cs  =  do
      a <- optionMaybe p
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
-- Pitch - Affi instances

instance Affi Pitch where
  affi (Pitch l a o) = affi l . affi a . shows o
    
    
instance Affi PitchLabel where 
    affi (PitchLabel l a) = affi l . affi a
        
-- | Print sharps as with muliple '#'s only
instance Affi Accidental where
  affi Nat          = id
  affi Sharp        = showChar '#'
  affi DoubleSharp  = showString "##" 
  affi Flat         = showChar 'b'     
  affi DoubleFlat   = showString "bb" 
  
instance Affi PitchLetter where
    affi = shows

--------------------------------------------------------------------------------
-- Pitch - Deco instances

instance Deco Pitch where 
  deco = decoPitch

-- | Parsec parser for 'Pitch'.
decoPitch :: Parser Pitch
decoPitch = fn <$> deco <*> option 4 positiveInt <*> option 0 signedInt
  where
    fn pl o c = pitch pl o                     
                       
                       
instance Deco PitchLabel where
  deco = decoPitchLabel

-- | Parsec parser for 'PitchLabel'.
decoPitchLabel :: Parser PitchLabel
decoPitchLabel = PitchLabel <$> decoPitchLetter <*> decoAccidental


-- hyper abstract - but is it simpler, clearer? 
-- 
-- decoPitchLabel = PitchLabel <$> deco <*> deco
-- or, 
-- decoPitchLabel' = deco2 PitchLabel
-- deco2 :: (Deco a, Deco b) => (a -> b -> c) -> Parser c
-- deco2 fn = fn <$> deco <*> deco 


instance Deco PitchLetter where
  deco = decoPitchLetter

-- | Parsec parser for 'PitchLetter'.
decoPitchLetter :: Parser PitchLetter    
decoPitchLetter = letter <$> oneOf "ABCDEFG" 
  where 
    letter 'A' = A
    letter 'B' = B
    letter 'C' = C
    letter 'D' = D
    letter 'E' = E
    letter 'F' = F
    letter 'G' = G 
    
instance Deco Accidental where 
  deco = decoAccidental

-- | Parsec parser for 'Accidental'.
decoAccidental :: Parser Accidental 
decoAccidental = withLongestString (lexString) $ 
  [ ("##", DoubleSharp)
  , ("#", Sharp)
  , ("bb", DoubleFlat)
  , ("b", Flat)
  , ("", Nat)
  ]

         
--------------------------------------------------------------------------------
-- Scale - Affi instances

instance Affi Scale where
  affi (Scale r ps) = hsepS $ map affi ps
  
--------------------------------------------------------------------------------
-- Scale - Deco instances




--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------

instance Read MidiPitch where 
  readsPrec _ s = readsParsec (int >>= return . midiPitch) s
  
instance Read Hertz where 
  readsPrec _ s = readsParsec (float >>= return . hertz) s
  
instance Read OctavePitchClass where 
  readsPrec _ s = readsParsec (float >>= return . octavePitchClass) s
    
instance Read OctaveFractional where 
  readsPrec _ s = readsParsec (float >>= return . octaveFractional) s
  
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------
instance Show MidiPitch where
  showsPrec _ = shows . midiValue

instance Show Hertz where
  showsPrec _ = shows . hertzValue
  
instance Show OctavePitchClass where
  showsPrec _ = shows . ovePCValue

instance Show OctaveFractional where
  showsPrec _ = shows . oveFrValue

    
    
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.ParserCombinators
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Two continuation parser combinators.
-- 
--------------------------------------------------------------------------------

module DirectoryMetrics.ParserCombinators
  (
    Parser

  , Result(..)
  , ParseError

  , runParser
  , runParserEither
  , parseFromFile

  , apply
  , failure
  , throwError
  , (<?>)
  , lookahead
  , peek
  , eof
  , satisfy
  , oneOf
  , noneOf
  , restOfLine
  , dropLine
  , dropLines

  , chainl1
  , chainr1
  , chainl
  , chainr
  , choice
  , count
  , between
  , option
  , optionMaybe
  , optionUnit
  , skipOne
  , skipMany
  , skipMany1
  , many1
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , manyTill  
  , manyTill1
  , eitherOf

  -- * Char parsers
  , char
  , string
  , anyChar
  , upper
  , lower
  , letter
  , alphaNum
  , digit
  , hexDigit
  , octDigit
  , newline
  , tab
  , space
  , lexeme

  , natural
  , integer
  , int

  ) where

import Control.Applicative
import Control.Monad
import Data.Char


data ParserInput = ParserInput 
      { current_line    :: String
      , restore_line    :: !String
      , rest_of_input   :: [String]
      }


-- inputRestore :: ParserInput -> ParserInput
-- inputRestore inp = inp {current_line = restore_line inp }
            
inputToNextLine :: ParserInput -> ParserInput
inputToNextLine inp = case rest_of_input inp of
    []        -> ParserInput { current_line  = "" 
                             , restore_line  = ""
                             , rest_of_input = [] }

    (l:ls)    -> ParserInput { current_line  = l
                             , restore_line  = l
                             , rest_of_input = ls }

inputEOF :: ParserInput -> Bool
inputEOF inp = null (rest_of_input inp) && null (current_line inp)


makeParserInput :: String -> ParserInput 
makeParserInput = go . lines
  where
    go []       = ParserInput "" "" []
    go (s:ss)   = ParserInput s  s  ss

data Result ans = Fail String | Okay !ParserInput ans 


type SK a ans = a -> FK ans -> ParserInput -> Result ans
type FK ans   = Result ans


newtype Parser a = Parser { 
      getParser :: forall ans. SK a ans -> FK ans -> ParserInput -> Result ans }


type ParseError   = String




runParser :: Parser a -> String -> Result a
runParser p ss = getParser p skZero fkZero (makeParserInput ss)
  where
    skZero = \ans _ inp -> Okay inp ans
    fkZero = Fail ""



runParserEither :: Parser a -> String -> Either ParseError a
runParserEither p = post . runParser p
  where    
    post (Okay _ a)    = Right a
    post (Fail err)    = Left err

parseFromFile :: FilePath -> Parser a -> IO (Either ParseError a)
parseFromFile path p = 
    readFile path >>= return . runParserEither p
     


-- @return@ of Monad, @pure@ of Applicative
--
yield  :: a -> Parser a
yield a = Parser $ \sk fk ss -> sk a fk ss


-- mplus of MonadPlus, (<|>) of Applicative.
--
alt :: Parser a -> Parser a -> Parser a
alt p1 p2 = Parser $ \sk fk ss -> getParser p1 sk (getParser p2 sk fk ss) ss

infixl 5 `apply`

apply :: Parser a -> (a -> b) -> Parser b
apply = flip fmap


failure :: Parser a
failure = Parser $ \_ fk _ -> fk


-- eager-many / zero-or-more (many of Applicative)
--
eagerMany :: Parser a -> Parser [a]
eagerMany p = (p >>= \r -> eagerMany p `apply` \rs -> (r:rs)) `alt` return [] 

-- eager-some / one-or-more (some of Applicative)
--
eagerSome :: Parser a -> Parser [a]
eagerSome p = p >>= \r -> eagerMany p `apply` \rs -> (r:rs)


instance Functor Parser where
  fmap f mf = Parser $ \sk -> getParser mf $ \a -> (sk . f) a
     


instance Applicative Parser where
  pure = yield
  (<*>) = ap

instance Alternative Parser where
  empty = failure
  (<|>) = alt
  many  = eagerMany
  some  = eagerSome

 
instance Monad Parser where
  return  = yield
  m >>= k = Parser $ \sk -> getParser m $ \a -> getParser (k a) sk


instance MonadPlus Parser where
  mzero = failure
  mplus = alt



--------------------------------------------------------------------------------
-- Combinators


throwError :: String -> Parser a
throwError err_msg = Parser $ \_ _ _ -> Fail err_msg


infixr 0 <?>

(<?>) :: Parser a -> String -> Parser a
p <?> err_msg = Parser $ \sk fk ss -> getParser p sk (swapMsg fk) ss
  where
    swapMsg (Fail _) = Fail err_msg
    swapMsg okay     = okay


-- | This one is from Chris Okasaki\'s \"Even Higher-Order 
-- Functions for Parsing\".
--
lookahead :: Parser a -> (a -> Parser b) -> Parser b 
lookahead p mf  = Parser $ \sk fk -> 
    getParser p (\a fk2 -> getParser (mf a) sk fk2) fk


-- | Peek tries the supplied parse, but does not consume input 
-- \*\* even when the parse succeeds \*\*.
--
peek :: Parser a -> Parser a
peek p = Parser $ \sk fk inp -> 
    getParser p (\a fk2 _ -> sk a fk2 inp) fk inp


eof :: Parser ()
eof = Parser go
  where
    go sk fk inp | inputEOF inp = sk () fk inp
    go _  fk _                  = fk


char :: Char -> Parser Char
char ch = Parser go
  where
    go sk fk inp = case current_line inp of
                    (c:cs) | c == ch -> sk ch fk (inp { current_line = cs})
                    _                -> fk


satisfy :: (Char -> Bool) -> Parser Char
satisfy test = Parser go
  where
    go sk fk inp = case current_line inp of 
                    (c:cs) | test c -> sk c fk (inp { current_line = cs})
                    _               -> fk 


oneOf           :: [Char] -> Parser Char
oneOf cs        = satisfy (`elem` cs)

noneOf          :: [Char] -> Parser Char
noneOf cs       = satisfy (`notElem` cs)


restOfLine      :: Parser String
restOfLine      = Parser go
  where
    go sk fk inp = sk (current_line inp) fk (inputToNextLine inp)


dropLine        :: Parser ()
dropLine        = restOfLine >> return ()

dropLines       :: Int -> Parser ()
dropLines n     = count n dropLine >> return ()

-- Note - the type sigs of the chain parsers can be generalized 
-- to any MonadPlus.
--

chainl1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainl1 p op = p >>= rest 
  where 
    rest x = mplus (op >>= \f -> p >>= \a -> rest (f x a)) (return x) 
               

chainr1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan 
   where 
     scan   = p >>= rest 
     rest x = mplus (op >>= \f -> scan >>= \a -> rest (f x a)) (return x) 

chainl :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainl p op v = mplus (chainl1 p op) (return v)

chainr :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainr p op v = mplus (chainr1 p op) (return v)


infixr 5 <:> 
 
-- | Applicative cons.
--
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2


choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty 
   
count :: Applicative f => Int -> f a -> f [a]
count i p | i <= 0    = pure []
          | otherwise = p <:> count (i-1) p 
          
between :: Applicative f => f open -> f close -> f a -> f a
between o c a = o *> a <* c

          
option :: Alternative f => a -> f a -> f a
option x p          = p <|> pure x

optionMaybe :: Alternative f => f a -> f (Maybe a)
optionMaybe = optional

-- aka Parsecs /optional/
optionUnit :: Alternative f => f a -> f ()
optionUnit p = () <$ p <|> pure ()

skipOne :: Applicative f => f a -> f ()
skipOne p = p *> pure ()

skipMany :: Alternative f => f a -> f ()
skipMany p = many_p
  where 
    many_p = some_p <|> pure ()
    some_p = p       *> many_p

skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p

-- | 'many1' an alias for Control.Applicative 'some'. 
--
many1 :: Alternative f => f a -> f [a]
many1 = some

sepBy :: Alternative f => f a -> f b -> f [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Alternative f => f a -> f b -> f [a]
sepBy1 p sep = p <:> step 
  where 
    step = (sep *> p) <:> step <|> pure []

sepEndBy :: Alternative f => f a -> f b -> f [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

sepEndBy1 :: Alternative f => f a -> f b -> f [a]
sepEndBy1 p sep = (p <* sep) <:> step 
  where
    step = (p <* sep) <:> step <|> pure []
    
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = step <|> pure [] 
  where
    step  = p <:> (final <|> step)
    final = [] <$ end 

manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p end = p <:> step 
  where
    step  = final <|> (p <:> step)
    final = [] <$ end


eitherOf :: Parser a -> Parser b -> Parser (Either a b)
eitherOf pa pb = (Left <$> pa) <|> (Right <$> pb) 

--------------------------------------------------------------------------------
-- Char parsers


string :: String -> Parser String
string ss = mapM char ss

anyChar :: Parser Char
anyChar = Parser go
  where
   go sk fk inp = case current_line inp of
                    (s:ss) ->  sk s fk (inp { current_line = ss })
                    _      -> fk


upper       :: Parser Char
upper       = satisfy isUpper

lower       :: Parser Char
lower       = satisfy isLower

letter      :: Parser Char
letter      = satisfy isAlpha

alphaNum    :: Parser Char
alphaNum    = satisfy isAlphaNum

digit       :: Parser Char
digit       = satisfy isDigit 

hexDigit    :: Parser Char
hexDigit    = satisfy isHexDigit

octDigit    :: Parser Char
octDigit    = satisfy isOctDigit

newline     :: Parser Char
newline     = char '\n'

tab         :: Parser Char
tab         = char '\t'

space       :: Parser Char
space       = satisfy isSpace


lexeme      :: Parser a -> Parser a
lexeme p    = p <* many space


naturalParser :: Num a => (Char -> a) -> Parser a
naturalParser conv = post 0 <$> many1 digit
  where
    post ac (c:cs) = post (ac * 10 + (conv c)) cs
    post ac []     = ac

signedParser :: Num a => Parser a -> Parser a
signedParser pnum = ($) <$> psign <*> pnum
  where
    psign = option id (negate <$ char '-')


natural :: Parser Integer
natural = naturalParser (fromIntegral . digitToInt)


integer :: Parser Integer
integer = signedParser natural

int     :: Parser Int
int     = signedParser (naturalParser digitToInt)
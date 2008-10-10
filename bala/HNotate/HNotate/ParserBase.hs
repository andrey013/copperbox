
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ParserBase
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- ParserBase - utilty parsers and meta-comment parsing.
--
--------------------------------------------------------------------------------

module HNotate.ParserBase where

import HNotate.Env
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad
import Data.List (sortBy)
import Data.Monoid
import Data.Sequence hiding (length, reverse)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)


-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap



data Nested = NestStart | NestEnd
  deriving (Eq,Show)
  
-- Label the meta directives with an index as the parse progresses...
type PState = Int

type StParser a =  GenParser Char PState a

parseFromFileState :: GenParser Char st a 
                   -> FilePath 
                   -> st 
                   -> IO (Either ParseError a)
parseFromFileState p fname st = do
    input <- readFile fname
    return (runParser p st fname input)

parseTestState :: Show a => StParser a -> [Char] -> PState -> IO ()
parseTestState p input st = 
    case (runParser p st "" input) of
      Left err -> putStr "parse error at " >> print err
      Right x -> print x

incrCount :: StParser Int
incrCount = do 
    i <- getState  
    updateState (+1)
    return i



--------------------------------------------------------------------------------
-- Pre-processor helpers


newtype Token = Token { getIsland :: String }
  deriving Show

streamTokens :: [Token] -> String
streamTokens []     = ""
streamTokens (x:xs) = work x xs ""
  where
    work (Token x) []     d = d `append` x
    work (Token x) (y:ys) d = work y ys (d `append` x)
    
    append acc a = acc ++ " " ++ a

    
twoPass :: StParser [Token] -> StParser a -> SourceName -> IO (Either ParseError a)
twoPass prepro parser filepath = 
    either fk sk =<< parseFromFileState prepro filepath 0
  where
    sk ts  = either fk (return . Right) (runParser parser 0 
                                                   pp_name (streamTokens ts))
             
    fk err = return $ Left err
    
    pp_name  = "post-processed " ++ filepath
    
--------------------------------------------------------------------------------
-- Abc and LilyPond 'expression views' have the same shape but differ
-- in terminal commands 

exprView :: StParser (Env -> Env) -> StParser ExprView
exprView cmds = ExprView <$> exprs
  where
    exprs :: StParser [Expr]
    exprs = whiteSpace >> many1 expr1
    
    expr1 :: StParser Expr
    expr1 = choice [metaAction, letExpr] 

    metaAction :: StParser Expr
    metaAction = Action 
        <$> incrCount <*> ((lexeme startMeta) *> metadirective) 
                      <*  (lexeme endMeta)
                  
    letExpr :: StParser Expr          
    letExpr = eitherparse startNested cmds >>= either nestedBlock cmdExpr
      where
        nestedBlock ()    = (LetExpr id) <$> nestedk []
        cmdExpr     cmd   = (LetExpr cmd)  <$> exprs


    nestedk :: [Expr] -> StParser [Expr]
    nestedk cca = eitherparse endNested expr1 >>= either end cont
      where 
        end  () = return $ reverse cca
        cont a  = nestedk (a:cca)   

    metadirective :: StParser MetaDirective
    metadirective = metaoutput
      where
        metaoutput :: StParser MetaDirective
        metaoutput = MetaOutput <$> (outputScheme <* colon) <*> identifier
    
    outputScheme :: StParser OutputScheme 
    outputScheme = choice [ lyRelative, abcDefault ]
      
    lyRelative  = LyRelative <$ symbol "relative" 
    abcDefault  = AbcDefault <$ symbol "default"
    
    -- Abc meta symbols are replaced after preprocesing with LilyPond ones 
    startMeta         :: CharParser st String
    startMeta         = symbol "%{#" 
        
    endMeta           :: CharParser st String 
    endMeta           = symbol "#%}" 
    
    startNested       :: StParser ()
    startNested       = () <$ symbol "{"
    
    endNested         :: StParser ()
    endNested         = () <$ symbol "}"


--------------------------------------------------------------------------------
-- Abc and LilyPond 'expression views' have the same shape but differ
-- in terminal commands 

textView :: StParser a -> StParser b -> StParser TextualView
textView meta_start meta_end = TextualView <$> collect mempty
  where
    collect se = do 
        txt    <- waterUpto marker
        at_end <- option False (True <$ eof)
        case at_end of
          True -> return $ se `add` txt
          False -> do { mark <- metamarkK
                      ; collect $ (se `add` txt) |> mark }    

    metamarkK :: StParser TextElement    
    metamarkK = MetaMark 
        <$> incrCount <*> sourcePosition <*> (manyTill anyChar meta_end) 
    
    add se "" = se
    add se ss = se |> (SourceText ss)  
    
    marker = eitherparse eof (lexeme meta_start)
          <?> "marker"  
    
--------------------------------------------------------------------------------
-- Utility functions

    
    
manyWaterIsland :: GenParser Char st a -> GenParser Char st [a]
manyWaterIsland p = many $ try (water p) 

manyWaterIslandCollect :: GenParser Char st a -> GenParser Char st [(String,a)]
manyWaterIslandCollect p = many $ try (collectWater p) 


-- Note - the supplied parser can't match against eof! 
water :: GenParser Char st a -> GenParser Char st a
water p = do
    a <- optparse (eitherparse (eof <?> "") p)    
    case a of
      Just (Left _) -> fail "water - eof reached - parse failed"
      Just (Right ans) -> return ans
      Nothing -> anyChar >> water p 
  <?> "water"      

waterMaybe :: GenParser Char st a -> GenParser Char st (Maybe a)
waterMaybe p = do
    a <- optparse (eitherparse (eof <?> "") p)    
    case a of
      Just (Left _) -> return Nothing
      Just (Right ans) -> return (Just ans)
      Nothing -> anyChar >> waterMaybe p 
  <?> "waterMaybe" 
  
-- | Collect the water as a string until p parses.     
collectWater :: GenParser Char st a -> GenParser Char st (String,a)
collectWater p = colwater []
  where
    colwater cs  =  do
      a <- optparse p
      case a of 
        Just a -> return (reverse cs, a)
        Nothing -> anyChar >>= \c -> colwater (c:cs)
        
waterUpto :: GenParser Char st a -> GenParser Char st String
waterUpto p = collect []
  where 
    collect cs = do 
      a <- optparse p
      case a of 
        Just a -> return (reverse cs)
        Nothing -> anyChar >>= \c -> collect (c:cs) 
            
        
              
sourcePosition :: GenParser Char st SrcPos
sourcePosition = makeSrcPos <$> getPosition


makeSrcPos pos = SrcPos {
    _src_line     = sourceLine pos,
    _src_column   = sourceColumn pos,
    _src_file     = sourceName pos
  } 


chooseString :: [String] -> GenParser Char st String  
chooseString = choice . map string

-- | Match the longest string.
longestString :: [String] -> GenParser Char st String
longestString = choice . map (try . string) . reverse . sortBy longer
  where longer a b = (length a) `compare` (length b)
  

-- parse a character but return unit    
charDrop :: Char -> GenParser Char st ()
charDrop a = char a >> return () 



-- | Return the count of the number of parses, rather than a list of elements.
-- (Note the @count@ combinator in Parsec works differently, it will parse a 
-- element n times).
counting :: GenParser Char st a -> GenParser Char st Int
counting  p = length <$> many p

-- | Version of @counting@ that must succeed at least once.
counting1 :: GenParser Char st a -> GenParser Char st Int
counting1 p = length <$> many1 p



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


--------------------------------------------------------------------------------
-- Lexical analysis

baseLex           = P.makeTokenParser emptyDef

-- | @lexeme@ from ParsecChar.
lexeme            :: CharParser st a -> CharParser st a
lexeme            = P.lexeme baseLex

symbol            :: String -> CharParser st String
symbol            = P.symbol baseLex

identifier        :: CharParser st String
identifier        = P.identifier baseLex

integer           :: CharParser st Integer
integer           = P.integer baseLex

int               :: CharParser st Int
int               = fromIntegral <$> integer 

colon             :: CharParser st String
colon             = P.colon baseLex

braces            :: CharParser st a -> CharParser st a
braces            = P.braces baseLex

commaSep          :: CharParser st a -> CharParser st [a]
commaSep          = P.commaSep baseLex

whiteSpace        :: CharParser st ()
whiteSpace        = P.whiteSpace baseLex

-- 'tightident' a version of identifier that doesn't 
-- consume trailing whitespace
tightident :: CharParser st String
tightident = try $ 
    (:) <$> P.identStart emptyDef <*> many (P.identLetter emptyDef)

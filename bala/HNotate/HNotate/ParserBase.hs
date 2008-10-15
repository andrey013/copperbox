
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

import HNotate.CommonUtils (para)
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>), empty )
import Control.Monad
import Data.Char (isSpace)
import Data.List (sortBy)
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (length, reverse)
import Prelude hiding (null)
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
-- Common machinery for preprcessing

waterAcc :: GenParser Char st (TokenF a) -> GenParser Char st (Seq a)
waterAcc p = step empty <?> "waterAcc"
  where 
    step acc = do
      a <- optparse (eitherparse (eof <?> "") p)    
      case a of
        Just (Left _) -> return $ acc
        Just (Right f) -> step (f acc)
        Nothing -> anyChar >> step acc 


-- intersperse is easy as a paramorphism!
untoken :: Seq String -> String
untoken = para phi ""
  where 
      phi :: String -> (Seq String, String) -> String 
      phi c (rest, acc)   | null rest      = c ++ acc
                          | otherwise      = c ++ (' ':acc)
   
  
type TokenF a = Seq a -> Seq a


dropToken :: TokenF String
dropToken = \se -> se

token1 :: (a -> String) -> a -> TokenF String
token1 f a = \se -> se |> f a

token2 :: (a -> String) -> (b -> String) -> a -> b -> TokenF String
token2 f g a b  = \se -> se |> f a |> g b

token3 :: (a -> String) -> (b -> String) -> (c -> String) 
       -> a -> b -> c -> TokenF String
token3 f g h a b c  = \se -> se |> f a |> g b |> h c


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

   


twoPass :: (FilePath -> IO (Either ParseError String)) 
           -> Parser [Expr] -> FilePath -> IO (Either ParseError [Expr])
twoPass prepro parser filepath = 
    either fk sk =<< prepro filepath
  where
    sk ss  = either fk (return . Right) (parse parser pp_name ss)
             
    fk err = return $ Left err
    
    pp_name  = "post-processed " ++ filepath
    


--------------------------------------------------------------------------------
-- Common parsers for meta directives



metaCmd :: String -> StParser String
metaCmd ss = symbol ss <* colon 

-- Abc meta symbols are replaced after preprocesing with LilyPond ones 
startMeta         :: StParser String
startMeta         = symbol "%{#" 
    
endMeta           :: StParser String 
endMeta           = symbol "#%}" 

startNested       :: StParser ()
startNested       = () <$ symbol "{"

endNested         :: StParser ()
endNested         = () <$ symbol "}"

metameter         :: StParser MetaDirective    
metameter         = MetaMeter <$> (metaCmd "meter_pattern" *> undefined)


--------------------------------------------------------------------------------
-- Abc and LilyPond 'expression views' have the same shape but differ
-- in terminal commands 

{-

exprView :: StParser (Env -> Env) -> StParser Hoas
exprView cmds = Hoas <$> exprs
  where
    exprs :: StParser [HoasExpr]
    exprs = whiteSpace >> many1 expr1
    
    expr1 :: StParser HoasExpr
    expr1 = choice [metaAction, letExpr] 

    metaAction :: StParser HoasExpr
    metaAction = id 
        <$> <- incrCount <*> -> ((lexeme startMeta) *> metadirective) 
                      <*  (lexeme endMeta)
                  
    letExpr :: StParser HoasExpr          
    letExpr = eitherparse startNested cmds >>= either nestedBlock cmdExpr
      where
        nestedBlock ()    = (HLetExpr id) <$> nestedk []
        cmdExpr     cmd   = (HLetExpr cmd)  <$> exprs


    nestedk :: [HoasExpr] -> StParser [HoasExpr]
    nestedk cca = eitherparse endNested expr1 >>= either end cont
      where 
        end  () = return $ reverse cca
        cont a  = nestedk (a:cca)   

    metadirective :: StParser HoasExpr
    metadirective = metaoutput
      where
        metaoutput :: StParser HoasExpr
        metaoutput = (\a b -> HOutputDirective  (Just a) b)
                  <$> outputScheme <*> identifier
    
    outputScheme :: StParser OutputScheme 
    outputScheme = choice [ lyRelative, abcDefault ]
      
    lyRelative  = OutputRelative <$ metaCmd "relative" 
    abcDefault  = OutputDefault  <$ metaCmd "default"
    
-}
    
-- syntax of meta comments is common to both Abc & LilyPond

topLevelExprs :: [Parser Term] -> Parser [Expr] 
topLevelExprs ps = whiteSpace >> (many $ expr ps)


expr :: [Parser Term] -> Parser Expr
expr ps = Expr <$> (term ps) <*> option [] (exprList ps)

exprList :: [Parser Term] -> Parser [Expr]
exprList ps = between openNesting closeNesting exprs
  where exprs = many1 (expr ps)


openNesting     :: Parser String
openNesting     = symbol "{"

closeNesting    :: Parser String
closeNesting    = symbol "}"


term :: [Parser Term] -> Parser Term
term specialTerms = choice specialTerms <|> genericTerm

genericTerm :: Parser Term
genericTerm = id <$> (symbol "%{#" *> p <* symbol "#%}")
  where
    p = choice [outputDirective, meterPatternT] 


outputDirective :: Parser Term
outputDirective = OutputDirective <$> 
    (directive "output" *> optparse scheme) <*> identifier
  
scheme :: Parser OutputScheme
scheme = choice [relative, dflt]
  where
    relative = OutputRelative <$ cmdsymbol "relative"
    dflt     = OutputDefault  <$ cmdsymbol "default"

meterPatternT :: Parser Term 
meterPatternT = Let . LetMeterPattern <$> 
    (directive "meter_pattern" *> meterPattern)

meterPattern :: Parser MeterPattern
meterPattern = (,) <$> sepBy1 int plus <*> (slash *> simpleDuration)
  where 
    plus  = symbol "+"  
    slash = symbol "/"  

simpleDuration :: Parser Duration
simpleDuration = (convRatio . (1%)) <$> int 

    
cmdsymbol :: String -> Parser String
cmdsymbol = try . symbol . ('\\' :)

directive :: String -> Parser String
directive s = symbol s <* lexeme colon

--------------------------------------------------------------------------------
-- Abc and LilyPond 'expression views' have the same shape but differ
-- in terminal commands 


-- Water to the left, at the pos of the output directive
-- Because both views are interpreted topdown, left-to-right
-- we don't need to store what the output directive says
-- (actually we don't really need is src position either).
-- The result sequence always ends with (water,Nothing),
-- otherwise we should always have (water, Just pos) - but
-- water might be "".




collectWaterAcc :: GenParser Char st a -> GenParser Char st (Seq TextChunk)
collectWaterAcc p = step empty "" <?> "collectWaterAcc"
  where 
    step se retaw = do
      a <- optparse $ eitherparse (eof <?> "") (withPos p)    
      case a of
          -- Eof
        Just (Left _)         -> return $ se |> (reverse retaw, Nothing)
        
          -- p matched
        Just (Right (pos,_))  -> step (se |> (reverse retaw, Just pos)) ""
                
          -- more water
        Nothing               -> anyChar >>= \c -> step se (c:retaw) 

withPos :: GenParser Char st a -> GenParser Char st (SrcPos,a)        
withPos p = (,) <$> sourcePosition <*> p   

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

nonwhite :: GenParser Char st String
nonwhite = lexeme (many1 nonwhiteChar)

nonwhiteChar :: GenParser Char st Char
nonwhiteChar = satisfy $ not . isSpace 

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

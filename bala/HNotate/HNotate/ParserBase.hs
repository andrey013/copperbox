
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
import HNotate.Monads
import HNotate.MusicRepDatatypes
import HNotate.TemplateDatatypes
import HNotate.PrettyInstances

import Control.Applicative hiding (many, optional, (<|>), empty )
import Control.Monad
import Data.Char (isSpace)
import Data.List (sortBy, intersperse)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (length, reverse)
import Prelude hiding (null)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.PrettyPrint.Leijen as PP

-- ExprParser returns the 'expressions of interest' - the parts
-- of the input file that we know how to interpret. 
type ExprParser = FilePath -> IO (Either ParseError [Expr])

-- TextChunkParser returns a 'text view' of the input file - 
-- source to be preserved plus locations of holes to be plugged.
type TextChunkParser = Parser (Seq TextChunk)

  
-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap




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


data Token = Tk_LBr | Tk_RBr 
           | Tk_None
           | Tk1 String 
           | Tk2 String String 
           | Tk3 String String String
  deriving (Eq,Show)

dropToken :: TokenF a
dropToken = \se -> se



beginNest :: TokenF Token
beginNest = \se -> se |> Tk_LBr

endNest :: TokenF Token
endNest = \se -> se |> Tk_RBr

token1 :: (a -> String) -> a -> TokenF Token
token1 f a = \se -> se |> Tk1 (f a)

token2 :: (a -> String) -> (b -> String) -> a -> b -> TokenF Token
token2 f g a b  = \se -> se |> Tk2 (f a) (g b)

token3 :: (a -> String) -> (b -> String) -> (c -> String) 
       -> a -> b -> c -> TokenF Token
token3 f g h a b c  = \se -> se |> Tk3 (f a) (g b) (h c)

--------------------------------------------------------------------------------
-- Rewrite the token stream

data Tree = Node String [Tree]
  deriving (Show)



build :: [Token] -> [Tree]
build = step [] 
  where 
    step acc [] = acc
    step acc xs = let (ts,xs') = build1 xs in step (acc ++ ts) xs'

build1 :: [Token] -> ([Tree], [Token])
build1 (Tk_LBr  :xs)  = let (t1,xs') = build1 xs in ([anode t1], xs')
build1 (Tk_RBr  :xs)  = ([],xs) 
build1 (tok     :xs)  = let (t1,xs') = build1 xs in ([enode tok t1],xs') 
  
-- anonymous node
anode :: [Tree] -> Tree
anode = Node "" 

-- empty node
enode :: Token -> [Tree] -> Tree
enode (Tk1 s)     ts = Node s ts
enode (Tk2 s t)   ts = Node (s ++ " " ++ t) ts
enode (Tk3 s t u) ts = Node (s ++ " " ++ t ++ " " ++ u) ts
enode (Tk_None)   ts = Node "" ts

validNode :: Tree -> Maybe Tree
validNode (Node "" []) = Nothing
validNode (Node "" xs) = case prune xs of 
    []  -> Nothing 
    xs' -> Just (Node "NONE" xs')
    
validNode (Node s  xs) = Just (Node s (prune xs))

prune :: [Tree] -> [Tree]
prune = catMaybes . map validNode 

tree1 :: [Tree] -> Tree
tree1 [a] = a
tree1 xs  = Node "NONE" xs

ppTree :: Tree -> String
ppTree = show . ppt
  where
    ppt (Node s []) = PP.string s PP.<$> PP.empty
    ppt (Node s xs) = PP.string s PP.<$> 
                            PP.indent 2 (PP.braces $ PP.hsep $ map ppt xs)   

rewriteTokenStream :: [Token] -> String
rewriteTokenStream = ppTree . tree1 . prune . build



   
--------------------------------------------------------------------------------
-- Pre-processor helpers 


twoPass :: (FilePath -> IO (Either ParseError String)) 
           -> Parser [Expr] -> FilePath -> IO (Either ParseError [Expr])
twoPass prepro parser filepath = 
    let pp_name  = "post-processed " ++ filepath in 
    prepro filepath >>= either (return . Left) (return . parse parser pp_name)

twoPass_debug :: (FilePath -> IO (Either ParseError String)) 
           -> Parser [Expr] -> FilePath -> IO (Either ParseError [Expr])
twoPass_debug prepro parser filepath = 
    let pp_name  = "post-processed " ++ filepath in runIOInIO $ 
    o1 prepro filepath >>=  
    either (return . Left) (o2 (parse parser pp_name))
  where
    -- don't print expressions here - the are printed in another step
    -- (also note 02 isn't a monadic step as the function parse isn't monadic)
    o2 = genWriteStep  "Expression count..." ppExprCountErr
    o1 = genWriteStepM "After preprocessed step..." ppPreproErr    

ppPreproErr :: Either ParseError String -> PP.Doc
ppPreproErr = either (PP.string . show) PP.string

ppExprCountErr :: Either ParseError [Expr] -> PP.Doc
ppExprCountErr = either (PP.string . show) (PP.int . length)
--------------------------------------------------------------------------------
-- Abc and LilyPond 'expression views' have the same shape but differ
-- in terminal commands 

    
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
term specialTerms = choice specialTerms <|> noneTerm <|> genericTerm 


noneTerm :: Parser Term
noneTerm = Let LetNone <$ symbol "NONE"


genericTerm :: Parser Term
genericTerm = choice [outputDirective, meterPattern_md, partial_md] 


outputDirective :: Parser Term
outputDirective = OutputDirective <$> 
    (directive "output" *> optparse scheme) <*> identifier
  
scheme :: Parser OutputScheme
scheme = choice [relative, dflt]
  where
    relative = OutputRelative <$ cmdsymbol "relative"
    dflt     = OutputDefault  <$ cmdsymbol "default"

meterPattern_md :: Parser Term 
meterPattern_md = Let . LetMeterPattern <$> 
    (directive "meter_pattern" *> meterPattern)

meterPattern :: Parser MeterPattern
meterPattern = (,) <$> sepBy1 int plus <*> (slash *> simpleDuration)
  where 
    plus  = symbol "+"  
    slash = symbol "/"  

simpleDuration :: Parser Duration
simpleDuration = (convRatio . (1%)) <$> int 

-- Abc has no equivalent of 'partial' so we have another
-- meta-directive 
partial_md :: Parser Term
partial_md = Let . LetPartial <$> 
    (directive "partial" *> haskellDuration)

haskellDuration :: Parser Duration
haskellDuration = (\n d scale -> (n%d) * (scale%1)) <$>
    (integer <* symbol "%") <*> integer <*> option 1 (symbol "*" *> integer)
    
cmdsymbol :: String -> Parser String
cmdsymbol = try . symbol . ('\\' :)

directive :: String -> Parser String
directive s = try (symbol $ '#':s) <* lexeme colon

--------------------------------------------------------------------------------
-- Abc and LilyPond 'expression views' have the same shape but differ
-- in terminal commands 


-- Collect water to the left of the output directive.
-- Because both views are interpreted topdown, left-to-right
-- we don't need to store what the output directive says
-- (actually we don't really need is src position either).
-- The last element of sequence should always be (water,Nothing),
-- and the 'inits' should be (water, Just pos). 
-- Water may sometimes be "" (empty).

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


    
--------------------------------------------------------------------------------
-- Utility functions
     
              
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


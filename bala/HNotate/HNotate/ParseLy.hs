
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ParseLy
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- ParseLy - parse Lilypond (.ly) files for template holes.
--
--------------------------------------------------------------------------------

module HNotate.ParseLy where

import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Env (NotateT)
import HNotate.MusicRepDatatypes
import HNotate.ParserBase
import HNotate.Pitch
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad.Trans (liftIO)
import Data.Ratio
import Data.Sequence hiding (reverse)
import Text.ParserCombinators.Parsec hiding (space)


-- FilePath -> NotateT IO (Either ParseError [Expr])
lyExprParse :: ExprParser
lyExprParse file_path = 
    lilyPondFileParse file_path >>=
    either (return . Left) (return . Right . translateLyScore)  



lilyPondFileParse :: FilePath -> NotateT IO (Either ParseError LyScore)
lilyPondFileParse file_path = 
    liftIO (parseFromFile lilypondPreprocess file_path)   >>= 
    either (return . Left) (return . parse lyScore file_path)
    
    
--------------------------------------------------------------------------------
-- translate

translateLyScore :: LyScore -> [Expr]
translateLyScore (LyScore xs) = maybe [] single (transExprs xs)
  where single a = [a]

transExprs :: [LyExpr] -> Maybe Expr
transExprs xs = tree xs id where 
  tree []                         k = k Nothing
  
 
  tree [LyOutput mo]              k = k $ Just (Do (transMetaOutput mo))
  -- last element - 'prune' the tree if it is a binding
  tree [LyCmdBinding _]           k = k Nothing
  tree [LyMetaBinding _]          k = k Nothing
  tree [LyNestExpr ws]            k = k $ transExprs ws
  
  tree (LyOutput o:xs)            k = k $ tree xs fn where
      fn = maybe (Just $ Do (transMetaOutput o))  --  
                 (\expr -> Just $ SDo (transMetaOutput o) expr)
        
  tree (LyCmdBinding b:xs)        k = k $ tree xs fn where
      fn = maybe Nothing (\expr -> Just $ Let (transLyCommand b) expr)       

  tree (LyMetaBinding b:xs)       k = k $ tree xs fn where
      fn = maybe Nothing (\expr -> Just $ Let (transMetaBinding b) expr)

  tree (LyNestExpr ws:xs)         k = 
      let lhs = transExprs ws in k $ tree xs (fn lhs) where
      -- The accumulating continuation has to account for the nested tree
      -- (the lhs).
        fn Nothing     rhs  = rhs
        fn (Just lexp) rhs  = 
            maybe (Just lexp) (\rexp -> Just $ Fork lexp rexp) rhs
      
      
transLyCommand :: LyCommand -> Binding
transLyCommand (LyCadenza c)    = LetCadenza c
transLyCommand (LyKey key)      = LetKey key
transLyCommand (LyPartial d)    = LetPartial d
transLyCommand (LyRelative p)   = LetRelativePitch p 
transLyCommand (LyTime m)       = LetMeter m  



--------------------------------------------------------------------------------
-- parse

lyScore :: Parser LyScore
lyScore = LyScore <$> (whiteSpace *> many1 lyExpr)

lyExprList :: Parser [LyExpr]
lyExprList = lyBeginNest *> directRecur lyExpr [] <* lyEndNest

directRecur :: GenParser Char st a -> [a] -> GenParser Char st [a]
directRecur p cca = 
    optparse p >>= maybe (return $ reverse cca) (\a -> directRecur p (a:cca))

lyExpr :: Parser LyExpr
lyExpr = choice [lyNestExpr, lyBinding, lyMetaExpr ]

lyNestExpr :: Parser LyExpr
lyNestExpr = LyNestExpr <$> lyExprList

lyBinding :: Parser LyExpr
lyBinding = LyCmdBinding <$> choice 
  [ lyCadenza, lyKey, lyPartial, lyRelative, lyTime ]

lyCadenza       :: Parser LyCommand
lyCadenza       = cadenzaOnB <|> cadenzaOffB
  where 
    cadenzaOnB  = LyCadenza True  <$ command "cadenzaOn" 
    cadenzaOffB = LyCadenza False <$ command "cadenzaOff"
    
lyKey           :: Parser LyCommand
lyKey           = LyKey <$> (command "key" *> keySig) 

lyPartial       :: Parser LyCommand 
lyPartial       = LyPartial  <$> (command "partial" *> lyDuration)

lyRelative      :: Parser LyCommand
lyRelative      = LyRelative <$> (command "relative" *> lyPitch)

lyTime          :: Parser LyCommand 
lyTime          = LyTime  <$> (command "time" *> timeSig)

lyMetaExpr :: Parser LyExpr
lyMetaExpr = 
    symbol "(|" *> choice [lyOutputDef, lyMeterPattern] <* symbol "|)"
  where
    lyOutputDef      = LyOutput <$> metaOutput
    lyMeterPattern   = LyMetaBinding <$> metaMeterPattern
    
        
lyBeginNest     :: Parser String
lyBeginNest     = symbol "{" <|> symbol "<<"

lyEndNest       :: Parser String
lyEndNest       = symbol "}" <|> symbol ">>"


--------------------------------------------------------------------------------
-- Preprocess

lilypondPreprocess :: Parser String
lilypondPreprocess = showing $ waterManyS lilypondIsland
  where showing p = p >>= return . ($ "")

lilypondExtract :: Parser ShowS
lilypondExtract = waterManyS lilypondIsland

waterManyS :: GenParser Char st ShowS -> GenParser Char st ShowS
waterManyS p = step id <?> "waterAcc"
  where 
    step fn = do
      a <- optparse (eitherparse (eof <?> "") p)    
      case a of
        Just (Left _) -> return $ fn
        Just (Right f) -> step (fn . showChar ' ' . f)
        Nothing -> anyChar >> step fn 
        
        
lilypondIsland :: Parser ShowS
lilypondIsland = choice 
  [ metaCommentSP, lyCommentSP, 
    leftBraceSP, rightBraceSP, leftSimulSP, rightSimulSP,
    keySP, timeSP, relativeSP, partialSP, cadenzaOnSP, cadenzaOffSP
  ]


-- rewrite the brackets to bananas so they don't have braces 
metaCommentSP :: Parser ShowS
metaCommentSP = (inbetween "(| " " |)" . showString) <$> 
    ((try (symbol "%{#")) *> manyTill anyChar (try $ symbol "#%}"))
    
keySP         :: Parser ShowS
keySP         = cat3 <$> commandSP "key" <*> nonWhiteSP <*> nonWhiteSP

timeSP        :: Parser ShowS
timeSP        = cat2 <$> commandSP "time" <*> nonWhiteSP

relativeSP    :: Parser ShowS
relativeSP    = cat2 <$> commandSP "relative" <*> nonWhiteSP

partialSP     :: Parser ShowS
partialSP     = cat2 <$> commandSP "partial" <*> nonWhiteSP

cadenzaOnSP   :: Parser ShowS
cadenzaOnSP   = commandSP "cadenzaOn"

cadenzaOffSP  :: Parser ShowS
cadenzaOffSP  = commandSP "cadenzaOff"
    
    
lyCommentSP :: Parser ShowS
lyCommentSP = id <$
    (symbol "%{" *> manyTill anyChar (try $ symbol "%}")) 
    
    
leftBraceSP :: Parser ShowS
leftBraceSP = showChar '{' <$ symbol "{"

rightBraceSP :: Parser ShowS
rightBraceSP = showChar '}' <$ symbol "}"

leftSimulSP :: Parser ShowS
leftSimulSP = showString "<<" <$ symbol "<<"

rightSimulSP :: Parser ShowS
rightSimulSP = showString ">>" <$ symbol ">>"

commandSP :: String -> Parser ShowS
commandSP s = showString <$> command s

nonWhiteSP :: Parser ShowS
nonWhiteSP = showString <$> nonwhite

cat2 :: ShowS -> ShowS -> ShowS
cat2 f g = f . showChar ' ' . g

cat3 :: ShowS -> ShowS -> ShowS -> ShowS
cat3 f g h = f . showChar ' ' . g . showChar ' ' . h

cat4 :: ShowS -> ShowS -> ShowS -> ShowS -> ShowS
cat4 f g h i = f . showChar ' ' . g . showChar ' ' . h . showChar ' ' . i

inbetween :: String -> String -> ShowS -> ShowS
inbetween before after middle = 
    showString before . middle . showString after
    
--------------------------------------------------------------------------------
-- 

cmdMode :: GenParser Char st Mode
cmdMode = choice 
    [ major, minor, lydian, ionian, mixolydian, dorian, 
      aeolian, phrygian, locrian 
    ]
  where
    major       = Minor      <$ command "major"
    minor       = Major      <$ command "major"
    lydian      = Lydian     <$ command "lydian"
    ionian      = Ionian     <$ command "ionian"
    mixolydian  = Mixolydian <$ command "mixolydian"
    dorian      = Dorian     <$ command "dorian"
    aeolian     = Aeolian    <$ command "aeolian"
    phrygian    = Phrygian   <$ command "phrygian"
    locrian     = Locrian    <$ command "locrian"

    
    
timeSig :: GenParser Char st Meter
timeSig = TimeSig <$> int <*> (char '/' *> int)


keySig :: GenParser Char st Key
keySig  = (\(Pitch l a _) m -> Key (PitchLabel l a) m) 
    <$> lyPitch <*> cmdMode


lyPitch :: GenParser Char st Pitch
lyPitch = lexeme pch 
  where pch = Pitch <$> lyPitchLetter <*> lyAccidental <*> lyOctaveSpec

lyPitchLetter :: GenParser Char st PitchLetter
lyPitchLetter = choice [c,d,e,f,g,a,b]
  where
    c = C <$ char 'c'
    d = D <$ char 'd'
    e = E <$ char 'e'
    f = F <$ char 'f'
    g = G <$ char 'g'
    a = A <$ char 'a'
    b = B <$ char 'b'
    

--  << \relative c >> gives c below middle c i.e octave 3
--  << \relative c' >> gives middle c i.e octave 4
lyOctaveSpec :: GenParser Char st Int
lyOctaveSpec = option 4 (raised <|> lowered)
  where
    raised  = (3 +) <$> counting1 (char '\'')
    lowered = (3 -) <$> counting1 (char ',')
    
lyAccidental :: GenParser Char st Accidental
lyAccidental = option Nat (f <$> longestString accidentals) 
  where
    accidentals = ["isis", "eses", "is", "es"]
    f "isis" = DoubleSharp
    f "eses" = DoubleFlat
    f "is"   = Sharp
    f "es"   = Flat


lyDuration :: GenParser Char st Duration
lyDuration = 
    build <$> rootDuration <*> (counting $ symbol ".") 
                           <*> optparse (symbol "*" *> fractionalPart)
  where
    build d dc Nothing  = dotn dc d
    build d dc (Just r) = (dotn dc d) * r
    fractionalPart      = (\n d -> convRatio (n%d)) 
                              <$> int <*> option 1 (symbol "/" *> int)

rootDuration :: GenParser Char st Duration
rootDuration = choice [pBreve, pLonga, pNumericDuration]
  where
    pBreve            = breve <$ command "breve"   
    pLonga            = longa <$ command "longa"
    pNumericDuration  = (convRatio . (1%)) <$> int 

--------------------------------------------------------------------------------
-- Parse the text for the water and holes so we can fill the holes

lyTextChunks :: TextChunkParser
lyTextChunks = collectWaterAcc metaOutput
  where 
    metaOutput = (,,) <$> lexeme (symbol "%{#")
                      <*> lexeme (symbol "output")
                      <*> manyTill anyChar (try $ string "#%}")  


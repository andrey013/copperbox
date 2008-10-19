
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ParseAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- ParseAbc - parse Abc files for template holes.
--
--------------------------------------------------------------------------------

module HNotate.ParseAbc where

import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.ParserBase
import HNotate.Pitch
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>), empty)
import Control.Monad (when)
import Data.Char
import Data.List (unfoldr)
import Data.Monoid
import Data.Sequence hiding (reverse)
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (space)

abcExprView_TwoPass :: ExprParser
abcExprView_TwoPass = twoPass preprocessAbc parseAbcExprs

abcExprView_TwoPass_debug :: ExprParser
abcExprView_TwoPass_debug = twoPass_debug preprocessAbc parseAbcExprs

--------------------------------------------------------------------------------
-- Preprocess

preprocessAbc :: FilePath -> IO (Either ParseError String)
preprocessAbc path = either (return . Left) (return . Right . rewriteAbcToks) 
                          =<< (parseFromFile abcExtract path)
  

abcExtract :: Parser (Seq Token)
abcExtract = waterAcc $ choice 
    [ metaComment, abcComment,
      -- fields
      tunenumber, key, meter
      
    ]

rewriteAbcToks :: Seq Token -> String
rewriteAbcToks se = rewriteTokenStream $ unfoldr step (0,se)
  where
    step (i, se) = phi (i, viewl se)
    
    -- exit the unfold
    phi (0, EmptyL)           = Nothing
    
    -- enqueue close braces 
    phi (i, EmptyL)           = unnest i empty
   
    -- enqueue the open brace and increase the nesting level
    phi (i, Tk_LBr :< se)     = Just (Tk_LBr,   (i+1, se))
    
    -- 'X' is new tune -- close all braces then enqueue 'X' 
    phi (i, (Tk2 "X:" _) :< se)
          | i > 0             = unnest i se
          | otherwise         = Just (Tk_None,   (i, se))
    
    -- normal case - produce value and go next
    phi (i, e :< se)          = Just (e,         (i, se)) 
    
       
    -- i is guaranteed (>1)
    -- Produce 1 closeBrace, and enqueue any further closeBrace's
    -- into the token stream
    unnest :: Int -> Seq Token -> Maybe (Token, (Int,Seq Token))
    unnest 1 se = Just (Tk_RBr, (0, se))
    unnest i se = Just (Tk_RBr, (0, addleft (replicate (i-1) Tk_RBr) se))
    
    addleft :: [a] -> Seq a -> Seq a
    addleft xs = ((fromList xs) ><) 


metaComment :: Parser (TokenF Token)
metaComment = token1 fn <$> 
    ((try $ symbol "%#") *> manyTill anyChar lineEnding)
  where
    -- prefix with a hash  
    fn s = '#' : s
                
             
abcComment :: Parser (TokenF Token)
abcComment = dropToken <$ 
    (symbol "%{" *> manyTill anyChar lineEnding) 

tunenumber  :: Parser(TokenF Token)
tunenumber  = dyap beginNest (token2 id show) <$> 
    fieldsymbol 'X' <*> int
    
key         :: Parser(TokenF Token)
key         = dyap beginNest (token2 id id)  <$>
    fieldsymbol 'K' <*> restOfLine
    
meter       :: Parser(TokenF Token)
meter       = dyap beginNest (token2 id id)  <$>   
    fieldsymbol 'M' <*> restOfLine
    
    
fieldsymbol :: Char -> Parser String
fieldsymbol c = try $ symbol [c,':']

restOfLine :: GenParser Char st String
restOfLine = manyTill anyChar lineEnding

lineEnding :: GenParser Char st ()
lineEnding = choice [ () <$ newline, eof] 



-- | Dyadic apply \/ compose - apply the binary function g to a and b, 
-- then apply the unary function f to the result.
-- dyap :: a -> b -> (c -> d) -> (a -> b -> c) ->  ->  d
dyap :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
dyap f g a b se = f (g a b se) 


--------------------------------------------------------------------------------
-- Parse

parseAbcExprs :: Parser [Expr]
parseAbcExprs = topLevelExprs abcTermParsers

abcTermParsers :: [Parser Term]
abcTermParsers = [numberT, keyT, timeT]

numberT :: Parser Term
numberT = (Let LetNone) <$ (fieldsymbol 'X' *> int)

keyT :: Parser Term
keyT = Let . LetKey     <$> (fieldsymbol 'K' *> keySig)

timeT :: Parser Term 
timeT = Let . LetMeter  <$> (fieldsymbol 'M' *> timeSig)
     <?> "timeT"


--------------------------------------------------------------------------------
-- Parse the text for the water and holes so we can fill the holes

abcTextChunks :: TextChunkParser
abcTextChunks = collectWaterAcc (metaOutput)
  where 
    metaOutput = (,,) <$> lexeme (symbol "%#") 
                      <*> lexeme (symbol "output")
                      <*> uptoNewline ""                    
    
    -- Important - must not consume the newline
    -- Use direct recursion as 'many' parser can't be used 
    -- (empty string restriction). 
    uptoNewline cca = do 
        at_end <- option False (True <$ eof) 
        case at_end of
          True -> return (reverse cca)
          False -> optparse (satisfy (/='\n')) >>=
                   maybe (return $ reverse cca) (\c -> uptoNewline (c:cca))  

                   
    



timeSig :: GenParser Char st Meter
timeSig = TimeSig <$> int <*> (char '/' *> int)


-- TODO accidentals 
-- see ABC 2.0 spec - Klezmer (Ahavoh Rabboh) / Arabic music (Maqam Hedjaz)
keySig :: GenParser Char st Key
keySig = (\(Pitch l a _) m -> Key (PitchLabel l a) m) 
    <$> lexeme abcPitch <*> option Major abcMode


abcDuration :: GenParser Char st Duration
abcDuration = (\n d -> convRatio (n%d)) <$> int <*> (char '/' *> int)


abcPitch :: GenParser Char st Pitch
abcPitch = build <$> abcAccidental <*> basenote <*> octaveDiff
  where build a (i,nt) od  = Pitch nt a (i + od)



-- C is middle C (C4)
-- c is one octve above (C5)
-- C, is (C3)
-- c' is (C5)

-- c, or C' shouldn't occur in Abc files but will be normalized anyway


basenote :: GenParser Char st (Int,PitchLetter)
basenote = build <$> satisfy ((flip elem) "CDEFGABcdefgab")
  where build ch        = (octave ch, letter $ toUpper ch)
        octave ch       = if isUpper ch then 4 else 5
        
        letter 'C'      = C;   letter 'D'       = D;
        letter 'E'      = E;   letter 'F'       = F;
        letter 'G'      = G;   letter 'A'       = A;
        letter 'B'      = B;   letter _         = error "bassenote"

abcAccidental :: GenParser Char st Accidental
abcAccidental = option Nat (choice [dblSharp, sharp, dblFlat, flat, natural])
  where dblSharp = DoubleSharp <$ string "^^" 
        sharp    = Sharp       <$ string "^"  
        dblFlat  = DoubleFlat  <$ string "__" 
        flat     = Flat        <$ string "_"  
        natural  = Nat         <$ string "="
        
octaveDiff :: GenParser Char st Int    
octaveDiff = option 0 (choice [octaveLow, octaveHigh])
  where octaveLow   = negate  <$> counting1 (char ',')
        octaveHigh  = counting1 (char '\'')
        
        
abcMode :: GenParser Char st Mode
abcMode = choice 
    [ major, lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian, 
      minor {- minor must be last - to respect longest match -} 
    ]
  where minor       = Minor      <$ choice 
              [leading3 'm' 'i' 'n', "M" <$ charDrop 'M', "m" <$ charDrop 'm']
        major       = Major      <$ leading3 'm' 'a' 'j'
        lydian      = Lydian     <$ leading3 'l' 'y' 'd'
        ionian      = Ionian     <$ leading3 'i' 'o' 'n'
        mixolydian  = Mixolydian <$ leading3 'm' 'i' 'x'
        dorian      = Dorian     <$ leading3 'd' 'o' 'r'
        aeolian     = Aeolian    <$ leading3 'a' 'e' 'o'
        phrygian    = Phrygian   <$ leading3 'p' 'h' 'r'
        locrian     = Locrian    <$ leading3 'l' 'o' 'c' 

    


--------------------------------------------------------------------------------
-- utility parsers  

-- Abc looks only at the first 3 characters of a mode specification     
leading3 :: Char -> Char -> Char -> GenParser Char st String   
leading3 a b c = lexeme $ try (caten <$> p a <*> p b <*> p c <*> many letter)
  where caten a b c xs = a:b:c:xs
        p a = choice [char a, char $ toUpper a] 
        
        
                
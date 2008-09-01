
--------------------------------------------------------------------------------
-- |
-- Module      :  ParseAbc
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

module ParseAbc where

import Duration
import ExtractionDatatypes
import ParserBase
import Pitch

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad (when)
import Data.Char
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (space)


-- meter e.g. M:6/8

-- unit note length e.g. L:1/4

-- key e.g. K:C major


-- pretending that Abc files are nested makes things a bit ugly


parseAbcSrc :: Parser [SrcExpr]
parseAbcSrc = do 
    try (water beginNested)
    -- can' use `many` with a parser that uses `waterMaybe`
    xs <- manyEOF (nestedk []) [] 
    return xs

manyEOF p acc = do
  at_end <- option False (True <$ eof)
  if at_end then return (reverse acc) else (p >>= \a -> manyEOF p (a:acc))

  
nestedk :: [SrcExpr] -> Parser SrcExpr
nestedk acc = do
    elt <- waterMaybe (eitherparse endNested 
                                   (choice [abcCommand, abcDirective]))
    case elt of
      Nothing -> return $ Nested $ reverse acc
      Just (Left _) -> return $ Nested $ reverse acc      
      Just (Right a)  -> nestedk (a:acc)
  <|> fail "unterminated nesting"  


beginNested       :: Parser Nested
beginNested       = NestStart <$ field 'X' (manyTill anyChar (try newline))


-- end nest uses 'X' field like begin nest
endNested         :: Parser Nested
endNested         = NestEnd <$ field 'X' int


abcCommand :: Parser SrcExpr
abcCommand = Commmand <$> choice [cmdmeter, cmdkey, cmd_default_note_length]  
  <?> "abcCommand"

abcDirective :: Parser SrcExpr
abcDirective = Directive 
    <$> (metaCommentStart *> manyTill anyChar (try metaCommentEnd)) 
  <?> "abcDirective"
    
metaCommentStart :: CharParser st String
metaCommentStart = lexeme $ string "%#"

metaCommentEnd :: CharParser st String 
metaCommentEnd = choice ["\n" <$ newline, "\n" <$ eof] 

cmdmeter :: Parser Command
cmdmeter = CmdMeter <$> field 'M' timeSig 

timeSig :: Parser Meter
timeSig = TimeSig <$> int <*> (char '/' *> int)

cmdkey :: Parser Command
cmdkey = CmdKey <$> field 'K' keySig

keySig :: Parser Key
keySig = Key <$> abcPitch <*> abcMode


cmd_default_note_length :: Parser Command
cmd_default_note_length = CmdDefaultNoteLength <$> field 'L' abcDuration

abcDuration :: Parser Duration
abcDuration = (\n d -> Duration (n%d) 0) <$> int <*> (char '/' *> int)


abcPitch :: Parser Pitch
abcPitch = build <$> abcAccidental <*> basenote <*> octaveDiff
  where build a (i,nt) od  = Pitch nt a (i + od)



-- C is middle C (C4)
-- c is one octve above (C5)
-- C, is (C3)
-- c' is (C5)

-- c, or C' shouldn't occur in Abc files but will be normalized anyway


basenote :: Parser (Int,PitchLetter)
basenote = build <$> satisfy ((flip elem) "CDEFGABcdefgab")
  where build ch        = (octave ch, letter $ toUpper ch)
        octave ch       = if isUpper ch then 4 else 5
        
        letter 'C'      = C;   letter 'D'       = D;
        letter 'E'      = E;   letter 'F'       = F;
        letter 'G'      = G;   letter 'A'       = A;
        letter 'B'      = B;   letter _         = error "bassenote"

abcAccidental :: Parser Accidental
abcAccidental = option Nat (choice [dblSharp, sharp, dblFlat, flat, natural])
  where dblSharp = DoubleSharp <$ string "^^" 
        sharp    = Sharp       <$ string "^"  
        dblFlat  = DoubleFlat  <$ string "__" 
        flat     = Flat        <$ string "_"  
        natural  = Nat         <$ string "="
        
octaveDiff :: Parser Int    
octaveDiff = option 0 (choice [octaveLow, octaveHigh])
  where octaveLow   = negate  <$> counting1 (char ',')
        octaveHigh  = counting1 (char '\'')
        
        
abcMode :: Parser Mode
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

field :: Char -> Parser a -> Parser a
field ch p = symbol [ch,':'] *> p          

endOfLine :: Parser ()
endOfLine = () <$ white *> optional trailingComment *> linefeed
  where white           = many $ satisfy (\c -> c == ' ' || c == '\t')
        trailingComment = char '%' *> textual

linefeed :: Parser ()     
linefeed = () <$ choice [char '\r',  char '\n']


-- Abc looks only at the first 3 characters of a mode specification     
leading3 :: Char -> Char -> Char -> Parser String   
leading3 a b c = lexeme $ try (caten <$> p a <*> p b <*> p c <*> many letter)
  where caten a b c xs = a:b:c:xs
        p a = choice [char a, char $ toUpper a] 
        
        
textual :: Parser [Char]
textual = many1 (satisfy isTextChar)
  where isTextChar :: Char -> Bool
        isTextChar c = isAlpha c || isDigit c || isSpecial c
        
        isSpecial c  = c `elem`  " \t\"!#$&'()*+,-./:;<=>?@[\\]^_`{|}~"
                
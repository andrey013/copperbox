
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

import HNotate.Duration
import HNotate.MusicRepDatatypes
import HNotate.ParserBase
import HNotate.Pitch
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad (when)
import Data.Char
import Data.Monoid
import Data.Sequence ( (|>) )
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (space)



abcPIV :: FilePath -> IO (Either ParseError PIV)
abcPIV filepath = parseFromFileState interpretableView filepath 0


abcSPV :: FilePath -> IO (Either ParseError SPV)
abcSPV filepath = parseFromFileState textView filepath 0

textView :: StParser SPV
textView = SPV <$> collect mempty
  where
    collect se = do 
      txt    <- upTo (eitherparse eof (lexeme metaCommentStart))
      at_end <- option False (True <$ eof)
      case at_end of
        True -> return $ se `add` txt
        False -> do { mark <- metamarkK
                    ; collect $ (se `add` txt) |> mark }  
    
    add se "" = se
    add se ss = se |> (SourceText ss)   

-- Don't look for a terminator in Abc the lexeme parser over metadirective
-- will have consumed the line ending.
metamarkK = MetaMark 
    <$> incrCount <*> sourcePosition <*> metadirective 


-- meter e.g. M:6/8

-- unit note length e.g. L:1/4

-- key e.g. K:C major


-- pretending that Abc files are nested makes things a bit ugly


interpretableView :: StParser PIV
interpretableView = do 
    try (water beginNested)
    -- can' use `many` with a parser that uses `waterMaybe`
    xs <- manyEOF (nestedk []) [] 
    return $ PIV xs

manyEOF p acc = do
  at_end <- option False (True <$ eof)
  if at_end then return (reverse acc) else (p >>= \a -> manyEOF p (a:acc))

  
nestedk :: [ScoreElement] -> StParser ScoreElement
nestedk acc = do
    elt <- waterMaybe (eitherparse endNested 
                                   (choice [abcCommand, abcDirective]))
    case elt of
      Nothing -> return $ Nested $ reverse acc
      Just (Left _) -> return $ Nested $ reverse acc      
      Just (Right a)  -> nestedk (a:acc)
  <|> fail "unterminated nesting"  


beginNested       :: StParser Nested
beginNested       = NestStart <$ field 'X' (manyTill anyChar (try newline))


-- end nest uses 'X' field like begin nest
endNested         :: StParser Nested
endNested         = NestEnd <$ field 'X' int


abcCommand :: StParser ScoreElement
abcCommand = Command <$> choice 
  [cmdMeter, cmdKey, cmdUnitNoteLength]  
  <?> "abcCommand"

abcDirective :: StParser ScoreElement
abcDirective = Directive 
    <$> incrCount <*> (metaCommentStart *> metadirective) <* metaCommentEnd


metadirective :: StParser MetaDirective
metadirective = metaoutput

metaoutput :: StParser MetaDirective
metaoutput = MetaOutput <$> (identifier <* colon) <*> tightident


        
            
metaCommentStart :: CharParser st String
metaCommentStart = lexeme $ string "%#"

metaCommentEnd :: CharParser st String 
metaCommentEnd = choice ["\n" <$ newline, "\n" <$ eof] 


-- Meter may also indicate no meter 'M:none'
-- (equivalent to LilyPond's Cadenza On)
cmdMeter :: StParser Command
cmdMeter = mkMeter <$> field 'M' (eitherparse timeSig (symbol "none"))
  where mkMeter (Left tm) = CmdMeter tm
        mkMeter (Right _) = CmdCadenzaOn 
cmdKey :: StParser Command
cmdKey = CmdKey <$> field 'K' keySig

cmdUnitNoteLength :: StParser Command
cmdUnitNoteLength = CmdUnitNoteLength <$> field 'L' abcDuration


timeSig :: StParser Meter
timeSig = TimeSig <$> int <*> (char '/' *> int)

keySig :: StParser Key
keySig = Key <$> abcPitch <*> abcMode




abcDuration :: StParser Duration
abcDuration = (\n d -> Duration (n%d) 0) <$> int <*> (char '/' *> int)


abcPitch :: StParser Pitch
abcPitch = build <$> abcAccidental <*> basenote <*> octaveDiff
  where build a (i,nt) od  = Pitch nt a (i + od)



-- C is middle C (C4)
-- c is one octve above (C5)
-- C, is (C3)
-- c' is (C5)

-- c, or C' shouldn't occur in Abc files but will be normalized anyway


basenote :: StParser (Int,PitchLetter)
basenote = build <$> satisfy ((flip elem) "CDEFGABcdefgab")
  where build ch        = (octave ch, letter $ toUpper ch)
        octave ch       = if isUpper ch then 4 else 5
        
        letter 'C'      = C;   letter 'D'       = D;
        letter 'E'      = E;   letter 'F'       = F;
        letter 'G'      = G;   letter 'A'       = A;
        letter 'B'      = B;   letter _         = error "bassenote"

abcAccidental :: StParser Accidental
abcAccidental = option Nat (choice [dblSharp, sharp, dblFlat, flat, natural])
  where dblSharp = DoubleSharp <$ string "^^" 
        sharp    = Sharp       <$ string "^"  
        dblFlat  = DoubleFlat  <$ string "__" 
        flat     = Flat        <$ string "_"  
        natural  = Nat         <$ string "="
        
octaveDiff :: StParser Int    
octaveDiff = option 0 (choice [octaveLow, octaveHigh])
  where octaveLow   = negate  <$> counting1 (char ',')
        octaveHigh  = counting1 (char '\'')
        
        
abcMode :: StParser Mode
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

field :: Char -> StParser a -> StParser a
field ch p = symbol [ch,':'] *> p          

endOfLine :: StParser ()
endOfLine = () <$ white *> optional trailingComment *> linefeed
  where white           = many $ satisfy (\c -> c == ' ' || c == '\t')
        trailingComment = char '%' *> textual

linefeed :: StParser ()     
linefeed = () <$ choice [char '\r',  char '\n']


-- Abc looks only at the first 3 characters of a mode specification     
leading3 :: Char -> Char -> Char -> StParser String   
leading3 a b c = lexeme $ try (caten <$> p a <*> p b <*> p c <*> many letter)
  where caten a b c xs = a:b:c:xs
        p a = choice [char a, char $ toUpper a] 
        
        
textual :: StParser [Char]
textual = many1 (satisfy isTextChar)
  where isTextChar :: Char -> Bool
        isTextChar c = isAlpha c || isDigit c || isSpecial c
        
        isSpecial c  = c `elem`  " \t\"!#$&'()*+,-./:;<=>?@[\\]^_`{|}~"
                
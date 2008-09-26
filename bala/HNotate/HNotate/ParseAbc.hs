
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
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.ParserBase
import HNotate.Pitch
import HNotate.PreprocessTemplate (abcPrePro)
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad (when)
import Data.Char
import Data.Monoid
import Data.Sequence ( (|>) )
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (space)


abcTextualView :: FilePath -> IO (Either ParseError TextualView)
abcTextualView path = parseFromFileState (textView start end) path 0
  where
    start = lexeme $ string "%#"
    end   = choice [() <$ newline, () <$ eof] 


abcExprView_TwoPass :: FilePath -> IO (Either ParseError ExprView)
abcExprView_TwoPass = twoPass abcPrePro abcExprView
                
abcExprView :: StParser ExprView
abcExprView = exprView updateWithAbcCommands

--


updateWithAbcCommands :: StParser (Env -> Env)
updateWithAbcCommands = choice $
  [cmdTuneNumber, cmdMeter, cmdKey, cmdUnitNoteLength]  


-- meter e.g. M:6/8

-- unit note length e.g. L:1/4

-- key e.g. K:C major


-- 

cmdTuneNumber :: StParser (Env -> Env)
cmdTuneNumber = id <$ field 'X' int

-- Meter may also indicate no meter 'M:none'
-- (equivalent to LilyPond's Cadenza On)
cmdMeter :: StParser (Env -> Env)
cmdMeter = mkMeter <$> field 'M' (eitherparse timeSig (symbol "none"))
  where mkMeter (Left tm) = set_current_meter tm
        mkMeter (Right _) = set_cadenza True
        
         
cmdKey :: StParser (Env -> Env)
cmdKey = set_current_key <$> field 'K' keySig

cmdUnitNoteLength :: StParser  (Env -> Env)
cmdUnitNoteLength = set_unit_note_length <$> field 'L' abcDuration


timeSig :: StParser Meter
timeSig = TimeSig <$> int <*> (char '/' *> int)

keySig :: StParser Key
keySig = Key <$> lexeme abcPitch <*> option Major abcMode




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

-- | command - note using try is essential to consume the whole command
-- without it we may consume a blacklash of a different command and not be 
-- able to backtrack. 
command     :: String -> CharParser st String
command ss  = lexeme $ try $ string ('\\':ss)

startMeta   :: CharParser st String
-- startMeta   = symbol "<<<"
startMeta   = symbol "%{#" 
    
endMeta     :: CharParser st String 
-- endMeta     = symbol ">>>"
endMeta     = symbol "#%}" 

startNested       :: StParser ()
startNested       = () <$ symbol "{"

endNested         :: StParser ()
endNested         = () <$ symbol "}"

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
                
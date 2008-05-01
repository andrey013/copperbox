--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.LilyPond.Parser
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Parser for a subset of LilyPond files
--
--------------------------------------------------------------------------------

module Bala.Format.LilyPond.Parser where

import Bala.Base.BaseExtra
import Bala.Format.LilyPond.Datatypes

import Prelude hiding (break)

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad

import Text.ParserCombinators.Parsec hiding (space)

-- note need to be careful with lexeme parser 

-- permutations?

parseLilyPond :: Parser LilyPondFile
parseLilyPond = many $ choice [version,header]


glyphs :: Parser Glyphs
glyphs = Glyphs <$> many1 (lexeme glyph)

glyph :: Parser Glyph
glyph = choice [ GlyphEvent <$> glyphEvent, GlyphMark <$> mark, GlyphCommmand <$> glyphCommand]



glyphCommand = choice 
  [ bar, crescendoStart, decrescendoStart,  crescendoEnd, break, noBreak
  ]


glyphEvent :: Parser GlyphEvent
glyphEvent = choice [ GEvtNote <$> note, GEvtChord <$> chord, GEvtRest <$> rest]
                
                    
note :: Parser Note
note = Note <$> pitch <*> optparse duration

rest :: Parser Rest
rest = Rest <$> (char 'r' *> duration)

duration :: Parser Duration
duration = f <$> int <*> optparse (char '.')
  where 
    f i Nothing  = Duration i
    f i (Just _) = Dotted i
  

-- expression :: Parser String
-- expression = bracedWater

pitch :: Parser Pitch
pitch = Pitch <$> pitchLetter <*> optparse accidental <*> optparse octaveSpec

-- | temporary expedience
pitchstring :: Parser String
pitchstring = show <$> pitch


octaveSpec :: Parser OctaveSpec
octaveSpec = raised <|> lowered
  where
    raised  = Raised  <$> counting1 (char '\'')
    lowered = Lowered <$> counting1 (char ',')
    
accidental :: Parser Accidental
accidental = f <$> longestString accidentals
  where
    
    accidentals = [ "isis", "eses", "is", "es" ]
    f "isis" = DoubleSharp
    f "eses" = DoubleFlat
    f "is"   = Sharp
    f "es"   = Flat


articulation :: Parser Articulation 
articulation = f <$> verticalPlacement <*>  eitherparse amark int
  where 
    amark = oneOf ".->^+_"
    
    f a (Left ch) = Articulation a ch
    f a (Right i) = Fingering a i 


verticalPlacement :: Parser VerticalPlacement
verticalPlacement = choice $ map (uncurry f)  [('^',VAbove), ('_',VBelow), ('-',VDefault)] 
  where
    f ch constr = constr <$ char ch


    
microTone :: Parser MicroTone
microTone = choice $ map (uncurry f) [("ih",HalfSharp), ("eh", HalfFlat)] 
  where
    f str constr = constr <$ lexeme (string str)  
    
stringMark :: Parser String
stringMark = char '^' *> stringLiteral

chord :: Parser Chord
chord = Chord <$> angles (many1 $ lexeme pitch) <*> optparse duration




--------------------------------------------------------------------------------
-- marks
--------------------------------------------------------------------------------

mark :: Parser Mark
mark = choice 
  [ MarkTie <$ tie, MarkSlur <$> slur, MarkBeam <$> beam, barCheck ]


-- Careful do not consume the placement prefix if it isn't followed by a slur
slur :: Parser Slur
slur = choice [(try placedSlur), slurStart, slurEnd]
  where 
    placedSlur = (SlurStart . Just) <$> (verticalPlacement <* symbol "(")
    slurStart  = SlurStart Nothing  <$  symbol "("
    slurEnd    = SlurEnd            <$  symbol ")"
    
beam :: Parser Beam
beam = choice [beamStart, beamEnd]
  where 
    beamStart  = BeamStart  <$  symbol "["
    beamEnd    = BeamEnd    <$  symbol "]"

barCheck :: Parser Mark
barCheck = MarkBarCheck <$ symbol "|"
        
--------------------------------------------------------------------------------
-- commands
--------------------------------------------------------------------------------



command0 :: (Command) -> String -> Parser Command
command0 f name = f <$ command name

nullaryCommand :: String -> Parser Command
nullaryCommand name = command0 (NullaryCommand name) name

command1 :: (a -> Command) -> String -> Parser a -> Parser Command
command1 f name p = f <$> (command name *> lexeme p)

exprCommand :: String -> Parser Command
exprCommand name = command1 (ExprCommand name) name bracedWater

unaryCommand :: String -> Parser String -> Parser Command
unaryCommand name = command1 (UnaryCommand name) name

command2 :: (a -> b -> Command) -> String -> Parser a -> Parser b -> Parser Command
command2 f name p p' = f <$> (command name *> lexeme p) <*> lexeme p'

binaryCommand :: String -> Parser String -> Parser String -> Parser Command
binaryCommand name = command2 (BinaryCommand name) name 


new :: Parser NewDecl -> Parser Command
new = command1 CmdNew "new" 

break :: Parser Command
break = nullaryCommand "break" 

noBreak :: Parser Command
noBreak = nullaryCommand "noBreak" 


bar :: Parser Command
bar = unaryCommand "bar" (doubleQuoted barMark)


version :: Parser Command
version = command1 CmdVersion "version" (doubleQuoted deweyNumber)
  where deweyNumber = sepBy1 int (char '.')

header :: Parser Command
header = exprCommand "header" 


key :: Parser Command
key = command2 CmdKey "key" pitch (keytype <|> mode)
 

keytype :: Parser Command
keytype = nullaryCommand "major" <|> nullaryCommand "minor"

mode :: Parser Command
mode = choice $ map nullaryCommand [ "ionian", "locrian",
          "aeolian", "mixolydian", "lydian", "phrygian", "dorian" ]
  
        
clef :: Parser Command
clef = unaryCommand "clef" clefname 
  where
    clefname = longestString clefs <|> quotedclef
    
    quotedclef = doubleQuoted $ many1 (alphaNum <|> oneOf "_^")
     
    clefs = ["treble", "violin", "G", "G2",
             "alto", "C", "tenor", "bass", "F",            
             "french", "soprano", "mezzosoprano",
             "baritone", "varbaritone", "subbass",
             "percussion", "tab"
             ]

dynamic :: Parser Command
dynamic = NullaryCommand <$> (longestString dynamic_mark)  
  where
    dynamic_mark = ["ppppp", "pppp", "ppp", "pp", "p", 
                    "mp", "mf", "f", "ff", "fff", "ffff", 
                    "fp", "sf", "sff", "sp", "spp", "sfz", "rfz"
                    ]

espressivo :: Parser Command
espressivo = nullaryCommand "espressivo"

breath :: Parser Command
breath = nullaryCommand "breath"

glissando :: Parser Command
glissando = nullaryCommand "glissando"

arpeggio :: Parser Command
arpeggio = nullaryCommand "arpeggio"

arpeggioBracket :: Parser Command
arpeggioBracket = nullaryCommand "arpeggioBracket"

arpeggioUp :: Parser Command
arpeggioUp = nullaryCommand "arpeggioUp"

arpeggioDown :: Parser Command
arpeggioDown = nullaryCommand "arpeggioDown"

arpeggioNeutral :: Parser Command
arpeggioNeutral = nullaryCommand "arpeggioNeutral"

 
time :: Parser Command
time = command1 (uncurry CmdTimeSignature) "time" fraction


partial :: Parser Command
partial = unaryCommand "partial" (show <$> int)

italianGrace :: Parser Command
italianGrace =  nullaryCommand "appoggiatura" 
            <|> nullaryCommand "acciaccatura"

tempo :: Parser Command 
tempo = command2 CmdTempo "tempo" (duration) ((symbol "=") *> int)


crescendoStart :: Parser Command
crescendoStart = nullaryCommand "<"

decrescendoStart :: Parser Command
decrescendoStart = nullaryCommand ">"

crescendoEnd :: Parser Command
crescendoEnd = nullaryCommand "!"

phrasingSlurStart :: Parser Command
phrasingSlurStart = nullaryCommand "("

phrasingSlurEnd :: Parser Command
phrasingSlurEnd = nullaryCommand ")"

chordmode :: Parser Command
chordmode = command1 CmdChordmode "chordmode" (braces $ many1 chordname)

--------------------------------------------------------------------------------
-- Chordmode
--------------------------------------------------------------------------------

chordname :: Parser Chordname
chordname = Chordname <$> note <*> optparse (colon *> chordSuffix)


chordSuffix :: Parser ChordSuffix
chordSuffix = ChordSuffix <$> optparse altint <*> chordModifier <*> chordSteps

altint = (,) <$> int <*> optparse chordAlt


chordModifier :: Parser ChordModifier
chordModifier = option CModM (choice [minor])
  where      
    minor = CModMinor <$ char 'm'


chordSteps :: Parser [ChordStep]
chordSteps = option [] (dot *> sepBy chordStep dot)
 
chordStep :: Parser ChordStep 
chordStep = choice [cstepAdd, cstepRemove ]
  where 
    cstepAdd    = CStepAdd <$> int <*> optparse chordAlt
    cstepRemove = CStepRemove <$> int
    
chordAlt :: Parser ChordAlt 
chordAlt = choice [ CAltPlus <$ char '+', CAltMinus <$ char '-']

--------------------------------------------------------------------------------
-- Guitar 
--------------------------------------------------------------------------------

cmdNewTabStaff :: Parser Command
cmdNewTabStaff = new newTabStaff

newTabStaff = NewTabStaff <$>  (litTabStaff *> chexpr)
  where 
    litTabStaff  = lexeme $ string "TabStaff"
    chexpr       = braces $ many1 (lexeme guitarNote)
    
guitarNote :: Parser GuitarNote
guitarNote = GuitarNote <$> note <*> guitarString

guitarString :: Parser Int
guitarString = char '\\' *> int

--------------------------------------------------------------------------------
-- vocal music
--------------------------------------------------------------------------------

cmdAddlyrics :: Parser Command
cmdAddlyrics = command1 CmdAddlyrics "addlyrics" bracedWater

{-
A word in Lyrics mode begins with: an alphabetic character, _, ?, !, :, ', the control 
characters ^A through ^F, ^Q through ^W, ^Y, ^^, any 8-bit character with ASCII code over 127, 
or a two-character combination of a backslash followed by one of Ô, ', ", or ^.\
-}

lyricWord = (:) <$> choice [letter, oneOf lyric_special] <*> many letter
  where
    lyric_special = "_?!:'"

lyricContent = choice [syllable, centeredHyphen, extenderLine]    
  where 
    syllable       = Syllable       <$> many1 letter  
                                    <*> syllableCont 
                                    <*> optparse int
    centeredHyphen = CenteredHyphen <$ symbol "--"
    extenderLine   = ExtenderLine   <$ symbol "__"

syllableCont :: Parser SyllableCont      
syllableCont = option NoSyllableCont (SyllableCont <$ symbol "-")


cmdMelisma :: Parser Command
cmdMelisma = nullaryCommand "melisma" 

cmdMelismaEnd :: Parser Command
cmdMelismaEnd = nullaryCommand "melismaEnd" 

--------------------------------------------------------------------------------
-- percussion
--------------------------------------------------------------------------------

cmdDrums :: Parser Command
cmdDrums = command1 CmdDrums "drums" p
  where
    p = expression (many1 $ lexeme drumnote)

drumnote :: Parser Drumnote
drumnote = Drumnote <$> drumname <*> optparse duration

drumname :: Parser String
drumname = longestString
  [ "bda",    "acousticbassdrum"
  , "bd",     "bassdrum"
  , "ssh",    "hisidestick"
  , "ss",     "sidestick"
  , "ssl",    "losidestick"
  , "sna",    "acousticsnare"
  , "sn",     "snare"
  , "hc",     "handclap"
  , "sne",    "electricsnare"
  , "tomfl",  "lowfloortom"
  , "hhc",    "closedhihat"
  , "hh",     "hihat"
  , "tomfh",  "highfloortom"
  , "hhp",    "pedalhihat"
  , "toml",   "lowtom"
  , "hho",    "openhihat"
  , "hhho",   "halfopenhihat"
  , "tomml",  "lowmidtom"
  , "tommh",  "himidtom"
  , "cymca",  "crashcymbala"
  , "cymc",   "crashcymbal"
  , "tomh",   "hightom"
  , "cymra",  "ridecymbala"
  , "cymr",   "ridecymbal"
  , "cymch",  "chinesecymbal"
  , "rb",     "ridebell"
  , "tamb",   "tambourine"
  , "cyms",   "splashcymbal"
  , "cb",     "cowbell"
  , "cymcb",  "crashcymbalb"
  , "vibs",   "vibraslap"
  , "cymrb",  "ridecymbalb"
  , "bohm",   "mutehibongo"
  , "boh",    "hibongo"
  , "boho",   "openhibongo"
  , "bolm",   "mutelobongo"
  , "bol",    "lobongo"
  , "bolo",   "openlobongo"
  , "cghm",   "mutehiconga"
  , "cglm",   "muteloconga"
  , "cgho",   "openhiconga"
  , "cgh",    "hiconga"
  , "cglo",   "openloconga"
  , "cgl",    "loconga"
  , "timh",   "hitimbale"
  , "timl",   "lotimbale"
  , "agh",    "hiagogo"
  , "agl",    "loagogo"
  , "cab",    "cabasa"
  , "mar",    "maracas"
  , "whs",    "shortwhistle"
  , "whl",    "longwhistle"
  , "guis",   "shortguiro"
  , "guil",   "longguiro"
  , "gui",    "guiro"
  , "cl",     "claves"
  , "wbh",    "hiwoodblock"
  , "wbl",    "lowoodblock"
  , "cuim",   "mutecuica"
  , "cuio",   "opencuica"
  , "trim",   "mutetriangle"
  , "tri",    "triangle"
  , "trio",   "opentriangle"
  , "tt",     "tamtam"
  , "ua",     "oneup"
  , "ub",     "twoup"
  , "uc",     "threeup"
  , "ud",     "fourup"
  , "ue",     "fiveup"
  , "da",     "onedown"
  , "db",     "twodown"
  , "dc",     "threedown"
  , "dd",     "fourdown"
  , "de",     "fivedown"
  ] 
   
--------------------------------------------------------------------------------
-- other
--------------------------------------------------------------------------------

expression :: Parser a -> Parser a
expression = between (lexeme braceOpen) (lexeme braceClose) 
         
bracedWater :: Parser String
bracedWater = fst <$> (braceOpen *> collectWater braceClose)

fraction :: Parser (Int,Int)
fraction = (,) <$> (int <* char '/') <*> int

barMark :: Parser String
barMark = longestString
        [ "unbroken ||:", "broken ||:"
        , ".|.", ":|:", "||", "|.", ".|", "|:", ":|", "|" , ":"] 
        
comment :: Parser ()
comment = char '%' *> text *> lineEnd
  where lineEnd = () <$ choice [char '\r',  char '\n']
        text    = noneOf "\n\r"


doubleAngles :: Parser a -> Parser a
doubleAngles = between (symbol "<<") (symbol ">>")  
        
--------------------------------------------------------------------------------
-- lexer combinators 
--------------------------------------------------------------------------------

-- | command - note using try is essential otherwise we might the forward
-- slash will be consumed 
command :: String -> CharParser st String
command ss = lexeme $ try $ string ('\\':ss)


-- | e.g "Voice"
context :: CharParser st String
context = (:) <$> upper <*> many (upper <|> lower) 
        

tie :: CharParser st Char
tie = char '~'

equals :: CharParser st Char
equals = char '='

dot :: CharParser st Char
dot = char '.'

colon :: CharParser st Char
colon = char ':'

doubleQuote :: CharParser st Char
doubleQuote = char '"'

singleQuote :: CharParser st Char
singleQuote = char '\''


pitchLetter :: CharParser st Char
pitchLetter = oneOf "cdefgab"

doubleAngleOpen :: CharParser st String
doubleAngleOpen = string "<<"

doubleAngleClose :: CharParser st String
doubleAngleClose = string ">>"

angleOpen :: CharParser st Char
angleOpen = char '<'

angleClose :: CharParser st Char
angleClose = char '>'

beamOpen :: CharParser st Char
beamOpen = char '['

beamClose :: CharParser st Char
beamClose = char ']'

braceOpen :: CharParser st Char
braceOpen = char '{'

braceClose :: CharParser st Char
braceClose = char '}'



       
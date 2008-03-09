--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Abc.Parser
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- (INCOMPLETE) Parser for ABC format files
-- |
--------------------------------------------------------------------------------

module Bala.Format.Abc.Parser where

import Bala.Base.BaseExtra
import Bala.Format.Abc.Datatypes


import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad

import Data.Char hiding (Space)
import Data.List (sortBy)
import Text.ParserCombinators.Parsec hiding (space)


abcFile :: Parser [AbcFileElement]
abcFile = many1 fileElement

fileElement :: Parser AbcFileElement
fileElement = choice [ fieldsElement, tuneElement, texElement, 
                        dropComment, dropFeed]
  where fieldsElement = AbcFileFieldsElement <$> fileFields 
        tuneElement   = AbcTuneElement       <$> abcTune
        texElement    = AbcTexCommandElement <$> texCommand
        
        dropComment   = comment     *> fileElement
        dropFeed      = linefeed    *> fileElement

  
  

fileFields :: Parser [Field]
fileFields = many1 $ choice 
                    [ fieldFile,      fieldBook,        fieldGroup 
                    , fieldHistory,   fieldInformation, fieldMeter 
                    , fieldOrigin,    fieldRhythm
                    ]

fieldFile :: Parser Field
fieldFile = readField 'F' text FieldFile

abcTune :: Parser AbcTune
abcTune = (,) <$> abcHeader <*> abcMusic

  

abcHeader :: Parser AbcHeader
abcHeader = AbcHeader <$> fieldNumber <*  optional comment <*> many1 fieldTitle
                                      <*> many otherFields <*> fieldKey
 
  

fieldNumber = readField 'X' readNat FieldNumber

fieldTitle  = readField 'T' text FieldTitle


otherFields :: Parser Field
otherFields = choice [ fieldArea,         fieldBook,        fieldComposer 
                     , fieldDiscography,  fieldElemskip,    fieldGroup
                     , fieldHistory,      fieldInformation, fieldDefaultLength
                     , fieldMeter,        fieldNotes,       fieldOrigin
                     , fieldParts,        fieldTempo,       fieldRhythm
                     , fieldSource,       fieldTranscrnotes
                     ]
  
  {- <|> comment -}

readField ch fn constr = constr <$> (char ch *> char ':' *> fn <* endOfLine)
   
fieldArea           = readField 'A' text  FieldArea
fieldBook           = readField 'B' text  FieldBook
fieldComposer       = readField 'C' text  FieldComposer
fieldDiscography    = readField 'D' text  FieldDiscography
fieldElemskip       = readField 'E' text  FieldElemskip
fieldGroup          = readField 'G' text  FieldGroup
fieldHistory        = readField 'H' (text `sepBy` endOfLine) FieldHistory    
fieldInformation    = readField 'I' text  FieldInformation
fieldDefaultLength  = readField 'L' noteLengthStrict FieldDefaultLength
fieldMeter          = readField 'M' meter FieldMeter
fieldNotes          = readField 'N' text  FieldNotes
fieldOrigin         = readField 'O' text  FieldOrigin
fieldParts          = readField 'P' parts FieldParts  
fieldTempo          = readField 'Q' tempo FieldTempo
fieldRhythm         = readField 'R' text  FieldRhythm
fieldSource         = readField 'S' text  FieldSource
fieldTranscrnotes   = readField 'Z' text  FieldTranscrNotes
fieldKey            = readField 'K' key   FieldKey


       

key :: Parser Key
key = keySpec <|> hp
  where
    hp = char 'H' *> (pu <|> pl)
    pu = HighlandNoKey      <$ char 'P'
    pl = HighlandMixolydian <$ char 'p'
    


keySpec :: Parser Key
keySpec = Key <$> inner
  where inner      = (,,) <$> keynote <*> (pmaybe modeSpec) <*>  many accidental
        accidental = char ' ' *> globalAccidental
  

keynote = (,) <$> basenote <*> pmaybe keyAccidental

keyAccidental :: Parser KeyAccidental
keyAccidental = sharp <|> flat
  where sharp = KeySharp <$ char '#'
        flat  = KeyFlat  <$ char 'b'

   
modeSpec :: Parser ModeSpec    
modeSpec = (,) <$> ospaceMode <*> extratext 
  where ospaceMode = pmaybe (char ' ') *> mode

extratext :: Parser String
extratext = many (satisfy isAlpha)


globalAccidental :: Parser GlobalAccidental
globalAccidental = (,) <$> accidental <*> basenote


mode :: Parser Mode
mode = choice [ modeMajor, modeLydian, modeIonian, modeMixolydian, 
                 modeDorian, modeAeolian, modePhrygian,
                 modeLocrian, modeMinor]
  where
    modeMinor       = ModeMinor      <$ chooseString ["min", "MIN", "m", "M"] 
    modeMajor       = ModeMajor      <$ chooseString ["maj", "MAJ"]
    modeLydian      = ModeLydian     <$ chooseString ["lyd", "LYD"]
    modeIonian      = ModeIonian     <$ chooseString ["ion", "ION"]
    modeMixolydian  = ModeMixolydian <$ chooseString ["mix", "MIX"]
    modeDorian      = ModeDorian     <$ chooseString ["dor", "DOR"]
    modeAeolian     = ModeAeolian    <$ chooseString ["aeo", "AEO"]
    modePhrygian    = ModePhrygian   <$ chooseString ["phr", "PHR"]
    modeLocrian     = ModeLocrian    <$ chooseString ["loc", "LOC"]

-- maybe modes should handle full names e.g. Gmajor


meter :: Parser Meter
meter = cutTime <|> commonTime <|> meterFraction
  where cutTime     = MeterCutTime    <$ string "C|"
        commonTime  = MeterCommonTime <$ char 'C'



meterFraction :: Parser Meter
meterFraction = Meter <$> fraction
  where fraction = (,) <$> readNat <* char '/' <*> readNat
 


-- left rec ??
tempo :: Parser Tempo
tempo = ta <|> tb <|> tc
  where eqk = char '=' *> readNat
  
        ta = TempoC <$> (char 'C' *> noteLength) <*> eqk
        
        tb = TempoAbsolute <$> noteLengthStrict <*> eqk
             
        tc = Tempo <$> readNat

noteLengthStrict :: Parser NoteLengthStrict
noteLengthStrict = (,) <$> readNat <*> (char '/' *> readNat)




parts :: Parser [Part]
parts = many1 partSpec


partSpec :: Parser Part
partSpec = unaryPart <|> nestedPart <|> repeatedPart
  where unaryPart     = PartElem 0 <$> part 
        
        nestedPart    = PartTree <$> readNat <*> parens (many1 partSpec)
        
        repeatedPart  = PartElem <$> readNat <*> part

part :: Parser Char
part = satisfy $ (flip elem) ['A'..'Z']



endOfLine :: Parser ()
endOfLine = () <$ white *> optional trailingComment *> linefeed
  where white           = many $ satisfy (\c -> c == ' ' || c == '\t')
        trailingComment = char '%' *> text
  



-------------------------------------------------------------------------------

parseElems = many1 element

abcMusic :: Parser AbcMusic
abcMusic = (many1 abcLine) <* linefeed


abcLine :: Parser AbcLine
abcLine = elementList <|> mtf <|> tcmd
  where elementList = Elements      <$> many1 element <* lineEnder
        mtf         = MidTuneField  <$> midTuneField
        tcmd        = MidTexCommand <$> texCommand

element :: Parser Element
element = choice [ noteElem, tupletElem, barElem, repElem, slurElem,
                   spaceElem ]  -- <|> userElem
  where noteElem    = NoteElement   <$> noteElement
        tupletElem  = TupletElement <$> tupletSpec <*> many1 noteElement
        barElem     = Barline       <$> barline
        repElem     = NthRepeat     <$> nthRepeat
        slurElem    = Slur          <$> (beginSlur <|> endSlur)
        spaceElem   = Space         <$  space
        userElem    = undefined  -- AbcUserDefinedElement



      
lineEnder :: Parser ()
lineEnder = comment <|> linefeed <|> lineBreak <|> noLineBreak 
  
tupletElement = (,) <$> tupletSpec <*> many1 noteElement


tupletSpec :: Parser TupletSpec
tupletSpec = (char '(') *> tups 
  where tups = readNat `sepBy` (char ':')


noteElement :: Parser NoteElement
noteElement = (,) <$> noteStem <*> pmaybe brokenRhythm


noteStem :: Parser NoteStem
noteStem = build <$> pmaybe guitarChord <*> pmaybe graceNotes <*> many gracings
                                        <*> noteOrMulti

  where 
    noteOrMulti = (Left <$> note) <|> (Right <$> multiNote)
    
    build mgc mgn gs (Left n)   = (mgc, mgn, gs, [n])
    build mgc mgn gs (Right ns) = (mgc, mgn, gs, ns)




multiNote :: Parser [Note]
multiNote = squares $ many1 note
  where squares = between (char '[') (char ']')


note :: Parser Note
note = Note <$> noteOrRest <*> pmaybe noteLength <*> pmaybe tie




noteOrRest :: Parser NoteOrRest
noteOrRest = choice [ (NotePitch <$> notePitch), (Rest <$ rest) ]




notePitch :: Parser PitchSpec
notePitch = build <$> pmaybe accidental <*> basenote <*> pmaybe octave
  where build ma n mo  = PitchSpec n ma mo


octave :: Parser Octave    
octave = octaveLow <|> octaveHigh
  where octaveLow   = OctaveLow  <$> counting1 (char ',')
        octaveHigh  = OctaveHigh <$> counting1 (char '\'')



noteLength :: Parser NoteLength    
noteLength = (,) <$> readNat <*> pmaybe readNat


accidental :: Parser Accidental
accidental = choice [dblSharp, sharp, dblFlat, flat]
  where dblSharp = DoubleSharp <$ string "^^" 
        sharp    = Sharp       <$ string "^"  
        dblFlat  = DoubleFlat  <$ string "__" 
        flat     = Flat        <$ string "_"  
        natural  = Natural     <$ string "="


basenote :: Parser BaseNote
basenote = satisfy ((flip elem) "CDEFGABcdefgab")

 
rest :: Parser NoteOrRest
rest = Rest <$ char 'z'



brokenRhythm :: Parser BrokenRhythm
brokenRhythm = dnext <|> dprev
  where dnext = DottedRight <$> counting1 (char '<')
        dprev = DottedLeft  <$> counting1 (char '>')

  


tie :: Parser Tie
tie = char '-' >> return Tie


gracings :: Parser Gracing
gracings = tilde <|> stacatto <|> downBow <|> upBow
  where tilde     = Tilde    <$ char '~'
        stacatto  = Stacatto <$ char '.'
        downBow   = DownBow  <$ char 'v'
        upBow     = UpDown   <$ char 'u'


graceNotes :: Parser GraceNotes  
graceNotes = braces $ many1 notePitch
  where braces = between (char '{') (char '}') 



guitarChord :: Parser GuitarChord
guitarChord = fchord <|> unchord
  where 
    fchord  = dblQuoted formalChord
    unchord = UninterpretedChord <$> dblQuoted text


formalChord :: Parser GuitarChord
formalChord = FormalChord <$> basenote <*> pmaybe chordType 
                                       <*> pmaybe (char '/' >> basenote)

    
chordType :: Parser String
chordType = choice $ map string
    ["m", "7", "m7", "0", "o", "+", "mb5", "sus", "sus4"
        , "maj7", "mmaj7", "7sus4", "dim", "dim7", "7b5", "m7b5"
        , "6", "b6", "m6", "mb6", "46", "maj9", "9", "add9"
        , "7b9" , "m9"]



barline :: Parser Barline
barline = choice [ barDbl, barThickThin, barThinThick,
                    repLeft, repRight, repBoth, barSgl ]
  where barSgl       = BarSingle     <$ char '|'
        barDbl       = BarDouble     <$ string "||" 
        barThickThin = BarThickThin  <$ string "[|"
        barThinThick = BarThinThick  <$ string "|]"
        repLeft      = RepeatLeft    <$ string ":|"
        repRight     = RepeatRight   <$ string "|:"
        repBoth      = RepeatBoth    <$ string "::"



  
nthRepeat :: Parser RepeatMark 
nthRepeat = choice [firstRep, secondRep, firstEnd, secondEnd]
  where 
    firstRep     = RepeatFirst  <$ string "[1"
    secondRep    = RepeatSecond <$ string "[2"
    firstEnd     = EndingFirst  <$ string "|1"
    secondEnd    = EndingSecond <$ string ":|2"


                  
beginSlur, endSlur :: Parser Slur                
beginSlur   = SlurBegin <$ char '('
endSlur     = SlurEnd   <$ char ')'


midTuneField :: Parser Field
midTuneField = tuneField


tuneField :: Parser Field
tuneField = choice [ fieldElemskip, fieldKey, fieldDefaultLength,
                      fieldMeter, fieldPart, fieldTempo, fieldTitle, 
                      fieldWords ]




fieldPart, fieldWords :: Parser Field
fieldPart = readField 'P' partSpec FieldPart
  
fieldWords = readField 'W' ws FieldWords
  where ws = text >>= \cs -> endOfLine >> return cs


userDefined = satisfy $ (flip elem) ['H'..'Z']

texCommand :: Parser TexCommand
texCommand = (char '\\') *> text <* linefeed


space :: Parser ()     
space = () <$ (char ' ' <|> char '\t')


comment :: Parser ()
comment = char '%' *> text *> ending
  where ending = linefeed <|> noLineBreak <|> lineBreak
    
lineBreak :: Parser ()  
lineBreak = char '!' *> linefeed
    
noLineBreak :: Parser ()    
noLineBreak = char '\\' *> linefeed

linefeed :: Parser ()     
linefeed = () <$ choice [char '\r',  char '\n']


text :: Parser [Char]
text = many1 (satisfy isTextChar)
  where isTextChar :: Char -> Bool
        isTextChar c = isAlpha c || isDigit c || isSpecial c
        
        isSpecial = (flip elem)  " \t\"!#$&'()*+,-./:;<=>?@[\\]^_`{|}~"




------------------------------------------------


                  
--------------------------------------------------------------------------------
-- Useful combinators
--------------------------------------------------------------------------------                  



chooseString = choice . map string

counting, counting1 :: Parser a -> Parser Int

counting p = length <$> many p

counting1 p = length <$> many1 p

dblQuoted = between (char '"') (char '"')


readNat :: Parser Int
readNat =  many1 (satisfy isDigit) >>= return . read 


pmaybe :: Parser a -> Parser (Maybe a)
pmaybe p = option Nothing (p >>= return . Just)









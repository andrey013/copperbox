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

import Bala.Format.Abc.Datatypes

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad

import Data.Char
import Data.List (sortBy)
import Text.ParserCombinators.Parsec hiding ( {- token, -} space)




instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap
   

{- parse = readP_to_S -}

  


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

  
  

fileFields :: Parser [AbcField]
fileFields = many1 $ choice 
                    [ fieldFile,      fieldBook,        fieldGroup 
                    , fieldHistory,   fieldInformation, fieldMeter 
                    , fieldOrigin,    fieldRhythm
                    ]

fieldFile :: Parser AbcField
fieldFile = readField 'F' text AbcFileField

abcTune :: Parser AbcTune
abcTune = (,) <$> abcHeader <*> abcMusic

  

abcHeader :: Parser AbcHeader
abcHeader = AbcHeader <$> fieldNumber <*  optional comment <*> many1 fieldTitle
                                      <*> many otherFields <*> fieldKey
 
  

fieldNumber = readField 'X' readNat AbcNumberField

fieldTitle  = readField 'T' text AbcTitleField  


otherFields :: Parser AbcField
otherFields = choice [ fieldArea,         fieldBook,        fieldComposer 
                     , fieldDiscography,  fieldElemskip,    fieldGroup
                     , fieldHistory,      fieldInformation, fieldDefaultLength
                     , fieldMeter,        fieldNotes,       fieldOrigin
                     , fieldParts,        fieldTempo,       fieldRhythm
                     , fieldSource,       fieldTranscrnotes
                     ]
  
  {- <|> comment -}

readField ch fn constr = constr <$> (char ch *> char ':' *> fn <* endOfLine)
   
fieldArea           = readField 'A' text  AbcAreaField
fieldBook           = readField 'B' text  AbcBookField
fieldComposer       = readField 'C' text  AbcComposerField
fieldDiscography    = readField 'D' text  AbcDiscographyField
fieldElemskip       = readField 'E' text  AbcElemskipField
fieldGroup          = readField 'G' text  AbcGroupField
fieldHistory        = readField 'H' (text `sepBy` endOfLine) AbcHistoryField    
fieldInformation    = readField 'I' text  AbcInformationField
fieldDefaultLength  = readField 'L' noteLengthStrict AbcDefaultLengthField
fieldMeter          = readField 'M' meter AbcMeterField
fieldNotes          = readField 'N' text  AbcNotesField
fieldOrigin         = readField 'O' text  AbcOriginField
fieldParts          = readField 'P' parts AbcPartsField  
fieldTempo          = readField 'Q' tempo AbcTempoField
fieldRhythm         = readField 'R' text  AbcRhythmField
fieldSource         = readField 'S' text  AbcSourceField
fieldTranscrnotes   = readField 'Z' text  AbcTranscrNotesField
fieldKey            = readField 'K' key   AbcKeyField


       

key :: Parser AbcKey
key = keySpec <|> hp
  where
    hp = char 'H' *> (pu <|> pl)
    pu = AbcHighlandNoKey      <$ char 'P'
    pl = AbcHighlandMixolydian <$ char 'p'
    


keySpec :: Parser AbcKey
keySpec = AbcKey <$> inner
  where inner      = (,,) <$> keynote <*> (pmaybe modeSpec) <*>  many accidental
        accidental = char ' ' *> globalAccidental
  

keynote = (,) <$> basenote <*> pmaybe keyAccidental

keyAccidental :: Parser AbcKeyAccidental
keyAccidental = sharp <|> flat
  where sharp = AbcSharpKey <$ char '#'
        flat  = AbcFlatKey  <$ char 'b'

   
modeSpec :: Parser Mode_spec    
modeSpec = (,) <$> ospaceMode <*> extratext 
  where ospaceMode = pmaybe (char ' ') *> mode

extratext :: Parser String
extratext = many (satisfy isAlpha)


globalAccidental :: Parser Global_accidental
globalAccidental = (,) <$> accidental <*> basenote


mode :: Parser AbcMode
mode = choice [ modeMajor, modeLydian, modeIonian, modeMixolydian, 
                 modeDorian, modeAeolian, modePhrygian,
                 modeLocrian, modeMinor]
  where
    modeMinor       = AbcMinorMode      <$ chooseString ["min", "MIN", "m", "M"] 
    modeMajor       = AbcMajorMode      <$ chooseString ["maj", "MAJ"]
    modeLydian      = AbcLydianMode     <$ chooseString ["lyd", "LYD"]
    modeIonian      = AbcIonianMode     <$ chooseString ["ion", "ION"]
    modeMixolydian  = AbcMixolydianMode <$ chooseString ["mix", "MIX"]
    modeDorian      = AbcDorianMode     <$ chooseString ["dor", "DOR"]
    modeAeolian     = AbcAeolianMode    <$ chooseString ["aeo", "AEO"]
    modePhrygian    = AbcPhrygianMode   <$ chooseString ["phr", "PHR"]
    modeLocrian     = AbcLocrianMode    <$ chooseString ["loc", "LOC"]

-- maybe modes should handle full names e.g. Gmajor


meter :: Parser AbcMeter
meter = cutTime <|> commonTime <|> meterFraction
  where cutTime     = AbcCutTimeMeter    <$ string "C|"
        commonTime  = AbcCommonTimeMeter <$ char 'C'



meterFraction :: Parser AbcMeter
meterFraction = AbcMeter <$> readNat <* char '/' <*> readNat
 


-- left rec ??
tempo :: Parser AbcTempo
tempo = ta <|> tb <|> tc
  where eqk = char '=' *> readNat
  
        ta = AbcCTempo <$> (char 'C' *> noteLength) <*> eqk
        
        tb = AbcAbsoluteTempo <$> noteLengthStrict <*> eqk
             
        tc = AbcTempo <$> readNat

noteLengthStrict :: Parser Note_length_strict
noteLengthStrict = (,) <$> readNat <*> (char '/' *> readNat)




parts :: Parser [AbcPart]
parts = many1 partSpec


partSpec :: Parser AbcPart
partSpec = unaryPart <|> nestedPart <|> repeatedPart
  where unaryPart     = AbcPartElem 0 <$> part 
        
        nestedPart    = AbcPartTree <$> readNat <*> parens (many1 partSpec)
        
        repeatedPart  = AbcPartElem <$> readNat <*> part

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
  where elementList = AbcElements      <$> many1 element <* lineEnder
        mtf         = AbcMidTuneField  <$> midTuneField
        tcmd        = AbcMidTexCommand <$> texCommand

element :: Parser AbcElement
element = choice [ noteElem, tupletElem, barElem, repElem, slurElem,
                   spaceElem ]  -- <|> userElem
  where noteElem    = AbcNoteElement      <$> noteElement
        tupletElem  = AbcTupletElement    <$> tupletSpec <*> many1 noteElement
        barElem     = AbcBarlineElement   <$> barline
        repElem     = AbcRepeatElement    <$> nthRepeat
        slurElem    = AbcSlurElement      <$> (beginSlur <|> endSlur)
        spaceElem   = AbcSpaceElement     <$  space
        userElem    = undefined  -- AbcUserDefinedElement



      
lineEnder :: Parser ()
lineEnder = comment <|> linefeed <|> lineBreak <|> noLineBreak 
  
tupletElement = (,) <$> tupletSpec <*> many1 noteElement


tupletSpec :: Parser Tuplet_spec
tupletSpec = (char '(') *> tups 
  where tups = readNat `sepBy` (char ':')


noteElement :: Parser Note_element
noteElement = (,) <$> noteStem <*> pmaybe brokenRhythm


noteStem :: Parser Note_stem
noteStem = build <$> pmaybe guitarChord <*> pmaybe graceNotes <*> many gracings
                                        <*> noteOrMulti

  where 
    noteOrMulti = (Left <$> note) <|> (Right <$> multiNote)
    
    build mgc mgn gs (Left n)   = (mgc, mgn, gs, [n])
    build mgc mgn gs (Right ns) = (mgc, mgn, gs, ns)




multiNote :: Parser [AbcNote]
multiNote = squares $ many1 note
  where squares = between (char '[') (char ']')


note :: Parser AbcNote
note = AbcNote <$> noteOrRest <*> pmaybe noteLength <*> pmaybe tie




noteOrRest :: Parser AbcValue
noteOrRest = choice [ (AbcPitchValue <$> pitch), (AbcRest <$ rest) ]




pitch :: Parser AbcPitch
pitch = build <$> pmaybe accidental <*> basenote <*> pmaybe octave
  where build ma n mo  = AbcPitch n ma mo


octave :: Parser AbcOctave    
octave = lowOctave <|> highOctave
  where lowOctave   = AbcLowOctave  <$> counting1 (char ',')
        highOctave  = AbcHighOctave <$> counting1 (char '\'')



noteLength :: Parser Note_length    
noteLength = (,) <$> readNat <*> pmaybe readNat


accidental :: Parser AbcAccidental
accidental = choice [dblSharp, sharp, dblFlat, flat]
  where dblSharp = AbcDoubleSharp <$ string "^^" 
        sharp    = AbcSharp       <$ string "^"  
        dblFlat  = AbcDoubleFlat  <$ string "__" 
        flat     = AbcFlat        <$ string "_"  
        natural  = AbcNatural     <$ string "="


basenote :: Parser Basenote
basenote = satisfy ((flip elem) "CDEFGABcdefgab")

 
rest :: Parser AbcValue
rest = AbcRest <$ char 'z'



brokenRhythm :: Parser AbcBrokenRhythm
brokenRhythm = dnext <|> dprev
  where dnext = AbcDottedRight <$> counting1 (char '<')
        dprev = AbcDottedLeft  <$> counting1 (char '>')

  


tie :: Parser AbcTie
tie = char '-' >> return AbcTie



gracings :: Parser AbcGracing
gracings = tilde <|> stacatto <|> downBow <|> upBow
  where tilde     = AbcTilde    <$ char '~'
        stacatto  = AbcStacatto <$ char '.'
        downBow   = AbcDownBow  <$ char 'v'
        upBow     = AbcUpDown   <$ char 'u'


graceNotes :: Parser Grace_notes  
graceNotes = braces $ many1 pitch
  where braces = between (char '{') (char '}') 



guitarChord :: Parser AbcGuitarChord
guitarChord = fchord <|> unchord
  where 
    fchord  = dblQuoted formalChord
    unchord = AbcUninterpretedChord <$> dblQuoted text


formalChord :: Parser AbcGuitarChord
formalChord = AbcFormalChord <$> basenote <*> pmaybe chordType 
                                          <*> pmaybe (char '/' >> basenote)

    
chordType :: Parser String
chordType = choice $ map string
    ["m", "7", "m7", "0", "o", "+", "mb5", "sus", "sus4"
        , "maj7", "mmaj7", "7sus4", "dim", "dim7", "7b5", "m7b5"
        , "6", "b6", "m6", "mb6", "46", "maj9", "9", "add9"
        , "7b9" , "m9"]



barline :: Parser AbcBarline
barline = choice [ barDbl, barThickThin, barThinThick,
                    repLeft, repRight, repBoth, barSgl ]
  where barSgl       = AbcSingleBar     <$ char '|'
        barDbl       = AbcDoubleBar     <$ string "||" 
        barThickThin = AbcThick_ThinBar <$ string "[|"
        barThinThick = AbcThin_ThickBar <$ string "|]"
        repLeft      = AbcLeftRepeat    <$ string ":|"
        repRight     = AbcRightRepeat   <$ string "|:"
        repBoth      = AbcBothRepeat    <$ string "::"



  
nthRepeat :: Parser AbcRepeatMark 
nthRepeat = choice [firstRep, secondRep, firstEnd, secondEnd]
  where 
    firstRep     = AbcFirstRepeat  <$ string "[1"
    secondRep    = AbcSecondRepeat <$ string "[2"
    firstEnd     = AbcFirstEnding  <$ string "|1"
    secondEnd    = AbcSecondEnding <$ string ":|2"


                  
beginSlur, endSlur :: Parser AbcSlur                
beginSlur   = AbcBeginSlur <$ char '('
endSlur     = AbcEndSlur   <$ char ')'


midTuneField :: Parser AbcField
midTuneField = tuneField


tuneField :: Parser AbcField
tuneField = choice [ fieldElemskip, fieldKey, fieldDefaultLength,
                      fieldMeter, fieldPart, fieldTempo, fieldTitle, 
                      fieldWords ]




fieldPart, fieldWords :: Parser AbcField
fieldPart = readField 'P' partSpec AbcPartField
  
fieldWords = readField 'W' ws AbcWordsField
  where ws = text >>= \cs -> endOfLine >> return cs


userDefined = satisfy $ (flip elem) ['H'..'Z']

texCommand :: Parser Tex_command
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

parens = between (char '(') (char ')')







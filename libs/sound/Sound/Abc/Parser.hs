
module Sound.Abc.Parser where

import Sound.Abc.Datatypes

import Control.Applicative hiding (many, optional)
import Control.Monad

import Data.Char hiding (Space)
import Data.List (sortBy)
import Text.ParserCombinators.ReadP



instance Applicative ReadP where
  pure = return
  (<*>) = ap
   

parse = readP_to_S

  


abcFile :: ReadP [AbcFileElement]
abcFile = repeated fileElement

fileElement :: ReadP AbcFileElement
fileElement = choiceB [ fieldsElement, tuneElement, texElement, 
                        dropComment, dropFeed]
  where fieldsElement = AbcFileFieldsElement <$> fileFields 
        tuneElement   = AbcTuneElement       <$> abcTune
        texElement    = AbcTexCommandElement <$> texCommand
        
        dropComment   = comment     *> fileElement
        dropFeed      = linefeed    *> fileElement

  
  

fileFields :: ReadP [AbcField]
fileFields = many1 $ choice 
                    [ fieldFile,      fieldBook,        fieldGroup 
                    , fieldHistory,   fieldInformation, fieldMeter 
                    , fieldOrigin,    fieldRhythm
                    ]

fieldFile :: ReadP AbcField
fieldFile = readField 'F' text AbcFileField

abcTune :: ReadP AbcTune
abcTune = (,) <$> abcHeader <*> abcMusic

  

abcHeader :: ReadP AbcHeader
abcHeader = AbcHeader <$> fieldNumber <*  optional comment <*> many1 fieldTitle
                                      <*> many otherFields <*> fieldKey
 
  

fieldNumber = readField 'X' readNat AbcNumberField

fieldTitle  = readField 'T' text AbcTitleField  


otherFields :: ReadP AbcField
otherFields = choice [ fieldArea,         fieldBook,        fieldComposer 
                     , fieldDiscography,  fieldElemskip,    fieldGroup
                     , fieldHistory,      fieldInformation, fieldDefaultLength
                     , fieldMeter,        fieldNotes,       fieldOrigin
                     , fieldParts,        fieldTempo,       fieldRhythm
                     , fieldSource,       fieldTranscrnotes
                     ]
  
  {- +++ comment -}

readField ch fn constr = constr <$> (char ch *> char ':' *> fn <* endOfLine)
   
fieldArea           = readField 'A' text  AbcAreaField
fieldBook           = readField 'B' text  AbcBookField
fieldComposer       = readField 'C' text  AbcComposerField
fieldDiscography    = readField 'D' text  AbcDiscographyField
fieldElemskip       = readField 'E' text  AbcElemskipField
fieldGroup          = readField 'G' text  AbcGroupField
fieldHistory        = readField 'H' (text `sepBy` endOfLine) AbcHistoryField    
fieldInformation    = readField 'I' text  AbcInformationField
fieldDefaultLength  = readField 'L' noteLength AbcDefaultLengthField
fieldMeter          = readField 'M' meter AbcMeterField
fieldNotes          = readField 'N' text  AbcNotesField
fieldOrigin         = readField 'O' text  AbcOriginField
fieldParts          = readField 'P' parts AbcPartsField  
fieldTempo          = readField 'Q' tempo AbcTempoField
fieldRhythm         = readField 'R' text  AbcRhythmField
fieldSource         = readField 'S' text  AbcSourceField
fieldTranscrnotes   = readField 'Z' text  AbcTranscrNotesField
fieldKey            = readField 'K' key   AbcKeyField


       

key :: ReadP AbcKey
key = keySpec +++ hp
  where
    hp = char 'H' *> (pu +++ pl)
    pu = AbcHighlandNoKey      <$ char 'P'
    pl = AbcHighlandMixolydian <$ char 'p'
    


keySpec :: ReadP AbcKey
keySpec = AbcKey <$> inner
  where inner      = (,,) <$> keynote <*> (pmaybe modeSpec) <*>  many accidental
        accidental = char ' ' *> globalAccidental
  

keynote = (,) <$> basenote <*> pmaybe keyAccidental

keyAccidental :: ReadP AbcKeyAccidental
keyAccidental = sharp <++ flat
  where sharp = AbcSharpKey <$ char '#'
        flat  = AbcFlatKey  <$ char 'b'

   
modeSpec :: ReadP Mode_spec    
modeSpec = (,) <$> ospaceMode <*> extratext 
  where ospaceMode = pmaybe (char ' ') *> mode

extratext :: ReadP String
extratext = munch isAlpha


globalAccidental :: ReadP Global_accidental
globalAccidental = (,) <$> accidental <*> basenote


mode :: ReadP AbcMode
mode = choiceB [ modeMajor, modeLydian, modeIonian, modeMixolydian, 
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




meter :: ReadP AbcMeter
meter = cutTime <++ commonTime <++ meterFraction
  where cutTime     = AbcCutTimeMeter    <$ string "C|"
        commonTime  = AbcCommonTimeMeter <$ char 'C'



meterFraction :: ReadP AbcMeter
meterFraction = AbcMeter <$> readNat <* char '/' <*> readNat
 


-- left rec ??
tempo :: ReadP AbcTempo
tempo = ta <++ tb <++ tc
  where eqk = char '=' *> readNat
  
        ta = AbcCTempo <$> (char 'C' *> noteLength) <*> eqk
        
        tb = AbcAbsoluteTempo <$> noteLengthStrict <*> eqk
             
        tc = AbcTempo <$> readNat

noteLengthStrict :: ReadP Note_length_strict
noteLengthStrict = (,) <$> readNat <*> (char '/' *> readNat)




parts :: ReadP [AbcPart]
parts = many1 partSpec


partSpec :: ReadP AbcPart
partSpec = unaryPart <++ nestedPart <++ repeatedPart
  where unaryPart     = AbcPartElem 0 <$> part 
        
        nestedPart    = AbcPartTree <$> readNat <*> parens (many1 partSpec)
        
        repeatedPart  = AbcPartElem <$> readNat <*> part

part :: ReadP Char
part = satisfy $ (flip elem) ['A'..'Z']



endOfLine :: ReadP ()
endOfLine = () <$ white *> optional trailingComment *> linefeed
  where white           = munch (\c -> c == ' ' || c == '\t')
        trailingComment = char '%' *> text
  



-------------------------------------------------------------------------------

parseElems = many1 element

abcMusic :: ReadP AbcMusic
abcMusic = (many1 abcLine) <* linefeed


abcLine :: ReadP AbcLine
abcLine = elementList +++ mtf +++ tcmd
  where elementList = AbcElements      <$> many1 element <* lineEnder
        mtf         = AbcMidTuneField  <$> midTuneField
        tcmd        = AbcMidTexCommand <$> texCommand

element :: ReadP AbcElement
element = choice [ noteElem, tupletElem, barElem, repElem, slurElem,
                   spaceElem ]  -- +++ userElem
  where noteElem    = AbcNoteElement      <$> noteElement
        tupletElem  = AbcTupletElement    <$> tupletSpec <*> many1 noteElement
        barElem     = AbcBarlineElement   <$> barline
        repElem     = AbcRepeatElement    <$> nthRepeat
        slurElem    = AbcSlurElement      <$> beginSlur +++ endSlur
        spaceElem   = AbcSpaceElement     <$  space
        userElem    = undefined  -- AbcUserDefinedElement



      
lineEnder :: ReadP ()
lineEnder = comment +++ linefeed +++ lineBreak +++ noLineBreak 
  
tupletElement = (,) <$> tupletSpec <*> many1 noteElement


tupletSpec :: ReadP Tuplet_spec
tupletSpec = (char '(') *> tups 
  where tups = readNat `sepBy` (char ':')


noteElement :: ReadP Note_element
noteElement = (,) <$> noteStem <*> pmaybe brokenRhythm


noteStem :: ReadP Note_stem
noteStem = build <$> pmaybe guitarChord <*> pmaybe graceNotes <*> many gracings
                                        <*> noteOrMulti

  where 
    noteOrMulti = (Left <$> note) <++ (Right <$> multiNote)
    
    build mgc mgn gs (Left n)   = (mgc, mgn, gs, [n])
    build mgc mgn gs (Right ns) = (mgc, mgn, gs, ns)




multiNote :: ReadP [AbcNote]
multiNote = squares $ many1 note
  where squares = between (char '[') (char ']')


note :: ReadP AbcNote
note = AbcNote <$> noteOrRest <*> pmaybe noteLength <*> pmaybe tie




noteOrRest :: ReadP AbcValue
noteOrRest = choice [ (AbcPitchValue <$> pitch), (AbcRest <$ rest) ]




pitch :: ReadP AbcPitch
pitch = build <$> pmaybe accidental <*> basenote <*> pmaybe octave
  where build ma n mo  = AbcPitch n ma mo


octave :: ReadP AbcOctave    
octave = lowOctave <++ highOctave
  where lowOctave   = AbcLowOctave  <$> counting1 (char ',')
        highOctave  = AbcHighOctave <$> counting1 (char '\'')



noteLength :: ReadP Note_length    
noteLength = (,) <$> readNat <*> pmaybe readNat


accidental :: ReadP AbcAccidental
accidental = choiceB [dblSharp, sharp, dblFlat, flat]
  where dblSharp = AbcDoubleSharp <$ string "^^" 
        sharp    = AbcSharp       <$ string "^"  
        dblFlat  = AbcDoubleFlat  <$ string "__" 
        flat     = AbcFlat        <$ string "_"  
        natural  = AbcNatural     <$ string "="


basenote :: ReadP Basenote
basenote = satisfy ((flip elem) "CDEFGABcdefgab")

 
rest :: ReadP AbcValue
rest = AbcRest <$ char 'z'



brokenRhythm :: ReadP AbcBrokenRhythm
brokenRhythm = dnext +++ dprev
  where dnext = AbcDottedRight <$> counting1 (char '<')
        dprev = AbcDottedLeft  <$> counting1 (char '>')

  


tie :: ReadP AbcTie
tie = char '-' >> return AbcTie



gracings :: ReadP AbcGracing
gracings = tilde <++ stacatto <++ downBow <++ upBow
  where tilde     = AbcTilde    <$ char '~'
        stacatto  = AbcStacatto <$ char '.'
        downBow   = AbcDownBow  <$ char 'v'
        upBow     = AbcUpDown   <$ char 'u'


graceNotes :: ReadP Grace_notes  
graceNotes = braces $ many1 pitch
  where braces = between (char '{') (char '}') 



guitarChord :: ReadP AbcGuitarChord
guitarChord = fchord <++ unchord
  where 
    fchord  = dblQuoted formalChord
    unchord = AbcUninterpretedChord <$> dblQuoted text


formalChord :: ReadP AbcGuitarChord
formalChord = AbcFormalChord <$> basenote <*> pmaybe chordType 
                                          <*> pmaybe (char '/' >> basenote)

    
chordType :: ReadP String
chordType = longest $
    ["m", "7", "m7", "0", "o", "+", "mb5", "sus", "sus4"
        , "maj7", "mmaj7", "7sus4", "dim", "dim7", "7b5", "m7b5"
        , "6", "b6", "m6", "mb6", "46", "maj9", "9", "add9"
        , "7b9" , "m9"]



barline :: ReadP AbcBarline
barline = choiceB [ barDbl, barThickThin, barThinThick,
                    repLeft, repRight, repBoth, barSgl ]
  where barSgl       = AbcSingleBar     <$ char '|'
        barDbl       = AbcDoubleBar     <$ string "||" 
        barThickThin = AbcThick_ThinBar <$ string "[|"
        barThinThick = AbcThin_ThickBar <$ string "|]"
        repLeft      = AbcLeftRepeat    <$ string ":|"
        repRight     = AbcRightRepeat   <$ string "|:"
        repBoth      = AbcBothRepeat    <$ string "::"



  
nthRepeat :: ReadP AbcRepeatMark 
nthRepeat = choiceB [firstRep, secondRep, firstEnd, secondEnd]
  where 
    firstRep     = AbcFirstRepeat  <$ string "[1"
    secondRep    = AbcSecondRepeat <$ string "[2"
    firstEnd     = AbcFirstEnding  <$ string "|1"
    secondEnd    = AbcSecondEnding <$ string ":|2"


                  
beginSlur, endSlur :: ReadP AbcSlur                
beginSlur   = AbcBeginSlur <$ char '('
endSlur     = AbcEndSlur   <$ char ')'


midTuneField :: ReadP AbcField
midTuneField = tuneField


tuneField :: ReadP AbcField
tuneField = choiceB [ fieldElemskip, fieldKey, fieldDefaultLength,
                      fieldMeter, fieldPart, fieldTempo, fieldTitle, 
                      fieldWords ]




fieldPart, fieldWords :: ReadP AbcField
fieldPart = readField 'P' partSpec AbcPartField
  
fieldWords = readField 'W' ws AbcWordsField
  where ws = text >>= \cs -> endOfLine >> return cs


userDefined = satisfy $ (flip elem) ['H'..'Z']

texCommand :: ReadP Tex_command
texCommand = (char '\\') *> text <* linefeed


space :: ReadP ()     
space = () <$ (char ' ' +++ char '\t')


comment :: ReadP ()
comment = char '%' *> text *> ending
  where ending = linefeed +++ noLineBreak +++ lineBreak
    
lineBreak :: ReadP ()  
lineBreak = char '!' *> linefeed
    
noLineBreak :: ReadP ()    
noLineBreak = char '\\' *> linefeed

linefeed :: ReadP ()     
linefeed = () <$ choice [char '\r',  char '\n']


text :: ReadP [Char]
text = munch1 isTextChar
  where isTextChar :: Char -> Bool
        isTextChar c = isAlpha c || isDigit c || isSpecial c
        
        isSpecial = (flip elem)  " \t\"!#$&'()*+,-./:;<=>?@[\\]^_`{|}~"




------------------------------------------------


                  
--------------------------------------------------------------------------------
-- Useful combinators
--------------------------------------------------------------------------------                  

-- returns the longest sequence of successful parsers, rather than their
-- permutations
repeated :: ReadP a -> ReadP [a]
repeated p = rep []
  where rep acc = (p >>= \a -> rep (a:acc)) <++ return (reverse acc)
  
  
choiceB :: [ReadP a] -> ReadP a
-- ^ Left-biased version of choice        
choiceB []     = pfail
choiceB [p]    = p
choiceB (p:ps) = p <++ choiceB ps

chooseStringB :: [String] -> ReadP String
chooseStringB = choiceB . map string


chooseString :: [String] -> ReadP String
chooseString = choice . map string

counting, counting1 :: ReadP a -> ReadP Int

counting p = counting1 p <++ return 0

counting1 p = counting' 1
  where counting' i = p >> counting' (i+1) <++ return i

dblQuoted = between (char '"') (char '"')

longest :: [String] -> ReadP String 
longest = chooseStringB . reverse . sortBy longstring
  where longstring s1 s2 = compare (length s1) (length s2)

readNat :: ReadP Int
readNat =  munch1 isDigit >>= return . read 


pmaybe :: ReadP a -> ReadP (Maybe a)
pmaybe p = option Nothing (p >>= return . Just)

parens = between (char '(') (char ')')







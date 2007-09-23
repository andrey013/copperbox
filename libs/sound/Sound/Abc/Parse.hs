
module Sound.Abc.Parse where

import Sound.Abc.Datatypes

import Control.Monad
import Data.Char hiding (Space)
import Data.List (sortBy)
import Text.ParserCombinators.ReadP


parse = readP_to_S

  

abcFile = many $ choice 
  [ just (abcTune >>= return . Tune)
  , nothing comment
  , nothing linefeed
  , just (texCommand >>= return . Command)
  , just (fileFields >>= return . Field)
  ]
  where just p    = p >>= return . Just
        nothing p = p >>  return Nothing



fileFields :: ReadP Field
fileFields = choice [ fieldFile,      fieldBook,        fieldGroup 
                    , fieldHistory,   fieldInformation, fieldMeter 
                    , fieldOrigin,    fieldRhythm
                    ]

fieldFile :: ReadP Field
fieldFile = readField 'F' text Field_file

abcTune :: ReadP (Header, [Abc_line])
abcTune = do 
  h <- abcHeader
  m <- abcMusic
  return (h, m)
  

abcHeader :: ReadP Header
abcHeader = do
  nt <- fieldNumber
  optional comment
  ts <- many1 fieldTitle
  fs <- many otherFields
  fk <- fieldKey
  return (nt, ts, fs, fk) 


  
  

fieldNumber = readField 'X' readNat Field_number



fieldTitle = readField 'T' text Field_title  


otherFields :: ReadP Field
otherFields = choice [ fieldArea,         fieldBook,        fieldComposer 
                     , fieldDiscography,  fieldElemskip,    fieldGroup
                     , fieldHistory,      fieldInformation, fieldDefaultLength
                     , fieldMeter,        fieldNotes,       fieldOrigin
                     , fieldParts,        fieldTempo,       fieldRhythm
                     , fieldSource,       fieldTranscrnotes
                     ]
  
  {- +++ comment -}

readField ch fn constr = char ch >> char ':' >> fn >>= \cs -> 
                         endOfLine >> return (constr cs)
   
fieldArea           = readField 'A' text  Field_area
fieldBook           = readField 'B' text  Field_book
fieldComposer       = readField 'C' text  Field_composer
fieldDiscography    = readField 'D' text  Field_discography
fieldElemskip       = readField 'E' text  Field_elemskip
fieldGroup          = readField 'G' text  Field_group
fieldHistory        = readField 'H' (text `sepBy` endOfLine) Field_history    
fieldInformation    = readField 'I' text  Field_information
fieldDefaultLength  = readField 'L' noteLength Field_default_length
fieldMeter          = readField 'M' meter Field_meter
fieldNotes          = readField 'N' text  Field_notes
fieldOrigin         = readField 'O' text  Field_origin
fieldParts          = readField 'P' parts Field_parts  
fieldTempo          = readField 'Q' tempo Field_tempo
fieldRhythm         = readField 'R' text  Field_rhythm
fieldSource         = readField 'S' text  Field_source
fieldTranscrnotes   = readField 'Z' text  Field_transcrnotes
fieldKey            = readField 'K' key   Field_key


       

key :: ReadP Key
key = keySpec +++ hp
  where
    hp = char 'H' >> pl +++ pu
    pl = char 'p' >> return Hp
    pu = char 'P' >> return HP_NO_KEY 


keySpec :: ReadP Key
keySpec = do
    kn <- keynote
    oms <- pmaybe modeSpec
    xs <- many accidental
    return (Key (kn, oms, xs))
  where accidental = char ' ' >> globalAccidental >>= return   



keynote = do
  n <- basenote
  mka <- pmaybe keyAccidental
  return (n, mka)




keyAccidental :: ReadP Key_accidental     
keyAccidental = sharp <++ flat
  where sharp = char '#' >> return KEY_SHARP
        flat  = char 'b' >> return KEY_FLAT

   
modeSpec :: ReadP Mode_spec    
modeSpec = do
  pmaybe $ char ' '
  m <- mode
  txt <- extratext
  return (m, txt)
 

extratext :: ReadP String
extratext = munch isAlpha


globalAccidental :: ReadP Global_accidental
globalAccidental = do
  a <- accidental
  b <- basenote
  return (a,b)



mode :: ReadP Mode
mode = modeMajor  <++ modeLydian   <++ modeIonian    <++ modeMixolydian 
                  <++ modeDorian   <++ modeAeolian   <++ modePhrygian
                  <++ modeLocrian  <++ modeMinor
  where
    modeMinor       = chooseString ["min", "MIN", "m", "M"] >> return MIN
    modeMajor       = chooseString ["maj", "MAJ"] >> return MAJ
    modeLydian      = chooseString ["lyd", "LYD"] >> return LYD
    modeIonian      = chooseString ["ion", "ION"] >> return ION
    modeMixolydian  = chooseString ["mix", "MIX"] >> return MIX
    modeDorian      = chooseString ["dor", "DOR"] >> return DOR
    modeAeolian     = chooseString ["aeo", "AEO"] >> return AEO
    modePhrygian    = chooseString ["phr", "PHR"] >> return PHR
    modeLocrian     = chooseString ["loc", "LOC"] >> return LOC




meter :: ReadP Meter
meter = cutTime <++ commonTime <++ meterFraction
  where cutTime     = string "C|" >> return CUT_TIME
        commonTime  = char 'C'    >> return COMMON_TIME 



meterFraction :: ReadP Meter
meterFraction = do 
  l <- readNat
  char '/'
  r <- readNat
  return (Meter l r)
  


-- left rec ??
tempo :: ReadP Tempo
tempo = ta <++ tb <++ tc
  where eqk = char '=' >> readNat >>= return 
  
        ta = char 'C' >> noteLength >>= \nl -> 
             eqk >>= \i -> 
             return (C_tempo nl i) 
        
        tb = noteLengthStrict >>= \n ->
             eqk >>= \i -> 
             return (Absolute_tempo n i)
             
        tc = readNat >>= \i -> return (Tempo i)

noteLengthStrict :: ReadP Note_length_strict
noteLengthStrict = do
  l <- readNat
  char '/'
  r <- readNat
  return (l,r)




parts :: ReadP [Part]
parts = many1 partSpec


partSpec :: ReadP Part
partSpec = unaryPart <++ nestedPart <++ repeatedPart
  where unaryPart     = part >>= \ch -> return (PartElem 0 ch) 
        
        nestedPart    = readNat >>= \i -> 
                        parens (many1 partSpec) >>= \cs ->
                        return (PartTree i cs)
        
        repeatedPart  = readNat >>= \i -> part >>= \ch -> return (PartElem i ch) 

part :: ReadP Char
part = satisfy $ (flip elem) ['A'..'Z']



endOfLine :: ReadP ()
endOfLine = do
  munch (\c -> c == ' ' || c == '\t')
  optional (char '%' >> text)
  linefeed



-------------------------------------------------------------------------------


abcMusic = many1 abcLine >>= \ls -> linefeed >> return ls


abcLine :: ReadP Abc_line
abcLine = elementList +++ (midTuneField >>= return . Mid_tune_field)
                      +++ (texCommand   >>= return . Tex_command)
  where elementList = many1 element >>= \es -> 
                      lineEnder     >>  return (Element_list es)


element :: ReadP Element
element = noteElem +++ tupletElem +++ barElem +++ repElem +++ slurElem
                   +++ spaceElem  -- +++ userElem
  where noteElem    = noteElement >>= return . Note_element
        tupletElem  = tupletSpec  >>= \t ->
                      many1 noteElement >>= \ns ->
                      return (Tuplet_element t ns)
                       
        barElem     = barline   >>= return . Barline
        repElem     = nthRepeat >>= return . Nth_repeat
        slurElem    = beginSlur +++ endSlur >>= return . Slur
        spaceElem   = space >> return Space
        userElem    = undefined



      
lineEnder :: ReadP ()
lineEnder = comment +++ linefeed +++ lineBreak +++ noLineBreak 
  
tupletElement = do
  tsp <- tupletSpec
  es <- many1 noteElement
  return (tsp, es)



tupletSpec :: ReadP Tuplet_spec
tupletSpec = do
  char '('
  xs <- readNat `sepBy` (char ':')
  return xs


noteElement :: ReadP Note_element
noteElement = do
  ns <- noteStem
  mbr <- pmaybe brokenRhythm
  return (ns, mbr)


noteStem :: ReadP Note_stem
noteStem =do 
  mgc <- pmaybe guitarChord
  mgn <- pmaybe graceNotes
  gs <- many gracings
  en <- (note >>= return . Left) <++ (multiNote >>= return . Right)
  case en of
        Left n -> return $ (mgc, mgn, gs, [n])
        Right ns -> return $ (mgc, mgn, gs, ns)



multiNote :: ReadP [Note]
multiNote = squares $ many1 note
  where squares = between (char '[') (char ']')


note :: ReadP Note
note = do
  n <- noteOrRest
  ml <- pmaybe noteLength
  mt <- pmaybe tie
  return (n,ml,mt)



noteOrRest :: ReadP Note_or_rest
noteOrRest = (pitch >>= return . Pitch) +++ (rest >> return Rest) 




pitch :: ReadP (Maybe Accidental, Basenote, Maybe Octave)
pitch = do
  ma <- pmaybe accidental
  n <- basenote
  mo <- pmaybe octave
  return (ma,n,mo)




octave :: ReadP Octave    
octave = lowOctave <++ hiOctave
  where lowOctave = counting1 (char '\'') >>= return . Octave_low
        hiOctave  = counting1 (char ',')  >>= return . Octave_hi



noteLength :: ReadP Note_length    
noteLength = do 
  i <- readNat
  mj <- pmaybe readNat
  return $ (i,mj)
  




accidental :: ReadP Accidental
accidental = choiceB [dblSharp, sharp, dblFlat, flat]
  where dblSharp = fn "^^" DBL_SHARP
        sharp    = fn "^"  SHARP
        dblFlat  = fn "__" DBL_FLAT
        flat     = fn "_"  FLAT
        natural  = fn "="  NATURAL
        fn s a = string s >> return a



        

basenote :: ReadP Basenote
basenote = satisfy ((flip elem) "CDEFGABcdefgab")

 
rest :: ReadP Note_or_rest
rest = char 'z' >> return Rest



brokenRhythm :: ReadP Broken_rhythm
brokenRhythm = dnext +++ dprev
  where dnext = counting1 (char '<') >>= return . DOT_NEXT
        dprev = counting1 (char '>') >>= return . DOT_PREV

  


tie :: ReadP Tie
tie = char '-' >> return TIE



gracings :: ReadP Gracing
gracings = tilde <++ stacatto <++ downBow <++ upBow
  where tilde     = char '~' >> return TILDE
        stacatto  = char '.' >> return STACATTO
        downBow   = char 'v' >> return DOWN_BOW
        upBow     = char 'u' >> return UP_BOW


graceNotes :: ReadP Grace_notes  
graceNotes = braces $ many1 pitch
  where braces = between (char '{') (char '}') 



guitarChord :: ReadP Guitar_chord
guitarChord = dblQuoted formalChord <++ (dblQuoted text >>= return . Chord_text)
  where dblQuoted = between (char '"') (char '"')


formalChord :: ReadP Guitar_chord
formalChord = do
  n <- basenote
  mct <- pmaybe chordType
  mn <- pmaybe (char '/' >> basenote)
  return (Formal_chord n mct mn)



    
chordType :: ReadP String
chordType = longest $
    ["m", "7", "m7", "0", "o", "+", "mb5", "sus", "sus4"
        , "maj7", "mmaj7", "7sus4", "dim", "dim7", "7b5", "m7b5"
        , "6", "b6", "m6", "mb6", "46", "maj9", "9", "add9"
        , "7b9" , "m9"]



barline :: ReadP Barline
barline = barDbl <++ barThickThin <++ barThinThick 
                 <++ repLeft      <++ repRight      <++ repBoth 
                 <++ barSgl
  where barSgl       = char '|' >> return BARLINE
        barDbl       = string "||" >> return BAR_DBL
        barThickThin = string "[|" >> return BAR_THICK_THIN 
        barThinThick = string "|]" >> return BAR_THIN_THICK 
        repLeft      = string ":|" >> return REP_LEFT
        repRight     = string "|:" >> return REP_RIGHT 
        repBoth      = string "::" >> return REP_BOTH



  
nthRepeat :: ReadP Nth_repeat 
nthRepeat = firstRep <++ secondRep <++ firstEnd <++ secondEnd
  where 
    firstRep     = string "[1"   >> return  FIRST_REPEAT
    secondRep    = string "[2"   >> return  SECOND_REPEAT
    firstEnd     = string "|1"   >> return FIRST_ENDING
    secondEnd    = string ":|2"  >> return SECOND_ENDING


                  
beginSlur, endSlur :: ReadP Slur                
beginSlur   = char '(' >> return SLUR_BEGIN
endSlur     = char ')' >> return SLUR_END


midTuneField :: ReadP Field
midTuneField = tuneField


tuneField :: ReadP Field
tuneField = choiceB [ fieldElemskip, fieldKey, fieldDefaultLength
                    , fieldMeter, fieldPart, fieldTempo, fieldTitle
                    , fieldWords 
                    ]


fieldPart :: ReadP Field
fieldPart = do
  string "P:"
  ps <- parts
  return (Field_parts ps)
  
  

fieldWords = do 
  string "W:"
  cs <- text
  endOfLine
  return (Field_words cs)


userDefined = satisfy $ (flip elem) ['H'..'Z']


texCommand = char '\\' >> text >>= \cs -> linefeed >> return cs


space :: ReadP ()     
space = char ' ' +++ char '\t' >> return ()


comment :: ReadP ()
comment = char '%' >> text >> ending
  where ending = linefeed +++ noLineBreak +++ lineBreak
    
lineBreak :: ReadP ()  
lineBreak = char '!' >> linefeed
    
noLineBreak :: ReadP ()    
noLineBreak = char '\\' >> linefeed

linefeed :: ReadP ()     
linefeed = char '\r' +++  char '\n' >> return ()


text :: ReadP [Char]
text = munch1 isTextChar
  where isTextChar :: Char -> Bool
        isTextChar c = isAlpha c || isDigit c || isSpecial c
        
        isSpecial = (flip elem)  " \t\"!#$&'()*+,-./:;<=>?@[\\]^_`{|}~"




------------------------------------------------


                  
--------------------------------------------------------------------------------
-- Useful combinators
--------------------------------------------------------------------------------                  

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


longest :: [String] -> ReadP String 
longest = chooseStringB . reverse . sortBy longstring
  where longstring s1 s2 = compare (length s1) (length s2)

readNat :: ReadP Int
readNat =  munch1 isDigit >>= return . read 


pmaybe :: ReadP a -> ReadP (Maybe a)
pmaybe p = option Nothing (p >>= return . Just)

parens = between (char '(') (char ')')







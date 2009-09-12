{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondDoc
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty printers for LilyPond.
--
--------------------------------------------------------------------------------


module Mullein.LilyPondDoc
  (
  -- * Printing glyphs
    note
  , pitch
  , pitchLabel
  , duration
  , rest
  , spacer
  , tie

  , chordForm
  , graceForm
  , beamForm

  , (**^)
  , (**-)
  , (**\)


  -- * LilyPond literals and syntax
  -- *** Commands and comments
  , command
  , comment


  -- *** Time signatures
  , time
  
  -- *** Bar lines
  , doubleBar
  , singleBar

  -- *** Stems
  , stemUp
  , stemDown
  , stemNeutral

  -- *** Clefs
  , clef

  -- *** Unmetered music
  , cadenzaOn
  , cadenzaOff

  -- *** Key signature
  , key

  -- *** Score structure
  , nestBraces
  , simultaneous
  , overlay
  , score
  , context
  , contextExpr
  , contextVoice
  , contextTabVoice
  , new
  , newStaff
  , newStaffGroup
  , newTabStaff
  , voiceOne
  , voiceTwo
  , markup
  , book
  , bookpart
  , version
  , layout
  , layoutExpr
  , relative
  , drummode
  , fretDiagram

  -- *** Titles
  , header
  , headerElement
  , dedication
  , title
  , subtitle
  , instrument
  , composer
  , copyright
  , tagline

  -- *** Files and variables
  , include
  , variableDef
  , variableUse
  , schemeDef

  , midi
  , midiExpr

  ) where


import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils

import Text.PrettyPrint.Leijen

import Data.Char ( isAlpha )



--------------------------------------------------------------------------------
-- Printing glyphs

-- | Print a note, the duration is a Maybe value. Nothing indicates
-- that the note has the same duration as the previous glyph.
note :: Pitch -> Maybe Duration -> Doc
note p md = pitch p <> maybe empty duration md

-- | Print a Pitch.
pitch :: Pitch -> Doc 
pitch pch@(Pitch _ _ o) = pitchLabel (label pch) <> ove o where
    ove i | i > 0       = text $ replicate i       '\''
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = empty


-- | Print a 'PitchLabel'.
pitchLabel :: PitchLabel -> Doc
pitchLabel (PitchLabel l ma) = 
    char (toLowerLChar l) <> maybe empty accidental ma
  where 
    accidental :: Accidental -> Doc
    accidental Nat            = text "!"    -- check correctness
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"

-- | Print a Duration.
duration :: Duration -> Doc
duration = maybe empty df . lyRepresentation
  where
    df (LyCmd ss,dc) = dots dc $ command ss 
    df (LyNum i,dc)  = dots dc $ int i

    dots :: Int -> (Doc -> Doc)
    dots i | i > 0     = (<> text (replicate i '.'))
           | otherwise = id



-- | Print a rest, duration is a Maybe value - Nothing indicates
-- that the rest has the same duration as the previous glyph.  
rest :: Maybe Duration -> Doc
rest md = char 'r' <> maybe empty duration md


-- | Print an invisible rest, commonly used to align overlayed 
-- bars. Duration is a Maybe value - Nothing indicates
-- that the spacer has the same duration as the previous glyph.  
spacer :: Maybe Duration -> Doc
spacer md = char 's' <> maybe empty duration md

-- | Print a tie.
tie :: Doc
tie = char '~'


-- | Chords - notes printed inside angle brackets, followed by 
-- duration, e.g.:
--
-- @ 
--  \<c e g\>4
-- @ 
chordForm :: [Doc] -> Maybe Duration -> Doc
chordForm xs md = angles (hsep xs) <> maybe empty duration md


-- | Grace notes - @\\grace@ command then expression inside braces,
-- e.g:
--
-- @ 
--  \\grace { f32[ e] }
-- @ 
graceForm :: [Doc] -> Doc
graceForm [x] = command "grace" <+> braces x where
graceForm xs  = command "grace" <+> braces (beamForm xs)

  
-- | Beams - first element printed outside the square brackets, e.g.:
-- @ 
--  c16 [e g c]
-- @ 
beamForm :: [Doc] -> Doc
beamForm (x:xs) = x <> char '[' <+> hsep xs <> char ']'
beamForm []     = emptyDoc

infixr 6 **^, **-, **\

(**^) :: Doc -> Doc -> Doc
a **^ b = a <> char '^' <> b

(**-) :: Doc -> Doc -> Doc
a **- b = a <> char '-' <> b

(**\) :: Doc -> Doc -> Doc
a **\ b = a <> char '_' <> b


--------------------------------------------------------------------------------
-- Lilypond literals and syntax


-- *** Commands and comments

-- | Print a some command, commands are prefixed with a slash, 
-- e.g.: @\\voiceOne@, @\\unfoldRepeats@.
command :: String -> Doc
command = (char '\\' <>) . text 

-- | Print a comment, comments can be multi-line.
comment :: String -> Doc
comment s = text "%{" <+> string s  <+> text "%}"

-- *** Time signatures

time :: Int -> Int -> Doc
time n d = command "time" <+> int n <> char '/' <> int d


-- *** Bar lines

-- | Print a double bar line @||@.
doubleBar :: Doc 
doubleBar = command "bar" <+> dquotes (text "||")

-- | Print a single bar line @|@.
singleBar :: Doc
singleBar = text "|"


-- *** stems

-- | @\\stemUp@.
stemUp                  :: Doc
stemUp                  = command "stemUp"  

-- | @\\stemDown@.
stemDown                :: Doc
stemDown                = command "stemDown"    

-- | @\\stemNeutral@.
stemNeutral             :: Doc
stemNeutral             = command "stemNeutral"  


--------------------------------------------------------------------------------
-- *** Clef

-- | @\\clef ...@ - typical values @treble@, @alto@, @bass@, @tenor@, 
-- @percussion@, @tabClef@.
clef :: String -> Doc
clef str = command "clef" <+> text str

--------------------------------------------------------------------------------
-- *** Unmetered music

-- | @\\cadenzaOn@.
cadenzaOn     :: Doc
cadenzaOn     = command "cadenzaOn"

-- | @\\cadenzaOff@.
cadenzaOff    :: Doc
cadenzaOff    = command "cadenzaOff"

--------------------------------------------------------------------------------
-- *** Key signature

-- | @\\key ... ... @ - key pitch mode. Typical values of mode are
-- @major@, @minor@, and the church modes @dorian@, @locrian@, etc.
key :: PitchLabel -> String -> Doc
key lbl mode = command "key" <+> pitchLabel lbl <+> command mode



--------------------------------------------------------------------------------
-- *** Score structure

-- | Enclose expression within braces @{ ... }@. The open brace
-- is printed on the current line, then a line break, then the  
-- expression is printed with indent level two. The closing brace
-- is printed on a new line.
nestBraces :: Doc -> Doc
nestBraces e = lbrace <$> indent 2 e <$> rbrace 


-- | @\<\< \\n... \\n... \\n \>\>@ - print a list of expressions 
-- within a simultaneous block. Each expression is printed on a
-- separate line.
simultaneous :: [Doc] -> Doc
simultaneous ds = text "<<" <$> indent 2 (vsep ds) <$> text ">>"


-- | @\<\< ... \\\\ ... \>\>@ - print simultaneous expressions 
-- with a double forward slash separator.
overlay :: [Doc] -> Doc
overlay = dblangles . vsep . punctuate (text " \\\\") . map spaceBraces


-- | @\\score {\\n ...\\n }@.
score                 :: Doc -> Doc
score e               = command "score" <+> nestBraces e

-- | @\\context ... @.
context               :: Doc -> Doc
context e             = command "context" <+> e


-- | @\\context {\\n ...\\n }@.
contextExpr           :: Doc -> Doc
contextExpr e         = command "context" <+> nestBraces e


-- | @\\context Voice = "..." ... @.
contextVoice          :: String -> Doc -> Doc
contextVoice s e      = context (text "Voice") 
                          <+> equals <+> (dquotes $ text s) <+> e


-- | @\\context TabVoice = "..." ... @.
contextTabVoice       :: String -> Doc -> Doc
contextTabVoice s e   = context (text "TabVoice") 
                          <+> equals <+> (dquotes $ text s) <+> e


-- | @\\new ... ... @ - e.g. @Staff@, @Voice@ then expression.
new                   :: String -> Doc -> Doc
new ss e              = command "new" <+> text ss <+> e


-- | @\\new Staff ... @.
newStaff              :: Doc -> Doc
newStaff              = new "Staff" 

-- | @\\new StaffGroup ... @.
newStaffGroup         :: Doc -> Doc
newStaffGroup         = new "StaffGroup" 


-- | @\\new TabStaff ... @.
newTabStaff           :: Doc -> Doc
newTabStaff           = new "TabStaff" 


-- | @\\voiceOne @.
voiceOne              :: Doc
voiceOne              = command "voiceOne"

-- | @\\voiceTwo @.
voiceTwo              :: Doc
voiceTwo              = command "voiceTwo" 



-- | @\\markup ... @.
markup                :: Doc -> Doc
markup e              = command "markup" <+> e


-- | @\\book {\\n ...\\n }@.
book                  :: Doc -> Doc
book e                = command "book" <+> nestBraces e


-- | @\\bookpart {\\n ...\\n }@.
bookpart              :: Doc -> Doc
bookpart e            = command "bookpart" <+> nestBraces e


-- | @\\version { ... }@.
version               :: String -> Doc
version ss            = command "version" <+> dquotes (text ss)


-- | @\\layout { }@.
layout                :: Doc
layout                = command "layout" <+> braces space

-- | @\\layout {\\n ...\\n }@.
layoutExpr            :: Doc -> Doc
layoutExpr e          = command "layout" <+> nestBraces e


-- | @\\relative PITCH {\\n ... \\n}@ - print a relative block.
-- The musical expression should have been transformed with
-- 'rewitePitch' before being rendered to LilyPond.
relative :: Pitch -> Doc -> Doc 
relative p expr = command "relative" <+> pitch p' <+> nestBraces expr
  where
    p' = modifyOctave ((octave p) - 4) p

-- | @\\drummode {\\n ...\\n }@.
drummode            :: Doc -> Doc
drummode e          = command "drummode" <+> nestBraces e


-- | @\\fret-diagram #\"...\"@.  
fretDiagram           :: String -> Doc
fretDiagram s         = command "fret-diagram" <+> char '#' 
                          <> (dquotes $ text s)


--------------------------------------------------------------------------------
-- *** Titles


-- | @\header { ... }@ - print a header block.
header                :: [Doc] -> Doc
header xs             = command "header" <+> nestBraces (vcat xs)

-- | @name = val@ - primitive combinator for building header 
-- elements. Use this if you want markup, placement information etc.
-- in the right-hand side.
headerElement :: String -> Doc -> Doc
headerElement name val = text name <+> equals <+> val

-- | @dedication = \"...\"@.  
dedication            :: String -> Doc
dedication            = headerElement "dedication" . dquotes . text

-- | @title = \"...\"@.  
title                 :: String -> Doc
title                 = headerElement "title" . dquotes . text

-- | @subtitle = \"...\"@.  
subtitle              :: String -> Doc
subtitle              = headerElement "subtitle" . dquotes . text

-- | @instrument = \"...\"@.  
instrument            :: String -> Doc
instrument            = headerElement "instrument" . dquotes . text

-- | @composer = \"...\"@.  
composer              :: String -> Doc
composer              = headerElement "composer" . dquotes . text

-- | @copyright = \"...\"@.  
copyright             :: String -> Doc
copyright             = headerElement "copyright" . dquotes . text

-- | @tagline = \"...\"@.  
tagline               :: String -> Doc
tagline               = headerElement "tagline" . dquotes . text




--------------------------------------------------------------------------------
-- *** Files and variables


-- | @\include \"...\"@ - print a header block.
include               :: String -> Doc
include ss            = command "include" <+> dquotes (text ss)

-- | @varName = ...@ - define a variable. The variable name should only
-- contain alphabetic characters, otherwise an error is thrown.
variableDef           :: String -> Doc -> Doc
variableDef ss e         
  | all isAlpha ss    = text ss <+> equals <+> e
  | otherwise         = error $ "LilyPondDoc.variableDef - " ++ ss ++ 
                                " - should only contain alphabetic characters."

-- | @\\varName@.
variableUse           :: String -> Doc
variableUse ss  
  | all isAlpha ss    = command ss
  | otherwise         = error $ "LilyPondDoc.variableUse - " ++ ss ++ 
                                " - should only contain alphabetic characters."

-- | @varName = #( ... )@.
schemeDef :: String -> String -> Doc
schemeDef ss str
  | all isAlpha ss    = text ss <+> equals <+> char '#' <> (parens $ string str)
  | otherwise         = error $ "LilyPondDoc.schemeDef - " ++ ss ++ 
                                " - should only contain alphabetic characters."


-- | @\\midi { }@.
midi                  :: Doc
midi                  = command "midi" <+> braces space

-- | @\\midi {\\n ...\\n }@.
midiExpr              :: Doc -> Doc
midiExpr e            = command "midi" <+> nestBraces e



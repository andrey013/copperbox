{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.LilyPondDoc
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty printers for LilyPond.
--
--------------------------------------------------------------------------------


module Neume.Extra.LilyPondDoc
  (
  -- * Types
    VarName

  -- * Printers  
  , annoAbove
  , annoCenter
  , annoBelow

  , lyTrue
  , lyFalse

  -- * LilyPond literals and syntax
  -- ** Commands and comments
  , command
  , comment
  , lineComment

  -- ** Time and key signatures
  , time
  , key

  
  -- ** Bar lines
  , doubleBar
  , singleBar

  -- ** Stems
  , stemUp
  , stemDown
  , stemNeutral

  -- ** Clefs
  , clef

  -- ** Unmetered music
  , cadenzaOn
  , cadenzaOff

  -- ** Score structure
  , nestBraces
  , simultaneous
  , overlay
  , score
  , scoreExpr
  , context
  , contextExpr
  , contextVoice
  , contextTabVoice
  , new
  , with
  , newStaff
  , newStaffGroup
  , newTabStaff
  , newVoice
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
  , parallelMusic  

  -- ** Titles
  , header
  , headerElement
  , dedication
  , title
  , subtitle
  , instrument
  , composer
  , copyright
  , tagline

  -- * Repeats
  , repeatvolta
  , alternative



  -- ** Files, variables, overrides
  , include
  , variableDef
  , variableUse
  , schemeDef
  , schemeStringLiteral
  , override

  -- ** Midi directives
  , midi
  , midiExpr
  , tempoWholesPerMinute
  , midiMinimumVolume
  , midiMaximumVolume
  ) where


import Neume.Core.LilyPondPretty 
import Neume.Core.Pitch
import Neume.Core.Utils.Common ( ftrunc )
import Neume.Core.Utils.Pretty

import Text.PrettyPrint.Leijen                  -- package: wl-pprint

import Data.Char ( isAlpha )


type VarName = String


infixr 6 `annoAbove`, `annoCenter`, `annoBelow`

annoAbove           :: Doc -> Doc -> Doc
a `annoAbove` b     = a <> char '^' <> b

annoCenter          :: Doc -> Doc -> Doc
a `annoCenter` b    = a <> char '-' <> b

annoBelow           :: Doc -> Doc -> Doc
a `annoBelow` b     = a <> char '_' <> b

-- | @ #t @
lyTrue :: Doc
lyTrue = text "#t"

-- | @ #f @
lyFalse :: Doc 
lyFalse = text "#f"

--------------------------------------------------------------------------------
-- Lilypond literals and syntax


-- Commands and comments

-- | Print a some command, commands are prefixed with a slash, 
-- e.g.: @\\voiceOne@, @\\unfoldRepeats@.
command :: String -> Doc
command = (char '\\' <>) . text 

-- | @{% ... %}@ - print a comment, comments can be multi-line.
comment :: String -> Doc
comment s = text "%{" <+> string s <+> text "%}"


-- | @%% ... @ - print a comment, comments must be a single line.
lineComment :: String -> Doc
lineComment s = text "%%" <+> string s



-- Time and key signatures

-- | @\\time .../... @ - time signature.
time :: Int -> Int -> Doc
time n d = command "time" <+> int n <> char '/' <> int d


-- | @\\key ... ... @ - key pitch mode. Typical values of mode are
-- @major@, @minor@, and the church modes @dorian@, @locrian@, etc.
key :: PitchLabel -> String -> Doc
key lbl mode = command "key" <+> pitchLabel lbl <+> command mode



-- stems

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
-- Clef

-- | @\\clef ...@ - typical values @treble@, @alto@, @bass@, @tenor@, 
-- @percussion@, @tabClef@.
clef :: String -> Doc
clef str = command "clef" <+> text str

--------------------------------------------------------------------------------
-- Unmetered music

-- | @\\cadenzaOn@.
cadenzaOn     :: Doc
cadenzaOn     = command "cadenzaOn"

-- | @\\cadenzaOff@.
cadenzaOff    :: Doc
cadenzaOff    = command "cadenzaOff"



--------------------------------------------------------------------------------
-- Score structure



-- | @\<\< \\n... \\n... \\n \>\>@ - print a list of expressions 
-- within a simultaneous block. Each expression is printed on a
-- separate line.
simultaneous :: [Doc] -> Doc
simultaneous ds = text "<<" <$> indent 2 (vsep ds) <$> text ">>"


-- | @\<\< ... \\\\ ... \>\>@ - print simultaneous expressions 
-- with a double forward slash separator.
overlay :: [Doc] -> Doc
overlay = dblangles . vsep . punctuate (text " \\\\")


-- | @\\score@.
-- NOTE - LilyPond commonly uses @\\Score@ with uppercase start 
-- (which one is more appropriate for this combinator is to 
-- resolve).
score                 :: Doc
score                 = command "score"

-- | @\\score {\\n ...\\n }@.
scoreExpr             :: Doc -> Doc
scoreExpr e           = command "score" <+> nestBraces e

-- | @\\context@.
context               :: Doc
context               = command "context"


-- | @\\context {\\n ...\\n }@.
contextExpr           :: Doc -> Doc
contextExpr e         = context <+> nestBraces e


-- | @\\context Voice = "..." ... @.
contextVoice          :: String -> Doc -> Doc
contextVoice s e      = context <+> (text "Voice") 
                          <+> equals <+> (dquotes $ text s) <+> e


-- | @\\context TabVoice = "..." ... @.
contextTabVoice       :: String -> Doc -> Doc
contextTabVoice s e   = context <+> (text "TabVoice") 
                          <+> equals <+> (dquotes $ text s) <+> e


-- | @\\new ... ... @ - e.g. @Staff@, @Voice@ then expression.
new                   :: String -> Doc -> Doc
new ss e              = command "new" <+> text ss <+> e


-- | @\\with { \\n ... \\n } ... @ - e.g. @Staff@, @Voice@ then expression.
with                   :: Doc -> Doc -> Doc
with e1 e2              = command "with" <+> nestBraces e1 <$> e2


-- | @\\new Staff ... @.
newStaff              :: Doc -> Doc
newStaff              = new "Staff" 

-- | @\\new StaffGroup ... @.
newStaffGroup         :: Doc -> Doc
newStaffGroup         = new "StaffGroup" 


-- | @\\new TabStaff ... @.
newTabStaff           :: Doc -> Doc
newTabStaff           = new "TabStaff" 

-- | @\\new Voice { ... } @.
newVoice              :: Doc -> Doc
newVoice e            = command "new" <+> text "Voice" <+> e


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
    p' = setOctave (octave p - 3) p  -- Lilypond is 3 octaves below Neume


-- | @\\drummode {\\n ...\\n }@.
drummode            :: Doc -> Doc
drummode e          = command "drummode" <+> nestBraces e



--------------------------------------------------------------------------------
-- Titles


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
-- Bar lines, repeats

-- | Print a double bar line @||@.
doubleBar :: Doc 
doubleBar = command "bar" <+> dquotes (text "||")

-- | Print a single bar line @|@.
singleBar :: Doc
singleBar = text "|"


-- Repeats

-- | @\\repeat volta n {\\n ... \\n}@ - print a repeated block.
--
repeatvolta :: Int -> Doc -> Doc 
repeatvolta i expr = 
    command "repeat" <+> text "volta" <+> int i <+> nestBraces expr

-- | @\\alternative { \\n { ... } \\n { ... } ... }@
--
alternative :: [Doc] -> Doc
alternative = (command "alternative" <+>) . nestBraces . vsep . map braces 



-- | @\\parallelMusic #'( ... ) {\\n ...\\n }@.
parallelMusic       :: [String] -> Doc -> Doc
parallelMusic xs e  = 
    command "parallelMusic" <+> text "#'" <> parens names <+> nestBraces e
  where
    names = hsep $ map text xs


--------------------------------------------------------------------------------
-- Files, variables, overides


-- | @\include \"...\"@ - print a header block.
include               :: String -> Doc
include ss            = command "include" <+> dquotes (text ss)

-- | @varName = ...@ - define a variable. The variable name should only
-- contain alphabetic characters, otherwise an error is thrown.
variableDef           :: VarName -> Doc -> Doc
variableDef ss e         
  | all isAlpha ss    = text ss <+> equals <+> e
  | otherwise         = error $ "LilyPondDoc.variableDef - " ++ ss ++ 
                                " - should only contain alphabetic characters."

-- | @\\varName@ - the variable name should only contain 
-- alphabetic characters.
variableUse           :: VarName -> Doc
variableUse ss  
  | all isAlpha ss    = command ss
  | otherwise         = error $ "LilyPondDoc.variableUse - " ++ ss ++ 
                                " - should only contain alphabetic characters."

-- | @varName = #( ... )@ - the variable name should only contain 
-- alphabetic characters.
schemeDef :: VarName -> Doc -> Doc
schemeDef ss d
  | all isAlpha ss    = text ss <+> equals <+> char '#' <> d
  | otherwise         = error $ "LilyPondDoc.schemeDef - " ++ ss ++ 
                                " - should only contain alphabetic characters."

schemeStringLiteral :: String -> Doc
schemeStringLiteral ss = char '#' <> dquotes (text ss)

-- | @\\override ... #'... = #...@
override :: String -> String -> Doc -> Doc
override obj prop d = command "override" <+> text obj  <+> text "#'" 
                                         <>  text prop <+> equals 
                                         <+> char '#'  <> d




--------------------------------------------------------------------------------
-- Midi directives

-- | @\\midi { }@.
midi                  :: Doc
midi                  = command "midi" <+> braces space

-- | @\\midi {\\n ...\\n }@.
midiExpr              :: Doc -> Doc
midiExpr e            = command "midi" <+> nestBraces e


-- | @tempoWholesPerMinute = #(ly:make-moment ... ...)@.
-- - notes x note-length. e.g. 72 4 - 72 quarter notes per minute.
tempoWholesPerMinute :: Int -> Int -> Doc
tempoWholesPerMinute n nw = schemeDef "tempoWholesPerMinute" moment 
  where
    moment = parens $ text "ly:make-moment" <+> int n <+> int nw


-- | @midiMinimumVolume = #...@.
midiMinimumVolume :: Float -> Doc
midiMinimumVolume = schemeDef "midiMinimumVolume" . text . ftrunc

-- | @midiMinimumVolume = #...@.
midiMaximumVolume :: Float -> Doc
midiMaximumVolume = schemeDef "midiMaximumVolume" . text . ftrunc

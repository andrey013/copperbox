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
    note
  , pitch
  , pitchLabel
  , optDuration
  , overlay
  , rest
  , spacer
  , chordForm
  , graceForm
  , tie
  , beam
  , doubleBar
  , singleBar
  , command
  , comment
  , relative
  ) where


import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils


import Text.PrettyPrint.Leijen



note :: Pitch -> Maybe Duration -> Doc
note p md = pitch p <> optDuration md

pitch :: Pitch -> Doc 
pitch (Pitch l a o) = pitchLabel l a <> ove o where
    ove i | i > 0       = text $ replicate i       '\''
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = empty



pitchLabel :: PitchLetter -> Maybe Accidental -> Doc
pitchLabel l a = char (toLowerLChar l) <> maybe empty accidental a
  where 
    accidental :: Accidental -> Doc
    accidental Nat            = text "!"    -- check correctness
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"


optDuration :: Maybe Duration -> Doc
optDuration Nothing  = empty
optDuration (Just d) = maybe empty df $ lyRepresentation d where
  df (LyCmd ss,dc) = dots dc $ command ss 
  df (LyNum i,dc)  = dots dc $ int i


dots :: Int -> (Doc -> Doc)
dots i | i > 0     = (<> text (replicate i '.'))
       | otherwise = id



overlay :: [Doc] -> Doc
overlay = dblangles . vsep . punctuate (text " \\\\") . map spacedBraces


rest :: Maybe Duration -> Doc
rest md = char 'r' <> optDuration md

spacer :: Maybe Duration -> Doc
spacer md = char 's' <> optDuration md

chordForm :: [Doc] -> Maybe Duration -> Doc
chordForm xs md = angles (hsep xs) <> optDuration md

graceForm :: [Doc] -> Doc
graceForm [x] = command "grace" <+> braces x where
graceForm xs  = command "grace" <+> braces (beam xs)


tie :: Doc
tie = char '~'

  
-- | Beams - first element printed outside the square brackets, e.g.:
-- @ c [e g] @
--  
beam :: [Doc] -> Doc
beam (x:xs) = x <> char '[' <+> hsep xs <> char ']'
beam []     = emptyDoc


doubleBar :: Doc 
doubleBar = command "bar" <+> dquotes (text "||")

singleBar :: Doc
singleBar = text "|"


command :: String -> Doc
command = (char '\\' <>) . text 


comment :: String -> Doc
comment s = text "%{" <+> string s  <+> text "%}"



relative :: Pitch -> Doc -> Doc 
relative p expr = command "relative" <+> pitch p' <+> nestBraces expr
  where
    p' = modifyOctave ((octave p) - 4) p



nestBraces :: Doc -> Doc
nestBraces d = lbrace <$> indent 2 d <$> rbrace 


{-


direction :: Direction -> Doc 
direction Above  = char '^' 
direction Below  = char '_'
direction Center = char '-'


-- Note LilyPond drops the printed repeat start if the repeat is the first
-- element (so we don't have to).


lydocRepeat :: Doc
lydocRepeat = command "repeat" <+> text "volta 2" <+> lbrace

altStart :: Doc
altStart = space <> rbrace `nextLine` command "alternative" <+> lbrace 
                           `nextLine` lbrace

altNext :: Doc
altNext = space <> rbrace `nextLine` lbrace


endBraces :: Int -> Doc
endBraces i | i <=0     = emptyDoc
            | otherwise = indent ((i-2)*2) rbrace `nextLine` endBraces (i-1)

-}




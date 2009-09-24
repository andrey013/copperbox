{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

-- ghci ...
-- :set -i../src


module BluesE where

import Bala.BeatPattern hiding ( Beat(..) )
import Bala.Chord
import Bala.ChordDiagram
import Bala.Interval
import Bala.Mullein
import Bala.NamedPitches ( e4, a4, b4 )
import Bala.Structural
import Bala.Utils

import Mullein.LilyPond hiding ( Duration, rest, makeChord, Pitch, score )
import qualified Mullein.LilyPond               as M
import qualified Mullein.NamedElements          as M


import Data.JoinList ( JoinList, toList, join )


import Text.PrettyPrint.Leijen hiding ( dot, (<>) )



infixr 5 <<>>
(<<>>) :: JoinList a -> JoinList a -> JoinList a
(<<>>) = join

instance InterpretRest PDGlyph where
  interpretRest = mkRest

instance InterpretRest TabGlyph where
  interpretRest = makeSpacer . toDuration

--------------------------------------------------------------------------------
-- chords

eChord :: Chord
eChord = major e4 # no3 # insert octave1 # insert major10 # insert perfect12
                  # insert octave2


aChord :: Chord
aChord = major a4 # no3 # insert octave1 # insert major10 # insert perfect12


b7Chord :: Chord
b7Chord = major b4 # no5 # min7 # insert perfect8 # insert perfect12 


eChord' :: FretDiagramDef
eChord' = ("EChord", "E", makeChordDiagram [0,2,2,1,0,0])

aChord' :: FretDiagramDef
aChord' = ("AChord", "A", makeChordDiagram [x,0,2,2,2,0])

b7Chord' :: FretDiagramDef
b7Chord' = ("BSevenChord", "B7", makeChordDiagram [x,2,1,2,0,2])


--------------------------------------------------------------------------------



fret_diags :: [FretDiagramDef]
fret_diags = [eChord', aChord', b7Chord']

output1 :: IO ()
output1 = runLilyPond "blues.ly" bluesDoc


-- guitarStrings :: [PDGlyph]
-- guitarStrings = zipWith fn [e4,a4,d5,g5,b5,e6] (repeat M.wn)
--   where
--     fn p d = M.makeNote (toPitch p) () d


bass :: [StdGlyph StringNumber]
bass = zipWith ($) (toList score) (repeat M.qn) 
  where
    es      = twice $ string2 (6,4)   $ rootSeventh  eChord 
    as      = twice $ string2 (5,4)   $ rootFifth    aChord
    bs      = twice $ string2 (5,4)   $ bass2        b7Chord
    score   = es ~*  4                    <<>> 
              as ~*  2 <<>>  es ~* 2      <<>>
              bs       <<>>  as           <<>> es ~* 2

melody :: [StdGlyph StringNumber]
melody = zipWith ($) (toList score) (rhy1 ++ rhy1 ++ rhy3)
  where
    (ex,ey,ez)  = high3 eChord
    (ax,ay,az)  = high3 aChord
    (_bx,by,bz) = high3 b7Chord
    es          = strings [3,2,1,2,3]         [ex,ey,ez,ey,ex]
    aes         = strings [3,2,1,2,3]         [ax,ay,ez,ey,ex]
    b7aes       = strings [2,1,1,2,1,2]       [by,bz,az,ay,ez,ey]
    score       = es   <<>> aes  <<>> b7aes
    rhy1        = [M.wn,M.wn,M.hn,M.hn,M.wn]
    rhy3        = [M.hn,M.hn,M.hn,M.hn,M.wn,M.wn]

bluesDoc :: Doc
bluesDoc  =  version "2.12.2" 
         <^> fretDiagramDefs fret_diags
         <^> notes_def
         <^> blues_tab_def
         <^> book (scoreExpr (staff_group_doc <$> layout <$> midi_spec))
  where
    midi_spec = midiContextScoreTempo 120 4


notes_def :: Doc
notes_def = variableDef "Blues" $
    relative M.middle_c (key M.e_nat "major" 
                             <$> time' M.four_four_time
                             <$> tune)
  where
    tune    = simpleOutput $ renderPhrase lyGlyph
                           $ rewritePitch M.middle_c 
                           $ rewriteDuration 
                           $ overlayNoteLists (meterPattern M.four_four_time) 
                                              [melody, bass]




blues_tab_def :: Doc
blues_tab_def = chordBassTabDef (M.e_nat, "major")
                                M.four_four_time
                                ("BluesTabMelody",  melody)
                                ("BluesTabBass",    bass)
                                



staff_group_doc :: Doc
staff_group_doc = newStaffGroup $ simultaneous [noteStaff, tabStaff]
  where 
    noteStaff = newStaff    $ command "Blues"
    tabStaff  = newTabStaff $ nestBraces (simultaneous [overrides,tv1,tv2])
    tv1       = contextTabVoice "upper" (command "BluesTabMelody")
    tv2       = contextTabVoice "lower" (command "BluesTabBass")



overrides :: Doc
overrides = vsep [o1,o2] where
  o1 = override "TabStaff.Stem" "transparent" lyTrue
  o2 = override "TabStaff.Beam" "transparent" lyTrue


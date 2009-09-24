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
import Bala.Duration
import Bala.Interval
import Bala.MelodyPattern
import Bala.Mullein
import Bala.NamedPitches ( e4, a4, b4, d5, g5, b5, e6 )
import Bala.Pitch hiding ( PitchLetter(..) )
import Bala.Utils

import Mullein.LilyPond hiding ( Duration, rest, makeChord, Pitch )
import qualified Mullein.LilyPond               as M
import qualified Mullein.NamedElements          as M


import Data.JoinList ( JoinList, fromList, toList, join, wrap )


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

guitarStrings :: [PDGlyph]
guitarStrings = zipWith fn [e4,a4,d5,g5,b5,e6] (repeat M.wn)
  where
    fn p d = M.makeNote (toPitch p) () d


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
    (ex,ey,ez) = high3 eChord
    (ax,ay,az) = high3 aChord
    (bx,by,bz) = high3 b7Chord
    es         = strings [3,2,1,2,3]         [ex,ey,ez,ey,ex]
    aes        = strings [3,2,1,2,3]         [ax,ay,ez,ey,ex]
    b7aes      = strings [2,1,1,2,1,2]       [by,bz,az,ay,ez,ey]
    score      = es   <<>> aes  <<>> b7aes
    rhy1       = [M.wn,M.wn,M.hn,M.hn,M.wn]
    rhy3       = [M.hn,M.hn,M.hn,M.hn,M.wn,M.wn]


type GlyphF anno = M.Duration -> StdGlyph anno


unpair :: (a,a) -> JoinList a
unpair = (uncurry join) . dist wrap

anno1 :: a -> Pitch -> GlyphF a
anno1 = flip $ M.makeNote . toPitch

anno2 :: (a,a) -> (Pitch,Pitch) -> (GlyphF a, GlyphF a)
anno2 = (uncurry prod) . dist anno1

annos :: [a] -> [Pitch] -> [GlyphF a]
annos = matchZipWith anno1


string1 :: StringNumber -> Pitch -> JoinList (GlyphF StringNumber)
string1 = wrap `oo` anno1

string2 :: (StringNumber,StringNumber) 
        -> (Pitch,Pitch) 
        -> JoinList (GlyphF StringNumber)
string2 = unpair `oo` anno2


strings :: [StringNumber] -> [Pitch] -> JoinList (GlyphF StringNumber)
strings = fromList `oo` annos


rootFifth :: Chord -> (Pitch,Pitch)
rootFifth ch = (chordRoot ch, maybe err id $ chordFifth ch)
  where
    err = error $ "Chord " ++ show ch ++ " has no fifth."


rootSeventh :: Chord -> (Pitch,Pitch)
rootSeventh ch = (chordRoot ch, maybe err id $ nthTone 8 ch)
  where
    err = error $ "Chord " ++ show ch ++ " has no eighth (octave)."



bass2 :: Chord -> (Pitch,Pitch)
bass2 ch = (chordRoot ch, fn ch) where
  fn = maybe (error $ "bass2 - chord " ++ show ch ++ " has no third") id . chordThird
  
high3 :: Chord -> (Pitch,Pitch,Pitch) 
high3 = step . pitchContent where
  step [x,y,z] = (x,y,z)
  step (_:xs)  = step xs
  step _       = error $ "high3 - too few tones in chord."
  

bluesDoc :: Doc
bluesDoc  =  version "2.12.2" 
         <^> fretDiagramDefs fret_diags
         <^> notes_def
         <^> blues_tab_def
         <^> book (scoreExpr (staff_group_doc <$> layout <$> midi))


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


{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Mullein.LilyPond
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- LilyPond stuff
--
--------------------------------------------------------------------------------

module Bala.Mullein.LilyPond
  ( 
  -- * Write a file and render to LilyPond 
    runLilyPond 

  -- * Guitar chords / fret diagrams
  , LyGuitarChord(..)
  , chordDiagramDef

  -- * Document templates
  , tabPartDef
  , chordBassTabDef
  , fretDiagramPictures

  ) where


import Bala.Chord
import Bala.ChordDiagram
import Bala.Utils

import Mullein.LilyPond

import System.Process ( system )
import Text.PrettyPrint.Leijen


--------------------------------------------------------------------------------
-- Duration




runLilyPond :: FilePath -> Doc -> IO ()
runLilyPond path doc = do 
  writeDoc path doc
  system $ "lilypond " ++ path
  return ()


 
--------------------------------------------------------------------------------
-- LilyPond


data LyGuitarChord = LyGuitarChord { 
      getChordName    :: String,
      getChordAlias   :: String, 
      getChord        :: Chord, 
      getFretDiagram  :: ChordDiagram 
    }


chordDiagramDef :: LyGuitarChord -> Doc
chordDiagramDef (LyGuitarChord name alias _ diag) = 
    comment name <$> variableDef alias (fn diag)
  where
    fn = markup . fretDiagram . standardMarkup


----------------------------------------------------------------------------------
-- Templates


-- TODO [TabGlyph] too specific

tabPartDef  :: String 
            -> (PitchLabel,String) 
            -> (Int,Int) 
            -> Rational 
            -> (Maybe Doc) 
            -> [TabGlyph] 
            -> Doc
tabPartDef name (p,mode) (n,d) barlen mbvoice glyphs = 
    variableDef name (nestBraces ( key p mode <$> time n d <$*> mbvoice
                                              <$> mkContent glyphs))
  where 
    mkContent = simpleOutput . renderPhrase lyTabGlyph 
                             . rewriteDuration 
                             . rewritePitchAbs (-5)
                             . phraseNoPulses barlen


chordBassTabDef :: (String,[TabGlyph]) -> (String,[TabGlyph])
                -> (PitchLabel,String)
                -> (Int,Int)
                -> Rational
                -> Doc
chordBassTabDef (chname,chglyphs) (bsname,bsglyphs) keydesc timedesc barlen = 
      tabPartDef chname keydesc timedesc barlen (Just voiceOne) chglyphs
  <^> tabPartDef bsname keydesc timedesc barlen (Just voiceTwo) bsglyphs


fretDiagramPictures :: String -> Rational -> [SpacerGlyph] -> Doc
fretDiagramPictures name barlen spacers = 
    variableDef name (nestBraces $ mkContent spacers)
  where
    mkContent = simpleOutput . renderPhrase lySpacerGlyph
                             . rewriteDuration 
                             . phraseNoPulses barlen


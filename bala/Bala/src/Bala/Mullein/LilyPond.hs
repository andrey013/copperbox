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

  ) where


import Bala.Chord
import Bala.ChordDiagram

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

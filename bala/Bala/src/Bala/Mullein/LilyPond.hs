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

  -- * Shorthand
  , time'

  -- * Guitar chords / fret diagrams
  , DefName
  , ChordName
  , FretDiagramDef
  , fretDiagramDef
  , fretDiagramDefs

  -- * Document templates
  , tabPartDef
  , chordBassTabDef
  , fretDiagramPictures
  , midiContextScoreTempo

  ) where


import Bala.ChordDiagram hiding ( x )
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


-- | @\\time ... @ - extracts the time signature from a 
-- MetricalSpec 
time' :: MetricalSpec -> Doc
time' = time . timeSignature
 
--------------------------------------------------------------------------------
-- LilyPond guitar notation


type DefName        = String
type ChordName      = String

type FretDiagramDef = (DefName,ChordName,ChordDiagram)



fretDiagramDef :: DefName -> ChordName -> ChordDiagram -> Doc
fretDiagramDef defname propername diag = 
    comment propername <$> variableDef defname (fn diag)
  where
    fn = markup . fretDiagram . standardMarkup

fretDiagramDefs :: [FretDiagramDef] -> Doc
fretDiagramDefs = vsepsep . map (\(x,y,z) -> fretDiagramDef x y z)

-- uncurry3 ?

----------------------------------------------------------------------------------
-- Templates


-- TODO [TabGlyph] too specific

tabPartDef  :: String 
            -> (PitchLabel,String) 
            -> MetricalSpec
            -> (Maybe Doc) 
            -> [TabGlyph] 
            -> Doc
tabPartDef name (p,mode) mspec mbvoice glyphs = 
    variableDef name (nestBraces ( key p mode <$>  (time $ timeSignature mspec) 
                                              <$*> mbvoice
                                              <$>  mkContent glyphs))
  where 
    mkContent = simpleOutput . renderPhrase lyTabGlyph 
                             . rewriteDuration 
                             . rewritePitchAbs (-5)
                             . phraseNoPulses (sum $ meterPattern mspec)


chordBassTabDef :: (PitchLabel,String)
                -> MetricalSpec
                -> (String,[TabGlyph]) 
                -> (String,[TabGlyph])
                -> Doc
chordBassTabDef keydesc mspec (chname,chglyphs) (bsname,bsglyphs) = 
      tabPartDef chname keydesc mspec (Just voiceOne) chglyphs
  <^> tabPartDef bsname keydesc mspec (Just voiceTwo) bsglyphs


fretDiagramPictures :: String -> Rational -> [SpacerGlyph] -> Doc
fretDiagramPictures name barlen spacers = 
    variableDef name (nestBraces $ mkContent spacers)
  where
    mkContent = simpleOutput . renderPhrase lySpacerGlyph
                             . rewriteDuration 
                             . phraseNoPulses barlen

-- MidiExpr

-- | midi context with a tempo directive: 
-- @ 
-- \\midi { 
--    \\context { 
--       \\Score 
--         tempoWholesPerMinute = #(ly:makeMoment ... ...)
--    }
-- }
-- @ 
midiContextScoreTempo :: Int -> Int -> Doc
midiContextScoreTempo n nw = midiExpr $ contextExpr doc where
  doc = command "Score" <$> tempoWholesPerMinute n nw
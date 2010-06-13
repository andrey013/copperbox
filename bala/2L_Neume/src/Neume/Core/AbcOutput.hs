{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.AbcOutput
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print ABC.
--
--------------------------------------------------------------------------------


module Neume.Core.AbcOutput 
  (

    AbcOutput(..)
  , runRender
  
  , renderGlyph
  ) where

import Neume.Core.AbcPretty
import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.ModularSyntax
import Neume.Core.Pitch hiding ( octave )
import Neume.Core.Utils.OneList ( OneList, toListF )
import Neume.Core.Utils.Pretty

import Text.PrettyPrint.Leijen hiding ( sep, (<$>) )     -- package: wl-print

import MonadLib.Monads                  -- package: monadLib

import Control.Applicative hiding ( empty ) 
import qualified Data.Foldable as F


--------------------------------------------------------------------------------



type OutputAbc gly a = Reader (gly -> Doc) a

class AbcOutput repr where
   renderAbcPhraseImage :: repr gly -> OutputAbc gly PhraseImage


runRender :: AbcOutput repr => (gly -> Doc) -> repr gly -> PhraseImage
runRender f = runReader f . renderAbcPhraseImage


type NoteSep = Doc -> Doc -> Doc


-- Just instances for the Phrase formats...

instance AbcOutput Full where
  renderAbcPhraseImage (Full (Phrase name bars)) = 
      Phrase name <$> mapM oBarMD bars 

instance AbcOutput Undiv where
  renderAbcPhraseImage (Undiv (Phrase name bars)) =
      Phrase name <$> mapM oBar bars 


instance AbcOutput Unmetered where
  renderAbcPhraseImage (Unmetered (Phrase name mds)) =
      (\mds' -> Phrase name [mds']) <$> oMetricalDivs (<+>) mds



oBarMD :: Bar (MetricalDiv gly) -> OutputAbc gly BarImage
oBarMD bar = oMetricalDivs (<+>) bar 

oBar :: Bar gly -> OutputAbc gly BarImage
oBar bar = (\f -> hsep $ map f bar) <$> ask


-- MetricalDiv\'s need some extra 'context' - beamed notes have 
-- no whitespace between them, so must be printed with (<>).
--

oMetricalDivs :: NoteSep -> [MetricalDiv gly] -> OutputAbc gly Doc
oMetricalDivs sep xs = sepList sep <$> mapM (oMetricalDiv sep) xs

oMetricalDiv :: NoteSep -> MetricalDiv gly -> OutputAbc gly Doc
oMetricalDiv _  (Atom e)       = (\f -> f e) <$> ask
oMetricalDiv _  (Beamed notes) = oMetricalDivs (<>) notes
oMetricalDiv op (N_Plet pm xs) = (doc1 <+>) <$> oMetricalDivs op xs
  where
    doc1 = pletContext $ pletStats pm (length xs)



pletStats :: PletMult -> Int -> (Int,Int,Int)
pletStats (n,d) len = (fromIntegral d, fromIntegral n,len)



renderGlyph :: Glyph anno Pitch AbcMultiplier -> Doc
renderGlyph = oGlyph

oGlyph :: Glyph anno Pitch AbcMultiplier -> Doc
oGlyph (GlyNote n d t)      = oNote n d <> tied t
oGlyph (Rest dm)            = rest dm
oGlyph (Spacer dm)          = spacer dm
oGlyph (Chord ps dm t)      = (chordForm $ oChordPitches dm ps) <> tied t
oGlyph (Graces xs)          = graceForm $ toListF oGraceNote xs


oNote :: Note anno Pitch -> AbcMultiplier -> Doc
oNote (Note _ p) dm = note p dm

oGraceNote :: GraceNote anno Pitch AbcMultiplier -> Doc
oGraceNote (GraceNote _ p dm) = note p dm


-- print each note with the same multiplier
--
-- > [C8E8G8]
--
oChordPitches :: AbcMultiplier -> OneList (Note anno Pitch) -> [Doc]
oChordPitches dm = map (\(Note _ p) -> note p dm) . F.toList


tied :: Tie -> Doc
tied NoTie = empty
tied Tie   = tie

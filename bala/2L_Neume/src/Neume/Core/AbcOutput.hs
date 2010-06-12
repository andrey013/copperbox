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
import Neume.Core.ModularSyntax
import Neume.Core.Pitch hiding ( octave )
import Neume.Core.Utils.OneList ( OneList, toListF )
import Neume.Core.Utils.Pretty

import Text.PrettyPrint.Leijen hiding ( sep )     -- package: wl-print

import qualified Data.Foldable as F


--------------------------------------------------------------------------------



type OutputAbc gly a = (gly -> Doc) -> a

class AbcOutput repr where
   renderAbcPhraseImage :: repr gly -> OutputAbc gly PhraseImage


runRender :: AbcOutput repr => (gly -> Doc) -> repr gly -> PhraseImage
runRender f = renderAbcPhraseImage `flip` f


type NoteSep = Doc -> Doc -> Doc


-- Just instances for the Phrase formats...

instance AbcOutput Full where
  renderAbcPhraseImage (Full (Phrase name bars)) = \f -> 
      Phrase name $ map (oBarMD `flip` f) bars 

instance AbcOutput Undiv where
  renderAbcPhraseImage (Undiv (Phrase name bars)) = \f -> 
      Phrase name $ map (oBar `flip` f) bars 


instance AbcOutput Unmetered where
  renderAbcPhraseImage (Unmetered (Phrase name mds)) = \f -> 
      Phrase name [oMetricalDivs (<+>) mds f]



oBarMD :: Bar (MetricalDiv gly) -> OutputAbc gly BarImage
oBarMD bar = \f -> oMetricalDivs (<+>) bar f 

oBar :: Bar gly -> OutputAbc gly BarImage
oBar bar = \f -> hsep $ map f bar


-- MetricalDiv\'s need some extra 'context' - beamed notes have 
-- no whitespace between them, so must be printed with (<>).
--

oMetricalDivs :: NoteSep -> [MetricalDiv gly] -> OutputAbc gly Doc
oMetricalDivs sep xs = \f -> 
    sepList sep $ map (oMetricalDiv sep `flip` f) xs

oMetricalDiv :: NoteSep -> MetricalDiv gly -> OutputAbc gly Doc
oMetricalDiv _  (WrapMD (Atom e))       = \f -> f e
oMetricalDiv op (WrapMD (N_Plet pm xs)) = \f -> 
    pletContext (pletStats pm (length xs)) <+> oMetricalDivs op xs f
oMetricalDiv _  (WrapMD (Beamed notes)) = oMetricalDivs (<>) notes


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

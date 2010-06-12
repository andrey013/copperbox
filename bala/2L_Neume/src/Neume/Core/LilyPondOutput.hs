{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.LilyPondOutput
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print LilyPond
--
--------------------------------------------------------------------------------


module Neume.Core.LilyPondOutput 
  (
    LilyPondOutput(..)  
  , runRender
  , renderGlyph
  , renderGraphic
  ) where

import Neume.Core.Duration
import Neume.Core.LilyPondPretty
import Neume.Core.ModularSyntax
import Neume.Core.Utils.OneList
import Neume.Core.Utils.Pretty

import Text.PrettyPrint.Leijen          -- package: wl-pprint

import qualified Data.Foldable          as F



--------------------------------------------------------------------------------
-- Render



-- Note - for LilyPond percussion we might want either the long 
-- or short name printing, so renderGlyph isn't a good 
-- candidate for a Type Class.



type OutputLy gly a = (gly -> Doc) -> a


class LilyPondOutput repr where
   renderLyPhraseImage :: repr gly -> OutputLy gly PhraseImage


runRender :: LilyPondOutput repr => (gly -> Doc) -> repr gly -> PhraseImage
runRender f = renderLyPhraseImage `flip` f


-- Just instances for the Phrase formats...

instance LilyPondOutput Full where
  renderLyPhraseImage (Full (Phrase name bars)) = \f -> 
      Phrase name $ map (oBarMD `flip` f) bars 

instance LilyPondOutput Undiv where
  renderLyPhraseImage (Undiv (Phrase name bars)) = \f -> 
      Phrase name $ map (oBar `flip` f) bars 


instance LilyPondOutput Unmetered where
  renderLyPhraseImage (Unmetered (Phrase name mds)) = \f -> 
      Phrase name [hsep $ oMetricalDivs mds f]



oBarMD :: Bar (MetricalDiv gly) -> OutputLy gly BarImage
oBarMD bar = \f -> hsep $ oMetricalDivs bar f 

oBar :: Bar gly -> OutputLy gly BarImage
oBar bar = \f -> hsep $ map f bar


oMetricalDivs ::  [MetricalDiv gly] -> OutputLy gly [Doc]
oMetricalDivs xs = \f -> map (oMetricalDiv `flip` f) xs

oMetricalDiv :: MetricalDiv gly -> OutputLy gly Doc
oMetricalDiv (WrapMD (Atom e))       = \f -> f e 
oMetricalDiv (WrapMD (N_Plet mp xs)) = pletForm mp . oMetricalDivs xs
oMetricalDiv (WrapMD (Beamed notes)) = beamForm . oMetricalDivs notes 

-- annos generally printed _after_ duration...

renderGlyph :: (pch -> Doc) -> (anno -> DocS) 
            -> Glyph anno pch (Maybe Duration) 
            -> Doc
renderGlyph = oGlyph



-- this is a hack to get anno as a suffix - needs improving ...
oGlyph :: (pch -> Doc) -> (anno -> DocS) -> Glyph anno pch (Maybe Duration) -> Doc
oGlyph f g (GlyNote n d t)  = oNote f g d n <> tied t
oGlyph _ _ (Rest d)         = rest d
oGlyph _ _ (Spacer d)       = spacer d
oGlyph f g (Chord ps d t)   = chordForm (toListF (oNote f g Nothing) ps) d <> tied t
oGlyph f g (Graces os)      = graceForm $ oGraceNotes f g os



oNote :: (pch -> Doc) -> (anno -> DocS) -> Maybe Duration -> Note anno pch -> Doc
oNote f g od (Note a p) = g a (f p <> maybe empty duration od) 

oGraceNotes :: (pch -> Doc) 
            -> (anno -> DocS)
            -> OneList (GraceNote anno pch (Maybe Duration)) 
            -> [Doc]
oGraceNotes f g = map gf . F.toList where
  gf (GraceNote a p d) = g a (f p <> maybe empty duration d)



tied :: Tie -> Doc
tied NoTie = empty
tied Tie   = tie

renderGraphic :: (gly -> Maybe Duration -> Doc) 
              -> Graphic gly (Maybe Duration) 
              -> Doc
renderGraphic f (Graphic g d) = f g d
renderGraphic _ (Skip d)      = spacer d 



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

import Text.PrettyPrint.Leijen hiding ( (<$>) )      -- package: wl-pprint

import MonadLib.Monads                  -- package: monadLib

import Control.Applicative hiding ( empty )
import qualified Data.Foldable          as F



--------------------------------------------------------------------------------
-- Render



-- Note - for LilyPond percussion we might want either the long 
-- or short name printing, so renderGlyph isn't a good 
-- candidate for a Type Class.



type OutputLy gly a = Reader (gly -> Doc) a


class LilyPondOutput repr where
   renderLyPhraseImage :: repr gly -> OutputLy gly PhraseImage


runRender :: LilyPondOutput repr => (gly -> Doc) -> repr gly -> PhraseImage
runRender f = runReader f . renderLyPhraseImage


-- Just instances for the Phrase formats...

instance LilyPondOutput Full where
  renderLyPhraseImage (Full (Phrase name bars)) = 
      Phrase name <$> mapM oBarMD bars 

instance LilyPondOutput Undiv where
  renderLyPhraseImage (Undiv (Phrase name bars)) =
      Phrase name <$> mapM oBar bars 


instance LilyPondOutput Unmetered where
  renderLyPhraseImage (Unmetered (Phrase name mds)) =
      (\mds' -> Phrase name [hsep mds']) <$> oMetricalDivs mds



oBarMD :: Bar (MetricalDiv gly) -> OutputLy gly BarImage
oBarMD bar = hsep <$> oMetricalDivs bar

oBar :: Bar gly -> OutputLy gly BarImage
oBar bar = (\f -> hsep $ map f bar) <$> ask


oMetricalDivs ::  [MetricalDiv gly] -> OutputLy gly [Doc]
oMetricalDivs = mapM oMetricalDiv 

oMetricalDiv :: MetricalDiv gly -> OutputLy gly Doc
oMetricalDiv (Atom e)       = (\f -> f e) <$> ask
oMetricalDiv (N_Plet mp xs) = pletForm mp <$> oMetricalDivs xs
oMetricalDiv (Beamed notes) = beamForm    <$> oMetricalDivs notes 

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



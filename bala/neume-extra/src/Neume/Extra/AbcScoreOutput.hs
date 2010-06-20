{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.AbcScoreOutput
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Output the Score representation.
--
--------------------------------------------------------------------------------

module Neume.Extra.AbcScoreOutput
  (

    AbcImageAlg(..)
  , abcImageScore
  , stdAbcAlg

  , barNumber


  ) where

import Neume.Core.Duration
import Neume.Core.AbcOutput
import Neume.Core.AbcTrafo
import Neume.Core.Pitch
import Neume.Core.SpellingMap
import Neume.Core.Syntax
import Neume.Core.Utils.Pretty

import Neume.Extra.AbcDoc
import Neume.Extra.Common
import Neume.Extra.ScoreSyntax

import MonadLib                         -- package: monadLib
import Text.PrettyPrint.Leijen          -- package: wl-pprint



-- Note - this needs to be at the type of @Score@ so that 
-- relative-pitch transformations can be statefully chained.
-- 

data AbcImageAlg repr gly gly' = AbcImageAlg
      { glyph_printer   :: gly' -> Doc
      , duration_trafo  :: forall shape. 
                           Score shape (repr gly) -> Score shape (repr gly')
      , pitch_trafo     :: forall shape.
                           Score shape (repr gly) -> Score shape (repr gly)
      }

abcImageScore :: AbcOutput repr 
              => AbcImageAlg repr gly gly' 
              -> Score shape (repr gly) 
              -> Score shape PhraseImage
abcImageScore (AbcImageAlg 
    { glyph_printer  = pp
    , duration_trafo = df
    , pitch_trafo    = pf }) = fmap (runRender pp) . df . pf



stdAbcAlg :: (AbcPitchSpellTrafo repr, AbcDurMultTrafo repr)
          => AbcSpellingMap 
          -> Rational
          -> AbcImageAlg repr (Glyph anno Pitch Duration)
                              (Glyph anno Pitch AbcMultiplier)
stdAbcAlg spell_map unit_dur = AbcImageAlg
    { glyph_printer     = renderGlyph
    , duration_trafo    = fmap (runDurMultTrafo    unit_dur)
    , pitch_trafo       = fmap (runPitchSpellTrafo spell_map)
    }



--------------------------------------------------------------------------------


barNumber :: BarNum -> DocS
barNumber i = (abcComment ("Bar " ++ show i) <$>)


-- Interspersing bars:
--
-- 1. The first bar should not be prefixed - even if it is a 
--    repeat.
--
-- 2. A 'straight' can be printed with at bar-line after the last
--    bar, regardless of what follows it. 
--     
-- 3. A repeat-start can start a line.
--
-- 4. Back-to-back repeats can print :| on one line and |: on the
--    next line.
--
-- 5. Alternative repeats - each repeat is started with its number 
--    e.g. [1 . The initial repeats are terminated with :| , the 
--    final repeat is terminated with ||
--
-- 6. A tune should end with || or :|
--

-- The start and end are index-sensitive:
-- 
-- 1. Don't print repeat-start on the first bar.
-- 
-- 2. Change | to || on last bar (leave repeat as is)
--
-- 3. Don't print LINE_CONT on last bar
--


{-

-- | Default bar numbering function.
--
barNumber :: BarNum -> DocS
barNumber i = ((text $ "%% Bar " ++ show i) <$>)



type ScoreM a = StateT BarNum (ReaderT BarNumF Id) a

runScoreM :: BarNumF -> BarNum -> ScoreM a -> a
runScoreM f n mf = fst $ runId $ runReaderT f $ runStateT n mf


-- anacrusis can start with barnum=0...

-- | A single linear score representation...
--
inlineScore :: BarNumF -> BarNum -> Score shape PhraseImage -> Doc
inlineScore f n sc = runScoreM f n $ renderInline sc


renderInline :: Score shape PhraseImage -> ScoreM Doc
renderInline Nil              = return empty

renderInline (Linear e xs)    = do { d1  <- concatPhraseImage e
                                   ; d2  <- renderInline xs
                                   ; return $ d1 <$> d2 
                                   }
 
renderInline (Repeat e xs)    = do { d1  <- concatPhraseImage e
                                   ; d2  <- renderInline xs
                                   ; return $ (repeatvolta 2 d1) <$> d2
                                   }
                                 
renderInline (RepAlt e es xs) = do { d1  <- concatPhraseImage e
                                   ; d2  <- mapM concatPhraseImage es
                                   ; d3  <- renderInline xs
                                   ; let n = length es
                                   ; return $  (repeatvolta n d1) 
                                           <$> (alternative d2)
                                           <$> d3
                                   }
                    

-- This was at the wrong type - its giving a new name and 
-- transformer to each element in the alternatives list
--
-- In reality we want to be zipping across both a score and
-- a "(name x transformer) list" with the same shape.
--
-- Solution... ScorePlan.
--
           
type PhraseTransformer = DocS

type DefinitionsElement = (VarName,PhraseTransformer)

defnsScore :: ScorePlan shape DefinitionsElement -> Doc
defnsScore PNil = empty
defnsScore (PLinear (n,_) ps) = variableUse n <$> defnsScore ps
defnsScore (PRepeat (n,_) ps) = variableUse n <$> defnsScore ps
defnsScore (PRepAlt (n,_) ps) = variableUse n <$> defnsScore ps


defnsDefns :: BarNumF -> BarNum 
           -> ScorePlan shape DefinitionsElement 
           -> Score     shape PhraseImage
           -> Doc
defnsDefns f n sp sc = runScoreM f n $ renderDefns sp sc


renderDefns :: ScorePlan shape DefinitionsElement 
            -> Score     shape PhraseImage
            -> ScoreM Doc
renderDefns PNil               Nil              = return empty

renderDefns (PLinear (n,f) ps) (Linear e xs)    = do 
    { d1 <- concatPhraseImage e
    ; d2 <- renderDefns ps xs
    ; let def1 = variableDef n $ f d1
    ; return (def1 <$> d2)
    }

renderDefns (PRepeat (n,f) ps) (Repeat e xs)    = do
    { d1 <- concatPhraseImage e
    ; d2 <- renderDefns ps xs
    ; let def1 = variableDef n (f $ repeatvolta 2 d1)
    ; return (def1 <$> d2)
    }

renderDefns (PRepAlt (n,f) ps) (RepAlt e es xs) = do
    { d1 <- concatPhraseImage e
    ; d2  <- mapM concatPhraseImage es
    ; d3  <- renderDefns ps xs 
    ; let def1 = variableDef n 
                   (f $ (repeatvolta (length es) d1 <$> alternative d2))
    ; return (def1 <$> d3)
    }

renderDefns _                  _                = 
    error "renderDefns - impossible, type level shape should stop this."




concatPhraseImage :: PhraseImage -> ScoreM Doc
concatPhraseImage (Phrase xs) = liftM vsep (mapM barImage xs)

barImage :: BarImage -> ScoreM Doc
barImage d = sets (\s -> (s,s+1)) >>= \n  ->
             ask                  >>= \f  ->
             return (f n $ d <+> singleBar)

-}
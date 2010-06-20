{-# LANGUAGE GADTs                      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.LilyPondScoreOutput
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

module Neume.Extra.LilyPondScoreOutput
  (

    BarNum
  
  , barNumber
  , inlineScore

  , DefinitionsElement
  , defnsScore
  , defnsDefns

  ) where

import Neume.Core.Syntax
import Neume.Core.Utils.Pretty

import Neume.Extra.LilyPondDoc
import Neume.Extra.ScoreSyntax

import MonadLib                         -- package: monadLib
import Text.PrettyPrint.Leijen          -- package: wl-pprint

type BarNum = Int

type BarNumF = BarNum -> DocS

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
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

-- anacrusis can start with barnum=0...

inlineScore :: BarNumF -> BarNum -> Score shape PhraseImage -> Doc
inlineScore f n sc = runScoreM f n $ renderInline sc


runScoreM :: BarNumF -> BarNum -> ScoreM a -> a
runScoreM f n mf = fst $ runId $ runReaderT f $ runStateT n mf



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
                               

concatPhraseImage :: PhraseImage -> ScoreM Doc
concatPhraseImage (Phrase xs) = liftM vsep (mapM barImage xs)

barImage :: BarImage -> ScoreM Doc
barImage d = sets (\s -> (s,s+1)) >>= \n  ->
             ask                  >>= \f  ->
             return (f n $ d <+> singleBar)
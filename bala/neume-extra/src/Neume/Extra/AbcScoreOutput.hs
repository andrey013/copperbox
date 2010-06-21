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

  , AbcBarNumF
  , barNumber

  , inlineScore         -- needs new name

  ) where

import Neume.Core.Duration
import Neume.Core.AbcOutput
import Neume.Core.AbcTrafo
import Neume.Core.Pitch
import Neume.Core.SpellingMap
import Neume.Core.Syntax
import Neume.Core.Utils.Stream ( Stream(..) )

import Neume.Extra.AbcDoc
import Neume.Extra.Common
import Neume.Extra.ScoreSyntax

import MonadLib                         -- package: monadLib
import Text.PrettyPrint.Leijen          -- package: wl-pprint

import Prelude hiding ( repeat )

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

type AbcBarNumF = BarNum -> Maybe Doc

barNumber :: AbcBarNumF
barNumber i = Just $ abcComment $ "Bar " ++ show i

type AbcLineWidths = Stream Int


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





type ScoreM a = StateT BarNum (ReaderT AbcBarNumF Id) a

runScoreM :: AbcBarNumF -> BarNum -> ScoreM a -> a
runScoreM f n mf = fst $ runId $ runReaderT f $ runStateT n mf



-- anacrusis can start with barnum=0...

-- | A single linear score representation...
--
inlineScore :: AbcBarNumF -> BarNum -> Score shape PhraseImage -> Doc
inlineScore f n sc = vsep $ contTraf $ runScoreM f n $ renderInline sc
  where
    contTraf = id -- TODO


renderInline :: Score shape PhraseImage -> ScoreM [Doc]
renderInline Nil              = return []

renderInline (Linear e xs)    = do 
    { zs  <- phraseImages e
    ; ds  <- renderInline xs
    ; case ds of 
        [] -> return $ linearFinal zs
        _  -> return $ linear zs ++ ds
    }
 
renderInline (Repeat e xs)    = do 
    { start <- initialBar
    ; zs    <- phraseImages e
    ; ds    <- renderInline xs
    ; case start of
        True -> return $ repeatInitial zs ++ ds
        _    -> return $ repeat zs ++ ds
    }

{-                                 
renderInline (RepAlt e es xs) = do { d1  <- concatPhraseImage e
                                   ; d2  <- mapM concatPhraseImage es
                                   ; d3  <- renderInline xs
                                   ; let n = length es
                                   ; return $  (repeatvolta n d1) 
                                           <$> (alternative d2)
                                           <$> d3
                                   }
                    
-}

initialBar :: ScoreM Bool
initialBar = liftM (<=1) get


phraseImages :: PhraseImage -> ScoreM [(Maybe Doc,Doc)]
phraseImages (Phrase xs) = mapM barImage xs


barImage :: BarImage -> ScoreM (Maybe Doc, Doc)
barImage d = sets (\s -> (s,s+1)) >>= \n  ->
             ask                  >>= \f  ->
             return (f n, d)


linear :: [(Maybe Doc,Doc)] -> [Doc]
linear = map fn where
  fn (c, d) = mbLines c d <+> singleBar  

linearFinal :: [(Maybe Doc,Doc)] -> [Doc]
linearFinal []      = []
linearFinal (x:xs)  = step x xs
  where
    step (c,d) []       = [mbLines c d <+> doubleBar]
    step (c,d) (y:ys)   = (mbLines c d <+> singleBar) : step y ys
    


repeat :: [(Maybe Doc,Doc)] -> [Doc]
repeat []         = []
repeat ((s,t):xs) = intraRepeat (s, lrepeat <+> t) xs

repeatInitial :: [(Maybe Doc,Doc)] -> [Doc]
repeatInitial []         = []
repeatInitial ((s,t):xs) = intraRepeat (s, t) xs   -- no initial repeat symbol


intraRepeat :: (Maybe Doc,Doc) -> [(Maybe Doc,Doc)] -> [Doc]
intraRepeat (c,d) []     = [mbLines c d <+> rrepeat]
intraRepeat (c,d) (y:ys) = (mbLines c d <+> singleBar) : intraRepeat y ys 


mbLines :: Maybe Doc -> Doc -> Doc
mbLines Nothing  d     = d
mbLines (Just c) d     = c <$> d





{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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

  , overlay2

  , AbcLineWidths
  , four_bars_per_line
  , lineWidths 

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
import Neume.Core.Utils.Pretty ( DocS )
import qualified Neume.Core.Utils.Stream        as S
import Neume.Core.Utils.Stream ( Stream(..) )
import Neume.Core.Utils.HList
import Neume.Extra.AbcDoc
import Neume.Extra.Common
import Neume.Extra.ScoreSyntax

import MonadLib                         -- package: monadLib
import Text.PrettyPrint.Leijen          -- package: wl-pprint

import qualified Prelude as Pre
import Prelude hiding ( repeat, length )

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

type AbcPhraseOverlay = Phrase Doc

overlay2 :: Score sh PhraseImage 
         -> Score sh PhraseImage 
         -> Score sh AbcPhraseOverlay
overlay2 = scoreZipWith f
  where 
    f (Phrase xs) (Phrase ys) = Phrase $ longZipWith g xs ys
    g d1 d2                   = d1 <+> overlay <$> d2

--------------------------------------------------------------------------------

type AbcBarNumF = BarNum -> DocS

barNumber :: AbcBarNumF
barNumber i = \d -> (abcComment $ "Bar " ++ show i) <$> d

type AbcLineWidths = Stream Int

four_bars_per_line :: AbcLineWidths
four_bars_per_line = S.repeat 4

-- repeat last in list forever...

lineWidths :: [Int] -> AbcLineWidths
lineWidths [] = error "lineWidths - empty list."
lineWidths xs = S.trail xs

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


data LeftPunct = LP_NONE | LP_REPEAT | LP_ALT Int
  deriving (Eq)

data RightPunct = RP_SGL | RP_DBL | RP_REPEAT
  deriving (Eq)

-- No - this structure doesn't allow easy adding of the 
-- punctuation tip to the last bar
--
data CollectBuffer = CollectBuffer 
      { cb_accumulator  :: H Doc
      , right_tip       :: Maybe (Doc, RightPunct)
      , next_left_punct :: LeftPunct                    -- usually LP_NONE
      }


bufferZero :: CollectBuffer
bufferZero = CollectBuffer emptyH Nothing LP_NONE

finalize :: CollectBuffer -> [Doc]
finalize (CollectBuffer acc Nothing      _) = toListH acc
finalize (CollectBuffer acc (Just (d,r)) _) = 
    toListH $ acc `snocH` (suffixRP d $ promote r)
  where
    promote RP_SGL = RP_DBL
    promote z      = z

newtype ScoreM a = ScoreM { getScoreM :: StateT CollectBuffer 
                                        (StateT BarNum 
                                        (ReaderT AbcBarNumF Id)) a }

instance Functor ScoreM where
  fmap f = ScoreM . fmap f . getScoreM

instance Monad ScoreM where
  return a = ScoreM $ return a
  f >>= k  = ScoreM $ getScoreM f >>= getScoreM . k


-- BarNum for the state instance...

instance StateM ScoreM BarNum where
  get   = ScoreM $ lift get
  set a = ScoreM $ lift $ set a

instance ReaderM ScoreM AbcBarNumF where
  ask   = ScoreM $ lift $ lift $ ask


runScoreM :: AbcBarNumF -> BarNum -> ScoreM a -> CollectBuffer
runScoreM f n mf = post $ runId 
                        $ runReaderT f 
                        $ runStateT  n 
                        $ runStateT  bufferZero 
                        $ getScoreM mf
  where
    post ((_,b),_) = b
  




-- anacrusis can start with barnum=0...

-- | A single linear score representation...
--
inlineScore :: AbcBarNumF -> AbcLineWidths -> BarNum 
            -> Score shape PhraseImage 
            -> Doc
inlineScore f lw n sc = 
    addLineConts lw $ finalize $ runScoreM f n $ renderInline sc


addLineConts :: AbcLineWidths -> [Doc] -> Doc
addLineConts abclw ds = vsep $ step ds abclw where
    step [] _          = []
    step xs (n ::: sn) = let (ys,zs) = splitAt n xs 
                         in contLines ys : step zs sn
 
contLines :: [Doc] -> Doc
contLines []     = empty
contLines [e]    = e
contLines (e:es) = e <> lineCont <$> contLines es 


renderInline :: Score shape PhraseImage -> ScoreM ()
renderInline Nil              = return ()
renderInline (Linear e xs)    = phraseImages e >> renderInline xs
renderInline (Repeat e xs)    = repeated e >> renderInline xs
renderInline (RepAlt e es xs) = repeatAlt e es >> renderInline xs



repeated :: PhraseImage -> ScoreM ()
repeated e = start_REP >> phraseImages e >> end_REP

-- Note - the end of of the initial section (before the first 
-- alternative)  is just a single bar.
--
repeatAlt :: PhraseImage -> [PhraseImage] -> ScoreM ()
repeatAlt e es = 
    start_REP >> phraseImages e >> zipWithM_ fn es [0..] >> end_DBL
  where
    fn a n = start_ALT n >> phraseImages a >> end_REP
 


phraseImages :: PhraseImage -> ScoreM ()
phraseImages (Phrase xs) = mapM_ (barImage >=> fn) xs
  where
    fn (df,d) = snocBar df d

barImage :: BarImage -> ScoreM (DocS, Doc)
barImage d = sets (\s -> (s,s+1)) >>= \n  ->
             ask                  >>= \f  ->
             return (f n, d)



-- NOTE - can only set left punctation when right tip holds data.
-- This models dropping eliding printing a start-repeat for the 
-- first bar of a score.
--
-- Note too, that left-punctuation is snoc-ed /after/ the current
-- right tip.
-- 
setLP :: LeftPunct -> ScoreM () 
setLP punct = setsCB_ fn
  where
    fn buf = maybe buf sk $ right_tip buf
      where
        sk _ = buf { next_left_punct = punct }  

start_REP :: ScoreM () 
start_REP = setLP LP_REPEAT

start_ALT :: Int -> ScoreM ()
start_ALT n = setLP (LP_ALT n)

setRP :: RightPunct -> ScoreM ()
setRP punct = setsCB_ fn 
  where
    fn buf = maybe buf sk $ right_tip buf
      where
        sk (body,_) = buf { right_tip = Just (body,punct) }  

end_REP   :: ScoreM () 
end_REP    = setRP RP_REPEAT

end_DBL   :: ScoreM () 
end_DBL    = setRP RP_DBL

snocBar :: DocS -> Doc -> ScoreM ()
snocBar bartraf img = setsCB_ fn
   where
     fn buf = CollectBuffer new_acc (Just new_tip) LP_NONE 
       where 
         leftF   = prefixLP (next_left_punct buf) 
         new_tip = (bartraf $ leftF img, RP_SGL) 
         new_acc = case right_tip buf of 
                     Nothing    -> cb_accumulator buf
                     Just (b,p) -> cb_accumulator buf `snocH` suffixRP b p



setsCB_ :: (CollectBuffer -> CollectBuffer) -> ScoreM ()
setsCB_ f = ScoreM $ get >>= \cb -> set (f cb)

prefixLP :: LeftPunct -> Doc -> Doc
prefixLP LP_NONE    d = d
prefixLP LP_REPEAT  d = lrepeat <+> d
prefixLP (LP_ALT n) d = alternative n <+> d

suffixRP :: Doc -> RightPunct -> Doc
suffixRP d RP_SGL     = d <+> singleBar
suffixRP d RP_DBL     = d <+> doubleBar
suffixRP d RP_REPEAT  = d <+> rrepeat


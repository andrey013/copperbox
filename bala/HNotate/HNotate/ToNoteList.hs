{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ToNoteList
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  TypeSynonymInstances, mptc.
--
-- Render an Event 'Tree' to a score / note list format.
--
--------------------------------------------------------------------------------



module HNotate.ToNoteList where


import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NoteList
import HNotate.OnsetQueue
import HNotate.Pitch


import Control.Applicative hiding (empty)
import Control.Monad.Reader
import qualified Data.Foldable as F

import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence
import Prelude hiding (null, length)


import Text.PrettyPrint.Leijen (pretty) {- TEMP -}


type FlattenM a = Reader Env a

-- The Measure and its measure number.
type IndexedMeasure = (Int,ScoreMeasure)




toNoteList :: EventList -> Env -> ScoreNoteList
toNoteList evtlist env = 
    ScNoteList $ blockLine $ bracketIM  (meter_pattern env) $ 
                 runReader (flattenEventSeqeunce evtlist) env
  where 
    bracketIM mp ixs = fmap (\(i,m) -> (i, bracketMeasure mp m)) ixs


instance Applicative (Reader env) where
  pure = return
  (<*>) = ap

flattenEventSeqeunce :: EventList -> FlattenM (Seq IndexedMeasure)
flattenEventSeqeunce (EventSeq evts) = do
    ml <- asks measure_length 
    p  <- asks partial_measure
    flatten1 ml p
  where
    flatten1 ml Nothing   = nextTok evts (mempty,1,ml,mempty)
    flatten1 ml (Just p)  = nextTok evts (mempty,0,ml-p,mempty)



nextTok :: Seq Evt -> Acc -> FlattenM (Seq IndexedMeasure)
nextTok se acc = case viewl se of 
    EmptyL          -> accSeal acc
    (Evt e) :< sse  -> fits sse acc e
    (Poly xs) :< sse -> mapM process1 xs >>= \ss ->
                        ss `accCombine` acc >>= (nextTok sse)    
  where
    process1 (EventSeq pe) = accDup acc >>= (nextTok pe)
        
fits se acc@(_,_,dleft,_) e = fits1 (glyphDuration e) dleft
  where
    fits1 gd dleft
        | gd == no_duration = e `accTip` acc >>= (nextTok se)
        | gd <  dleft       = e `accTip` acc >>= (nextTok se)
        | gd == dleft       = e `accTip` acc >>= accNext >>= (nextTok se)
        | otherwise         = split acc e gd dleft >>= (nextTok se)
        
    split acc e gd leftd = 
        let rightd = gd - leftd
            eleft  = onDuration (const leftd) e
            eright = onDuration (const rightd) e
        in eleft `accTip` acc >>= accTip SgTie
                              >>= accNext  
                              >>= accTip eright



type Acc = (Seq IndexedMeasure, Int, Duration, Seq ScoreGlyph)

accNext :: Acc -> FlattenM Acc 
accNext (se,i,d,ss)    = 
  (\ml -> (se |> (i,ScMeasure ss), i+1, ml, mempty)) <$> asks measure_length

accTip :: ScoreGlyph -> Acc -> FlattenM Acc    
accTip e (se,i,d,ss) = return (se, i, d - glyphDuration e, ss |> e)

accDup :: Acc -> FlattenM Acc
accDup (se,i,d,ss) = (\unl ml -> (mempty, i, d, spacers (ml-d) unl )) 
     <$> asks unit_note_length <*> asks measure_length

accSeal :: Acc -> FlattenM (Seq IndexedMeasure)
accSeal (se,i,d,ss) = fn <$> asks unit_note_length <*> asks measure_length
  where
    fn unl ml | d == ml   = se  -- aka new (empty) bar, don't accummulate
              | otherwise = se |> (i, ScMeasure $ ss >< (spacers d unl)) 


accCombine :: [Seq IndexedMeasure] -> Acc -> FlattenM Acc
accCombine xs (se,i,d,ss) = return (foldr (><) se xs,i,d,ss)


spacers :: Duration -> Duration -> Seq ScoreGlyph
spacers d unl = maybe mempty (fromList . map SgSpacer) (splitDuration d unl) 

    
   

instance OnsetEvent IndexedMeasure ScoreMeasure where
  onset (i,m) = (i,m)

  
blockLine :: (Seq IndexedMeasure) -> Seq ScoreBlock
blockLine se | null se   = mempty
             | otherwise = collapse $ buildQueue se
  where
    collapse :: OnsetQueue ScoreMeasure -> Seq ScoreBlock
    collapse oq = step mempty (viewH oq)
    
    step se ((i,sm) :>> oq)   = step (se |> mkBlock i sm) (viewH oq)
    step se EmptyQ            = se
    
    mkBlock i [x]   = ScSingleBlock i x
    mkBlock i xs    = ScPolyBlock i (fromList xs)


bracketMeasure :: MeterPattern -> ScoreMeasure -> ScoreMeasure
bracketMeasure mp (ScMeasure se)  = 
    work (mempty,mempty,durationZero) (countCount mp) (viewl se)
  where
    work acc         []     EmptyL      = brcktSeal acc

    work acc@(_,_,d)  (x:xs) (e :< sse)  = let ed = glyphDuration e in
      if (d+ed < x) 
        then work (brcktCurrent acc e ed) (x:xs) (viewl sse)
        else work (brcktNext acc e ed x)   xs    (viewl sse)    

    -- 'Recoverable error' -- meter pattern not as long as the measure
    -- add the remaining elements without trying to beam
    work acc          []      (e :< sse)  = error $ "mp short"
     --   let (ScMeasure left) = brcktSeal acc in ScMeasure ((left |> e) >< sse)
             
    -- 'Recoverable error' -- maybe not, probably should have a
    -- writer monad here
    work acc@(_,_,d)  _       EmptyL      = -- error $ "se short, seen " ++ show d
        brcktSeal acc

type BrcktAcc = (Seq ScoreGlyph, Seq ScoreGlyph, Duration)

brcktSeal (se,stk,_) = ScMeasure $ addBeamers se stk

-- 'next' signals we move into the next bracket group 
brcktNext :: BrcktAcc -> ScoreGlyph -> Duration -> Duration -> BrcktAcc
brcktNext (se,stk,d) e ed lim
    | ed < quarter = (addBeamers se stk, singleton e, rightDuration ed lim d) 
    | otherwise   = ((addBeamers se stk) |> e , mempty, rightDuration ed lim d)
  where 
  
    rightDuration elt_dur ll lc = elt_dur - (ll - lc)

-- Add the glyph to the current 'bracket'   
brcktCurrent :: BrcktAcc -> ScoreGlyph -> Duration -> BrcktAcc
brcktCurrent (se,stk,d) e ed 
    | ed < quarter = (se, stk |> e, d+ed) 
    | otherwise   = ((addBeamers se stk) |> e , mempty, d+ed)
  
  
countCount :: MeterPattern -> [Duration]
countCount (x:xs,s) = 
    scanl (\acc i -> acc + s * fromIntegral i) (s * fromIntegral x) xs


addBeamers :: Seq ScoreGlyph -> Seq ScoreGlyph -> Seq ScoreGlyph
addBeamers acc stk = add acc stk (length stk)
  where
    add acc _   0 = acc
    add acc stk 1 = acc >< stk
    add acc stk _ = (acc |> SgBeamStart) ><  (stk |> SgBeamEnd)



      
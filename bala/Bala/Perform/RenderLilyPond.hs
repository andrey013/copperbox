{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.RenderLilyPond
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Render an EventTree as to LilyPond.
--
--------------------------------------------------------------------------------

module Bala.Perform.RenderLilyPond where

import Bala.Perform.EventTree hiding (chord, grace)

import qualified Bala.Base as B
import Bala.Format.Output.OutputLilyPond hiding (duration, Sequence)

import Control.Applicative 
import Control.Monad.State
import Data.Sequence

type RenderM a = State RenderSt a


data RenderSt = RenderSt { 
    relative_pitch      :: B.Pitch,
    relative_duration   :: B.Duration
  } 

class LyRenderable a where
    isPitch             :: a -> Bool
    isRest              :: a -> Bool
    durationOf          :: a -> B.Duration
    pitchOf             :: a -> B.Pitch
    
    
st_zero = RenderSt { 
    --  Middle C (c') is the default in LilyPond. 
    relative_pitch = B.c4,
    -- A quarter note is the default duration in LilyPond. 
    relative_duration = B.quarter
  } 

(*!) e oa   = maybe e (e !) oa


lyPitch' () = lyPitch

                           
withRelativePitch :: B.Pitch -> RenderSt -> RenderSt
withRelativePitch p st = st { relative_pitch = p } 

  
runRenderLy  = evalState  

apAttr :: (SuffixAttr e a) => Ly a -> Ly e -> Ly e
apAttr = flip (!)


lyPitchLetter :: B.PitchLetter -> PitchName
lyPitchLetter = toEnum . fromEnum 


odisp :: B.Pitch -> B.Pitch -> Int
odisp p p' = 
  let (ityp, _)   = B.unInterval $ B.pitchDifference p p'
      absdisp     = ceiling $ (fromIntegral (ityp - 4) / 8.0)
      disp        = if p > p' then (negate absdisp) else absdisp in 
  if (ityp < 4) then 0 else disp


currentOctave :: B.Pitch -> RenderM Int
currentOctave p = do
    p' <- gets relative_pitch
    modify (\s -> s {relative_pitch = p})
    return $ odisp p' p
  



accidental :: SuffixAttr e Accidental => B.Accidental -> (Ly e -> Ly e)
accidental B.Flat          = apAttr flat
accidental B.Sharp         = apAttr sharp
accidental B.DoubleSharp   = apAttr doubleSharp
accidental B.DoubleFlat    = apAttr doubleFlat
accidental _               = id



octaveSpec :: SuffixAttr e OctaveSpec => Int -> (Ly e -> Ly e)            
octaveSpec i
    | i == 0    = id
    | i <  0    = apAttr $ lowered (abs i)
    | otherwise = apAttr $ raised i
    
lyPitch :: B.Pitch -> RenderM (Ly Pitch)    
lyPitch p = currentOctave p >>= \spec -> 
            return $ pch # actl # (octaveSpec spec)
  where
    pch  = pitch $ lyPitchLetter $ B.pitchLetter p
    actl = accidental $ B.pitchAccidental p



lyNote p = do 
    pch <- lyPitch p
    return (note pch)
    

duration :: B.Duration -> Maybe (Ly Duration) 
duration d 
  | d == B.longa                      = Just longa  
  | d == B.double_whole               = Just breve 
  | d == B.whole                      = Just $ dur 1
  | d == B.half                       = Just $ dur 2
  | d == B.quarter                    = Just $ dur 4
  | d == B.eighth                     = Just $ dur 8
  | d == B.sixteenth                  = Just $ dur 16
  | d == B.thirty_second              = Just $ dur 32
  | d == B.sixty_fourth               = Just $ dur 64
  | d == B.one_hundred_twenty_eighth  = Just $ dur 128
  | otherwise                         = Nothing




                 
dotpart i   
    | i > 0       = apAttr (dotted $ fromIntegral i)
    | otherwise   = id

data DurationStatus = SAME | DIFF

currentDuration :: B.Duration -> RenderM DurationStatus
currentDuration d = do
    d' <- gets relative_duration
    if (d == d') 
      then (return SAME) 
      else (modify (\s -> s {relative_duration = d}) >> return DIFF)



-- lyDuration :: B.Duration -> RenderM (OE (Ly Duration) (Ly CmdLongDuration))
lyDuration :: B.Duration -> RenderM (Maybe (Ly Duration))
lyDuration d = currentDuration d >>= mkD
  where 
    mkD SAME   = return Nothing
    mkD _      = let (dur,dots) = B.simplifyDuration d
                     root       = duration dur
                 in return $ maybe Nothing (Just . dotpart dots) root




-- has to be pitch for chords...
pitch'duration evt = do
    p     <- lyPitch (pitchOf evt)
    od    <- lyDuration (durationOf evt)
    return (p, od)    



rest'duration evt = do
    od   <- lyDuration (durationOf evt)
    return (rest, od)
    
pitchOrRest'duration evt 
    | isRest evt    = rest'duration evt >>= \(_,d) -> return (Right (),d)
    | otherwise     = pitch'duration evt >>= \(p,d) -> return (Left p,d) 



suffix k (Left p,od)     = k +++ (note p *! od)
suffix k (Right (),od)   = k +++ (rest *! od)


suffixChord :: (Append cxts Chord) => 
  Ly cxts -> [(Either (Ly Pitch) (), Maybe (Ly Duration))] -> Ly cxts
suffixChord k []              = k
suffixChord k xs@((_,od):_)   = k +++ khord xs
  where
    khord ps = let ps' = foldl fn [] ps 
               in chord ps' *! od
    fn acc (Left p,_) = p : acc
    fn acc _          = acc

suffixGrace :: (Append cxts CmdGrace) => 
  Ly cxts -> [(Either (Ly Pitch) (), Maybe (Ly Duration))] -> Ly cxts
suffixGrace k []    = k
suffixGrace k xs    = k +++ grace gblock
  where
    gblock = blockS $ foldr (flip suffix) elementBlk xs



       
oflat :: (LyRenderable evt) =>
         Ly CT_Element -> ViewL (EvtPosition evt) -> RenderM (Ly CT_Element)    
oflat lyk  EmptyL               = return lyk 

oflat lyk (Evt e :< sq)         = do
    e'        <- pitchOrRest'duration e
    oflat (lyk `suffix` e') (viewl sq)

oflat lyk (Sequence ts :< sq)   = do
    lyk'      <- oflat lyk (viewl sq) 
    xs        <- mapM (oflat elementBlk) (map viewl ts)
    return (merge lyk' xs)  
    
oflat lyk (StartPar :< sq)      =
    oflatPar (lyk,[]) (viewl sq)
   
oflat lyk (StartPre :< sq)      =
    oflatPre (lyk,[]) (viewl sq)

oflat lyk _                     =
    error "Invalid EventTree"
    
    
-- successive notes in a chord shouldn't change relative pitch            
oflatPar (lyk,stk) (Evt e :< sq) = do
    e'              <- pitchOrRest'duration e
    oflatPar (lyk, (e':stk)) (viewl sq)
  
oflatPar (lyk,stk) (EndPar :< sq) = 
    oflat (suffixChord lyk stk) (viewl sq)
      
oflatPar (lyk,stk) _              = 
    error "unterminated Par"
    
    
oflatPre (lyk,stk) (Evt e :< sq)  = do
    e'              <- pitchOrRest'duration e
    oflatPre (lyk, (e':stk)) (viewl sq)
  
oflatPre (lyk,stk) (EndPre :< sq) = 
    oflat (suffixGrace lyk stk) (viewl sq)
                        
oflatPre (lyk,stk) _              = 
    error "unterminated Pre"                   
                        



merge :: (Append cxts Poly, Append cxts Block) => Ly cxts -> [Ly a] -> Ly cxts
merge k []     = k
merge k (x:xs) = let poly = foldl fn (block x) xs in 
    k +++ openPoly +++ poly +++ closePoly
  where
    fn acc a = acc \\ (block a)

run'oflat :: (LyRenderable t) 
          => Ly CT_Element -> Seq (EvtPosition t) -> RenderM (Ly CT_Element)                 
run'oflat elt_list t = oflat elt_list (viewl t) 


renderLy1 :: (LyRenderable evt) =>
  Ly CT_Element -> EventTree evt -> RenderSt -> Ly CT_Element 
renderLy1 init_ly tree env = evalState (run'oflat init_ly tree) env
 
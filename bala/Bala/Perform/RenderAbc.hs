
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.RenderAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Render an EventTree as to ABC format.
--
--------------------------------------------------------------------------------

module Bala.Perform.RenderAbc where

import Bala.Perform.EventTree hiding (chord, grace)

import qualified Bala.Base.Base as B
import Bala.Format.SymAbc.AbcFormat

import Control.Applicative
import Control.Monad.State
import Data.Sequence
import Data.Ratio

type RenderM a = State RenderSt a


data RenderSt = RenderSt { 
    default_note_length     :: B.Duration,
    meter_size              :: Double,
    bar_duration_count      :: Double
  } 

abcEnv :: B.Duration -> B.MeterFraction -> RenderSt
abcEnv dnl mf = RenderSt 
    { default_note_length   = dnl,
      meter_size            = B.meterSize mf,
      bar_duration_count    = 0      
      }



   
class AbcRenderable a where
    isPitch             :: a -> Bool
    isRest              :: a -> Bool
    durationOf          :: a -> B.Duration
    pitchOf             :: a -> B.Pitch
  
runRenderAbc  = evalState  


barCount :: (CRepeatMark repr) 
         => B.Duration -> RenderM (Maybe (repr RepeatMark))
barCount dur = let d = B.durationSize dur in do
    c     <- gets bar_duration_count
    ms    <- gets meter_size 
    fn (c+d) ms
  where
    fn dc ms 
        | dc >= ms  = do { modify (\s -> s { bar_duration_count = 0  }) 
                         ; return (Just barline) }
                          
        | otherwise = do { modify (\s -> s { bar_duration_count = dc }) 
                         ; return Nothing }

                         
-- octaves 4 and below upper case pitch 
-- octaves 5 and above lower case pitch
abcPitchLetter :: B.PitchLetter -> PitchLetter
abcPitchLetter = toEnum . fromEnum 

octave1 :: PitchLetter -> PitchLetter
octave1 p = let i = fromEnum p in
    if (i < 7) then (toEnum $ i + 7) else p


octaveAttr :: (COctave repr) => Int -> Maybe (repr Octave)             
octaveAttr i
    | i < 4         = Just $ octaveLow (4 - i)
    | i > 5         = Just $ octaveHigh (i - 5)
    | otherwise     = Nothing


accidentalAttr :: (CAccidental repr) => B.Accidental -> Maybe (repr Accidental)
accidentalAttr (B.DoubleFlat)   = Just doubleFlat 
accidentalAttr (B.Flat)         = Just flat  
accidentalAttr (B.Sharp)        = Just sharp 
accidentalAttr (B.DoubleSharp)  = Just doubleSharp 
accidentalAttr _                = Nothing


abcPitch :: (CBaseNote repr, COctave repr, CAccidental repr, 
             CAttr repr, CPrefixAttr repr) 
         => B.Pitch -> repr BaseNote
abcPitch p = 
    let o  = B.octaveMeasure p
        pl = octLetterChange (abcPitchLetter $ B.pitchLetter p) o
        oa = octaveAttr o
        aa = accidentalAttr (B.pitchAccidental p)
    in aa `optPrefixAttr` (note pl) `optAttr` oa
              
  where
    octLetterChange pl o  | o > 4     = octave1 pl
                          | otherwise = pl              
    

abcDuration :: (CDuration repr) => B.Duration -> RenderM (Maybe (repr Duration))
abcDuration d = fn d <$> gets default_note_length
  where
    fn dur1 deft
      | dur1 == deft  = Nothing
      | otherwise     = let scale = denominator (B.rationalize deft)
                            r     = (B.rationalize dur1)
                            (n,d) = (numerator r, denominator r)    
                        in Just $ dur ( n*scale B.// d)

                        
pitch'duration evt = 
  let n = abcPitch (pitchOf evt) 
      d = durationOf evt in do
    od <- abcDuration d
    ob <- barCount d
    return (n,od,ob)  

rest'duration :: (CDuration repr, CRepeatMark repr, AbcRenderable evt) 
              => evt 
              -> RenderM (Maybe (repr Duration), Maybe (repr RepeatMark))
rest'duration evt = 
  let d = durationOf evt in do
    od <- abcDuration (durationOf evt)
    ob <- barCount d
    return (od,ob)
  
pitchOrRest'duration evt 
    | isRest evt    = rest'duration evt >>= \(od,ob) -> return (Right (),od,ob)
    | otherwise     = pitch'duration evt >>= \(p,od,ob) -> return (Left p,od,ob) 

applyDuration e Nothing    = e
applyDuration e (Just a)   = e `attr` a


    
suffix k (Left p,od,ob)     = (k +++ (p `applyDuration` od)) `optsnoc` ob
suffix k (Right (),od,ob)   = (k +++ (rest `applyDuration` od)) `optsnoc` ob

suffixChord k []    = k
suffixChord k xs    = k -- +++ khord xs
  where
    khord ps = chord $ foldl fn [] ps 
               
    fn acc (Left p,d,_) = (p `applyDuration` d) : acc
    fn acc _            = acc
    
suffixGrace k []    = k
suffixGrace k xs    = k -- +++ gracenotes undefined 
--  where
--    gblock = block $ foldr (flip suffix) elementCtx xs

oflat lyk  EmptyL               = return lyk 

oflat lyk (Evt e :< sq)         = do
    e'        <- pitchOrRest'duration e
    oflat (suffix lyk e') (viewl sq)
    
-- Sequence maps to voice overlay - supported by abcm2ps but not abc2ps
oflat lyk (Sequence ts :< sq)   = do
    lyk'      <- oflat lyk (viewl sq) 
    xs        <- mapM (oflat elementCtx) (map viewl ts)
    return (merge lyk' xs)  
{-        
oflat lyk (StartPar :< sq)      =
    oflatPar (lyk,[]) (viewl sq)
   
oflat lyk (StartPre :< sq)      =
    oflatPre (lyk,[]) (viewl sq)

oflat lyk _                     =
    error "Invalid EventTree"
    
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
-}    
    
        
-- need to 'plex' bars...        
merge k []     = k
merge k (x:xs) = foldl fn x xs
  where
    fn acc a = acc &\ a
    
        
run'oflat ellist t = oflat ellist (viewl t) 

                          
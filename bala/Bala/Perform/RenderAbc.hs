
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
    default_note_length   :: B.Duration,
    bar_count             :: Int
  } 

abcEnv :: B.Duration -> RenderSt
abcEnv dnl = RenderSt { default_note_length = dnl,
                        bar_count = 0 }


defaultNoteLength :: B.MeterFraction -> Double
defaultNoteLength m = 
  let r     = B.meterRatio m
      (n,d) = (fromIntegral $ numerator r, fromIntegral $ denominator r)
  in n / d
   
class AbcRenderable a where
    isPitch             :: a -> Bool
    isRest              :: a -> Bool
    durationOf          :: a -> B.Duration
    pitchOf             :: a -> B.Pitch
  
runRenderAbc  = evalState  

  
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
    return (n, od)  

rest'duration :: (CDuration repr, AbcRenderable evt) 
              => evt -> RenderM (Maybe (repr Duration))
rest'duration evt = do
  od <- abcDuration (durationOf evt)
  return od
  
pitchOrRest'duration evt 
    | isRest evt    = rest'duration evt >>= \od -> return (Right (),od)
    | otherwise     = pitch'duration evt >>= \(p,od) -> return (Left p,od) 

applyDuration e Nothing    = e
applyDuration e (Just a)   = e `attr` a


    
suffix k (Left p,od)     = k +++ (p `applyDuration` od)
suffix k (Right (),od)   = k +++ (rest `applyDuration` od)


oflat lyk  EmptyL               = return lyk 

oflat lyk (Evt e :< sq)         = do
    e'        <- pitchOrRest'duration e
    oflat (suffix lyk e') (viewl sq)
    
-- Sequence maps to voice overlay - supported by abcm2ps but not abc2ps
oflat lyk (Sequence ts :< sq)   = do
    lyk'      <- oflat lyk (viewl sq) 
    xs        <- mapM (oflat elementCtx) (map viewl ts)
    return (merge lyk' xs)  
        
        
-- need to 'plex' bars...        
merge k []     = k
merge k (x:xs) = foldl fn x xs
  where
    fn acc a = acc &\ a
    
        
run'oflat ellist t = oflat ellist (viewl t) 

                          
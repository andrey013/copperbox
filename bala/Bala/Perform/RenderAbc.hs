{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

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


import qualified Bala.Base as B
import Bala.Format.Output.OutputAbc hiding (Sequence)
import Bala.Perform.EventTree hiding (chord, grace)
import Bala.Perform.PerformBase


import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Sequence
import Data.Ratio


type ProcessM a = PerformM Perform_Abc_State Perform_Abc_Env a


data Perform_Abc_State = Perform_Abc_State { 
    bar_duration_count      :: Double
  }  
  deriving (Show)
  
data Perform_Abc_Env = Perform_Abc_Env {
    default_note_length     :: B.Duration,
    meter_size              :: Double,
    initial_abc_context     :: Abc CT_Element
  }
  deriving (Show)
  

intial_abc_state = Perform_Abc_State { 
    bar_duration_count    = 0
  } 
  
default_abc_env :: Perform_Abc_Env
default_abc_env = Perform_Abc_Env {
    default_note_length   = B.eighth,
    meter_size            = B.meterSize (4 B.// 4),
    initial_abc_context   = tune   
  } 



-- ZeroPitch & ZeroRest tracks the original (Base) duration so it can 
-- be used to update the render state if necessary

data EventZero = 
    ZeroPitch B.Duration (Abc Elt_Note) (Maybe (Abc Attr_Duration)) 
  | ZeroRest B.Duration (Maybe (Abc Attr_Duration)) 
  | ZeroUnknown

   
  
 

(*!) e oa   = maybe e (e !) oa



(*!**) oa  e    = maybe e (\a -> a !> e) oa


optAddR sq Nothing   = sq
optAddR sq (Just e)  = sq +++ e  




barCount :: B.Duration -> ProcessM (Maybe (Abc Elt_RepeatMark))        
barCount dur = let d = B.durationSize dur in do
    c     <- gets bar_duration_count
    ms    <- asks meter_size 
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


octaveAttr :: Int -> Maybe (Abc Attr_Octave)            
octaveAttr i
    | i < 4         = Just $ octaveLow (4 - i)
    | i > 5         = Just $ octaveHigh (i - 5)
    | otherwise     = Nothing


accidentalAttr :: B.Accidental -> Maybe (Abc Attr_Accidental)
accidentalAttr (B.DoubleFlat)   = Just doubleFlat 
accidentalAttr (B.Flat)         = Just flat  
accidentalAttr (B.Sharp)        = Just sharp 
accidentalAttr (B.DoubleSharp)  = Just doubleSharp 
accidentalAttr _                = Nothing

abcPitch :: B.Pitch -> Abc Elt_Note     
abcPitch p = 
    let o  = B.octaveMeasure p
        pl = octLetterChange (abcPitchLetter $ B.pitchLetter p) o
        oa = octaveAttr o
        aa = accidentalAttr (B.pitchAccidental p)
    in aa *!** (note pl) *! oa
              
  where
    octLetterChange pl o  | o > 4     = octave1 pl
                          | otherwise = pl              
    

abcDuration :: B.Duration -> ProcessM (Maybe (Abc Attr_Duration))
abcDuration d = fn d <$> asks default_note_length
  where
    fn dur1 deft
      | dur1 == deft  = Nothing
      | otherwise     = let scale = denominator (B.rationalize deft)
                            r     = (B.rationalize dur1)
                            (n,d) = (numerator r, denominator r)    
                        in Just $ dur ( n*scale B.// d)



updateBarCount :: Abc CT_Element -> EventZero -> ProcessM (Abc CT_Element)
updateBarCount k ez = case hasDur ez of 
    Nothing -> return k
    Just d  -> barCount d >>= \obl -> maybe (return k) (return . (k +++)) obl  
  where
    hasDur (ZeroPitch d _ _)  = Just d
    hasDur (ZeroRest d _)     = Just d
    hasDur _                  = Nothing
    




eventZero evt = case eventvalues evt of
    (Just p, Just d)    -> (ZeroPitch d) <$> pure (abcPitch p) <*> abcDuration d
    (Nothing, Just d)   -> (ZeroRest d)  <$> abcDuration d
    (Nothing, Nothing)  -> return ZeroUnknown
    
    
applyDuration e Nothing    = e
applyDuration e (Just a)   = e ! a


suffix k ez@(ZeroPitch _ p od)  = updateBarCount (k +++ (p *! od)) ez
suffix k ez@(ZeroRest _ od)     = updateBarCount (k +++ (rest *! od)) ez 
suffix k _                      = return k


-- At the end of a chord add its duration 
-- Assumption: use the duration of the first note in chord for the whole chord
suffixChord k stk =
  case viewl stk of
    EmptyL                      -> return k
    ez@(ZeroPitch _ _ od) :< _  -> updateBarCount (k +++ khord stk) ez

  where
    khord sq = chord $ F.foldr fn [] sq 

    fn (ZeroPitch _ p od) acc = (p *! od) : acc
    fn _                  acc = acc
    
        
suffixGrace k []    = k
suffixGrace k xs    = k +++ grace xs
  where
    grace ps = gracenotes $ foldl fn [] ps 
               
    fn acc (ZeroPitch _ p od) = (p *! od) : acc
    fn acc _                  = acc

oflat sqk  EmptyL               = return sqk 

oflat sqk (Evt e :< sq)         = do
    ez        <- eventZero e
    sqk'      <- suffix sqk ez
    oflat sqk' (viewl sq)
    
-- Sequence maps to voice overlay - supported by abcm2ps but not abc2ps
oflat sqk (Poly ts :< sq)   = do
    sqk'      <- oflat sqk (viewl sq) 
    xs        <- mapM (oflat tune) (map (viewl . unET) ts)
    return (merge sqk' xs)  
       
oflat sqk (StartPar :< sq)      =
    oflatPar (sqk,emptyseq) (viewl sq)
   
oflat sqk (StartPre :< sq)      =
    oflatPre (sqk,[]) (viewl sq)

oflat sqk _                     =
    error "Invalid EventTree"
    
oflatPar (sqk,stk) (Evt e :< sq) = do
    ez              <- eventZero e
    oflatPar (sqk, (stk |> ez)) (viewl sq)

   
oflatPar (sqk,stk) (EndPar :< sq) = do
    sqk'            <- suffixChord sqk stk
    oflat sqk' (viewl sq)
 
      
oflatPar (sqk,stk) _              = 
    error "unterminated Par"

-- grace notes shouldn't change bar count      
oflatPre (sqk,stk) (Evt e :< sq)  = do
    ez              <- eventZero e
    oflatPre (sqk, (ez:stk)) (viewl sq)
  
oflatPre (sqk,stk) (EndPre :< sq) = 
    oflat (suffixGrace sqk stk) (viewl sq)
                        
oflatPre (sqk,stk) _              = 
    error "unterminated Pre"     
   
    
        
-- need to 'plex' bars...        
merge k []     = k
merge k (x:xs) = foldl fn x xs
  where
    fn acc a = acc &\ a
    
        
run'oflat  t = do
    ellist <- asks initial_abc_context
    oflat ellist (viewl $ unET t) 

renderAbc1 :: (Perform evt B.Pitch B.Duration) =>
              EventTree evt -> Perform_Abc_Env -> Abc CT_Element
renderAbc1 tree env  = evalPerform (run'oflat tree) intial_abc_state env

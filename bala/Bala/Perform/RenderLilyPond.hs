{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

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

import Bala.Base
import Bala.Format.Output.OutputLilyPond hiding (duration, Sequence, 
          emptyseq, breve, longa, pitch)
import qualified Bala.Format.Output.OutputLilyPond as Ly
                    
import Bala.Perform.EventTree hiding (chord, grace)
import Bala.Perform.PerformBase


import Control.Applicative hiding (empty)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Sequence hiding (reverse)

type ProcessM a = PerformM Perform_Ly_State Perform_Ly_Env a

data Perform_Ly_State = Perform_Ly_State { 
    relative_pitch      :: Pitch,
    relative_duration   :: Duration
  }  
  deriving (Show)
  
data Perform_Ly_Env = Perform_Ly_Env {
    initial_ly_context        :: LyCxt_Element,
    initial_relative_pitch    :: Pitch
  }
  deriving (Show)
  
intial_ly_state = Perform_Ly_State { 
    --  Middle C (c') is the default in LilyPond. 
    relative_pitch = c4,
    -- A quarter note is the default duration in LilyPond. 
    relative_duration = quarter
  } 
  
default_ly_env :: Perform_Ly_Env
default_ly_env = Perform_Ly_Env { 
    initial_ly_context        = elementBlk,
    initial_relative_pitch    = c4 
  }

emptyseq :: Seq a
emptyseq = empty  

-- Note durations are optional for both Pitch and Rest 
-- Default durations don't get printed.
-- ZeroPitch tracks the original (Base) pitch so it can be used to update
-- the render state if necessary
data EventZero = ZeroPitch Pitch LyPitch (Maybe LyDuration)
               | ZeroRest (Maybe LyDuration) 
               | ZeroUnknown
  deriving Show            
                


(*!) e oa   = maybe e (e !) oa


lyPitch' () = lyPitch

                           
withRelativePitch :: Pitch -> Perform_Ly_State -> Perform_Ly_State
withRelativePitch p st = st { relative_pitch = p } 



apAttr :: (SuffixAttr e a) => Ly a -> Ly e -> Ly e
apAttr = flip (!)


transPitchLetter :: PitchLetter -> LyPitchName
transPitchLetter = toEnum . fromEnum 


odisp :: Pitch -> Pitch -> Int
odisp p p' = 
  let (ityp, _)   = unInterval $ pitchDifference p p'
      absdisp     = ceiling $ (fromIntegral (ityp - 4) / 8.0)
      disp        = if p > p' then (negate absdisp) else absdisp in 
  if (ityp < 4) then 0 else disp


currentOctave :: Pitch -> ProcessM Int
currentOctave p = do
    p' <- gets relative_pitch
    return $ odisp p' p
  
updateCurrentPitch :: EventZero -> ProcessM ()
updateCurrentPitch (ZeroPitch p _ _) = modify (\s -> s {relative_pitch = p})
updateCurrentPitch _                 = return ()

accidental :: (SuffixAttr e LyAccidentalT) 
           => Accidental 
           -> (Ly e -> Ly e)
accidental Flat          = apAttr flat
accidental Sharp         = apAttr sharp
accidental DoubleSharp   = apAttr doubleSharp
accidental DoubleFlat    = apAttr doubleFlat
accidental _             = id



octaveSpec :: SuffixAttr e LyOctaveSpecT => Int -> (Ly e -> Ly e)            
octaveSpec i
    | i == 0    = id
    | i <  0    = apAttr $ lowered (abs i)
    | otherwise = apAttr $ raised i
    
transPitch :: Pitch -> ProcessM LyPitch    
transPitch p = currentOctave p >>= \spec -> 
            return $ pch # actl # (octaveSpec spec) 
  where
    pch  = Ly.pitch $ transPitchLetter $ pitchLetter p
    actl = accidental $ pitchAccidental p


transNote :: Pitch -> ProcessM LyNote
transNote p = do 
    pch <- transPitch p
    return (note pch)
    

duration :: Duration -> Maybe LyDuration 
duration d 
  | d == longa                      = Just Ly.longa  
  | d == double_whole               = Just Ly.breve 
  | d == whole                      = Just $ Ly.duration 1
  | d == half                       = Just $ Ly.duration 2
  | d == quarter                    = Just $ Ly.duration 4
  | d == eighth                     = Just $ Ly.duration 8
  | d == sixteenth                  = Just $ Ly.duration 16
  | d == thirty_second              = Just $ Ly.duration 32
  | d == sixty_fourth               = Just $ Ly.duration 64
  | d == one_hundred_twenty_eighth  = Just $ Ly.duration 128
  | otherwise                       = Nothing




                 
dotpart i   
    | i > 0       = apAttr (dotted $ fromIntegral i)
    | otherwise   = id

data DurationStatus = SAME | DIFF

currentDuration :: Duration -> ProcessM DurationStatus
currentDuration d = do
    d' <- gets relative_duration
    if (d == d') 
      then (return SAME) 
      else (modify (\s -> s {relative_duration = d}) >> return DIFF)




transDuration :: Duration -> ProcessM (Maybe LyDuration)
transDuration d = currentDuration d >>= mkD
  where 
    mkD SAME   = return Nothing
    mkD _      = let (dur,dots) = simplifyDuration d
                     root       = duration dur
                 in return $ maybe Nothing (Just . dotpart dots) root
                 -- in error $ "root is " ++ show d


    
eventZero evt = case eventvalues evt of
    (Just p, Just d)    -> (ZeroPitch p) <$> transPitch p <*> transDuration d
    (Nothing, Just d)   -> ZeroRest  <$> transDuration d
    (Nothing, Nothing)  -> return ZeroUnknown


             
suffix k (ZeroPitch _ p od)   = k +++ (note p *! od)
suffix k (ZeroRest od)        = k +++ (rest *! od)
suffix k _                    = k

suffixChord :: (Append cxts LyChordT) => Ly cxts -> Seq EventZero -> Ly cxts
suffixChord k stk =
  case viewl stk of
    EmptyL                  -> k
    (ZeroPitch _ _ od) :< _ -> k +++ khord od stk

  where
    khord od sq = let ps' = F.foldr fn [] sq 
                  in chord ps' *! od
    fn (ZeroPitch _ p _) acc = p : acc
    fn _                 acc = acc

suffixGrace :: (Append cxts LyCmdGraceT) 
            => Ly cxts 
            -> [EventZero] 
            -> Ly cxts
suffixGrace k []    = k
suffixGrace k xs    = k +++ grace gblock
  where
    gblock = blockS $ foldr (flip suffix) elementBlk xs



       
oflat :: (Perform evt Pitch Duration) 
      => LyCxt_Element 
      -> ViewL (EvtPosition evt) 
      -> ProcessM (LyCxt_Element)
oflat lyk  EmptyL               = return lyk 

oflat lyk (Evt e :< sq)         = do
    ez        <- eventZero e
    updateCurrentPitch ez
    oflat (lyk `suffix` ez) (viewl sq)

oflat lyk (Poly ts :< sq)       = do
    lyk'      <- oflat lyk (viewl sq) 
    xs        <- mapM (oflat elementBlk) (map (viewl . unET) ts)
    return (merge lyk' xs)  
    
oflat lyk (StartPar :< sq)      =
    oflatPar (lyk,Nothing,emptyseq) (viewl sq)
   
oflat lyk (StartPre :< sq)      =
    oflatPre (lyk,[]) (viewl sq)

oflat lyk _                     =
    error "Invalid EventTree"
    
    
-- Important: successive notes in a chord shouldn't change relative pitch
           
oflatPar (lyk,ofirst,stk) (Evt e :< sq) = do
    ez              <- eventZero e
    ofirst' <- fn ofirst ez
    oflatPar (lyk,ofirst',(stk |> ez)) (viewl sq)
  where
    fn Nothing ez@(ZeroPitch p _ _) = updateCurrentPitch ez >> return (Just p)
    fn a       _                    = return a
      
oflatPar (lyk,ofirst,stk) (EndPar :< sq) = 
    oflat (suffixChord lyk stk) (viewl sq)
      
oflatPar (lyk,ofirst,stk) _              = 
    error "unterminated Par"
    
    
oflatPre (lyk,stk) (Evt e :< sq)  = do
    ez              <- eventZero e
    oflatPre (lyk, (ez:stk)) (viewl sq)
  
oflatPre (lyk,stk) (EndPre :< sq) = 
    oflat (suffixGrace lyk stk) (viewl sq)
                        
oflatPre (lyk,stk) _              = 
    error "unterminated Pre"                   
                        



merge :: (Append cxts LyPolyT, Append cxts LyBlockT) 
      => Ly cxts 
      -> [Ly a] 
      -> Ly cxts
merge k []     = k
merge k (x:xs) = let poly = foldl fn (block x) xs in 
    k +++ openPoly +++ poly +++ closePoly
  where
    fn acc a = acc \\ (block a)

run'oflat :: (Perform evt Pitch Duration) 
          => EventTree evt 
          -> ProcessM (LyCxt_Element)                 
run'oflat t = do 
    elt_list <- asks initial_ly_context
    oflat elt_list (viewl $ unET t) 


renderLy1 :: (Perform evt Pitch Duration) 
          => EventTree evt 
          -> Perform_Ly_Env
          -> LyCxt_Element 
renderLy1 tree env = evalPerform (run'oflat tree) ly_state env
  where
    ly_state = intial_ly_state { relative_pitch = (initial_relative_pitch env) }
 
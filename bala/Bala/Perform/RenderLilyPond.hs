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

import qualified Bala.Base as B
import Bala.Format.Output.OutputLilyPond hiding (duration, Sequence, emptyseq)
import Bala.Perform.EventTree hiding (chord, grace)
import Bala.Perform.PerformBase


import Control.Applicative hiding (empty)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Sequence hiding (reverse)

type ProcessM a = PerformM Perform_Ly_State Perform_Ly_Env a

data Perform_Ly_State = Perform_Ly_State { 
    relative_pitch      :: B.Pitch,
    relative_duration   :: B.Duration
  }  
  deriving (Show)
  
data Perform_Ly_Env = Perform_Ly_Env {
    initial_ly_context        :: Ly CT_Element,
    initial_relative_pitch    :: B.Pitch
  }
  deriving (Show)
  
intial_ly_state = Perform_Ly_State { 
    --  Middle C (c') is the default in LilyPond. 
    relative_pitch = B.c4,
    -- A quarter note is the default duration in LilyPond. 
    relative_duration = B.quarter
  } 
  
default_ly_env :: Perform_Ly_Env
default_ly_env = Perform_Ly_Env { 
    initial_ly_context        = elementBlk,
    initial_relative_pitch    = B.c4 
  }

emptyseq :: Seq a
emptyseq = empty  

-- Note durations are optional for both Pitch and Rest 
-- Default durations don't get printed.
-- ZeroPitch tracks the original (Base) pitch so it can be used to update
-- the render state if necessary
data EventZero = ZeroPitch B.Pitch (Ly Pitch) (Maybe (Ly Duration)) 
               | ZeroRest (Maybe (Ly Duration)) 
               | ZeroUnknown
  deriving Show            
                


(*!) e oa   = maybe e (e !) oa


lyPitch' () = lyPitch

                           
withRelativePitch :: B.Pitch -> Perform_Ly_State -> Perform_Ly_State
withRelativePitch p st = st { relative_pitch = p } 



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


currentOctave :: B.Pitch -> ProcessM Int
currentOctave p = do
    p' <- gets relative_pitch
    return $ odisp p' p
  
updateCurrentPitch :: EventZero -> ProcessM ()
updateCurrentPitch (ZeroPitch p _ _) = modify (\s -> s {relative_pitch = p})
updateCurrentPitch _                 = return ()

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
    
lyPitch :: B.Pitch -> ProcessM (Ly Pitch)    
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

currentDuration :: B.Duration -> ProcessM DurationStatus
currentDuration d = do
    d' <- gets relative_duration
    if (d == d') 
      then (return SAME) 
      else (modify (\s -> s {relative_duration = d}) >> return DIFF)




lyDuration :: B.Duration -> ProcessM (Maybe (Ly Duration))
lyDuration d = currentDuration d >>= mkD
  where 
    mkD SAME   = return Nothing
    mkD _      = let (dur,dots) = B.simplifyDuration d
                     root       = duration dur
                 in return $ maybe Nothing (Just . dotpart dots) root
                 -- in error $ "root is " ++ show d


    
eventZero evt = case (opitch evt, oduration evt) of
    (Just p, Just d)    -> (ZeroPitch p) <$> lyPitch p <*> lyDuration d
    (Nothing, Just d)   -> ZeroRest  <$> lyDuration d
    (Nothing, Nothing)  -> return ZeroUnknown


             
suffix k (ZeroPitch _ p od)   = k +++ (note p *! od)
suffix k (ZeroRest od)        = k +++ (rest *! od)
suffix k _                    = k

suffixChord :: (Append cxts Chord) => Ly cxts -> Seq EventZero -> Ly cxts
suffixChord k stk =
  case viewl stk of
    EmptyL                  -> k
    (ZeroPitch _ _ od) :< _ -> k +++ khord od stk

  where
    khord od sq = let ps' = F.foldr fn [] sq 
                  in chord ps' *! od
    fn (ZeroPitch _ p _) acc = p : acc
    fn _                 acc = acc

suffixGrace :: (Append cxts CmdGrace) => 
  Ly cxts -> [EventZero] -> Ly cxts
suffixGrace k []    = k
suffixGrace k xs    = k +++ grace gblock
  where
    gblock = blockS $ foldr (flip suffix) elementBlk xs



       
oflat :: (Perform evt) =>
         Ly CT_Element -> ViewL (EvtPosition evt) -> ProcessM (Ly CT_Element)
             
oflat lyk  EmptyL               = return lyk 

oflat lyk (Evt e :< sq)         = do
    ez        <- eventZero e
    updateCurrentPitch ez
    oflat (lyk `suffix` ez) (viewl sq)

oflat lyk (Sequence ts :< sq)   = do
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
                        



merge :: (Append cxts Poly, Append cxts Block) => Ly cxts -> [Ly a] -> Ly cxts
merge k []     = k
merge k (x:xs) = let poly = foldl fn (block x) xs in 
    k +++ openPoly +++ poly +++ closePoly
  where
    fn acc a = acc \\ (block a)

run'oflat :: (Perform evt) 
          => EventTree evt -> ProcessM (Ly CT_Element)                 
run'oflat t = do 
    elt_list <- asks initial_ly_context
    oflat elt_list (viewl $ unET t) 


renderLy1 :: (Perform evt) => EventTree evt -> Perform_Ly_Env -> Ly CT_Element 
renderLy1 tree env = evalPerform (run'oflat tree) ly_state env
  where
    ly_state = intial_ly_state { relative_pitch = (initial_relative_pitch env) }
 
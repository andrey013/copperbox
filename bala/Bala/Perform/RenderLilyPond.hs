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

import Bala.Perform.EventTree hiding (chord)

import qualified Bala.Base.Base as B
import Bala.Format.SymLilyPond.LilyPond

import Control.Applicative 
import Control.Monad.State

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

lyPitch' () = lyPitch

                           
relative :: B.Pitch -> RenderSt -> RenderSt
relative p st = st { relative_pitch = p } 

  
runRenderLy  = evalState  


withAttr :: (CAttr repr, Attribute elt att) 
         => repr att -> repr elt -> repr elt    
withAttr a e = e %% a


optAttr :: (CAttr repr, Attribute elt att) 
        => repr elt -> Maybe (repr att) -> repr elt
optAttr e Nothing   = e
optAttr e (Just a)  = e `attr` a



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
  

accidental :: (CAccidental repr) 
           => B.Accidental -> Maybe (repr Accidental)
accidental B.Flat          = Just flat
accidental B.Sharp         = Just sharp
accidental B.DoubleSharp   = Just doubleSharp
accidental B.DoubleFlat    = Just doubleFlat
accidental _               = Nothing

octaveSpec :: (COctaveSpec repr) => Int -> Maybe (repr OctaveSpec)             
octaveSpec i
    | i == 0    = Nothing
    | i <  0    = Just $ lowered (abs i)
    | otherwise = Just $ raised i
    
lyPitch :: (CPitch repr, CAttr repr, CAccidental repr, COctaveSpec repr) 
         => B.Pitch -> RenderM (repr Pitch)    
lyPitch p = currentOctave p >>= \spec -> 
            return $ pch `optAttr` actl `optAttr` (octaveSpec spec)
  where
    pch  = pitch $ lyPitchLetter $ B.pitchLetter p
    actl = accidental $ B.pitchAccidental p



lyNote p = do 
    pch <- lyPitch p
    return (note pch)
    

-- simpleDur :: Ratio Integral
durationpart d 
  | d == B.longa                      = Just $ Right longa  
  | d == B.double_whole               = Just $ Right breve 
  | d == B.whole                      = Just $ Left (duration 1)
  | d == B.half                       = Just $ Left (duration 2)
  | d == B.quarter                    = Just $ Left (duration 4)
  | d == B.eighth                     = Just $ Left (duration 8)
  | d == B.sixteenth                  = Just $ Left (duration 16)
  | d == B.thirty_second              = Just $ Left (duration 32)
  | d == B.sixty_fourth               = Just $ Left (duration 64)
  | d == B.one_hundred_twenty_eighth  = Just $ Left (duration 128)
  | otherwise                         = Nothing




                 
dotpart i   
    | i > 0       = Just (dotted $ fromIntegral i)
    | otherwise   = Nothing

data DurationStatus = SAME | DIFF

currentDuration :: B.Duration -> RenderM DurationStatus
currentDuration d = do
    d' <- gets relative_duration
    if (d == d') 
      then (return SAME) 
      else (modify (\s -> s {relative_duration = d}) >> return DIFF)


-- The type of a Duration is complicated by breve and longa not having the
-- same type as duration
    
type AltDuration repr = Either (repr Duration) (repr CmdLongDuration)
  
lyDuration :: (CCmdLongDuration repr,
               CDuration repr,
               CDotted repr,
               CAttr repr) 
           => B.Duration -> RenderM (Maybe (AltDuration repr))
                      
lyDuration d = currentDuration d >>= mkD
  where 
    mkD SAME    = return Nothing
    mkD _      = let (dur,dots) = B.simplifyDuration d
                     root       = durationpart dur
                 in case root of
                    Just (Left a)  -> return $ Just $ Left (a `optAttr` (dotpart dots))
                    Just (Right a) -> return $ Just $ Right (a `optAttr` (dotpart dots))
                    Nothing        -> return Nothing    

applyDuration e Nothing           = e
applyDuration e (Just (Left a))   = e `attr` a
applyDuration e (Just (Right b))  = e `attr` b



pitch'duration evt = do
  n' <- lyPitch (pitchOf evt)
  d' <- lyDuration (durationOf evt)
  return (n', d')    

rest'duration evt = do
  d' <- lyDuration (durationOf evt)
  return d'
    
pitchOrRest'duration evt 
    | isRest evt    = rest'duration evt >>= \d -> return (Right (),d)
    | otherwise     = pitch'duration evt >>= \(p,d) -> return (Left p,d) 

push e es = e:es

close k []              = k
close k [(Left n,d)]    = k +++ (note n `applyDuration` d)
close k [(Right (),d)]  = k +++ (rest `applyDuration` d)
close k xs@((_,d):_)    = k +++ khord xs
  where
    khord ps = let ps' = foldl fn [] ps 
               in chord ps' `applyDuration` d
    fn acc (Left p,_) = p : acc
    fn acc _          = acc



   
       
oflat (lyk,pstk)  EmptyTree     = return (lyk,pstk) 


-- Next pushes its element on the stack and closes it.
-- It might be the first note of a chord
oflat (lyk,pstk) (Next t evt)    = do 
    (lyk',pstk')    <- oflat (lyk,pstk) t
    e'              <- pitchOrRest'duration evt
    return (close lyk' (e':pstk'), [])


-- Par pushes onto the stack
oflat (lyk,pstk) (Par t evt)     = do 
    (lyk',pstk')    <- oflat (lyk,pstk) t
    e'              <- pitchOrRest'duration evt
    return (lyk', push e' pstk')
      
         
oflat (lyk,pstk) (Sequence t ts) = do
    (lyk',pstk')  <- oflat (lyk,pstk) t
    xs            <- mapM (oflat (elementCtx,[])) ts
    return (merge (close lyk' pstk') (map pclose xs), [])     
  where
    pclose (k,st) = close k st



merge :: (CBlock repr, CPoly repr, CSnocList repr CT_Element)
      => repr (SnocList CT_Element) -> [repr (SnocList CT_Element)] -> repr (SnocList CT_Element)
merge k []     = k
merge k (x:xs) = let poly = foldl fn (block x) xs in 
    k +++ openPoly +++ poly +++ closePoly
  where
    fn acc a = acc \\ (block a)


run'oflat () t = do 
    (lyk,pstk) <- oflat (elementCtx,[]) t 
    return (close lyk pstk)

 
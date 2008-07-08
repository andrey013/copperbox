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

{-
    mkPitch             :: forall repr ctx.  
                           (SymOctaveSpec repr,
                           SymAccidental repr,
                           SymAttr repr,
                           SymPitch repr)
                           => B.Pitch -> RenderM (repr (Pitch ctx))
                           
-}

                           
relative :: B.Pitch -> RenderSt -> RenderSt
relative p st = st { relative_pitch = p } 

  
runRenderLy  = evalState  


withAttr :: (SymAttr repr, Attribute a att) 
         => repr (att ctx_att) -> repr (a ctx) -> repr (a ctx)    
withAttr a e = e %% a


optAttr :: (SymAttr repr, Attribute a att) 
        => repr (a ctx) -> Maybe (repr (att ctx_att)) -> repr (a ctx)           
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
  

accidental :: (SymAccidental repr) 
           => B.Accidental -> Maybe (repr (Accidental ctx))
accidental B.Flat          = Just flat
accidental B.Sharp         = Just sharp
accidental B.DoubleSharp   = Just doubleSharp
accidental B.DoubleFlat    = Just doubleFlat
accidental _               = Nothing

octaveSpec :: (SymOctaveSpec repr) => Int -> Maybe (repr (OctaveSpec ctx))             
octaveSpec i
    | i == 0    = Nothing
    | i <  0    = Just $ lowered (abs i)
    | otherwise = Just $ raised i
    
lyPitch :: (SymPitch repr, SymAttr repr, SymAccidental repr, SymOctaveSpec repr) 
         => B.Pitch -> RenderM (repr (Pitch ctx))    
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
    
type AltDuration repr ctx = Either (repr (Duration ctx)) (repr (CmdLongDuration CT_Element))
  
lyDuration :: (SymCmdLongDuration repr,
               SymDuration repr,
               SymDotted repr,
               SymAttr repr) 
           => B.Duration -> RenderM (Maybe (AltDuration repr ctx))
                      
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



   
{-
oflat :: (SymChord repr, SymCList repr CT_Element, SymPoly repr, SymBlock repr) 
      => (t -> RenderM (repr (Note CT_Element)))
      -> (repr (CList CT_Element), [repr (Note CT_Element)])
      -> EventTree t
      -> RenderM (repr (CList CT_Element), [repr (Note CT_Element)])
-}         
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



merge :: (SymBlock repr, SymPoly repr, SymCList repr ctx) 
      => repr (CList ctx) -> [repr (CList subctx)] -> repr (CList ctx)
merge k []     = k
merge k (x:xs) = let poly = foldl fn (block x) xs in 
    k +++ openPoly +++ poly +++ closePoly
  where
    fn acc a = acc \\ (block a)


run'oflat () t = do 
  (lyk,pstk) <- oflat (elementCtx,[]) t 
  return (close lyk pstk)

 
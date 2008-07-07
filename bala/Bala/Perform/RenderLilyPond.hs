{-# LANGUAGE FlexibleContexts #-}

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

import Control.Monad.State

type RenderM a = State RenderSt a


data RenderSt = RenderSt { 
    relative_pitch       :: B.Pitch
  } 

st_zero = RenderSt { 
    --  Middle C (c') is the default in LilyPond. 
    relative_pitch = B.c4
  } 

relative :: B.Pitch -> RenderSt -> RenderSt
relative p st = st { relative_pitch = p } 

  
runRenderLy  = evalState  
  
  
-- lyPitch -> Pitch -> 
lyPitchLetter :: B.PitchLetter -> PitchName
lyPitchLetter = toEnum . fromEnum 

addAccidental :: (SymAccidental repr, SymPitch repr, SymAttr repr) 
              => B.Accidental -> repr (Pitch ctx) -> RenderM (repr (Pitch ctx))
addAccidental B.Flat          p = return $ p #@ flat
addAccidental B.Sharp         p = return $ p #@ sharp
addAccidental B.DoubleSharp   p = return $ p #@ doubleSharp
addAccidental B.DoubleFlat    p = return $ p #@ doubleFlat
addAccidental _               p = return $ p


odisp :: B.Pitch -> B.Pitch -> Int
odisp p p' = 
  let (ityp, _) = B.unInterval $ B.pitchDifference p p'
      absdisp  = ceiling $ (fromIntegral (ityp - 4) / 8.0)
      disp = if p > p' then (negate absdisp) else absdisp in 
  if (ityp < 4) then 0 else disp

octaveDisplacement :: B.Pitch -> RenderM Int
octaveDisplacement p = do
  p' <- gets relative_pitch
  return $ odisp p' p

updateRelative :: B.Pitch -> RenderM ()
updateRelative p = modify (\s -> s {relative_pitch = p})
  

                
addOctaveSpec :: (SymOctaveSpec repr, SymPitch repr, SymAttr repr) 
              => Int -> repr (Pitch ctx) -> RenderM (repr (Pitch ctx))                 
addOctaveSpec i p 
    | i == 0    = return $ p
    | i <  0    = return $ p #@ lowered (abs i)
    | otherwise = return $ p #@ raised i
  

lyPitch :: (SymPitch repr, SymAttr repr, SymAccidental repr, SymOctaveSpec repr) 
        => B.Pitch -> RenderM (repr (Pitch ctx))
lyPitch p = let name = lyPitchLetter $ B.pitchLetter p
                acc  = B.pitchAccidental p
  in do
    lyp   <- addAccidental acc (pitch name)
    spec  <- octaveDisplacement p
    updateRelative p
    addOctaveSpec spec lyp

    
lyPitches ps = foldM fn elementCtx  ps
  where
    fn acc e = lyPitch e >>= return . (acc +++)


push e es = e:es

close k []  = k
close k [e] = k +++ e 
close k xs  = k +++ chord xs 

oflat :: (SymPitch repr,
          SymAttr repr,
          SymAccidental repr,
          SymOctaveSpec repr,
          SymBlock repr,
          SymChord repr, 
          SymPoly repr,
          SymCList repr CT_Element) 
      => (repr (CList CT_Element), [repr (Pitch CT_Element)])
         -> EventTree B.Pitch
         -> RenderM (repr (CList CT_Element), [repr (Pitch CT_Element)])
oflat (lyk,pstk)  EmptyTree     = return (lyk,pstk) 

-- Next pushes its element on the stack and closes it.
-- It might be the first note of a chord
oflat (lyk,pstk) (Next t evt)    = do 
    (lyk',pstk')    <- oflat (lyk,pstk) t
    e'              <- lyPitch evt
    return (close lyk' (e':pstk'), [])


-- Par pushes onto the stack
oflat (lyk,pstk) (Par t evt)     = do 
    (lyk',pstk')    <- oflat (lyk,pstk) t
    e'              <- lyPitch evt
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
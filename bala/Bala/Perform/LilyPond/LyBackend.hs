
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.LilyPond.LyBackend
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit LilyPond from Score.
--
--------------------------------------------------------------------------------

module Bala.Perform.LilyPond.LyBackend where



import Bala.Format.Output.OutputLilyPond


import Bala.Perform.Base.PerformMonad
import Bala.Perform.LilyPond.Class
import Bala.Perform.LilyPond.LyScoreDatatypes

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (reverse)


type ProcessM pch dur a = 
       PerformM (Perform_Ly_State pch dur) (Perform_Ly_Env pch) a

data Perform_Ly_State pch dur = Perform_Ly_State { 
    relative_pitch      :: pch,
    relative_duration   :: dur
    -- part_refs           :: ScPolyRefs pch dur
  }  


data Perform_Ly_Env pch = Perform_Ly_Env {
    initial_ly_context        :: LyCxt_Element,
    initial_relative_pitch    :: pch 
  }



infixl 7 *!
(*!) e oa   = maybe e (e !) oa


state0 :: (LilyPondPitch pch, LilyPondDuration dur) => Perform_Ly_State pch dur
state0 = Perform_Ly_State { 
    relative_pitch      = middleC,
    relative_duration   = quaternoteDuration
  }  

default_ly_env :: LilyPondPitch pch => Perform_Ly_Env pch  
default_ly_env = Perform_Ly_Env {
    initial_ly_context        = elementStart,
    initial_relative_pitch    = middleC
  }


generateLilyPondScore :: (LilyPondPitch pch, LilyPondDuration dur)
                      => LyScScore pch dur
                      -> Perform_Ly_Env pch 
                      -> [LyCmdScore]
generateLilyPondScore sc env = evalPerform (renderScore sc) ly_state env
  where 
    ly_state = state0



getPitch (LyScNote scp _) = scp

getChordDuration :: LilyPondDuration dur => LyScGlyph pch dur -> dur
getChordDuration (LyScChord ((LyScNote _ d):_)) = d
getChordDuration _                              = quaternoteDuration




suffixWith :: Append cxts cxta
           => Ly cxts 
           -> ProcessM pch dur (Ly cxta) 
           -> ProcessM pch dur (Ly cxts)
suffixWith ctx f = (ctx +++) <$> f 

 
     

-- | @LyScScore --> \\book@ 
renderScore :: (LilyPondPitch pch, LilyPondDuration dur)
           => LyScScore pch dur 
           -> ProcessM pch dur [LyCmdScore]
renderScore (LyScScore se) = reverse <$> F.foldlM fn [] se
  where
    fn xs p = flip (:) xs <$> renderPart p
    
    

-- | @LyScPart --> \\score@ 
renderPart :: (LilyPondPitch pch, LilyPondDuration dur)
           => LyScPart pch dur 
           -> ProcessM pch dur LyCmdScore
renderPart (LyScPart i se) = 
    (score . block) <$> F.foldlM renderPolyPhrase elementStart se
  
  

renderPolyPhrase :: (LilyPondPitch pch, LilyPondDuration dur)
                 => LyCxt_Element 
                 -> LyScPolyPhrase pch dur  
                 -> ProcessM pch dur LyCxt_Element 
renderPolyPhrase cxt (LyScSingletonPhrase x)   =
    (cxt >|<) <$> renderSegment elementStart x

renderPolyPhrase cxt (LyScPolyPhrase xs)   = 
    mergePolys cxt <$> mapM (renderSegment elementStart) xs



mergePolys k (x:xs) = let poly = foldl fn (block x) xs in 
    k +++ openPoly +++ poly +++ closePoly
  where
    fn acc a = acc \\ (block a)
mergePolys k _      = error "mergePolys - ill constructed PolyPhrase - a bug"

       
renderSegment :: (LilyPondPitch pch, LilyPondDuration dur)
              => LyCxt_Element 
              -> LyScSegment pch dur 
              -> ProcessM pch dur LyCxt_Element  
renderSegment cxt (LyScSegment se) = F.foldlM renderMeasure cxt se



renderMeasure :: (LilyPondPitch pch, LilyPondDuration dur)
              => LyCxt_Element 
              -> LyScMeasure pch dur 
              -> ProcessM pch dur LyCxt_Element 
renderMeasure cxt (LyScMeasure i xs se) = F.foldlM renderGlyph cxt se



renderGlyph :: (LilyPondPitch pch, LilyPondDuration dur)
            => LyCxt_Element 
            -> LyScGlyph pch dur 
            -> ProcessM pch dur LyCxt_Element

renderGlyph cxt (LyScNote scp d)            = suffixWith cxt $
    fn <$> renderPitch scp  <*> differDuration d
  where
    fn p od = note p *! od 

renderGlyph cxt (LyScRest d)                = suffixWith cxt $
    (rest *!)   <$> differDuration d
    
renderGlyph cxt (LyScSpacer d)              = suffixWith cxt $ 
    (spacer *!) <$> differDuration d

renderGlyph cxt a@(LyScChord xs)            = suffixWith cxt $ 
    fn <$> mapM (renderPitch . getPitch) xs  
       <*> differDuration (getChordDuration a)
  where
    fn xs od = chord xs *! od
    
renderGlyph cxt (LyScGraceNotes xs)         = suffixWith cxt $ 
    fn <$> mapM (renderPitch . getPitch) xs
  where
    fn        = grace . blockS . foldl op elementStart
    op c e    = c +++ note e 


    
-- | @LyScPitch --> LyPitch@
renderPitch :: LilyPondPitch pch => pch -> ProcessM pch dur LyPitch 
renderPitch pch = 
    fn <$> pure (lyPitchName pch) <*> pure (lyAccidental pch) 
                                  <*> differOctaveSpec pch
  where
    fn pn oa oos = (pitch pn) *! oa *! oos                                  
      

    
differOctaveSpec :: LilyPondPitch pch 
                => pch 
                -> ProcessM pch dur (Maybe LyOctaveSpec)    
differOctaveSpec p = fn p <$> gets relative_pitch <*
                              modify (\s -> s {relative_pitch = p})   
  where
    fn new old = let i = octaveDist old new 
                 in case i `compare` 0 of
                      EQ -> Nothing
                      LT -> Just $ lowered (abs i)
                      GT -> Just $ raised i


differDuration :: LilyPondDuration dur 
              => dur 
              -> ProcessM pch dur (Maybe LyDuration)
differDuration d = fn d =<< gets relative_duration
  where
    fn new old | new == old   = return Nothing
               | otherwise    = do 
                      modify (\s -> s {relative_duration = new})
                      return $ optLyDuration new
    
                



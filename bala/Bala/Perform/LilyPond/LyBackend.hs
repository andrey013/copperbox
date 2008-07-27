
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

import qualified Bala.Base as B
import Bala.Format.Score
import Bala.Format.Output.OutputLilyPond

import Bala.Perform.LilyPond.Class
import Bala.Perform.Base.PerformMonad

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Ratio

type ProcessM pch dur a = PerformM (Perform_Ly_State pch dur) (Perform_Ly_Env pch) a

data Perform_Ly_State pch dur = Perform_Ly_State { 
    relative_pitch      :: pch,
    relative_duration   :: dur
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
                      => ScScore pch dur
                      -> Perform_Ly_Env pch 
                      -> LyCmdBook
generateLilyPondScore sc env = evalPerform (renderScore sc) ly_state env
  where 
    ly_state = state0



-- | @ScScore --> \\book@ 
renderScore :: (LilyPondPitch pch, LilyPondDuration dur)
           => ScScore pch dur 
           -> ProcessM pch dur LyCmdBook
renderScore (ScScore sp) = 
    (book . block) <$> F.foldlM renderPart bookStart sp


-- | @ScPart --> \\score@ 
renderPart :: (LilyPondPitch pch, LilyPondDuration dur)
           => LyCxt_Book
           -> ScPart pch dur 
           -> ProcessM pch dur LyCxt_Book          
renderPart cxt (ScPart i refs sp) = 
    (cxt +++) . (score . block) <$> F.foldlM renderPoly elementStart sp
  
  
  
renderPoly :: (LilyPondPitch pch, LilyPondDuration dur)
           => LyCxt_Element 
           -> ScPoly pch dur  
           -> ProcessM pch dur LyCxt_Element 
renderPoly cxt (ScPolyM mea)    = renderMeasure cxt mea
renderPoly cxt (ScPolyRef i)    = undefined



renderMeasure :: (LilyPondPitch pch, LilyPondDuration dur)
              => LyCxt_Element 
              -> ScMeasure pch dur 
              -> ProcessM pch dur LyCxt_Element 
renderMeasure cxt (ScMeasure i sg) = F.foldlM renderGlyph cxt sg



renderGlyph :: (LilyPondPitch pch, LilyPondDuration dur)
            => LyCxt_Element 
            -> ScGlyph pch dur 
            -> ProcessM pch dur LyCxt_Element

renderGlyph cxt (ScNote scp d)  = 
    (fn cxt) <$> renderPitch scp  <*> differDuration d
  where
    fn cxt pch od = cxt +++ (note pch) *! od
        
renderGlyph cxt (ScRest d)      = 
    (cxt +++) . (rest *!)   <$> differDuration d
    
renderGlyph cxt (ScSpacer d)    = 
    (cxt +++) . (spacer *!) <$> differDuration d


renderPitch :: LilyPondPitch pch => ScPitch pch -> ProcessM pch dur (LyPitch) 
renderPitch (ScPitch pch) = 
    fn <$> pure (mkPitchName pch) <*> pure (mkAccidental pch) 
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
                      return $ mkDuration new
    
                



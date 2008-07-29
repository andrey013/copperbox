
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


import Bala.Format.Score
import Bala.Format.Output.OutputLilyPond

import Bala.Perform.LilyPond.Class
import Bala.Perform.Base.PerformMonad

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (reverse)

type ProcessM pch dur a = PerformM (Perform_Ly_State pch dur) (Perform_Ly_Env pch) a

data Perform_Ly_State pch dur = Perform_Ly_State { 
    relative_pitch      :: pch,
    relative_duration   :: dur,
    part_refs           :: ScPartRefs pch dur
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
    relative_duration   = quaternoteDuration,
    part_refs           = mempty
  }  

default_ly_env :: LilyPondPitch pch => Perform_Ly_Env pch  
default_ly_env = Perform_Ly_Env {
    initial_ly_context        = elementStart,
    initial_relative_pitch    = middleC
  }

generateLilyPondScore :: (LilyPondPitch pch, LilyPondDuration dur)
                      => ScScore pch dur
                      -> Perform_Ly_Env pch 
                      -> [LyCmdScore]
generateLilyPondScore sc env = evalPerform (renderScore sc) ly_state env
  where 
    ly_state = state0


getPitch (ScNote scp _) = scp

getChordDuration :: LilyPondDuration dur => ScGlyph pch dur -> dur
getChordDuration (ScGroup ScChord ((ScNote _ d):_))   = d
getChordDuration _                                    = quaternoteDuration




suffixWith :: Append cxts cxta
           => Ly cxts 
           -> ProcessM pch dur (Ly cxta) 
           -> ProcessM pch dur (Ly cxts)
suffixWith ctx f = (ctx +++) <$> f 

setPartRefs refs f = do
    modify (\s -> s { part_refs = refs })
    f

withPartRefs :: (LilyPondPitch pch, LilyPondDuration dur)
             => (ScPartRefs pch dur -> ProcessM pch dur a) 
             -> ProcessM pch dur a 
withPartRefs f = do 
    dict <- gets part_refs
    f dict    
     

-- | @ScScore --> \\book@ 
renderScore :: (LilyPondPitch pch, LilyPondDuration dur)
           => ScScore pch dur 
           -> ProcessM pch dur [LyCmdScore]
renderScore (ScScore sp) = reverse <$> F.foldlM fn [] sp
  where
    fn xs p = flip (:) xs <$> renderPart p

-- | @ScPart --> \\score@ 
renderPart :: (LilyPondPitch pch, LilyPondDuration dur)
           => ScPart pch dur 
           -> ProcessM pch dur LyCmdScore
renderPart (ScPart i refs sm) = setPartRefs refs $ 
    (score . block) <$> F.foldlM renderMeasure elementStart sm
  
  
{-  
renderPoly :: (LilyPondPitch pch, LilyPondDuration dur)
           => LyCxt_Element 
           -> ScPoly pch dur  
           -> ProcessM pch dur LyCxt_Element 
renderPoly cxt (ScPolyM mea)    = renderMeasure cxt mea
renderPoly cxt (ScPolyRef idxs)   = do 
    refs    <- refsTo idxs
    polys   <- mapM poly1 refs
    return $ mergePolys cxt polys
  where
    refsTo :: (LilyPondPitch pch, LilyPondDuration dur) 
           => [Integer] 
           -> ProcessM pch dur [Seq (ScMeasure pch dur)]
    refsTo xs = withPartRefs $ \mp -> 
           return $ catMaybes $ map ((flip getPolyRef) mp) xs
               
    poly1 :: (LilyPondPitch pch, LilyPondDuration dur) 
          => Seq (ScMeasure pch dur) 
          -> ProcessM pch dur LyCxt_Element 
    poly1 sm = F.foldlM renderMeasure elementStart sm


mergePolys k []     = k
mergePolys k (x:xs) = let poly = foldl fn (block x) xs in 
    k +++ openPoly +++ poly +++ closePoly
  where
    fn acc a = acc \\ (block a)
    
-}    


renderMeasure :: (LilyPondPitch pch, LilyPondDuration dur)
              => LyCxt_Element 
              -> ScMeasure pch dur 
              -> ProcessM pch dur LyCxt_Element 
renderMeasure cxt (ScMeasure i xs sg) = F.foldlM renderGlyph cxt sg



renderGlyph :: (LilyPondPitch pch, LilyPondDuration dur)
            => LyCxt_Element 
            -> ScGlyph pch dur 
            -> ProcessM pch dur LyCxt_Element

renderGlyph cxt (ScNote scp d)            = suffixWith cxt $
    fn <$> renderPitch scp  <*> differDuration d
  where
    fn p od = note p *! od 

renderGlyph cxt (ScRest d)                = suffixWith cxt $
    (rest *!)   <$> differDuration d
    
renderGlyph cxt (ScSpacer d)              = suffixWith cxt $ 
    (spacer *!) <$> differDuration d

renderGlyph cxt a@(ScGroup ScChord xs)    = suffixWith cxt $ 
    fn <$> mapM (renderPitch . getPitch) xs  
       <*> differDuration (getChordDuration a)
  where
    fn xs od = chord xs *! od
    
renderGlyph cxt (ScGroup ScGraceNotes xs) = suffixWith cxt $ 
    fn <$> mapM (renderPitch . getPitch) xs
  where
    fn        = grace . blockS . foldl op elementStart
    op c e    = c +++ note e 


    

renderPitch :: LilyPondPitch pch => ScPitch pch -> ProcessM pch dur LyPitch 
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
    
                



{-# LANGUAGE MultiParamTypeClasses #-}


module TabApp where

import TabBase
import TabParser

import Bala.Base
import Bala.Format.Output.OutputLilyPond hiding (chord)
import Bala.Perform.EventTree
import Bala.Perform.PerformBase
import Bala.Perform.PerformLilyPond
import Bala.Perform.PerformMidi


import qualified Data.Foldable as F
import Data.List (nub, groupBy, sortBy)
import qualified Data.Map as M
import Data.Sequence hiding (drop)

type OnsetDuration = (Int,Int)
 
data Evt = EvtNote Pitch OnsetDuration
         | EvtRest OnsetDuration
  deriving Show

instance Perform Evt Pitch Duration where
  eventvalues (EvtNote p d) = (Just p, Just eighth)
  eventvalues (EvtRest d)   = (Nothing, Just eighth)
  
  
{-
instance Perform Evt where
    opitch (EvtNote p _) = Just p
    opitch _             = Nothing
     
    oduration (EvtNote _ (_,d)) = Just eighth 
    oduration (EvtRest (_,d)) = Just eighth -- TODO
-}    
    
data CardiEvt = SingleEvt Evt
              | MultiEvt [Evt] 
  deriving Show

type ODMap = M.Map Int Int

eventTimes :: Bar -> ODMap
eventTimes (Bar i xs) = let xs'   = nub $ map (\(TabNote i _ _) -> i) xs
                            xs''  = zip xs' (drop 1 (xs' ++ [i]))
                        in foldr fn M.empty xs''
  where
    fn (i,j) m = M.insert i (j - i) m                     

shuffleBar :: Bar -> [Evt]
shuffleBar bar@(Bar i xs) = let etmap = eventTimes bar 
                                notes = foldr (fn etmap) [] xs
                            in optprefix i notes
  where
    fn etmap (TabNote o _ p) acc  = 
        let d = M.findWithDefault 1 o etmap
        in EvtNote p (o,d) : acc

    -- no notes so fill with rest
    optprefix i []                        = [EvtRest (1,i)]
     
    optprefix _ xs@(e:_) | onset e > 1    = EvtRest (1,onset e) : xs
    
    optprefix _ xs                        = xs

buildEvtTree :: EventTree Evt -> [Evt] -> EventTree Evt
buildEvtTree tree xs = let xss = groupBy onsetTime xs
                       in foldl fn tree xss
  where 
    onsetTime a b = onset a == onset b
    
    fn tree [e] = tree # event e
    fn tree xs  = tree # chord (sortBy comparePitch xs)


comparePitch (EvtNote p _) (EvtNote p' _) = p' `compare` p'
      
         
onset (EvtNote _ (o,_)) = o 
onset (EvtRest (o,_))   = o


instance Affi Evt where
  affi (EvtNote p (o,d))  = tupledS [affi p, shows o, shows d]
  affi (EvtRest (o,d))    = tupledS [showChar 'r', shows o, shows d]
  
  

processTab :: Seq Bar -> EventTree Evt
processTab = F.foldl fn root 
  where
    fn tree bar = buildEvtTree tree (shuffleBar bar)
  

example_tab_lines = [5,14,23,32,41]
  
main = do
    sq <- parseTabfile "../../samples/tab/example-tab.txt" example_tab_lines (state_zero standard_tuning)
    writeMidi "../out/example-tab.midi" (toMidi sq)
    writeLy "../out/example-tab.ly" (tab_ly $ processTab sq)
    execLilyPondOn "../out/example-tab.ly"
  where
    toMidi sq = renderMidi1 (processTab sq) default_midi_env


tab_template musicexpr = 
    toplevel 
      +++ version "2.10.3" 
      +++ header (headerBlk +++ title "tab")
      +++ book
            (block (score 
                      (block (relative (_c ! raised 1) musicexpr))))
  

tab_ly tree = 
  let expr    = elementBlk +++ key _g major +++ clef treble +++ time (3//4)
      env     = default_ly_env { initial_ly_context = expr }
      ly_expr = renderLy1 tree env
  in tab_template ly_expr
              
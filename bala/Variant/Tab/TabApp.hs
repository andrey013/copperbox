{-# LANGUAGE TypeSynonymInstances #-}

-- ghci ...
-- :set -i../../Bala:../../ZMidi:../../HNotate

module TabApp where

import TabBase
import TabParser

import TabApp.Datatypes
import TabApp.StaffLocations 

import Bala hiding ( Pitch(..), Event(..) )
import HNotate

import qualified Data.Foldable as F
import Data.List (nub, groupBy, sortBy)
import qualified Data.Map as M
import Data.Sequence hiding (drop)

type OnsetDuration = (Int,Int)
 
data TEvt = EvtNote Pitch OnsetDuration
          | EvtRest OnsetDuration
  deriving Show

  
    
data CardiEvt = SingleEvt TEvt
              | MultiEvt [TEvt] 
  deriving Show

type ODMap = M.Map Int Int

eventTimes :: Bar -> ODMap
eventTimes (Bar i xs) = let xs'   = nub $ map (\(TabNote i _ _) -> i) xs
                            xs''  = zip xs' (drop 1 (xs' ++ [i]))
                        in foldr fn M.empty xs''
  where
    fn (i,j) m = M.insert i (j - i) m                     

shuffleBar :: Bar -> [TEvt]
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

buildEventList :: EventList -> [Event] -> EventList
buildEventList tree xs = undefined
{-
buildEventList tree xs = let xss = groupBy onsetTime xs
                         in foldl fn tree xss
  where 
    onsetTime a b = onset a == onset b
    
    fn tree [e] = tree # note e
    fn tree xs  = tree # chord (sortBy comparePitch xs)

-}
comparePitch (EvtNote p _) (EvtNote p' _) = p' `compare` p'
      
         
onset (EvtNote _ (o,_)) = o 
onset (EvtRest (o,_))   = o

  
  

processTab :: Seq Bar -> System
processTab = undefined
{-
processTab = system1 "tab" . F.foldl fn root 
  where
    fn tree bar = buildEventList tree (shuffleBar bar)
-}  

example_tab_lines = [5,14,23,32,41]

p1 :: Pitch
p1 = fromInteger (-1)

main = print p1 

{-  
main = do
    sq <- parseTabfile "../../samples/tab/example-tab.txt" example_tab_lines (state_zero standard_tuning)
    writeMidi "../out/example-tab.midi" (toMidi sq)
    writeLy "../out/example-tab.ly" (tab_ly $ processTab sq)
    runLilyPondOn "../out/example-tab.ly"
  where
    toMidi sq = systemToMidi default_midi_system (processTab sq)
-}
tab_ly evts = undefined -- systemToLy (default_ly_system "tab" pre) evts
--  where pre = elementStart +++ key _g major +++ clef treble +++ time (3,4)

             
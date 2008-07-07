

-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- :set -i..

module Main where

import Bala.Format.Midi.Midi
import Bala.Base.Base

import Bala.Perform.RenderMidi
import qualified Bala.Perform.EventTree as E
import Bala.Perform.EventTree ( (#) )

-- For Lilypond
import qualified Bala.Perform.RenderLilyPond as RLy
import Bala.Format.Base.SymBase
import Text.PrettyPrint.Leijen hiding (dot)

data NrEvent = Note Pitch Duration
             | Rest Duration
  deriving (Eq,Show)

instance Renderable NrEvent where 
  duration (Note _ d) = d
  duration (Rest d)   = d
  
  generates (Note _ _) = Just (\(Note p _) -> return (Left p))  
  generates (Rest _)   = Nothing 

dur 16  = sixteenth
dur 8   = eighth
dur 4   = quarter
dur 2   = half
dur 1   = whole
      
n :: Pitch -> Int -> NrEvent
n p i = Note p (dur i)

r :: Int -> NrEvent    
r = Rest . dur

-- a major

events_bars1_4 :: [NrEvent]
events_bars1_4 = 
      [ n a4 16, n b4 16, n c5is 16, n c5is 16, 
        n c5is 16, n a4 16, n c5is 16, n c5is 16,
        
        n c5is 16, n a4 16, n b4 16, n c5is 16,
        n b4 16, n a4 16, n a4 16, r 16,
        
        n e5 16, n d5 16, n c5is 16, n b4 16,
        n c5is 16, n a4 16, n b4 16, n c5is 16,
        
        n a4 16, n b4 16, n b4 16, n a4 16,
        n a4 8, r 8       
      ]
         

bars1_4 :: E.EventTree NrEvent
bars1_4 = foldl (flip E.event) E.root events_bars1_4
 

bulgarian6 = (E.Perf [bars1_4])   
  
main = output bulgarian6 "bulgarian6.midi"

-------

-- LilyPond handling is very unpolished

printDoc :: (() -> P a) -> IO ()
printDoc e = let sdoc = renderPretty 0.8 80 (unP (e ())) in do
    putStr ((displayS sdoc []) ++ "\n")
    
    
-- only handles pitches at the moment

extractPitches :: [NrEvent] -> [Pitch]
extractPitches = foldr fn [] 
  where
    fn (Note p _) acc = p : acc
    fn _          acc = acc

buildPitchTree :: [Pitch] -> E.EventTree Pitch
buildPitchTree = foldl (flip E.event) E.root  

tree1_4 = buildPitchTree $ extractPitches events_bars1_4
                       
bulgarian6_ly () = 
  RLy.runRenderLy (RLy.run'oflat () tree1_4) (RLy.relative c4 RLy.st_zero) 


    
demo_ly = printDoc bulgarian6_ly

  
  
  
  
  
   


-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- :set -i..

module Main where

import Bala.Format.Midi.Midi
import Bala.Base.Base

import Bala.Perform.RenderMidi
import qualified Bala.Perform.EventTree as E
import Bala.Perform.EventTree ( (#) )

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

pitches_bars1_4 :: [NrEvent]
pitches_bars1_4 = 
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
bars1_4 = foldl (flip E.event) E.root pitches_bars1_4
 

bulgarian6 = (E.Perf [bars1_4])   
  
main = output bulgarian6 "bulgarian6.midi"

  
  
  
  
  
   


-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- :set -i..

module Main where

import Bala.Format.Midi.Midi hiding (header)
import Bala.Base.Base

import Bala.Perform.RenderMidi
import qualified Bala.Perform.EventTree as E
import Bala.Perform.EventTree ( (#) )

-- For Lilypond
import qualified Bala.Perform.RenderLilyPond as RLy
import Bala.Format.Base.SymBase
import Bala.Format.SymLilyPond.LilyPond hiding (Pitch, Duration)
import Text.PrettyPrint.Leijen hiding (dot)

data NrEvent = Note Pitch Duration
             | Rest Duration
  deriving (Eq,Show)

instance Renderable NrEvent where 
  duration (Note _ d) = d
  duration (Rest d)   = d
  
  generates (Note _ _) = Just (\(Note p _) -> return (Left p))  
  generates (Rest _)   = Nothing 

durn 16  = sixteenth
durn 8   = eighth
durn 4   = quarter
durn 2   = half
durn 1   = whole
      
n :: Pitch -> Int -> NrEvent
n p i = Note p (durn i)

r :: Int -> NrEvent    
r = Rest . durn

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
  
main = do
    output bulgarian6 "bulgarian6.midi"
    outputLy "bulgarian6.ly" bulgarian6_ly
-------

-- LilyPond handling is very unpolished

printDoc :: (() -> P a) -> IO ()
printDoc e = let sdoc = renderPretty 0.8 80 (unP (e ())) in do
    putStr ((displayS sdoc []) ++ "\n")
    
outputLy :: FilePath -> (() -> P a) -> IO ()
outputLy filename e = let sdoc = renderPretty 0.8 80 (unP (e ())) in do
    writeFile filename ((displayS sdoc []) ++ "\n")
    
    

instance RLy.LyRenderable NrEvent where
    pitchOf (Note p _)        = p
    
    durationOf (Note _ d)     = d
    durationOf (Rest d)       = d 
     
    isPitch (Note _ _)        = True
    isPitch (Rest _)          = False
     
    isRest (Note _ _)         = False
    isRest (Rest _)           = True
    
        


bulgarian_template musicexpr = 
    toplevelCtx 
      +++ version "2.10.3" 
      +++ in_header (title "Bulgarian (6)")
      +++ in_book
            (in_score 
              (relative (_c %% raised 1) musicexpr))
  

bulgarian6_ly () = 
  let e     = elementCtx +++ key _a major +++ clef treble
      mexpr = RLy.runRenderLy (RLy.run'oflat e bars1_4) (RLy.relative c4 RLy.st_zero)
  in bulgarian_template mexpr
   

    
demo_ly = printDoc bulgarian6_ly

  
  
  
  
  
   


-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- :set -i..

module Main where

import Bala.Format.Midi hiding (header)
import Bala.Base

import Bala.Perform.RenderMidi
import qualified Bala.Perform.EventTree as E
import Bala.Perform.EventTree ( (#) )

-- For Lilypond
import qualified Bala.Perform.RenderLilyPond as R
import Bala.Format.Output.OutputLilyPond hiding (Pitch, Duration)

import System.Process (runCommand, waitForProcess)
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
  
main =  do
    output bulgarian6 "bulgarian6.midi"
    outputLy lyfile bulgarian6_ly
    ph <- runCommand ("lilypond " ++ lyfile)  
    waitForProcess ph
    return ()
  where
    lyfile = "bulgarian6.ly" 
    
       
-------

-- LilyPond handling is very unpolished

printDoc :: Ly a -> IO ()
printDoc e = let sdoc = renderPretty 0.8 80 (pretty (unLy e)) in do
    putStr ((displayS sdoc []) ++ "\n")
    
outputLy :: FilePath -> Ly a -> IO ()
outputLy filename e = let sdoc = renderPretty 0.8 80 (pretty (unLy e)) in do
    writeFile filename ((displayS sdoc []) ++ "\n")
    
    

instance R.LyRenderable NrEvent where
    pitchOf (Note p _)        = p
    
    durationOf (Note _ d)     = d
    durationOf (Rest d)       = d 
     
    isPitch (Note _ _)        = True
    isPitch (Rest _)          = False
     
    isRest (Note _ _)         = False
    isRest (Rest _)           = True
    
        


bulgarian_template musicexpr = 
    toplevel 
      +++ version "2.10.3" 
      +++ header (headerBlk +++ title "Bulgarian (6)")
      +++ book
            (block (score 
                      (block (relative (_c ! raised 1) musicexpr))))
  

bulgarian6_ly = 
  let e     = elementBlk +++ key _a major +++ clef treble
      mexpr = R.runRenderLy (R.run'oflat e bars1_4) (R.relative c4 R.st_zero)
  in bulgarian_template mexpr
   

    
demo_ly = printDoc bulgarian6_ly

  
  
  
  
  
   
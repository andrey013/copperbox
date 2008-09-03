
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- ghci ...
-- :set -i../../HNotate:../../ZMidi

module Main where


import HNotate

import Text.PrettyPrint.Leijen hiding (dot)


data NrEvent = Note Pitch Duration
             | Rest Duration
  deriving (Eq,Show)



instance Event NrEvent where
  eventvalues (Note p d) = (Just $ renderPitch p, Just $ renderDuration d)
  eventvalues (Rest d)   = (Nothing, Just $ renderDuration d)
  
  
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
         

bars1_4 :: EventTree NrEvent
bars1_4 = foldl (flip event) root events_bars1_4
 
bulgarian6 :: System NrEvent
bulgarian6 = system1 "bulgarian6" bars1_4   
  

lyOut sys   = let sc = toScore sys measure_2_4
                  notes = onNoteList sc "bulgarian6" $ \se -> 
                                translateLilyPond se middleC
              in case notes of
                  Just a -> putDoc $ pretty a
                  Nothing -> putStrLn "lookup failed"
       
-------

demo = lyOut bulgarian6

main = outputLilyPond bulgarian6 ly_template1 "templates/bulgarian6-out1.ly"


thingy = do 
  tryit ly_template1
  chunkit ly_template1

ly_template1  = "templates/ly0-bulgarian6.ly"
ly_template2  = "templates/ly0-bulgarian6-version.ly"


tryit filename = do 
    result <- parseLyTemplateExpr filename
    case result of
      Left err -> putStr $ show err
      Right ans -> putDoc $ vsep $ map pretty ans

chunkit filename = do 
    result <- parseLySourceChunks filename
    case result of
      Left err -> putStr $ show err
      Right ans -> putDoc $ pretty ans
            
  
{-
bulgarian6_midi = systemToMidi default_midi_system bulgarian6

bulgarian6_ly = systemToLy (default_ly_system "Bulgarian 6" pre) bulgarian6
  where pre = elementStart +++ key _a major +++ clef treble

main =  do
    writeMidi "out/bulgarian6.midi" bulgarian6_midi
    writeLy lyfile bulgarian6_ly
    runLilyPondOn lyfile
  where
    lyfile = "out/bulgarian6.ly" 
    

-}  
  
  
  
   
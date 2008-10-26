

module Main where

import Examples
import HNotate



import System.FilePath (dropExtension)
import System.Process (runCommand, waitForProcess)




main = do 
    putStrLn "Running Tests..."
    outputTestScore anacrusis_test_score
    outputTestScore unmetered_test_score
    outputTestScore hijaz_test_score

    

--------------------------------------------------------------------------------
-- Test scores
    
data TestScore = TestScore { 
    message       :: String,
    ts_system     :: System,
    abc_template  :: FilePath,
    ly_template   :: FilePath,
    abc_outfile   :: FilePath,
    ly_outfile    :: FilePath
  }


anacrusis_test_score :: TestScore
anacrusis_test_score = TestScore
    "Adeste Fidelis should have an anacrusis of a quarter note."
    adeste_fidelis
    "./templates/adeste-fidelis-abc0.abc"
    "./templates/adeste-fidelis-ly0.ly"
    "./out/adeste-fidelis-abc.abc"
    "./out/adeste-fidelis-ly.ly"

unmetered_test_score :: TestScore
unmetered_test_score = TestScore
    "Te Laudamus Domine should be unmetered - no barlines in the output."
    te_laudamus_domine
    "./templates/te-laudamus-domine-abc0.abc"
    "./templates/te-laudamus-domine-ly0.ly"
    "./out/te-laudamus-domine-abc.abc"
    "./out/te-laudamus-domine-ly.ly"

hijaz_test_score :: TestScore
hijaz_test_score = TestScore
    "Hijaz should have the key signature Bb Eb F#."
    hijaz_scale
    "./templates/hijaz-abc0.abc"
    "./templates/hijaz-ly0.ly"
    "./out/hijaz-abc.abc"
    "./out/hijaz-ly.ly"

--------------------------------------------------------------------------------
-- Helpers

outputTestScore :: TestScore -> IO ()
outputTestScore ts = do
    putDashedLine
    putStrLn (message ts)
    putDashedLine
    outputAbc      3 (ts_system ts)   (abc_template ts)  (abc_outfile ts)
    outputLilyPond 3 (ts_system ts)   (ly_template ts)   (ly_outfile ts)
    runAbcOn       (abc_outfile ts)
    runLilyPondOn  (ly_outfile ts)


runAbcOn :: FilePath -> IO ()
runAbcOn filename = do
    ph <- runCommand cmd   
    waitForProcess ph
    return ()
  where
    cmd = "abcm2ps -O " ++ dropExtension filename ++ ".ps  " ++ filename
    
    
runLilyPondOn :: FilePath -> IO ()
runLilyPondOn filename = do
    ph <- runCommand cmd   
    waitForProcess ph
    return ()
  where
    cmd = "lilypond -o " ++ dropExtension filename ++ "  " ++ filename

putDashedLine :: IO ()
putDashedLine = putStrLn (replicate 60 '-')


    

     

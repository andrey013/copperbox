{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Sco101 where

import ZScore.CSDDoc
import ZScore.CsoundGens
import ZScore.CsoundScore
import ZScore.Utils.FormatCombinators
import ZScore.OutputCSD

import System.Directory
import System.Process


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/" 
    writeScore "out/101.sco" sco101
    _ <- system "csound -o devaudio 101.orc out/101.sco"
    return ()


writeScore :: FilePath -> Score -> IO ()
writeScore file_path sco = writeFile file_path sco

full_csd :: Doc
full_csd = cs_options ["-o out/101.wav -W"]




type Score = String

sco101 :: Score
sco101 = unlines $ 
    [ ";FUNCTION 1 USES THE GEN10 SUBROUTINE TO COMPUTE A SINE WAVE"
    , "f 1  0 4096 10   1    "
    , "; INST    START     DURATION"
    , "i 101          0         1"
    , "i 101          3         0.5"
    ]

data I101 = I101 { start :: Double , duration :: Double }


-- All instruments have start and duration

data PField = PField 
      { pfield_name  :: String
      , pfield_val   :: Double
      }




demo01 :: IO ()
demo01 = putStr $ show $ format $ runScoBuilder $ do
    gen10 1 0 4096 [1.0]
    gen9 7 0 513 [(1,1,0), (3, 0.333, 180 )]
    i01_notes



i01_notes :: ScoBuilder ()
i01_notes = dyninst 1 0 3 []


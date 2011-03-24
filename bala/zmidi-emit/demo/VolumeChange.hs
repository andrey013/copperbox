{-# OPTIONS -Wall #-}


module VolumeChange where

import ZMidi.Emit

import ZMidi.Core                               -- package: zmidi-core

import System.Directory


main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

outfile_midi :: FilePath
outfile_midi = "./out/volume_change.mid"

demo01 :: IO ()
demo01 = do
    createDirectoryIfMissing True "./out/"
    putStrLn "Writing volume_change.mid..."
    writeHiMidi outfile_midi $ hiMidi `addTrack` track 0 tune
  where
    tune          = monoVoice 120 getting_quiet
    getting_quiet = instrument honky_tonk >> mapM_ volNote levels

levels :: [Int]
levels = step 16383
  where
    step n | n < 0 = []
    step n         = n : step (n - 400)

volNote :: Int -> NoteList ()
volNote vol = volume (fromIntegral vol) >> note (c_nat 4) dsixteenth 
                                        >> rest dquarter

demo02 :: IO ()
demo02 = do
    ans <- readMidi outfile_midi
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m


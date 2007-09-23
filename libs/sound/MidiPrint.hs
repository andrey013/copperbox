
-- ghci> :set -i./Sound/Midi

module Main where

import Sound.Midi.MidiFile 


import System.Environment

main :: IO ()
main = do 
  args <- getArgs
  case args of
    [path] -> process path
    _ ->  putStrLn "Usage: MidiPrint <filename>"

process :: FilePath -> IO ()
process filename = do
  ans <- readMidi filename
  case ans of
    Left err -> putStrLn $ "Parse error " ++ err
    Right (MidiFile _ ts) -> mapM_ printNotes ts


printNotes :: Track -> IO ()
printNotes (Track xs) = mapM_ f xs
  where 
    f (_, (VoiceEvent (NoteOff _ nt _))) = putStrLn $ "NoteOff " ++ show nt
    f (_, (VoiceEvent (NoteOn  _ nt _))) = putStrLn $ "NoteOn " ++ show nt
    f _                                  = return ()
  
{-  
  case ans of 
    Left err -> putStr err
    Right a -> putStr "OKAY"
    
-}    
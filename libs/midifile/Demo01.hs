
module Main where

import MidiDatatypes
import MidiWrite
import MidiRead




header1 :: Header
header1 = Header MF1 2 (TPB 96)

track1 :: Track
track1 = Track
          [ Event 0 (SMPTEOffset 0 0 0 0 0)
          , Event 0 (SetTempo 48000)
          , Event 0 (EndOfTrack)         
          ]


track2 :: Track
track2 = Track -- [Event 0   (NoteOn  1 60 127), Event 0 (EndOfTrack)]

          [ Event 0   (ProgramChange 1 1)
          
          , Event 0   (NoteOn  1 60 127)
          , Event 400 (NoteOff 1 60 127)
          , Event 0   (NoteOn  1 62 127)
          , Event 400 (NoteOff 1 62 127)
          , Event 0   (NoteOn  1 64 127)
          , Event 400 (NoteOff 1 64 127)
          , Event 0   (NoteOn  1 65 127)
          , Event 400 (NoteOff 1 65 127)
          , Event 0   (NoteOn  1 67 127)
          , Event 400 (NoteOff 1 67 127)
          , Event 0   (NoteOn  1 69 127)
          , Event 400 (NoteOff 1 69 127)
          , Event 0   (NoteOn  1 71 127)
          , Event 400 (NoteOff 1 71 127)
          , Event 0   (NoteOn  1 69 127)
          , Event 400 (NoteOff 1 69 127)                    
          , Event 0 (EndOfTrack) 
          ]


file1 :: MidiFile
file1 = MidiFile header1 [track1, track2]

main :: IO ()
main = do
  writeMidi "test1.mid" file1
  putStrLn "Write sucessful..."
  ast <- readMidi "test1.mid"
  putStrLn $ show ast

  
  



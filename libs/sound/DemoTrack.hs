
module Main where

import Sound.Midi.MidiFile 




header1 :: Header
header1 = Header MF1 2 (TPB 96)

track1 :: Track
track1 = Track
          [ (0, (MetaEvent $ SMPTEOffset 0 0 0 0 0))
          , (0, (MetaEvent $ SetTempo 48000))
          , (0, (MetaEvent $ EndOfTrack))
          ]


track2 :: Track
track2 = Track -- [Event 0   (NoteOn  1 60 127), (0 (EndOfTrack)]

          [ (0,   (VoiceEvent $ ProgramChange 1 1))
          
          , (0,   (VoiceEvent $ NoteOn  1 60 127))
          , (400, (VoiceEvent $ NoteOff 1 60 127))
          , (0,   (VoiceEvent $ NoteOn  1 62 127))
          , (400, (VoiceEvent $ NoteOff 1 62 127))
          , (0,   (VoiceEvent $ NoteOn  1 64 127))
          , (400, (VoiceEvent $ NoteOff 1 64 127))
          , (0,   (VoiceEvent $ NoteOn  1 65 127))
          , (400, (VoiceEvent $ NoteOff 1 65 127))
          , (0,   (VoiceEvent $ NoteOn  1 67 127))
          , (400, (VoiceEvent $ NoteOff 1 67 127))
          , (0,   (VoiceEvent $ NoteOn  1 69 127))
          , (400, (VoiceEvent $ NoteOff 1 69 127))
          , (0,   (VoiceEvent $ NoteOn  1 71 127))
          , (400, (VoiceEvent $ NoteOff 1 71 127))
          , (0,   (VoiceEvent $ NoteOn  1 69 127))
          , (400, (VoiceEvent $ NoteOff 1 69 127))
          , (0,   (MetaEvent  $ EndOfTrack))
          ]


file1 :: MidiFile
file1 = MidiFile header1 [track1, track2]

main :: IO ()
main = do
  writeMidi "test1.mid" file1
  putStrLn "Write sucessful..."
  ast <- readMidi "test1.mid"
  putStrLn $ show ast

  
  

  
  
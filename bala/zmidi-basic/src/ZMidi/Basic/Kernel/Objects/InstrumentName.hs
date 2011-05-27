{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.InstrumentName
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Lookup table for General MIDI instrument name.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.InstrumentName
  (
    instrumentName

  ) where


import qualified Data.IntMap as IntMap
import Data.Word



-- | Get the General MIDI instrument name, index should be in the 
-- range[0..127].
--
instrumentName :: Word8 -> String
instrumentName i = 
    maybe "" id $ IntMap.lookup (fromIntegral i) instrument_name_map

instrument_name_map :: IntMap.IntMap String
instrument_name_map = IntMap.fromAscList names
  where
   names =  [ (  0, "acoustic grand piano"      )
            , (  1, "bright acoustic piano"     )
            , (  2, "electric grand piano"      )
            , (  3, "honky tonk"                )
            , (  4, "electric piano 1"          )
            , (  5, "electric piano 2"          )
            , (  6, "harpsichord"               )
            , (  7, "clavicord"                 )

            , (  8, "celesta"                   )
            , (  9, "glockenspiel"              )
            , ( 10, "music box"                 )
            , ( 11, "vibraphone"                )
            , ( 12, "marimba"                   )
            , ( 13, "xylophone"                 )
            , ( 14, "tubular bells"             )
            , ( 15, "dulcimer"                  )

            , ( 16, "drawbar organ"             )
            , ( 17, "percussive organ"          )
            , ( 18, "rock organ"                )
            , ( 19, "church organ"              )
            , ( 20, "reel organ"                )
            , ( 21, "accordian"                 )
            , ( 22, "harmonica"                 )
            , ( 23, "tango accordian"           )

            , ( 24, "nylon acoustic guitar"     )
            , ( 25, "steel acoustic guitar"     )
            , ( 26, "jazz electric guitar"      )
            , ( 27, "clean electric guitar"     )
            , ( 28, "muted electric guitar"     )
            , ( 29, "overdriven guitar"         )
            , ( 30, "distortion guitar"         )
            , ( 31, "guitar harmonics"          )

            , ( 32, "acoustic bass"             )
            , ( 33, "finger electric bass"      )
            , ( 34, "pick electric bass"        )
            , ( 35, "fretless bass"             )
            , ( 36, "slap bass 1"               )
            , ( 37, "slap bass 2"               )
            , ( 38, "synth bass 1"              )
            , ( 39, "synth bass 2"              )

            , ( 40, "violin"                    )
            , ( 41, "viola"                     )
            , ( 42, "cello"                     )
            , ( 43, "contrabass"                )
            , ( 44, "tremolo strings"           )
            , ( 45, "pizzicato strings"         )
            , ( 46, "orchestral strings"        )
            , ( 47, "timpani"                   )

            , ( 48, "string ensemble 1"         )
            , ( 49, "string ensemble 2"         )
            , ( 50, "synth strings 1"           )
            , ( 51, "synth strings 2"           )
            , ( 52, "choir aahs"                )
            , ( 53, "voice oohs"                )
            , ( 54, "synth voice"               )
            , ( 55, "orchestra hit"             )
            
            , ( 56, "trumpet"                   )
            , ( 57, "trombone"                  )
            , ( 58, "tuba"                      )
            , ( 59, "muted trumpet"             )
            , ( 60, "french horn"               )
            , ( 61, "brass section"             )
            , ( 62, "synth brass 1"             )
            , ( 63, "synth brass 2"             )

            , ( 64, "soprano sax"               )
            , ( 65, "alto sax"                  )
            , ( 66, "tenor sax"                 )
            , ( 67, "baritone sax"              )
            , ( 68, "oboe"                      )
            , ( 69, "english horn"              )
            , ( 70, "bassoon"                   )
            , ( 71, "clarinet"                  )

            , ( 72, "piccolo"                   )
            , ( 73, "flute"                     )
            , ( 74, "recorder"                  )
            , ( 75, "pan flute"                 )
            , ( 76, "blown bottle"              )
            , ( 77, "shakuhachi"                )
            , ( 78, "whistle"                   )
            , ( 79, "ocarina"                   )

            , ( 80, "square lead"               )
            , ( 81, "sawtooth lead"             )
            , ( 82, "calliope lead"             )
            , ( 83, "chiff lead"                )
            , ( 84, "charang lead"              )
            , ( 85, "voice lead"                )
            , ( 86, "fifths lead"               )
            , ( 87, "bass lead"                 )

            , ( 88, "new age pad"               )
            , ( 89, "warm pad"                  )
            , ( 90, "polysynth pad"             )
            , ( 91, "choir pad"                 )
            , ( 92, "bowed pad"                 )
            , ( 93, "metallic pad"              )
            , ( 94, "halo pad"                  )
            , ( 95, "sweep pad"                 )

            , ( 96, "rain"                      )
            , ( 97, "soundtrack"                )
            , ( 98, "crystal"                   )
            , ( 99, "atmosphere"                )
            , (100, "brightness"                )
            , (101, "goblins"                   )
            , (102, "echoes"                    )
            , (103, "sci fi"                    )

            , (104, "sitar"                     )
            , (105, "banjo"                     )
            , (106, "shamisen"                  )
            , (107, "koto"                      )
            , (108, "kalimba"                   )
            , (109, "bagpipe"                   )
            , (110, "fiddle"                    )
            , (111, "shanai"                    )

            , (112, "tingle bell"               )
            , (113, "agogo"                     )
            , (114, "steel drums"               )
            , (115, "woodblock"                 )
            , (116, "taiko drum"                )
            , (117, "melodic tom"               )
            , (118, "synth drum"                )
            , (119, "reverse cymbal"            )

            , (120, "guitar fret noise"         )
            , (121, "breath noise"              )
            , (122, "seashore"                  )
            , (123, "bird tweet"                )
            , (124, "telephone ring"            )
            , (125, "helicopter"                )
            , (126, "applause"                  )
            , (127, "gunshot"                   )
            ]




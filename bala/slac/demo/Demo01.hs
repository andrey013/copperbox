{-# OPTIONS -Wall #-}

-- Read a MIDI file, output the syntax tree.
-- Are the files the same?


module Main where

import Sound.Slac.Supercollider.SynthDef
import Sound.Slac.Supercollider.SynthDef.Internal.IEEE754
import Sound.Slac.Supercollider.SynthDef.Internal.ParserMonad


import qualified Data.ByteString.Lazy as L
import Text.PrettyPrint.Leijen                  -- package: wumpus-basic
import System.Environment


process :: FilePath -> IO ()
process file_name = do
    ans <- readSynthDefFile file_name
    case ans of
      Left err -> print err
      Right a  -> do { print a; writeSynthDefFile (file_name ++ ".001") a }


demo01 = process "./samples/aSynthDef.scsyndef"
demo02 = process "./samples/default.scsyndef"



demo03 = unpackSingle 0x3d 0xcc 0xcc 0xcd

-- | should be (-1)
demo04 = runParser bs int16be
  where 
    bs = L.pack [0xff, 0xff]


demo05 = packSingle 0.0

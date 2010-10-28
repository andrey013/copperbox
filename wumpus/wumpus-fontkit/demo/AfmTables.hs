{-# OPTIONS -Wall #-}



module Main where

import Wumpus.FontKit.AfmV2Datatypes
import Wumpus.FontKit.AfmV2Parser
import Wumpus.FontKit.Utils.ParserCombinators
import Wumpus.FontKit.VersionNumber


import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Chains 
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.VersionNumber

import Control.Monad

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    process helvetica_def "./out/helvetica.eps"  "./out/helvetica.svg"


helvetica_def = "samples/n019003l.afm"


process :: FilePath -> FilePath -> FilePath -> IO ()
process afm_path out_ps out_svg = do 
    ss <- readFile afm_path 
    let ans = runParserEither afmFile ss
    case ans of
      Left err -> print err
      Right a -> let pic = fontTable (take 250 $ char_metrics a)
                 in writeEPS_latin1 out_ps  pic >>
                    writeSVG_latin1 out_svg pic



filetest :: FilePath -> IO ()
filetest afm_path = readFile afm_path >>= print . runParserEither afmFile

times_ctx :: DrawingContext
times_ctx = fontface times_roman $ standardContext 14


fontTable :: [CharacterMetrics] -> DPicture
fontTable cs = liftToPictureU $ execDrawing times_ctx $  
    zipWithM (\cm pt -> draw $ charGraphic cm `at` pt) cs ps
  where
    ps = unchain (coordinateScalingContext 200 20) $ tableDown 35 2

charGraphic :: CharacterMetrics -> DLocGraphic
charGraphic cm = \pt -> 
            (textline (char_name cm) `at` pt)
    `oplus` (textline (show $ char_code cm) `at` hdisplace 80 pt)
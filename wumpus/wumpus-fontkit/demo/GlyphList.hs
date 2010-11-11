{-# OPTIONS -Wall #-}


module GlyphList where

import Wumpus.FontKit.GlyphList
import Wumpus.FontKit.GlyphListParser

import Wumpus.Basic.Utils.FormatCombinators ( writeDoc )
import Wumpus.Basic.Utils.ParserCombinators

-- check to see of generated code compiles...
-- import Wumpus.Core.Text.GlyphNames
-- import Wumpus.Core.Text.GlyphIndices

import Data.Time
import System.Directory



-- Edit this path!
-- ***************
--
pathto_glyphlist :: FilePath
pathto_glyphlist = "./samples/glyphlist.txt"




main :: IO ()
main = do 
    dir_okay <- doesFileExist pathto_glyphlist
    if dir_okay then sk else fk
  where
    sk = createDirectoryIfMissing True "./out/" >> process1
    fk = putStrLn "Please edit the pathto_glyphlist variable appropriately..."
    
    



process1 :: IO ()
process1 = do
    putStrLn $ "Processing glyphlist..."
    ss <- readFile $ pathto_glyphlist
    let ans = runParserEither glyphList ss
    case ans of
      Left err -> print $ err
      Right xs -> do ztime  <- getZonedTime
                     let d1 = gen_GlyphNamesModule xs ztime
                     writeDoc "out/GlyphNames.lhs" d1
                     let d2 = gen_GlyphIndicesModule xs ztime
                     writeDoc "out/GlyphIndices.lhs" d2
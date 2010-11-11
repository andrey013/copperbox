{-# OPTIONS -Wall #-}


module GlyphList where

import Wumpus.FontKit.GlyphList
import Wumpus.FontKit.GlyphListParser

import Wumpus.Basic.Utils.FormatCombinators ( writeDoc )
import Wumpus.Basic.Utils.ParserCombinators

import Wumpus.Core.Text.GlyphNames

import Data.Time
import System.Directory

dummy = do 
    ztime <- getZonedTime 
    putStr $ show $ gen_GlyphListModule [] ztime


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
                     let doc = gen_GlyphListModule xs ztime
                     writeDoc "out/GlyphNames.lhs" doc

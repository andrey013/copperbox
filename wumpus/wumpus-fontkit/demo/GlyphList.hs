{-# OPTIONS -Wall #-}


module GlyphList where

import Wumpus.FontKit.GlyphList

import Wumpus.Basic.Utils.ParserCombinators

import System.Directory

-- Edit this path!
-- ***************
--
pathto_glyphlist :: FilePath
pathto_glyphlist = "./samples/glyphlist.txt"


demo1 = runParserEither glyphDesc "A;0041"
demo2 = runParserEither glyphName "A;0041"


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
    let ans = runParserEither afmFile ss
    case ans of
      Left err -> print $ err
      Right xs -> print $ show (length xs) ++ " enties..."

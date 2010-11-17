{-# OPTIONS -Wall #-}


module EncodingTables where

import Wumpus.FontKit.EncodingVector
import Wumpus.FontKit.EncodingTableParser

import Wumpus.Basic.Utils.FormatCombinators ( writeDoc )
import Wumpus.Basic.Utils.ParserCombinators

-- check to see of generated code compiles...
-- import Wumpus.Core.Text.GlyphNames
-- import Wumpus.Core.Text.GlyphIndices

import Data.Time
import System.Directory





symbolEncoding :: Encoding 
symbolEncoding = 
    Encoding { encoding_module_name    = "Wumpus.Core.Text.Symbol"
             , encoding_module_descr   = modu_descr
             , encoding_table_name     = "symbol_encoding"
             , encoding_table_descr    = table_descr
             }
  where
    modu_descr  = [ "Encoding vector for the Symbol font." ]
    table_descr = [ "Table mapping character numbers to glyph names for the"
                  , "Symbol font." ]




stdEncoding :: Encoding 
stdEncoding = 
    Encoding { encoding_module_name    = "Wumpus.Core.Text.StandardEncoding"
             , encoding_module_descr   = modu_descr
             , encoding_table_name     = "standard_encoding"
             , encoding_table_descr    = table_descr
             }
  where
    modu_descr  = [ "Encoding vector for the Standard Encoding." ]
    table_descr = [ "Table mapping character numbers to glyph names for the"
                  , "Standard Encoding." ]



latin1Encoding :: Encoding 
latin1Encoding = 
    Encoding { encoding_module_name    = "Wumpus.Core.Text.Latin1Encoding"
             , encoding_module_descr   = modu_descr
             , encoding_table_name     = "latin1_encoding"
             , encoding_table_descr    = table_descr
             }
  where
    modu_descr  = [ "Encoding vector for the Latin1 Encoding." ]
    table_descr = [ "Table mapping character numbers to glyph names for the"
                  , "Latin1 Encoding." ]


encoding_tables :: [(FilePath, Encoding, CharParser Int, FilePath)]
encoding_tables = 
    [ ("./input/symbolfont.txt", symbolEncoding, octcode, "out/Symbol.lhs")
    , ("./input/standardencoding.txt", stdEncoding, deccode
                                     , "out/StandardEncoding.lhs")
    , ("./input/latin1encoding.txt", latin1Encoding, deccode
                                     , "out/Latin1Encoding.lhs")
    ]


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/" 
    mapM_ process1 encoding_tables
    
    



process1 :: (FilePath, Encoding, CharParser Int, FilePath) -> IO ()
process1 (in_path, encoding, p, out_path) = do
    putStrLn $ "Processing " ++ in_path
    ss <- readFile in_path
    let ans = runParserEither (encodingTable p) ss
    case ans of
      Left err -> print $ err
      Right xs -> do ztime  <- getZonedTime
                     let d1 = gen_EncodingModule encoding xs ztime
                     writeDoc out_path d1

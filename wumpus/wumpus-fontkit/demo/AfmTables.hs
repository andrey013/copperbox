{-# OPTIONS -Wall #-}


module AfmTables where

import Wumpus.FontKit.AfmV2Datatypes
import Wumpus.FontKit.AfmV2Parser
import Wumpus.FontKit.HtmlReport


import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Chains 
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Utils.ParserCombinators

import Text.XHtml hiding ( name )               -- package: xhtml


import Control.Monad

import Numeric
import System.Directory


-- Edit this path!
-- ***************
--
font_directory :: FilePath
font_directory = "C:/cygwin/usr/share/ghostscript/fonts/"



main :: IO ()
main = do 
    dir_okay <- doesDirectoryExist font_directory
    if dir_okay then sk else fk
  where
    sk = do createDirectoryIfMissing True "./out/" 
            mapM_ (\(a,_,c) -> process1 a c) core13_alias_table           
    
    fk = putStrLn "Please edit the font_directory variable appropriately..."
    
    



process1 :: String -> FilePath -> IO ()
process1 font_ps_name afm_loc = do
    putStrLn $ unwords [ "Processing", font_ps_name, afm_loc ]
    ss <- readFile $ font_directory ++ afm_loc
    let ans = runParserEither afmFile ss
    case ans of
      Left err -> print err
      Right afm -> sk afm
  where
   sk afm = let out_html = "./out/" ++ font_ps_name ++ ".html" 
                report   = makeHtmlReport font_ps_name afm
            in do writeFile out_html (renderHtml report) 


makeHtmlReport :: String -> AfmFile -> Html
makeHtmlReport font_ps_name afm = 
    htmlReport font_ps_name
               (map extr $ char_metrics afm)
  where
    extr c = (char_name c, char_code c, char_bbox c)

times_ctx :: DrawingContext
times_ctx = fontface times_roman $ standardContext 14


fontTable :: [CharacterMetrics] -> DPicture
fontTable cs = liftToPictureU $ execTraceDrawing times_ctx $  
    zipWithM (\cm pt -> draw $ charGraphic cm `at` pt) cs ps
  where
    ps = unchain (coordinateScalingContext 200 20) $ tableDown 35 2

-- hcat :: LocGraphic u -> LocGra 

charGraphic :: CharacterMetrics -> DLocGraphic
charGraphic cm = 
            (escOne (char_name cm))
    `oplus` (prepro1 (hdisplace 20)  $ textline (char_name cm))
    `oplus` (prepro1 (hdisplace 120) $ textline (show $ char_code cm))
    `oplus` (prepro1 (hdisplace 120) $ textline (show $ char_code cm))
    `oplus` (prepro1 (hdisplace 140) $ textline (oct3 $ char_code cm))

escOne :: String -> DLocGraphic
escOne ss = textline $ "&#" ++ ss ++ ";"

oct3 :: Int -> String
oct3 i | i < 0 = "___"
oct3 i         = showOct i ""




--------------------------------------------------------------------------------



core13_alias_table :: [(String, String, FilePath)]
core13_alias_table =  
  [ ("Courier",                 "NimbusMonL-Regu",              "n022003l.afm")
  , ("Courier-Oblique",         "NimbusMonL-ReguObli",          "n022023l.afm")
  , ("Courier-Bold",            "NimbusMonL-Bold",              "n022004l.afm")
  , ("Courier-BoldOblique",     "NimbusMonL-BoldObli",          "n022024l.afm")
  
  , ("Helvetica",               "NimbusSanL-Regu",              "n019003l.afm")
  , ("Helvetica-Oblique",       "NimbusSanL-ReguItal",          "n019023l.afm")
  , ("Helvetica-Bold",          "NimbusSanL-Bold",              "n019004l.afm")
  , ("Helvetica-BoldOblique",   "NimbusSanL-BoldItal",          "n019024l.afm")

  , ("Times-Roman",             "NimbusRomNo9L-Regu",           "n021003l.afm")
  , ("Times-Italic",            "NimbusRomNo9L-ReguItal",       "n021023l.afm")
  , ("Times-Bold",              "NimbusRomNo9L-Medi",           "n021004l.afm")
  , ("Times-BoldItalic",        "NimbusRomNo9L-MediItal",       "n021024l.afm")

  , ("Symbol",                  "StandardSymL",                 "s050000l.afm")
  ]
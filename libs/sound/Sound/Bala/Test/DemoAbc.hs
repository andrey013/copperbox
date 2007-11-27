

-- :set -i../../..


module Sound.Bala.Test.DemoAbc where

import Sound.Bala.Format.Abc.AbcFormat
import qualified Sound.Bala.Base.PitchRep as P 
import Sound.Bala.Base.PitchRep hiding (Pitch)

import Text.ParserCombinators.ReadP
import Text.PrettyPrint.Leijen (putDoc, pretty, Pretty)

import System.IO

process :: FilePath -> IO ()
process filename = do
  text <- readFile filename
  case parse abcFile text of
    [(a,"")] -> putDoc (ppAbcFile a)
    xs -> putStr $ "Error: " ++ show xs

demo_file  = process "../resources/abc/scale-exercises.abc"

demoK  = parse fieldKey "K:Gmaj\n"
demoL  = parse fieldDefaultLength "L:1/8\n"

demoM  = parse fieldMeter "M:2/4\n"



demo01 = case parse abcLine "ABcd efga\n" of
  [(a,"")] -> putDoc (pretty a)
  _ -> putStr "err..."


demo02 = show (P.Pitch C Nat 4 0)
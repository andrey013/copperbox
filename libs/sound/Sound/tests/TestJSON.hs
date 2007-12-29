
-- :set -i../../..

module TestJSON where

import Sound.Bala.Format.JSON.JSON

demo = do
  str <- readFile "../Examples/cscale-midi2.json"
  let val = parseValue str
  putStrLn $ printValue val


-- :set -i./Text/Diff



module UDiffDemo where

import Text.Diff.UnifiedDiff


import Data.Char
import Data.List

-- import PPrint





main = main1 "./resources/unified.diff" 

main1 name = parseUDiffFile name >>= \ast -> putStr (render ast)

test name = 
  do{ contents <- readFile name
    ; let xs = lines contents
    ; mapM_ (putStrLn . show . (map ord) ) xs
    }


time1 = TimeStamp 2006 03 04 14 18 24 203125000 0000

f1 :: UdFile
f1 = UdFile hdr1 [hk1]

hdr1 :: UdFileHeader
hdr1 = UdFileHeader "lao.txt" time1 "tzu.txt" time1

hk1 :: UdHunk
hk1 = UdHunk (UdHunkHeader (1,7) (1,6))
             [ Removed "The Way that can be told of is not the eternal Way;"
             ]





-- main = pretty f1
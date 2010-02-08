

-- > :set -i../src:../../../Libraries/kangaroo/src

module Demo1 where

import Graphics.ZFont.ParseFont
import Graphics.ZFont.Syntax

main = demo


demo = do 
    (ans,log) <- runParseFont "./data/GenBasR.ttf"
    putStrLn log
    either putStrLn tablesPrint ans
  where
    tablesPrint font = do { print $ ff_head_table font
                       --   ; print $ ff_name_table font
                          ; putStrLn ""
                          ; print $ ff_offset_table font
                          ; putStrLn ""
                          ; print $ ff_glyf_table font
                          ; putStrLn ""
                          -- ; print $ ff_cmap_table font
                          ; putStrLn ""
                          ; print $ ff_post_table font
                          } 



demo2 = parseWith prolog "./data/GenBasR.ttf" >>= print
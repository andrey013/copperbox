{-# OPTIONS -Wall #-}

module SvgId where

import Wumpus.Core

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/svg_id01.eps"  link_pic
    writeSVG "./out/svg_id01.svg"  link_pic


link_pic :: Picture
link_pic = frame [ text1, text2 ]
  where
    text1 = xidPrim "text01" $ ztextlabel "Id at the text level" (P2 0 50) 
    text2 = xidPrim "group01" $ primGroup [ ztextlabel "Id at the group level" (P2 0 0) ]


{-# OPTIONS -Wall #-}

module Demo01 where


import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.FreshIR
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.PostScript
import Wumpus.Fresh.SVG
import Wumpus.Fresh.TextLatin1

demo01 :: IO ()
demo01 = printPicture pic1

pic1 :: DPicture
pic1 = frameMulti [ ellipse_ 20 10 zeroPt ]

demo02 :: Doc
demo02 = runPsMonad latin1Encoder $ fillArcPath (RGB255 0 0 0) (20::Double) zeroPt

demo03 :: Doc
demo03 = hsep (map int [1..10]) <> text "next"


demo04 :: Doc
demo04 = vcat [ text "one...", indentH 2 (text "two."), text "three." ]

demo05 :: Doc
demo05 = hangLines 4 $ map text ["one...", "two...", "three.", "four..", "five.."]

demo06 :: Doc
demo06 = indentLines 2 $ map text ["one...", "two...", "three.", "four..", "five.."]

demo07 :: Doc
demo07 = indentLines 2 [text ".....", demo05]
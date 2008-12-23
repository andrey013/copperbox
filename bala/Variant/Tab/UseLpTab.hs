

-- ghci ...
-- :set -i../../Bala:../../HNotate

module UseLpTab where

import LpTab.LpTab
import Bala.Base
import qualified HNotate as H
import HNotate.DocLilyPond
import Text.ParserCombinators.Parsec

main = do 
    ans <- tabParse "./_tabs/demo01.lptab"
    either fk sk ans
  where    
    sk ans = (outSys . mkSys) ans
    fk err = putStrLn $ show err 

    mkSys = H.system1 "tab" . toEventList std_tuning

    outSys sys = H.outputLilyPondDocu H.DebugOn sys tab_doc "./_out/tab.ly"
    
    tab_doc = lilypond 
                  [   definition "tabMelody" 
                    . expression 
                    . outputAbsolute "tab" 
                  ]
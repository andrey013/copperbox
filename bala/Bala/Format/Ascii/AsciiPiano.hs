

module Bala.Format.Ascii.AsciiPiano where




piano i = unlines ["",l1,l1,l1,l2,l3,l3,l3,l4,""]
  where 
    fn    = take $ i * 6
    l1    = fn line1 
    l2    = fn line2
    l3    = fn line3
    l4    = fn line4
    line1 = cycle "|  |   |  |   |   |  |   |  |   |  |   |  "
    line2 = cycle "|  |___|  |___|   |  |___|  |___|  |___|  "  
    line3 = cycle "|     "
    line4 = cycle "|_____"    

printPiano = putStr . piano


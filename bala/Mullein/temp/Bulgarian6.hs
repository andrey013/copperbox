

module Bulgarian6 where

import MulleinTemp
import NamedTemp

bars1_4 :: NoteList
bars1_4 = notelist $ 
           [ a4 & du16, b4, cis5, cis5, cis5, a4, cis5, cis5
        
           , cis5, a4, b4, cis5, b4, a4, a4, rest
        
           , e5, d5, cis5, b4, cis5, a4, b4, cis5
        
           , a4, b4, b4, a4 , a4 & du8, rest    
           ]
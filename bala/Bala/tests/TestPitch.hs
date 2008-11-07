
-- :set -i..



module TestPitch where

import Bala

middle_c :: Pitch
middle_c = read "C4"


phasing_trope :: [Pitch]
phasing_trope = map read (words phase)
  where phase = "E4 F#4 B4 C#5 D5 F#4 E4 C#5 B4 F#4 D5 C#5"

demo = print (map pc phasing_trope)

demo02 :: PC
demo02 = transpose 2 (pc n)
  where n::Pitch
        n = read "C4"
        
   
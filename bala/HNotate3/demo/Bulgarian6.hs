
module Bulgarian6 where

import HNotate.Duration
import HNotate.LinearForm
import HNotate.NoteFunctions

import Control.Applicative

-- 2/4 time so unl is sixteenth

bars1_4 = 
  root ... a4   ... b4  ... cis5  ... cis5
       ... cis5 ... a4  ... cis5  ... cis5
       //  cis5 ... a4  ... b4    ... cis5
       ... b4   ... a4  ... a4    ... rest
       //  e5   ... d5  ... cis5  ... b4 
       ... cis5 ... a4  ... b4    ... cis5 

       //  a4   ... b4  ... b4    ... a4 
       ... a4 *& du8    ... rest *& du8
         


(*&) :: ElementF -> Duration -> ElementF 
(*&) e d = swapDuration d <$> e

run f = f (Env { unit_note_length = du16 })


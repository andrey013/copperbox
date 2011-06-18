{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

module XanaduInst 
  (
    xanadu_orc
  , inst3
  ) where



import ZSnd.Core
import ZSnd.Core.Opcodes

import Prelude hiding ( abs, exp )


xanadu_orc :: Orch
xanadu_orc = Orch default_stereo_header [inst3]

inst3 :: Inst
inst3 = runInstBuilder 3 $ do 
    ishift        <- ivar 0.00666667
    ipch          <- ivar (cpspch $ pfield 5)
    ioct          <- ivar (octpch $ pfield 5)
    (kadsr::KSig) <- linseg 0 (pfield 3 / 3) 1.0 
                              [ ((pfield 3 / 3), 1.0), ((pfield 3 / 3), 0)]
    (kmodi::KSig) <- linseg 0 (pfield 3 / 3) 5 
                              [ ((pfield 3 / 3), 3), ((pfield 3 / 3), 0)]
    (kmodr::KSig) <- linseg (pfield 6) (pfield 3) (pfield 7) [] 
    a1            <- avar $ castAR $ kmodi*(parens ((kmodr-1)/kmodr))/2
    a1ndx         <- avar $ abs ((a1 * 2) / 20)
    a2            <- avar $ castAR $ kmodi*(parens ((kmodr+1)/kmodr))/2
    a3            <- tablei_ a1ndx 3 1 0 0
    (ao1::ASig)   <- oscil a1 ipch 2
    a4            <- avar $ exp((-0.5) * a3 + ao1)
    ao2           <- oscil (a2 * castAR ipch) ipch 2
    (aoutl::ASig) <- oscil (1000 * (castAR kadsr) * a4) 
                           (ao2 + castAR (cpsoct(ioct+ishift))) 1
    (aoutr::ASig) <- oscil (1000 * (castAR kadsr) * a4) 
                           (ao2 + castAR (cpsoct(ioct-ishift))) 1
    outs [aoutl, aoutr]

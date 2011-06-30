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

-- Note - this instrument is not exemplary...

inst3 :: PrimInst
inst3 = runInstU 3 $ do
    k1 <- klet $ linseg $ port0_gen $ (0, p3/3, 1, [(p3/3, 1), (p3/3, 0)])
    k2 <- klet $ linseg $ port0_gen $ (0, p3/3, 5, [(p3/3, 3), (p3/3, 0)])
    k3 <- alet $ linseg $ port0_gen $ (pfield 6, pfield 3, pfield 7, [])

    a1 <- alet $ mult2 $ port2_2 $ \kx2 kx3 -> (cast kx2, cast $ ((kx3-1)/kx3) / 2)
    (k2,k3) ==>> a1

    a2 <- alet $ opabs $ port1_1 $ \ax1 -> (ax1 * 2 / 20)
    a1 =>= a2 

    a3 <- alet $ mult2 $ port2_2 $ \kx2 kx3 -> (cast kx2, cast $ ((kx3+1)/kx3) / 2)
    (k2,k3) ==>> a3

    a4 <- alet $ tablei_ $ port1_5 $ \ax2 -> (ax2, 3, 1, 0, 0)
    a2 =>= a4
    

    a5 <- alet $ oscil $ port1_3 $ \ax1 -> (ax1, i2, 2)
    a1 =>= a5

    a6 <- alet $ opexp $ port2_1 $ \ax4 ax5 -> ((-0.5) * ax4 + ax5)
    (a4,a5) ==>> a6

    a7 <- alet $ oscil $ port1_3 $ \ax3 -> (ax3*i2, i2, 2)
    a3 =>= a7

    a8 <- alet $ oscil $ port3_3 $ \kx1 ax6 ax7 -> 
                  (1000*kx1*ax6, ax7+cpsoct(i3+i1), 1)
           
    (k1,a6,a7) ===>> a8

    a9 <- alet $ oscil $ port3_3 $ \kx1 ax6 ax7 -> 
                  (1000*kx1*ax6, ax7+cpsoct(i3-i1), 1)
           
    (k1,a6,a7) ===>> a9

    o1 <- alet $ out2 $ port2_2 $ \a b -> (a,b)
    (a8,a9) ==>> o1
    out o1

  where
    p3    = pfield 3
    
    i1,i2,i3 :: Conf IInit
    i1    = 0.006667
    i2    = cpspch (pfield 5)
    i3    = octpch (pfield 5)



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
xanadu_orc = runOrchU default_stereo_header (inst3 3)

-- Note - this instrument is not exemplary...

inst3 :: Int -> BuildOrch ()
inst3 inum = instr inum $ do
    kadsr   <- klet $ linseg 0 (p3/3) 1 [(p3/3, 1), (p3/3, 0)]

    kmodi   <- klet $ linseg 0 (p3/3) 5 [(p3/3, 3), (p3/3, 0)]
    kmodr   <- klet $ linseg (pfield 6) (pfield 3) (pfield 7) []

 
    a1      <- adef $ cast (var kmodi * ((var kmodi - 1) / (var kmodr) / 2))
    a1ndx   <- adef $ abs (var a1 * 2 / 20)
    a2      <- adef $ cast (var kmodi * ((var kmodi + 1) / (var kmodr) / 2))

    a3      <- alet $ tablei_ (var a1ndx) (tablefn 1) 1 0 0
    ao1     <- alet $ oscil   (var a1) ipch (tablefn 2)

    a4      <- adef $ exp ((-0.5) * var a3 + var ao1)

    ao2     <- alet $ oscil (var a2 * cast ipch) ipch (tablefn 2)
    
    aoutl  <- alet $ oscil (1000 * cast (var kadsr) * var a4)
                           (var ao2 + cast (cpsoct (ioct + ishift)))
                           (tablefn 1)

    aoutr  <- alet $ oscil (1000 * cast (var kadsr) * var a4)
                           (var ao2 + cast (cpsoct (ioct - ishift)))
                           (tablefn 1)

    out2 (var aoutl) (var aoutr)

  where
    p3    = pfield 3
    
    ishift, ipch, ioct :: Expr IInit
    ishift  = 0.006667
    ipch    = cpspch (pfield 5)
    ioct    = octpch (pfield 5)



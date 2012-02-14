{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Builtins.SignalModifiers
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Signal modifying opcodes.
-- 
--------------------------------------------------------------------------------

module Orchsyn.Builtins.SignalModifiers
  (

  -- * Standard filters
    portk
  , port   -- Csound port
  , tonek
  , tone
  , atonek
  , atone
  , resonk
  , reson
  , aresonk
  , areson
  , tonex
  , atonex
  , resonx
  , resonr
  , resonz
  , resony
  , lowres
  , lowresx
  , vlowres
  , lowpass2
  , hilbert
  , butterhp
  , butterhp_
  , butterlp
  , butterlp_
  , butterbp
  , butterbp_
  , butterbr
  , butterbr_


  -- * Specialized filters
  , nlfilt
  , pareq
  , dcblock
  , dcblock_

  -- * Envelope modifiers
  , linen
  , linenr
  , envlpx
  , envlpxr

  -- * Amplitude modifiers
  , rms
  , gain
  , balance
  , dam

  -- * signal limiters
  , wrap
  , mirror
  , limit

  -- * Delay
  , delayr
  , delayw
  , delay
  , delay1
  , deltap
  , deltapi
  , deltapn
  , deltap3

  , multitap
  , vdelay
  , vdelay3

  -- * Reverbaration
  , reverb
  , reverb2
  , nreverb
  , comb
  , alpass
  , nestedap

  -- * Panning ans spatialization
  , pan
  , pan2
  , locsig
  , locsig2

  ) where


import Orchsyn.Language.Expr
import Orchsyn.OrchMonad


--------------------------------------------------------------------------------
-- Standard filters


portk :: Expr KRate -> Expr KRate 
      -> Opcode1 KRate
portk ksig khtim =
    opcodeStmt1 "portk" args
  where
    args = [ uniRate ksig, uniRate khtim ]




port :: Expr KRate -> Expr IInit -> Opcode1 KRate
port ksig ihtim = 
    opcodeStmt1 "port" args 
  where
    args = [ uniRate ksig, uniRate ihtim ]


tonek :: Expr KRate -> Expr KRate -> Opcode1 KRate
tonek ksig khp = 
    opcodeStmt1 "tonek" args 
  where
    args = [ uniRate ksig, uniRate khp ]


tone :: Expr ARate -> Expr KRate -> Opcode1 ARate
tone asig khp = 
    opcodeStmt1 "tone" args 
  where
    args = [ uniRate asig, uniRate khp ]


atonek :: Expr KRate -> Expr KRate -> Opcode1 KRate
atonek ksig khp = 
    opcodeStmt1 "atonek" args 
  where
    args = [ uniRate ksig, uniRate khp ]


atone :: Expr ARate -> Expr KRate -> Opcode1 ARate
atone asig khp = 
    opcodeStmt1 "atone" args 
  where
    args = [ uniRate asig, uniRate khp ]


resonk :: Expr KRate -> Expr KRate -> Expr KRate -> Opcode1 KRate
resonk ksig kcf kbw =
    opcodeStmt1 "resonk" args 
  where
    args = [ uniRate ksig, uniRate kcf, uniRate kbw  ]


reson :: Expr ARate -> Expr KRate -> Expr KRate -> Opcode1 ARate
reson asig kcf kbw =
    opcodeStmt1 "reson" args 
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]


aresonk :: Expr KRate -> Expr KRate -> Expr KRate -> Opcode1 KRate
aresonk ksig kcf kbw =
    opcodeStmt1 "aresonk" args
  where
    args = [ uniRate ksig, uniRate kcf, uniRate kbw ]


areson :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
areson asig kcf kbw =
    opcodeStmt1 "areson" args
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]


tonex :: Expr ARate -> Expr KRate -> Opcode1 ARate
tonex asig khp =
    opcodeStmt1 "tonex" args 
  where
   args = [ uniRate asig, uniRate khp ]


atonex :: Expr ARate -> Expr KRate -> Opcode1 ARate
atonex asig khp =
    opcodeStmt1 "atonex" args 
  where
    args = [ uniRate asig, uniRate khp ]


resonx :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
resonx asig kcf kbw =
    opcodeStmt1 "resonx" args
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]


resonr :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
resonr asig kcf kbw =
    opcodeStmt1 "resonr" args 
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]


resonz :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
resonz asig kcf kbw =
    opcodeStmt1 "resonz" args 
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]

resony :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
resony asig kcf kbw =
    opcodeStmt1 "resony" args 
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]

lowres :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
lowres asig kcutoff kreson =
    opcodeStmt1 "lowres" args 
  where
    args = [ uniRate asig, uniRate kcutoff, uniRate kreson ]


lowresx :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Opcode1 ARate
lowresx asig kcutoff kreson = 
    opcodeStmt1 "lowresx" args
  where
    args = [ uniRate asig, uniRate kcutoff, uniRate kreson ]


vlowres :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Expr IInit -> Expr KRate 
        -> Opcode1 ARate
vlowres asig kfco kres iord ksep =
    opcodeStmt1 "vlowres" args 
  where
    args = [ uniRate asig,  uniRate kfco
           , uniRate kres,  uniRate iord
           , uniRate ksep ]


lowpass2 :: Expr ARate -> Expr KRate -> Expr KRate 
         -> Opcode1 ARate
lowpass2 asig kef kq = 
    opcodeStmt1 "lowpass2" args 
  where
    args = [ uniRate asig, uniRate kef, uniRate kq ]


hilbert :: Expr ARate -> Opcode2 ARate
hilbert asig = 
    opcodeStmt2 "hilbert" args
  where
    args = [ uniRate asig ]


butterhp :: Expr ARate -> Expr KRate -> Opcode1 ARate
butterhp asig kfreq =
    opcodeStmt1 "butterhp" args
  where
    args = [ uniRate asig,  uniRate kfreq ]


butterhp_ :: Expr ARate -> Expr KRate -> Expr IInit 
          -> Opcode1 ARate
butterhp_ asig kfreq iskip =
    opcodeStmt1 "butterhp" args 
  where
    args = [ uniRate asig, uniRate kfreq, uniRate iskip ]


butterlp :: Expr ARate -> Expr KRate -> Opcode1 ARate
butterlp asig kfreq =
    opcodeStmt1 "butterlp" args
  where
    args = [ uniRate asig, uniRate kfreq ]


butterlp_ :: Expr ARate -> Expr KRate -> Expr IInit 
          -> Opcode1 ARate
butterlp_ asig kfreq iskip =
    opcodeStmt1 "butterlp" args
  where
    args = [ uniRate asig, uniRate kfreq, uniRate iskip ]


butterbp :: Expr ARate -> Expr KRate -> Expr KRate -> Opcode1 ARate
butterbp asig kfreq kband =
    opcodeStmt1 "butterbp" args 
  where
    args = [ uniRate asig,  uniRate kfreq, uniRate kband ]

butterbp_ :: Expr ARate -> Expr KRate -> Expr KRate -> Expr IInit 
          -> Opcode1 ARate
butterbp_ asig kfreq kband iskip =
    opcodeStmt1 "butterbp" args 
  where
    args = [ uniRate asig,  uniRate kfreq
           , uniRate kband, uniRate iskip ]


butterbr :: Expr ARate -> Expr KRate -> Expr KRate 
         -> Opcode1 ARate
butterbr asig kfreq kband =
    opcodeStmt1 "butterbr" args 
  where
    args = [ uniRate asig, uniRate kfreq, uniRate kband ]

butterbr_ :: Expr ARate -> Expr KRate -> Expr KRate -> Expr IInit 
          -> Opcode1 ARate
butterbr_ asig kfreq kband iskip =
    opcodeStmt1 "butterbr" args 
  where
    args = [ uniRate asig,  uniRate kfreq
           , uniRate kband, uniRate iskip ]

--------------------------------------------------------------------------------
-- Specialized filters

nlfilt :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
nlfilt ain ka kb kd kL kC =
    opcodeStmt1 "nlfilt" args
  where
    args = [ uniRate ain,   uniRate ka
           , uniRate kb,    uniRate kd
           , uniRate kL,    uniRate kC ]


pareq :: Expr ARate -> Expr KRate 
      -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Opcode1 ARate
pareq asig kc iv iq imode =
    opcodeStmt1 "pareq" args 
  where
    args = [ uniRate asig,  uniRate kc
           , uniRate iv,    uniRate iq
           , uniRate imode ]


dcblock :: Expr ARate -> Opcode1 ARate
dcblock asig = 
    opcodeStmt1 "dcblock" args
  where
    args = [ uniRate asig ]

dcblock_ :: Expr ARate -> Expr IInit -> Opcode1 ARate
dcblock_ asig ig = 
    opcodeStmt1 "dcblock" args 
  where
    args = [ uniRate asig, uniRate ig ]


--------------------------------------------------------------------------------
-- Envelope modifiers

linen :: KA_Rate rate
      => Expr rate -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Opcode1 rate
linen amp irise idur idec =
    opcodeStmt1 "linen" args
  where
    args = [ uniRate amp,   uniRate irise
           , uniRate idur,  uniRate idec ]


linenr :: (KA_Rate rate,Rate r1)
       => Expr r1 -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
linenr amp irise idec iatdec =
    opcodeStmt1 "linenr" args 
  where
    args = [ uniRate amp,   uniRate irise
           , uniRate idec,  uniRate iatdec ]


envlpx :: (KA_Rate rate, Rate r1)
       => Expr r1 -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Int -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
envlpx amp irise idur idec ifn iatss iatdec = 
    opcodeStmt1 "envlpx" args
  where
    args = [ uniRate amp,       uniRate irise
           , uniRate idur,      uniRate idec
           , fromIntegral ifn,  uniRate iatss
           , uniRate iatdec ]


envlpxr :: (KA_Rate rate, Rate r1)
        => Expr r1 -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Int -> Expr IInit -> Expr IInit  
        -> Opcode1 rate
envlpxr amp irise idur idec ifn iatss iatdec =
    opcodeStmt1 "envlpxr" args 
  where
    args = [ uniRate amp,       uniRate irise
           , uniRate idur,      uniRate idec
           , fromIntegral ifn,  uniRate iatss
           , uniRate iatdec ]

--------------------------------------------------------------------------------
-- Amplitude modifiers

rms :: Expr ARate -> Opcode1 KRate
rms asig = 
    opcodeStmt1 "rms" args 
  where
    args = [ uniRate asig ]


gain :: Expr ARate -> Expr KRate -> Opcode1 ARate
gain asig krms = 
    opcodeStmt1 "gain" args 
  where
    args = [ uniRate asig, uniRate krms ] 

balance :: Expr ARate -> Expr ARate -> Opcode1 ARate
balance asig acomp = 
    opcodeStmt1 "balance" args
  where
    args = [ uniRate asig, uniRate acomp ]


dam :: Expr ARate -> Expr KRate -> Expr IInit 
    -> Expr IInit -> Expr IInit -> Expr IInit 
    -> Opcode1 ARate
dam asig kthreshold icomp1 icomp2 irtime iftime = 
    opcodeStmt1 "dam" args
  where
    args =  [ uniRate asig,   uniRate kthreshold
            , uniRate icomp1, uniRate icomp2
            , uniRate irtime, uniRate iftime ]

--------------------------------------------------------------------------------
-- Signal limiters

wrap :: Rate rate
     => Expr rate -> Expr rate -> Expr rate 
     -> Opcode1 rate
wrap sig low high =
    opcodeStmt1 "wrap" args
  where
    args = [ uniRate sig, uniRate low, uniRate high ]


mirror :: Rate rate
       => Expr rate -> Expr rate -> Expr rate 
       -> Opcode1 rate
mirror sig low high =
    opcodeStmt1 "mirror" args
  where
    args = [ uniRate sig, uniRate low, uniRate high ]

limit :: Rate rate
      => Expr rate -> Expr rate -> Expr rate 
      -> Opcode1 rate
limit sig low high =
    opcodeStmt1 "limit" args 
  where
    args = [ uniRate sig, uniRate low, uniRate high ]

--------------------------------------------------------------------------------
-- Delay

delayr :: Expr IInit -> Opcode1 ARate
delayr idlt = 
    opcodeStmt1 "delayr" args
  where
    args = [ uniRate idlt ]


-- | No answer ...
delayw :: Expr ARate -> Opcode0 ()
delayw asigar = 
    opcodeStmt0 "delayw" args
  where
    args = [ uniRate asigar ]


delay :: Expr ARate -> Expr IInit -> Opcode1 ARate
delay asig idlt =
    opcodeStmt1 "delay" args
  where
    args =  [ uniRate asig, uniRate idlt ]

delay1 :: Expr ARate -> Opcode1 ARate
delay1 asig = 
    opcodeStmt1 "delay1" args 
  where
    args = [ uniRate asig ]


deltap :: Expr KRate -> Opcode1 ARate
deltap kdlt = 
    opcodeStmt1 "deltap" args 
  where
    args = [ uniRate kdlt ]

 
-- | xdlt is seconds...
--
deltapi :: Rate r1 
        => Expr r1 -> Opcode1 ARate
deltapi xdlt = 
    opcodeStmt1 "deltapi" args 
  where
    args = [ uniRate xdlt ]


-- | xnumsamps is presumably an integer...
--
deltapn :: Rate r1
        => Expr r1 -> Opcode1 ARate
deltapn xnumsamps =
    opcodeStmt1 "deltapn" args 
  where
    args = [ uniRate xnumsamps ]


-- | xdlt is seconds...
--
deltap3 :: Rate r1 
        => Expr r1 -> Opcode1 ARate
deltap3 xdlt = 
    opcodeStmt1 "deltap3" args 
  where
    args = [ uniRate xdlt ]


multitap :: Expr ARate -> [(Expr IInit, Expr IInit)] -> Opcode1 ARate
multitap asig xs = 
    opcodeStmt1 "multitap" args 
  where
    args     = uniRate asig : concatMap fn xs
    fn (a,b) = [ uniRate a, uniRate b ]
     


vdelay :: Expr ARate -> Expr ARate -> Expr IInit 
       -> Opcode1 ARate
vdelay asig adel imaxdel =
    opcodeStmt1 "vdelay" args 
  where
    args = [ uniRate asig, uniRate adel, uniRate imaxdel ]

vdelay3 :: Expr ARate -> Expr ARate -> Expr IInit 
        -> Opcode1 ARate
vdelay3 asig adel imaxdel =
    opcodeStmt1 "vdelay3" args 
  where
    args = [ uniRate asig, uniRate adel, uniRate imaxdel ]

--------------------------------------------------------------------------------
-- Reverberation

reverb :: Expr ARate -> Expr KRate -> Opcode1 ARate
reverb asig kvrt = 
    opcodeStmt1 "reverb" args 
  where
    args = [ uniRate asig, uniRate kvrt ]


reverb2 :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Opcode1 ARate
reverb2 asig ktime khdif =
    opcodeStmt1 "reverb2" args 
  where
    args = [ uniRate asig, uniRate ktime, uniRate khdif ]


nreverb :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Opcode1 ARate
nreverb asig ktime khdif =
    opcodeStmt1 "nreverb" args 
  where
    args = [ uniRate asig, uniRate ktime, uniRate khdif ]


comb :: Expr ARate -> Expr KRate -> Expr IInit 
     -> Opcode1 ARate
comb asig kvrt ilpt =
    opcodeStmt1 "comb" args
  where
    args = [ uniRate asig, uniRate kvrt, uniRate ilpt ]


alpass :: Expr ARate -> Expr KRate -> Expr IInit 
       -> Opcode1 ARate
alpass asig kvrt ilpt =
    opcodeStmt1 "alpass" args 
  where
    args = [ uniRate asig, uniRate kvrt, uniRate ilpt ]
               

nestedap :: Expr ARate -> Expr IInit -> Expr IInit 
         -> Expr IInit -> Expr IInit
         -> Opcode1 ARate
nestedap asig imode imaxdel idel1 igain1 =
    opcodeStmt1 "nestedap" args 
  where
    args = [ uniRate asig,    uniRate imode
           , uniRate imaxdel, uniRate idel1
           , uniRate igain1 ]

--------------------------------------------------------------------------------
-- Panning and spatialization

-- Note - name /priority/ is given to the 4 channel versions.

-- | 4 channel pan.
--
-- 1 Table fn.
--
pan :: Expr ARate -> Expr KRate -> Expr KRate -> Int
    -> Opcode4 ARate
pan asig kx ky ifn = 
    opcodeStmt4 "pan" args 
  where
    args = [ uniRate asig,  uniRate kx
           , uniRate ky,    fromIntegral ifn ]

pan2 :: Rate r1
     =>  Expr ARate -> Expr r1 -> Opcode2 ARate
pan2 ain xp = 
    opcodeStmt2 "pan2" args
  where
    args = [ uniRate ain, uniRate xp ]

-- | 4 channel version of locsig.
--
locsig :: Expr ARate -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Opcode4 ARate
locsig asig kdegree kdistance kreverbsend = 
    opcodeStmt4 "locsig" args 
  where
    args = [ uniRate asig,      uniRate kdegree
           , uniRate kdistance, uniRate kreverbsend ]


-- | This is the 2 channel version of 'locsig'
--
locsig2 :: Expr ARate -> Expr KRate -> Expr KRate -> Expr KRate 
        -> Opcode2 ARate
locsig2 asig kdegree kdistance kreverbsend =
    opcodeStmt2 "locsig" args 
  where
    args = [ uniRate asig,      uniRate kdegree
           , uniRate kdistance, uniRate kreverbsend ]
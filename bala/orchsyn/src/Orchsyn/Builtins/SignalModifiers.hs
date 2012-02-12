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
           -> Instr (Expr KRate)
portk ksig khtim =
    opcodeStmt1 "portk" (rateOf undefined) args
  where
    args = [ uniRate ksig, uniRate khtim ]



port :: Expr KRate -> Expr IInit -> Instr (Expr KRate)
port ksig ihtim = 
    opcodeStmt1 "port" (rateOf undefined) args 
  where
    args = [ uniRate ksig, uniRate ihtim ]


tonek :: Expr KRate -> Expr KRate -> Instr (Expr KRate)
tonek ksig khp = 
    opcodeStmt1 "tonek" (rateOf undefined) args 
  where
    args = [ uniRate ksig, uniRate khp ]


tone :: Expr ARate -> Expr KRate -> Instr (Expr ARate)
tone asig khp = 
    opcodeStmt1 "tone" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate khp ]


atonek :: Expr KRate -> Expr KRate -> Instr (Expr KRate)
atonek ksig khp = 
    opcodeStmt1 "atonek" (rateOf undefined) args 
  where
    args = [ uniRate ksig, uniRate khp ]


atone :: Expr ARate -> Expr KRate -> Instr (Expr ARate)
atone asig khp = 
    opcodeStmt1 "atone" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate khp ]


resonk :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Instr (Expr KRate)
resonk ksig kcf kbw =
    opcodeStmt1 "resonk" (rateOf undefined) args 
  where
    args = [ uniRate ksig, uniRate kcf, uniRate kbw  ]


reson :: Expr ARate -> Expr KRate -> Expr KRate 
      -> Instr (Expr ARate)
reson asig kcf kbw =
    opcodeStmt1 "reson" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]


aresonk :: Expr KRate -> Expr KRate -> Expr KRate 
        -> Instr (Expr KRate)
aresonk ksig kcf kbw =
    opcodeStmt1 "aresonk" (rateOf undefined) args
  where
    args = [ uniRate ksig, uniRate kcf, uniRate kbw ]


areson :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Instr (Expr ARate)
areson asig kcf kbw =
    opcodeStmt1 "areson" (rateOf undefined) args
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]


tonex :: Expr ARate -> Expr KRate -> Instr (Expr ARate)
tonex asig khp =
    opcodeStmt1 "tonex" (rateOf undefined) args 
  where
   args = [ uniRate asig, uniRate khp ]


atonex :: Expr ARate -> Expr KRate -> Instr (Expr ARate)
atonex asig khp =
    opcodeStmt1 "atonex" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate khp ]


resonx :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Instr (Expr ARate)
resonx asig kcf kbw =
    opcodeStmt1 "resonx" (rateOf undefined) args
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]


resonr :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Instr (Expr ARate)
resonr asig kcf kbw =
    opcodeStmt1 "resonr" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]


resonz :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Instr (Expr ARate)
resonz asig kcf kbw =
    opcodeStmt1 "resonz" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]

resony :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Instr (Expr ARate)
resony asig kcf kbw =
    opcodeStmt1 "resony" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate kcf, uniRate kbw ]

lowres :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Instr (Expr ARate)
lowres asig kcutoff kreson =
    opcodeStmt1 "lowres" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate kcutoff, uniRate kreson ]


lowresx :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Instr (Expr ARate)
lowresx asig kcutoff kreson = 
    opcodeStmt1 "lowresx" (rateOf undefined) args
  where
    args = [ uniRate asig, uniRate kcutoff, uniRate kreson ]


vlowres :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Expr IInit -> Expr KRate 
        -> Instr (Expr ARate)
vlowres asig kfco kres iord ksep =
    opcodeStmt1 "vlowres" (rateOf undefined) args 
  where
    args = [ uniRate asig,  uniRate kfco
           , uniRate kres,  uniRate iord
           , uniRate ksep ]


lowpass2 :: Expr ARate -> Expr KRate -> Expr KRate 
         -> Instr (Expr ARate)
lowpass2 asig kef kq = 
    opcodeStmt1 "lowpass2" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate kef, uniRate kq ]


hilbert :: Expr ARate -> Instr (Expr ARate, Expr ARate)
hilbert asig = 
    opcodeStmt2 "hilbert" (rateOf undefined) args
  where
    args = [ uniRate asig ]


butterhp :: Expr ARate -> Expr KRate -> Instr (Expr ARate)
butterhp asig kfreq =
    opcodeStmt1 "butterhp" (rateOf undefined) args
  where
    args = [ uniRate asig,  uniRate kfreq ]


butterhp_ :: Expr ARate -> Expr KRate -> Expr IInit 
          -> Instr (Expr ARate)
butterhp_ asig kfreq iskip =
    opcodeStmt1 "butterhp" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate kfreq, uniRate iskip ]


butterlp :: Expr ARate -> Expr KRate -> Instr (Expr ARate)
butterlp asig kfreq =
    opcodeStmt1 "butterlp" (rateOf undefined) args
  where
    args = [ uniRate asig, uniRate kfreq ]


butterlp_ :: Expr ARate -> Expr KRate -> Expr IInit 
          -> Instr (Expr ARate)
butterlp_ asig kfreq iskip =
    opcodeStmt1 "butterlp" (rateOf undefined) args
  where
    args = [ uniRate asig, uniRate kfreq, uniRate iskip ]


butterbp :: Expr ARate -> Expr KRate -> Expr KRate -> Instr (Expr ARate)
butterbp asig kfreq kband =
    opcodeStmt1 "butterbp" (rateOf undefined) args 
  where
    args = [ uniRate asig,  uniRate kfreq, uniRate kband ]

butterbp_ :: Expr ARate -> Expr KRate -> Expr KRate -> Expr IInit 
          -> Instr (Expr ARate)
butterbp_ asig kfreq kband iskip =
    opcodeStmt1 "butterbp" (rateOf undefined) args 
  where
    args = [ uniRate asig,  uniRate kfreq
           , uniRate kband, uniRate iskip ]


butterbr :: Expr ARate -> Expr KRate -> Expr KRate 
         -> Instr (Expr ARate)
butterbr asig kfreq kband =
    opcodeStmt1 "butterbr" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate kfreq, uniRate kband ]

butterbr_ :: Expr ARate -> Expr KRate -> Expr KRate -> Expr IInit 
          -> Instr (Expr ARate)
butterbr_ asig kfreq kband iskip =
    opcodeStmt1 "butterbr" (rateOf undefined) args 
  where
    args = [ uniRate asig,  uniRate kfreq
           , uniRate kband, uniRate iskip ]

--------------------------------------------------------------------------------
-- Specialized filters

nlfilt :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Instr (Expr ARate)
nlfilt ain ka kb kd kL kC =
    opcodeStmt1 "nlfilt" (rateOf undefined) args
  where
    args = [ uniRate ain,   uniRate ka
           , uniRate kb,    uniRate kd
           , uniRate kL,    uniRate kC ]


pareq :: Expr ARate -> Expr KRate 
      -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Instr (Expr ARate)
pareq asig kc iv iq imode =
    opcodeStmt1 "pareq" (rateOf undefined) args 
  where
    args = [ uniRate asig,  uniRate kc
           , uniRate iv,    uniRate iq
           , uniRate imode ]


dcblock :: Expr ARate -> Instr (Expr ARate)
dcblock asig = 
    opcodeStmt1 "dcblock" (rateOf undefined) args
  where
    args = [ uniRate asig ]

dcblock_ :: Expr ARate -> Expr IInit -> Instr (Expr ARate)
dcblock_ asig ig = 
    opcodeStmt1 "dcblock" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate ig ]


--------------------------------------------------------------------------------
-- Envelope modifiers

linen :: (KA_Rate rate, MakeVar rate, TypeRate rate)
      => Expr rate -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Instr (Expr rate)
linen amp irise idur idec =
    opcodeStmt1 "linen" (rateOf amp) args
  where
    args = [ uniRate amp,   uniRate irise
           , uniRate idur,  uniRate idec ]


linenr :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate r1)
       => Expr r1 -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Instr (Expr rate)
linenr amp irise idec iatdec =
    opcodeStmt1 "linenr" (rateOf undefined) args 
  where
    args = [ uniRate amp,   uniRate irise
           , uniRate idec,  uniRate iatdec ]


envlpx :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate r1)
       => Expr r1 -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Int -> Expr IInit -> Expr IInit 
       -> Instr (Expr rate)
envlpx amp irise idur idec ifn iatss iatdec = 
    opcodeStmt1 "envlpx" (rateOf undefined) args
  where
    args = [ uniRate amp,       uniRate irise
           , uniRate idur,      uniRate idec
           , fromIntegral ifn,  uniRate iatss
           , uniRate iatdec ]


envlpxr :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate r1)
        => Expr r1 -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Int -> Expr IInit -> Expr IInit  
        -> Instr (Expr rate)
envlpxr amp irise idur idec ifn iatss iatdec =
    opcodeStmt1 "envlpxr" (rateOf undefined) args 
  where
    args = [ uniRate amp,       uniRate irise
           , uniRate idur,      uniRate idec
           , fromIntegral ifn,  uniRate iatss
           , uniRate iatdec ]

--------------------------------------------------------------------------------
-- Amplitude modifiers

rms :: Expr ARate -> Instr (Expr KRate)
rms asig = 
    opcodeStmt1 "rms" (rateOf undefined) args 
  where
    args = [ uniRate asig ]


gain :: Expr ARate -> Expr KRate -> Instr (Expr ARate)
gain asig krms = 
    opcodeStmt1 "gain" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate krms ] 

balance :: Expr ARate -> Expr ARate -> Instr (Expr ARate)
balance asig acomp = 
    opcodeStmt1 "balance" (rateOf undefined) args
  where
    args = [ uniRate asig, uniRate acomp ]


dam :: Expr ARate -> Expr KRate -> Expr IInit 
    -> Expr IInit -> Expr IInit -> Expr IInit 
    -> Instr (Expr ARate)
dam asig kthreshold icomp1 icomp2 irtime iftime = 
    opcodeStmt1 "dam" (rateOf undefined) args
  where
    args =  [ uniRate asig,   uniRate kthreshold
            , uniRate icomp1, uniRate icomp2
            , uniRate irtime, uniRate iftime ]

--------------------------------------------------------------------------------
-- Signal limiters

wrap :: (Rate rate, MakeVar rate, TypeRate rate)
     => Expr rate -> Expr rate -> Expr rate 
     -> Instr (Expr rate)
wrap sig low high =
    opcodeStmt1 "wrap" (rateOf sig) args
  where
    args = [ uniRate sig, uniRate low, uniRate high ]


mirror :: (Rate rate, MakeVar rate, TypeRate rate)
       => Expr rate -> Expr rate -> Expr rate 
       -> Instr (Expr rate)
mirror sig low high =
    opcodeStmt1 "mirror" (rateOf sig) args
  where
    args = [ uniRate sig, uniRate low, uniRate high ]

limit :: (Rate rate, MakeVar rate, TypeRate rate)
      => Expr rate -> Expr rate -> Expr rate 
      -> Instr (Expr rate)
limit sig low high =
    opcodeStmt1 "limit" (rateOf sig) args 
  where
    args = [ uniRate sig, uniRate low, uniRate high ]

--------------------------------------------------------------------------------
-- Delay

delayr :: Expr IInit -> Instr (Expr ARate)
delayr idlt = 
    opcodeStmt1 "delayr" (rateOf undefined) args
  where
    args = [ uniRate idlt ]


-- | No answer ...
delayw :: Expr ARate -> Instr ()
delayw asigar = 
    opcodeStmt0 "delayw" args
  where
    args = [ uniRate asigar ]


delay :: Expr ARate -> Expr IInit -> Instr (Expr ARate)
delay asig idlt =
    opcodeStmt1 "delay" (rateOf undefined) args
  where
    args =  [ uniRate asig, uniRate idlt ]

delay1 :: Expr ARate -> Instr (Expr ARate)
delay1 asig = 
    opcodeStmt1 "delay1" (rateOf undefined) args 
  where
    args = [ uniRate asig ]


deltap :: Expr KRate -> Instr (Expr ARate)
deltap kdlt = 
    opcodeStmt1 "deltap" (rateOf undefined) args 
  where
    args = [ uniRate kdlt ]

 
-- | xdlt is seconds...
--
deltapi :: Expr rate -> Instr (Expr ARate)
deltapi xdlt = 
    opcodeStmt1 "deltapi" (rateOf undefined) args 
  where
    args = [ uniRate xdlt ]


-- | xnumsamps is presumably an integer...
--
deltapn :: Expr rate -> Instr (Expr ARate)
deltapn xnumsamps =
    opcodeStmt1 "deltapn" (rateOf undefined) args 
  where
    args = [ uniRate xnumsamps ]


-- | xdlt is seconds...
--
deltap3 :: Expr rate -> Instr (Expr ARate)
deltap3 xdlt = 
    opcodeStmt1 "deltap3" (rateOf undefined) args 
  where
    args = [ uniRate xdlt ]


multitap :: Expr ARate -> [(Expr IInit, Expr IInit)] -> Instr (Expr ARate)
multitap asig xs = 
    opcodeStmt1 "multitap" (rateOf undefined) args 
  where
    args     = uniRate asig : concatMap fn xs
    fn (a,b) = [ uniRate a, uniRate b ]
     


vdelay :: Expr ARate -> Expr ARate -> Expr IInit 
       -> Instr (Expr ARate)
vdelay asig adel imaxdel =
    opcodeStmt1 "vdelay" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate adel, uniRate imaxdel ]

vdelay3 :: Expr ARate -> Expr ARate -> Expr IInit 
        -> Instr (Expr ARate)
vdelay3 asig adel imaxdel =
    opcodeStmt1 "vdelay3" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate adel, uniRate imaxdel ]

--------------------------------------------------------------------------------
-- Reverberation

reverb :: Expr ARate -> Expr KRate -> Instr (Expr ARate)
reverb asig kvrt = 
    opcodeStmt1 "reverb" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate kvrt ]


reverb2 :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Instr (Expr ARate)
reverb2 asig ktime khdif =
    opcodeStmt1 "reverb2" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate ktime, uniRate khdif ]


nreverb :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Instr (Expr ARate)
nreverb asig ktime khdif =
    opcodeStmt1 "nreverb" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate ktime, uniRate khdif ]


comb :: Expr ARate -> Expr KRate -> Expr IInit 
     -> Instr (Expr ARate)
comb asig kvrt ilpt =
    opcodeStmt1 "comb" (rateOf undefined) args
  where
    args = [ uniRate asig, uniRate kvrt, uniRate ilpt ]


alpass :: Expr ARate -> Expr KRate -> Expr IInit 
       -> Instr (Expr ARate)
alpass asig kvrt ilpt =
    opcodeStmt1 "alpass" (rateOf undefined) args 
  where
    args = [ uniRate asig, uniRate kvrt, uniRate ilpt ]
               

nestedap :: Expr ARate -> Expr IInit -> Expr IInit 
         -> Expr IInit -> Expr IInit
         -> Instr (Expr ARate)
nestedap asig imode imaxdel idel1 igain1 =
    opcodeStmt1 "nestedap" (rateOf undefined) args 
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
    -> Instr (Expr ARate, Expr ARate, Expr ARate, Expr ARate)
pan asig kx ky ifn = 
    opcodeStmt4 "pan" (rateOf undefined) args 
  where
    args = [ uniRate asig,  uniRate kx
           , uniRate ky,    fromIntegral ifn ]

pan2 :: Expr ARate -> Expr rate  
     -> Instr (Expr ARate, Expr ARate)
pan2 ain xp = 
    opcodeStmt2 "pan2" (rateOf undefined) args
  where
    args = [ uniRate ain, uniRate xp ]

-- | 4 channel version of locsig.
--
locsig :: Expr ARate -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Instr (Expr ARate, Expr ARate, Expr ARate, Expr ARate)
locsig asig kdegree kdistance kreverbsend = 
    opcodeStmt4 "locsig" (rateOf undefined) args 
  where
    args = [ uniRate asig,      uniRate kdegree
           , uniRate kdistance, uniRate kreverbsend ]


-- | This is the 2 channel version of 'locsig'
--
locsig2 :: Expr ARate -> Expr KRate -> Expr KRate -> Expr KRate 
        -> Instr (Expr ARate, Expr ARate)
locsig2 asig kdegree kdistance kreverbsend =
    opcodeStmt2 "locsig" (rateOf undefined) args 
  where
    args = [ uniRate asig,      uniRate kdegree
           , uniRate kdistance, uniRate kreverbsend ]
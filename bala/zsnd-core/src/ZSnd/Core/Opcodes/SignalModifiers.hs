{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes.SignalModifiers
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Signal modifying opcodes.
-- 
--------------------------------------------------------------------------------

module ZSnd.Core.Opcodes.SignalModifiers
  (

  -- * Standard filters
    portk
  , port
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

  ) where



import ZSnd.Core.CsoundInst

--------------------------------------------------------------------------------
-- Standard filters

portk :: Expr KR -> Expr KR -> InstBuilder (Expr KR)
portk ksig khtim = opcode "portk" [ getExpr ksig, getExpr khtim ]

port :: Expr KR -> Expr IR -> InstBuilder (Expr KR)
port ksig ihtim = opcode "port" [ getExpr ksig, getExpr ihtim ]

tonek :: Expr KR -> Expr KR -> InstBuilder (Expr KR)
tonek ksig khp = opcode "tonek" [ getExpr ksig, getExpr khp ]

tone :: Expr AR -> Expr KR -> InstBuilder (Expr AR)
tone asig khp = opcode "tone" [ getExpr asig, getExpr khp ]


atonek :: Expr KR -> Expr KR -> InstBuilder (Expr KR)
atonek ksig khp = opcode "atonek" [ getExpr ksig, getExpr khp ]

atone :: Expr AR -> Expr KR -> InstBuilder (Expr AR)
atone asig khp = opcode "atone" [ getExpr asig, getExpr khp ]

resonk :: Expr KR -> Expr KR -> Expr KR -> InstBuilder (Expr KR)
resonk ksig kcf kbw = 
    opcode "resonk" [ getExpr ksig, getExpr kcf, getExpr kbw ]

reson :: Expr AR -> Expr KR -> Expr KR -> InstBuilder (Expr AR)
reson asig kcf kbw = 
    opcode "reson" [ getExpr asig, getExpr kcf, getExpr kbw ]


aresonk :: Expr KR -> Expr KR -> Expr KR -> InstBuilder (Expr KR)
aresonk ksig kcf kbw = 
    opcode "aresonk" [ getExpr ksig, getExpr kcf, getExpr kbw ]

areson :: Expr AR -> Expr KR -> Expr KR -> InstBuilder (Expr AR)
areson asig kcf kbw = 
    opcode "areson" [ getExpr asig, getExpr kcf, getExpr kbw ]


tonex :: Expr AR -> Expr KR -> InstBuilder (Expr AR)
tonex asig khp = opcode "tonex" [ getExpr asig, getExpr khp ]

atonex :: Expr AR -> Expr KR -> InstBuilder (Expr AR)
atonex asig khp = opcode "atonex" [ getExpr asig, getExpr khp ]


resonx :: Expr AR -> Expr KR -> Expr KR -> InstBuilder (Expr AR)
resonx asig kcf kbw = 
    opcode "resonx" [ getExpr asig, getExpr kcf, getExpr kbw ]

resonr :: Expr AR -> Expr KR -> Expr KR -> InstBuilder (Expr AR)
resonr asig kcf kbw = 
    opcode "resonr" [ getExpr asig, getExpr kcf, getExpr kbw ]

resonz :: Expr AR -> Expr KR -> Expr KR -> InstBuilder (Expr AR)
resonz asig kcf kbw = 
    opcode "resonz" [ getExpr asig, getExpr kcf, getExpr kbw ]

resony :: Expr AR -> Expr KR -> Expr KR -> InstBuilder (Expr AR)
resony asig kcf kbw = 
    opcode "resony" [ getExpr asig, getExpr kcf, getExpr kbw ]

lowres :: Expr AR -> Expr KR -> Expr KR -> InstBuilder (Expr AR)
lowres asig kcutoff kreson = 
    opcode "lowres" [ getExpr asig, getExpr kcutoff, getExpr kreson ]

lowresx :: Expr AR -> Expr KR -> Expr KR -> InstBuilder (Expr AR)
lowresx asig kcutoff kreson = 
    opcode "lowresx" [ getExpr asig, getExpr kcutoff, getExpr kreson ]

vlowres :: Expr AR -> Expr KR -> Expr KR -> Expr IR -> Expr KR 
        -> InstBuilder (Expr AR)
vlowres asig kfco kres iord ksep = 
    opcode "vlowres" [ getExpr asig, getExpr kfco, getExpr kres
                     , getExpr iord, getExpr ksep ]


--------------------------------------------------------------------------------
-- Specialized filters

nlfilt :: Expr AR -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR
       -> InstBuilder (Expr AR)
nlfilt ain ka kb kd kL kC = 
    opcode "nlfilt" [ getExpr ain, getExpr ka, getExpr kb
                    , getExpr kd,  getExpr kL, getExpr kC ]


pareq :: Expr AR -> Expr KR -> Expr IR -> Expr IR -> Expr IR
      -> InstBuilder (Expr AR)
pareq asig kc iv iq imode = 
    opcode "pareq" [ getExpr asig, getExpr kc, getExpr iv
                   , getExpr iq,   getExpr imode ]


dcblock :: Expr AR -> InstBuilder (Expr AR)
dcblock asig = 
    opcode "dcblock" [ getExpr asig ]

dcblock_ :: Expr AR -> Expr IR -> InstBuilder (Expr AR)
dcblock_ asig ig = 
    opcode "dcblock" [ getExpr asig, getExpr ig ]


--------------------------------------------------------------------------------
-- Envelope modifiers

linen :: KA_Rate rate
      => Expr rate -> Expr IR -> Expr IR -> Expr IR -> InstBuilder (Expr rate)
linen amp irise idur idec = 
    opcode "linen" [ getExpr amp, getExpr irise, getExpr idur, getExpr idec ]

linenr :: KA_Rate rate
       => Expr rate -> Expr IR -> Expr IR -> Expr IR -> InstBuilder (Expr rate)
linenr amp irise idec iatdec = 
    opcode "linenr" [ getExpr amp, getExpr irise, getExpr idec, getExpr iatdec ]

envlpx :: KA_Rate rate
       => Expr rate -> Expr IR -> Expr IR -> Expr IR -> Expr IR 
       -> Expr IR -> Expr IR
       -> InstBuilder (Expr rate)
envlpx amp irise idur idec ifn iatss iatdec = 
    opcode "envlpx" [ getExpr amp, getExpr irise, getExpr idur
                    , getExpr idec, getExpr ifn, getExpr iatss
                    , getExpr iatdec ]

envlpxr :: KA_Rate rate
        => Expr rate -> Expr IR -> Expr IR -> Expr IR -> Expr IR 
        -> Expr IR -> Expr IR
        -> InstBuilder (Expr rate)
envlpxr amp irise idur idec ifn iatss iatdec = 
    opcode "envlpxr" [ getExpr amp, getExpr irise, getExpr idur
                     , getExpr idec, getExpr ifn, getExpr iatss
                     , getExpr iatdec ]



--------------------------------------------------------------------------------
-- Amplitude modifiers

rms :: Expr AR -> InstBuilder (Expr KR)
rms asig = opcode "rms" [ getExpr asig ]

gain :: Expr AR -> Expr KR -> InstBuilder (Expr AR)
gain asig krms = opcode "gain" [ getExpr asig, getExpr krms ]

balance :: Expr AR -> Expr AR -> InstBuilder (Expr AR)
balance asig acomp = opcode "balance" [ getExpr asig, getExpr acomp ]

dam :: Expr AR -> Expr KR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
    -> InstBuilder (Expr AR)
dam asig kthreshold icomp1 icomp2 irtime iftime = 
    opcode "dam" [ getExpr asig, getExpr kthreshold
                 , getExpr icomp1, getExpr icomp2
                 , getExpr irtime, getExpr iftime ]


--------------------------------------------------------------------------------
-- Signal limiters

wrap :: Opcode rate
     => Expr rate -> Expr rate -> Expr rate -> InstBuilder (Expr rate)
wrap sig low high =
    opcode "wrap" [ getExpr sig, getExpr low, getExpr high ]


mirror :: Opcode rate
       => Expr rate -> Expr rate -> Expr rate -> InstBuilder (Expr rate)
mirror sig low high =
    opcode "mirror" [ getExpr sig, getExpr low, getExpr high ]

limit :: Opcode rate
      => Expr rate -> Expr rate -> Expr rate -> InstBuilder (Expr rate)
limit sig low high =
    opcode "limit" [ getExpr sig, getExpr low, getExpr high ]

--------------------------------------------------------------------------------
-- Delay

delayr :: Expr IR -> InstBuilder (Expr AR)
delayr idlt = opcode "delayr" [ getExpr idlt ]

delayw :: Expr AR -> InstBuilder ()
delayw asigar = opcode0 "delayw" [ getExpr asigar ]

delay :: Expr AR -> Expr IR -> InstBuilder ()
delay asig idlt = opcode0 "delay" [ getExpr asig, getExpr idlt ]

delay1 :: Expr AR -> InstBuilder (Expr AR)
delay1 asig = opcode "delay" [ getExpr asig ]

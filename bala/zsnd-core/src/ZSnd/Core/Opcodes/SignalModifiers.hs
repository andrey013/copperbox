{-# LANGUAGE ScopedTypeVariables        #-}
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
    portmentok  -- Csound portk
  , portmento   -- Csound port
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


import ZSnd.Core.CsoundInst.Typed


--------------------------------------------------------------------------------
-- Standard filters

-- | Note this is @portk@ in Csound.
--
-- @port@ has special meaning in ZSnd, so it is not used here.
--
portmentok :: Expr KRate -> Expr KRate 
           -> Opcode1 KRate
portmentok ksig khtim =
    Opcode1 "portk" [ getExprK ksig, getExprK khtim ]



-- | Note this is @port@ in Csound.
--
-- @port@ has special meaning in ZSnd, so it is not used here.
--
portmento :: Expr KRate -> Expr IInit -> Opcode1 KRate
portmento ksig ihtim = 
    Opcode1 "port" [ getExprK ksig, getExprI ihtim ]


tonek :: Expr KRate -> Expr KRate -> Opcode1 KRate
tonek ksig khp = 
    Opcode1 "tonek" [ getExprK ksig, getExprK khp ]


tone :: Expr ARate -> Expr KRate -> Opcode1 ARate
tone asig khp = 
    Opcode1 "tone" [ getExprA asig, getExprK khp ]


atonek :: Expr KRate -> Expr KRate -> Opcode1 KRate
atonek ksig khp = 
    Opcode1 "atonek" [ getExprK ksig, getExprK khp ]


atone :: Expr ARate -> Expr KRate -> Opcode1 ARate
atone asig khp = 
    Opcode1 "atone" [ getExprA asig, getExprK khp ]


resonk :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Opcode1 KRate
resonk ksig kcf kbw =
    Opcode1 "resonk" [ getExprK ksig, getExprK kcf, getExprK kbw  ]


reson :: Expr ARate -> Expr KRate -> Expr KRate 
      -> Opcode1 ARate
reson asig kcf kbw =
    Opcode1 "reson" [ getExprA asig, getExprK kcf, getExprK kbw ]


aresonk :: Expr KRate -> Expr KRate -> Expr KRate 
        -> Opcode1 KRate
aresonk ksig kcf kbw =
    Opcode1 "aresonk" [ getExprK ksig, getExprK kcf, getExprK kbw ]


areson :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
areson asig kcf kbw =
    Opcode1 "areson" [ getExprA asig, getExprK kcf, getExprK kbw ]


tonex :: Expr ARate -> Expr KRate -> Opcode1 ARate
tonex asig khp =
    Opcode1 "tonex" [ getExprA asig, getExprK khp ]


atonex :: Expr ARate -> Expr KRate -> Opcode1 ARate
atonex asig khp =
    Opcode1 "atonex" [ getExprA asig, getExprK khp ]


resonx :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
resonx asig kcf kbw =
    Opcode1 "resonx" [ getExprA asig, getExprK kcf, getExprK kbw ]


resonr :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
resonr asig kcf kbw =
    Opcode1 "resonr" [ getExprA asig, getExprK kcf, getExprK kbw ]


resonz :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
resonz asig kcf kbw =
    Opcode1 "resonz" [ getExprA asig, getExprK kcf, getExprK kbw ]

resony :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
resony asig kcf kbw =
    Opcode1 "resony" [ getExprA asig, getExprK kcf, getExprK kbw ]

lowres :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
lowres asig kcutoff kreson =
    Opcode1 "lowres" [ getExprA asig, getExprK kcutoff
                     , getExprK kreson ]


lowresx :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Opcode1 ARate
lowresx asig kcutoff kreson = 
    Opcode1 "lowresx" [ getExprA asig, getExprK kcutoff
                      , getExprK kreson ]


vlowres :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Expr IInit -> Expr KRate 
        -> Opcode1 ARate
vlowres asig kfco kres iord ksep =
    Opcode1 "vlowres" [ getExprA asig,  getExprK kfco
                      , getExprK kres,  getExprI iord
                      , getExprK ksep ]


lowpass2 :: Expr ARate -> Expr KRate -> Expr KRate 
         -> Opcode1 ARate
lowpass2 asig kef kq = 
    Opcode1 "lowpass2" [ getExprA asig, getExprK kef, getExprK kq ]


hilbert :: Expr ARate -> Opcode2 ARate
hilbert asig = 
    Opcode2 "hilbert" [ getExprA asig ]


butterhp :: Expr ARate -> Expr KRate -> Opcode1 ARate
butterhp asig kfreq =
    Opcode1 "butterhp" [ getExprA asig,  getExprK kfreq ]


butterhp_ :: Expr ARate -> Expr KRate -> Expr IInit 
          -> Opcode1 ARate
butterhp_ asig kfreq iskip =
    Opcode1 "butterhp" [ getExprA asig,  getExprK kfreq
                       , getExprI iskip ]


butterlp :: Expr ARate -> Expr KRate -> Opcode1 ARate
butterlp asig kfreq =
    Opcode1 "butterlp" [ getExprA asig,  getExprK kfreq ]


butterlp_ :: Expr ARate -> Expr KRate -> Expr IInit 
          -> Opcode1 ARate
butterlp_ asig kfreq iskip =
    Opcode1 "butterlp" [ getExprA asig,  getExprK kfreq
                       , getExprI iskip ]


butterbp :: Expr ARate -> Expr KRate -> Expr KRate -> Opcode1 ARate
butterbp asig kfreq kband =
    Opcode1 "butterbp" [ getExprA asig,  getExprK kfreq
                       , getExprK kband ]

butterbp_ :: Expr ARate -> Expr KRate -> Expr KRate -> Expr IInit 
          -> Opcode1 ARate
butterbp_ asig kfreq kband iskip =
    Opcode1 "butterbp" [ getExprA asig,  getExprK kfreq
                       , getExprK kband, getExprI iskip ]


butterbr :: Expr ARate -> Expr KRate -> Expr KRate 
         -> Opcode1 ARate
butterbr asig kfreq kband =
    Opcode1 "butterbr" [ getExprA asig,  getExprK kfreq
                       , getExprK kband ]

butterbr_ :: Expr ARate -> Expr KRate -> Expr KRate -> Expr IInit 
          -> Opcode1 ARate
butterbr_ asig kfreq kband iskip =
    Opcode1 "butterbr" [ getExprA asig,  getExprK kfreq
                       , getExprK kband, getExprI iskip ]

--------------------------------------------------------------------------------
-- Specialized filters

nlfilt :: Expr ARate -> Expr KRate -> Expr KRate 
       -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Opcode1 ARate
nlfilt ain ka kb kd kL kC =
    Opcode1 "nlfilt" [ getExprA ain,  getExprK ka
                     , getExprK kb,   getExprK kd
                     , getExprK kL,   getExprK kC ]


pareq :: Expr ARate -> Expr KRate 
      -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Opcode1 ARate
pareq asig kc iv iq imode =
    Opcode1 "pareq" [ getExprA asig,   getExprK kc
                    , getExprI iv,     getExprI iq
                    , getExprI imode ]


dcblock :: Expr ARate -> Opcode1 ARate
dcblock asig = 
    Opcode1 "dcblock" [ getExprA asig ]

dcblock_ :: Expr ARate -> Expr IInit -> Opcode1 ARate
dcblock_ asig ig = 
    Opcode1 "dcblock" [ getExprA asig, getExprI ig ]


--------------------------------------------------------------------------------
-- Envelope modifiers

linen :: forall rate. (KA_Rate rate)
      => Expr rate -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Opcode1 rate
linen amp irise idur idec =
    Opcode1 "linen" [ getExprUniv amp,  getExprI irise
                    , getExprI idur,    getExprI idec ]


linenr :: forall rate1 rate. (KA_Rate rate)
       => Expr rate1 -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
linenr amp irise idec iatdec =
    Opcode1 "linenr" [ getExprUniv amp,   getExprI irise
                     , getExprI idec,     getExprI iatdec ]


envlpx :: forall rate1 rate. (KA_Rate rate)
       => Expr rate1 -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Expr ITableNum -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
envlpx amp irise idur idec ifn iatss iatdec = 
    Opcode1 "envlpx" [ getExprUniv amp,   getExprI irise
                     , getExprI idur,     getExprI idec
                     , getExprUniv ifn,   getExprI iatss
                     , getExprI iatdec ]


envlpxr :: forall rate1 rate. (KA_Rate rate)
        => Expr rate1 -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Expr ITableNum -> Expr IInit -> Expr IInit  
        -> Opcode1 rate
envlpxr amp irise idur idec ifn iatss iatdec =
    Opcode1 "envlpxr" [ getExprUniv amp,  getExprI irise
                      , getExprI idur,    getExprI idec
                      , getExprUniv ifn,  getExprI iatss
                      , getExprI iatdec ]

--------------------------------------------------------------------------------
-- Amplitude modifiers

rms :: Expr ARate -> Opcode1 KRate
rms asig = 
    Opcode1 "rms" [ getExprA asig ]


gain :: Expr ARate -> Expr KRate -> Opcode1 ARate
gain asig krms = 
    Opcode1 "gain" [ getExprA asig, getExprK krms ] 

balance :: Expr ARate -> Expr ARate -> Opcode1 ARate
balance asig acomp = 
    Opcode1 "balance" [ getExprA asig, getExprA acomp ]


dam :: Expr ARate -> Expr KRate -> Expr IInit 
    -> Expr IInit -> Expr IInit -> Expr IInit 
    -> Opcode1 ARate
dam asig kthreshold icomp1 icomp2 irtime iftime = 
    Opcode1 "dam" [ getExprA asig,    getExprK kthreshold
                  , getExprI icomp1,  getExprI icomp2
                  , getExprI irtime,  getExprI iftime ]

--------------------------------------------------------------------------------
-- Signal limiters

wrap :: forall rate. (Rate rate)
     => Expr rate -> Expr rate -> Expr rate 
     -> Opcode1 rate
wrap sig low high =
    Opcode1 "wrap" [ getExprUniv sig, getExprUniv low, getExprUniv high ]


mirror :: forall rate. (Rate rate)
       => Expr rate -> Expr rate -> Expr rate 
       -> Opcode1 rate
mirror sig low high =
    Opcode1 "mirror" [ getExprUniv sig, getExprUniv low, getExprUniv high ]

limit :: forall rate. (Rate rate)
      => Expr rate -> Expr rate -> Expr rate 
      -> Opcode1 rate
limit sig low high =
    Opcode1 "limit" [ getExprUniv sig, getExprUniv low, getExprUniv high ]

--------------------------------------------------------------------------------
-- Delay

delayr :: Expr IInit -> Opcode1 ARate
delayr idlt = 
    Opcode1 "delayr" [ getExprI idlt ]


-- | No answer ...
delayw :: Expr ARate -> Opcode0 rate
delayw asigar = 
    Opcode0 "delayw" [ getExprA asigar ]


delay :: Expr ARate -> Expr IInit -> Opcode1 ARate
delay asig idlt =
    Opcode1 "delay" [ getExprA asig, getExprI idlt ]

delay1 :: Expr ARate -> Opcode1 ARate
delay1 asig = 
    Opcode1 "delay1" [ getExprA asig ]


deltap :: Expr KRate -> Opcode1 ARate
deltap kdlt = 
    Opcode1 "deltap" [ getExprK kdlt ]

 
-- | xdlt is seconds...
--
deltapi :: Expr rate -> Opcode1 ARate
deltapi xdlt = 
    Opcode1 "deltapi" [ getExprUniv xdlt ]


-- | xnumsamps is presumably an integer...
--
deltapn :: Expr rate -> Opcode1 ARate
deltapn xnumsamps =
    Opcode1 "deltapn" [ getExprUniv xnumsamps ]


-- | xdlt is seconds...
--
deltap3 :: Expr rate -> Opcode1 ARate
deltap3 xdlt = 
    Opcode1 "deltap3" [ getExprUniv xdlt ]


multitap :: Expr ARate -> [(Expr IInit, Expr IInit)] -> Opcode1 ARate
multitap asig xs = 
    Opcode1 "multitap" (getExprA asig : concatMap fn xs)
  where
    fn (a,b) = [ getExprI a, getExprI b ]
     


vdelay :: Expr ARate -> Expr ARate -> Expr IInit 
       -> Opcode1 ARate
vdelay asig adel imaxdel =
    Opcode1 "vdelay" [ getExprA asig,   getExprA adel
                     , getExprI imaxdel ]

vdelay3 :: Expr ARate -> Expr ARate -> Expr IInit 
        -> Opcode1 ARate
vdelay3 asig adel imaxdel =
    Opcode1 "vdelay3" [ getExprA asig,  getExprA adel
                      , getExprI imaxdel ]

--------------------------------------------------------------------------------
-- Reverberation

reverb :: Expr ARate -> Expr KRate -> Opcode1 ARate
reverb asig kvrt = 
    Opcode1 "reverb" [ getExprA asig, getExprK kvrt ]


reverb2 :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Opcode1 ARate
reverb2 asig ktime khdif =
    Opcode1 "reverb2" [ getExprA asig, getExprK ktime
                      , getExprK khdif ]


nreverb :: Expr ARate -> Expr KRate -> Expr KRate 
        -> Opcode1 ARate
nreverb asig ktime khdif =
    Opcode1 "nreverb" [ getExprA asig, getExprK ktime
                      , getExprK khdif ]


comb :: Expr ARate -> Expr KRate -> Expr IInit 
     -> Opcode1 ARate
comb asig kvrt ilpt =
    Opcode1 "comb" [ getExprA asig, getExprK kvrt
                   , getExprI ilpt ]


alpass :: Expr ARate -> Expr KRate -> Expr IInit 
       -> Opcode1 ARate
alpass asig kvrt ilpt =
    Opcode1 "alpass" [ getExprA asig, getExprK kvrt
                     , getExprI ilpt ]
               

nestedap :: Expr ARate -> Expr IInit -> Expr IInit 
         -> Expr IInit -> Expr IInit
         -> Opcode1 ARate
nestedap asig imode imaxdel idel1 igain1 =
    Opcode1 "nestedap" [ getExprA asig,    getExprI imode
                       , getExprI imaxdel, getExprI idel1
                       , getExprI igain1 ]

--------------------------------------------------------------------------------
-- Panning and spatialization

-- Note - name /priority/ is given to the 4 channel versions.

-- | 4 channel pan.
--
-- 1 Table fn.
--
pan :: Expr ARate -> Expr KRate -> Expr KRate -> Expr ITableNum
    -> Opcode4 ARate
pan asig kx ky ifn = 
    Opcode4 "pan" [ getExprA asig,  getExprK kx
                  , getExprK ky,    getExprUniv ifn ]

pan2 :: Expr ARate -> Expr rate  -> Opcode2 ARate
pan2 ain xp = 
    Opcode2 "pan2" [ getExprA ain, getExprUniv xp ]

-- | 4 channel version of locsig.
--
locsig :: Expr ARate -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Opcode4 ARate
locsig asig kdegree kdistance kreverbsend = 
    Opcode4 "locsig" [ getExprA asig,      getExprK kdegree
                     , getExprK kdistance, getExprK kreverbsend ]


-- | This is the 2 channel version of 'locsig'
--
locsig2 :: Expr ARate -> Expr KRate -> Expr KRate -> Expr KRate 
        -> Opcode2 ARate
locsig2 asig kdegree kdistance kreverbsend =
    Opcode2 "locsig" [ getExprA asig,      getExprK kdegree
                     , getExprK kdistance, getExprK kreverbsend ]
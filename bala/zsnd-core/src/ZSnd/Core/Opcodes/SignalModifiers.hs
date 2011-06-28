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

  , MultitapConfig
  , MultitapOpcode
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


import ZSnd.Core.CsoundInst.Click
import ZSnd.Core.CsoundInst.Index
import ZSnd.Core.CsoundInst.Typed


--------------------------------------------------------------------------------
-- Standard filters

-- | Note this is @portk@ in Csound.
--
-- @port@ has special meaning in ZSnd, so it is not used here.
--
portmentok :: Opcode2 KRate KRate -> Element KRate
portmentok opF =
    mkElement "portk" inspec (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, khtim) -> 
                [ getConfK ksig, getConfK khtim ]



-- | Note this is @port@ in Csound.
--
-- @port@ has special meaning in ZSnd, so it is not used here.
--
portmento :: Opcode2 KRate IRate -> Element KRate
portmento opF = 
    mkElement "port" inspec (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, ihtim) -> 
                [ getConfK ksig, getConfI ihtim ]

tonek :: Opcode2 KRate KRate -> Element KRate
tonek opF = 
    mkElement "tonek" inspec (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, khp) -> 
                [ getConfK ksig, getConfK khp ]

tone :: Opcode2 ARate KRate -> Element ARate
tone opF = 
    mkElement "tone" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, khp) -> 
                [ getConfA asig, getConfK khp ]


atonek :: Opcode2 KRate KRate -> Element KRate
atonek opF = 
    mkElement "atonek" inspec (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, khp) -> 
                [ getConfK ksig, getConfK khp ]

atone :: Opcode2 ARate KRate -> Element ARate
atone opF = 
    mkElement "atone" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, khp) ->
                [ getConfA asig, getConfK khp ]

resonk :: Opcode3 KRate KRate KRate -> Element KRate
resonk opF = 
    mkElement "resonk" inspec (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, kcf, kbw) -> 
                [ getConfK ksig, getConfK kcf
                , getConfK kbw  ]

reson :: Opcode3 ARate KRate KRate -> Element ARate
reson opF =
    mkElement "reson" inspec  (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf, getConfK kbw ]


aresonk :: Opcode3 KRate KRate KRate -> Element KRate
aresonk opF =
    mkElement "aresonk" inspec (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, kcf, kbw) ->
                [ getConfK ksig, getConfK kcf
                , getConfK kbw ]

areson :: Opcode3 ARate KRate KRate -> Element ARate
areson opF =
    mkElement "areson" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf
                , getConfK kbw ]


tonex :: Opcode2 ARate KRate -> Element ARate
tonex opF =
    mkElement "tonex" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, khp) -> 
                [ getConfA asig, getConfK khp ]

atonex :: Opcode2 ARate KRate -> Element ARate
atonex opF =
    mkElement "atonex" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, khp) ->
                [ getConfA asig, getConfK khp ]


resonx :: Opcode3 ARate KRate KRate -> Element ARate
resonx opF =
    mkElement "resonx" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf
                , getConfK kbw ]

resonr :: Opcode3 ARate KRate KRate -> Element ARate
resonr opF = 
    mkElement "resonr" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf
                , getConfK kbw ]

resonz :: Opcode3 ARate KRate KRate -> Element ARate
resonz opF = 
    mkElement "resonz" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf
                , getConfK kbw ]

resony :: Opcode3 ARate KRate KRate -> Element ARate
resony opF = 
    mkElement "resony" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf
                , getConfK kbw ]

lowres :: Opcode3 ARate KRate KRate -> Element ARate
lowres opF = 
    mkElement "lowres" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcutoff, kreson) ->
                [ getConfA asig, getConfK kcutoff
                , getConfK kreson ]

lowresx :: Opcode3 ARate KRate KRate -> Element ARate
lowresx opF = 
    mkElement "lowresx" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcutoff, kreson) ->
                [ getConfA asig, getConfK kcutoff
                , getConfK kreson ]

vlowres :: Opcode5 ARate KRate KRate IRate KRate -> Element ARate
vlowres opF = 
    mkElement "vlowres" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kfco, kres, iord, ksep) ->
                [ getConfA asig,  getConfK kfco
                , getConfK kres,  getConfI iord
                , getConfK ksep ]


--------------------------------------------------------------------------------
-- Specialized filters

nlfilt :: Opcode6 ARate KRate KRate KRate KRate KRate -> Element ARate
nlfilt opF = 
    mkElement "nlfilt" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(ain, ka, kb, kd, kL, kC) ->
                [ getConfA ain,  getConfK ka
                , getConfK kb,   getConfK kd
                , getConfK kL,   getConfK kC ]


pareq :: Opcode5 ARate KRate IRate IRate IRate -> Element ARate
pareq opF =
    mkElement "pareq" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kc, iv, iq, imode) ->
                [ getConfA asig,   getConfK kc
                , getConfI iv,     getConfI iq
                , getConfI imode ]



dcblock :: Opcode1 ARate -> Element ARate
dcblock opF = 
    mkElement "dcblock" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \asig ->
                [ getConfA asig ]

dcblock_ :: Opcode2 ARate IRate -> Element ARate
dcblock_ opF = 
    mkElement "dcblock" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, ig) ->
                [ getConfA asig, getConfI ig ]


--------------------------------------------------------------------------------
-- Envelope modifiers

linen :: forall rate. (KA_Rate rate)
     => Opcode4 rate IRate IRate IRate -> Element rate
linen opF = 
    mkElement "linen" inspec (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(amp, irise, idur, idec) ->
                [ getConfUniv amp,  getConfI irise
                , getConfI idur,    getConfI idec ]

linenr :: forall rate. (KA_Rate rate)
      => Opcode4 rate IRate IRate IRate -> Element rate
linenr opF =
    mkElement "linenr" inspec (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(amp, irise, idec, iatdec) ->
                [ getConfUniv amp,   getConfI irise
                , getConfI idec,    getConfI iatdec ]

envlpx :: forall rate. (KA_Rate rate)
       => Opcode7 rate IRate IRate IRate IRate IRate IRate -> Element rate
envlpx opF = 
    mkElement "envlpx" inspec (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(amp, irise, idur, idec, ifn, iatss, iatdec) -> 
                [ getConfUniv amp,   getConfI irise
                , getConfI idur,     getConfI idec
                , getConfI ifn,      getConfI iatss
                , getConfI iatdec ]

envlpxr :: forall rate. (KA_Rate rate)
        => Opcode7 rate IRate IRate IRate IRate IRate IRate  -> Element rate
envlpxr opF = 
    mkElement "envlpxr" inspec (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(amp, irise, idur, idec, ifn, iatss, iatdec) ->
                [ getConfUniv amp,  getConfI irise
                , getConfI idur,    getConfI idec
                , getConfI ifn,     getConfI iatss
                , getConfI iatdec ]



--------------------------------------------------------------------------------
-- Amplitude modifiers

rms :: Opcode1 ARate -> Element KRate
rms opF = 
    mkElement "rms" inspec (Out1 K)
  where
    inspec = applyOpcode opF $ \asig ->
                [ getConfA asig ]

gain :: Opcode2 ARate KRate -> Element ARate
gain opF = 
    mkElement "gain" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, krms) ->
                [ getConfA asig, getConfK krms ] 

balance :: Opcode2 ARate ARate -> Element ARate
balance opF = 
    mkElement "balance" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, acomp) ->
                [ getConfA asig, getConfA acomp ]


dam :: Opcode6 ARate KRate IRate IRate IRate IRate -> Element ARate
dam opF = 
    mkElement "dam" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \(asig, kthreshold, icomp1, icomp2, irtime, iftime) ->
                   [ getConfA asig,    getConfK kthreshold
                   , getConfI icomp1,  getConfI icomp2
                   , getConfI irtime,  getConfI iftime ]

--------------------------------------------------------------------------------
-- Signal limiters

wrap :: forall rate. (Rate rate)
     => Opcode3 rate rate rate -> Element rate
wrap opF =
    mkElement "wrap" inspec (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(sig, low, high) ->
                [ getConfUniv sig, getConfUniv low
                , getConfUniv high ]


mirror :: forall rate. (Rate rate)
       => Opcode3 rate rate rate -> Element rate
mirror opF =
    mkElement "mirror" inspec (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(sig, low, high) ->
                [ getConfUniv sig, getConfUniv low
                , getConfUniv high ]

limit :: forall rate. (Rate rate)
      => Opcode3 rate rate rate -> Element rate
limit opF =
    mkElement "limit" inspec (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(sig, low, high) ->
                [ getConfUniv sig, getConfUniv low
                , getConfUniv high ]

--------------------------------------------------------------------------------
-- Delay

delayr :: Opcode1 IRate -> Element ARate
delayr opF = 
    mkElement "delayr" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \idlt ->
                [ getConfI idlt ]


-- No answer ...
delayw :: Opcode1 ARate -> Element rate
delayw opF = 
    mkElement "delayw" inspec Out0
  where
    inspec = applyOpcode opF $ \asigar ->
                 [ getConfA asigar ]


delay :: Opcode2 ARate IRate -> Element ARate
delay opF = 
    mkElement "delay" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, idlt) ->
                [ getConfA asig, getConfI idlt ]

delay1 :: Opcode1 ARate -> Element ARate
delay1 opF = 
    mkElement "delay1" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \asig ->
                [ getConfA asig ]


deltap :: Opcode1 KRate -> Element ARate
deltap opF = 
    mkElement "deltap" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \kdlt ->
                [ getConfK kdlt ]

-- | xdlt is seconds...
--
deltapi :: Opcode1 rate -> Element ARate
deltapi opF = 
    mkElement "deltapi" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \xdlt ->
                [ getConfUniv xdlt ]

-- | xnumsamps is presumably an integer...
--
deltapn :: Opcode1 rate -> Element ARate
deltapn opF = 
    mkElement "deltapn" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \xnumsamps ->
                [ getConfUniv xnumsamps ]


-- | xdlt is seconds...
--
deltap3 :: Opcode1 rate -> Element ARate
deltap3 opF = 
    mkElement "deltap3" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \xdlt ->
                [ getConfUniv xdlt ]

                
-- needs spcial typesig...


type MultitapConfig = ( Conf ARate, [(Conf IRate, Conf IRate)] )
                      
type MultitapOpcode = ElemRef -> PortDict -> Either FailMsg MultitapConfig


multitap :: MultitapOpcode -> Element ARate
multitap opF = 
    mkElement "multitap" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, xs) ->
                let fn (a,b) = [ getConfI a, getConfI b ]
                in (getConfA asig : concatMap fn xs)


vdelay :: Opcode3 ARate ARate IRate -> Element ARate
vdelay opF = 
    mkElement "vdelay" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, adel, imaxdel) ->
                [ getConfA asig,    getConfA adel
                , getConfI imaxdel ]

vdelay3 :: Opcode3 ARate ARate IRate -> Element ARate
vdelay3 opF = 
    mkElement "vdelay3" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, adel, imaxdel) ->
                [ getConfA asig,    getConfA adel
                , getConfI imaxdel ]

--------------------------------------------------------------------------------
-- Reverberation

reverb :: Opcode2 ARate KRate -> Element ARate
reverb opF = 
    mkElement "reverb" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kvrt) ->
                [ getConfA asig, getConfK kvrt ]

reverb2 :: Opcode3 ARate KRate KRate -> Element ARate
reverb2 opF = 
    mkElement "reverb2" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, ktime, khdif) ->
                [ getConfA asig, getConfK ktime
                , getConfK khdif ]

nreverb :: Opcode3 ARate KRate KRate -> Element ARate
nreverb opF = 
    mkElement "nreverb" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, ktime, khdif) ->
                [ getConfA asig, getConfK ktime
                , getConfK khdif ]

comb :: Opcode3 ARate KRate IRate -> Element ARate
comb opF = 
    mkElement "comb" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kvrt, ilpt) ->
                [ getConfA asig, getConfK kvrt
                , getConfI ilpt ]

alpass :: Opcode3 ARate KRate IRate -> Element ARate
alpass opF = 
    mkElement "alpass" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kvrt, ilpt) ->
                [ getConfA asig, getConfK kvrt
                , getConfI ilpt ]
               
nestedap :: Opcode5 ARate IRate IRate IRate IRate
         -> Element ARate
nestedap opF = 
    mkElement "nestedap" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, imode, imaxdel, idel1, igain1) ->
                [ getConfA asig,    getConfI imode
                , getConfI imaxdel, getConfI idel1
                , getConfI igain1 ]

--------------------------------------------------------------------------------
-- Panning and spatialization

-- Note - name /priority/ is given to the 4 channel versions.

-- | 4 channel pan.
--
pan :: Opcode4 ARate KRate KRate IRate -> Element ARate
pan opF = 
    mkElement "pan" inspec (OutN A 4)
  where
    inspec = applyOpcode opF $ \(asig, kx, ky, ifn) ->
                [ getConfA asig,    getConfK kx
                , getConfK ky,      getConfI ifn ]

pan2 :: Opcode4 ARate KRate KRate IRate -> Element ARate
pan2 opF = 
    mkElement "pan2" inspec (Out2 A)
  where
    inspec = applyOpcode opF $ \(asig, kx, ky, ifn) ->
                [ getConfA asig,    getConfK kx
                , getConfK ky,      getConfI ifn ]

-- | 4 channel version of locsig.
--
locsig :: Opcode4 ARate KRate KRate KRate -> Element ARate
locsig opF = 
    mkElement "locsig" inspec (OutN A 4)
  where
    inspec = applyOpcode opF $ \(asig, kdegree, kdistance, kreverbsend) ->
                [ getConfA asig,      getConfK kdegree
                , getConfK kdistance, getConfK kreverbsend ]


-- | This is the 2 channel version of 'locsig'
--
locsig2 :: Opcode4 ARate KRate KRate KRate -> Element ARate
locsig2 opF =
    mkElement "locsig" inspec (Out2 A)
  where
    inspec = applyOpcode opF $ \(asig, kdegree, kdistance, kreverbsend) ->
                [ getConfA asig,      getConfK kdegree
                , getConfK kdistance, getConfK kreverbsend ]

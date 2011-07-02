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
import ZSnd.Core.CsoundInst.Prim
import ZSnd.Core.CsoundInst.Typed


--------------------------------------------------------------------------------
-- Standard filters

-- | Note this is @portk@ in Csound.
--
-- @port@ has special meaning in ZSnd, so it is not used here.
--
portmentok :: Opcode2 KRate KRate -> Element KRate
portmentok opF =
    mkOpcode "portk" inspec [] (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, khtim) -> 
                [ getConfK ksig, getConfK khtim ]



-- | Note this is @port@ in Csound.
--
-- @port@ has special meaning in ZSnd, so it is not used here.
--
portmento :: Opcode2 KRate IInit -> Element KRate
portmento opF = 
    mkOpcode "port" inspec [] (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, ihtim) -> 
                [ getConfK ksig, getConfI ihtim ]

tonek :: Opcode2 KRate KRate -> Element KRate
tonek opF = 
    mkOpcode "tonek" inspec [] (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, khp) -> 
                [ getConfK ksig, getConfK khp ]

tone :: Opcode2 ARate KRate -> Element ARate
tone opF = 
    mkOpcode "tone" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, khp) -> 
                [ getConfA asig, getConfK khp ]


atonek :: Opcode2 KRate KRate -> Element KRate
atonek opF = 
    mkOpcode "atonek" inspec [] (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, khp) -> 
                [ getConfK ksig, getConfK khp ]

atone :: Opcode2 ARate KRate -> Element ARate
atone opF = 
    mkOpcode "atone" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, khp) ->
                [ getConfA asig, getConfK khp ]

resonk :: Opcode3 KRate KRate KRate -> Element KRate
resonk opF = 
    mkOpcode "resonk" inspec [] (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, kcf, kbw) -> 
                [ getConfK ksig, getConfK kcf
                , getConfK kbw  ]

reson :: Opcode3 ARate KRate KRate -> Element ARate
reson opF =
    mkOpcode "reson" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf, getConfK kbw ]


aresonk :: Opcode3 KRate KRate KRate -> Element KRate
aresonk opF =
    mkOpcode "aresonk" inspec [] (Out1 K)
  where
    inspec = applyOpcode opF $ \(ksig, kcf, kbw) ->
                [ getConfK ksig, getConfK kcf
                , getConfK kbw ]

areson :: Opcode3 ARate KRate KRate -> Element ARate
areson opF =
    mkOpcode "areson" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf
                , getConfK kbw ]


tonex :: Opcode2 ARate KRate -> Element ARate
tonex opF =
    mkOpcode "tonex" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, khp) -> 
                [ getConfA asig, getConfK khp ]

atonex :: Opcode2 ARate KRate -> Element ARate
atonex opF =
    mkOpcode "atonex" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, khp) ->
                [ getConfA asig, getConfK khp ]


resonx :: Opcode3 ARate KRate KRate -> Element ARate
resonx opF =
    mkOpcode "resonx" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf
                , getConfK kbw ]

resonr :: Opcode3 ARate KRate KRate -> Element ARate
resonr opF = 
    mkOpcode "resonr" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf
                , getConfK kbw ]

resonz :: Opcode3 ARate KRate KRate -> Element ARate
resonz opF = 
    mkOpcode "resonz" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf
                , getConfK kbw ]

resony :: Opcode3 ARate KRate KRate -> Element ARate
resony opF = 
    mkOpcode "resony" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcf, kbw) ->
                [ getConfA asig, getConfK kcf
                , getConfK kbw ]

lowres :: Opcode3 ARate KRate KRate -> Element ARate
lowres opF = 
    mkOpcode "lowres" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcutoff, kreson) ->
                [ getConfA asig, getConfK kcutoff
                , getConfK kreson ]

lowresx :: Opcode3 ARate KRate KRate -> Element ARate
lowresx opF = 
    mkOpcode "lowresx" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kcutoff, kreson) ->
                [ getConfA asig, getConfK kcutoff
                , getConfK kreson ]

vlowres :: Opcode5 ARate KRate KRate IInit KRate -> Element ARate
vlowres opF = 
    mkOpcode "vlowres" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kfco, kres, iord, ksep) ->
                [ getConfA asig,  getConfK kfco
                , getConfK kres,  getConfI iord
                , getConfK ksep ]


lowpass2 :: Opcode3 ARate KRate KRate -> Element ARate
lowpass2 opF = 
    mkOpcode "lowpass2" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kef, kq) ->
                [ getConfA asig, getConfK kef
                , getConfK kq ]



hilbert :: Opcode1 ARate -> Element ARate
hilbert opF = 
    mkOpcode "hilbert" inspec [] (Out2 A)
  where
    inspec = applyOpcode opF $ \asig ->
                [ getConfA asig ]


butterhp :: Opcode2 ARate KRate -> Element ARate
butterhp opF = 
    mkOpcode "butterhp" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kfreq) ->
                [ getConfA asig,  getConfK kfreq ]

butterhp_ :: Opcode3 ARate KRate IInit -> Element ARate
butterhp_ opF = 
    mkOpcode "butterhp" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kfreq, iskip) ->
                [ getConfA asig,  getConfK kfreq
                , getConfI iskip ]


butterlp :: Opcode2 ARate KRate -> Element ARate
butterlp opF = 
    mkOpcode "butterlp" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kfreq) ->
                [ getConfA asig,  getConfK kfreq ]

butterlp_ :: Opcode3 ARate KRate IInit -> Element ARate
butterlp_ opF = 
    mkOpcode "butterlp" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kfreq, iskip) ->
                [ getConfA asig,  getConfK kfreq
                , getConfI iskip ]


butterbp :: Opcode3 ARate KRate KRate -> Element ARate
butterbp opF = 
    mkOpcode "butterbp" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kfreq, kband) ->
                [ getConfA asig,  getConfK kfreq
                , getConfK kband ]

butterbp_ :: Opcode4 ARate KRate KRate IInit -> Element ARate
butterbp_ opF = 
    mkOpcode "butterbp" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kfreq, kband, iskip) ->
                [ getConfA asig,  getConfK kfreq
                , getConfK kband, getConfI iskip ]

butterbr :: Opcode3 ARate KRate KRate -> Element ARate
butterbr opF = 
    mkOpcode "butterbr" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kfreq, kband) ->
                [ getConfA asig,  getConfK kfreq
                , getConfK kband ]

butterbr_ :: Opcode4 ARate KRate KRate IInit -> Element ARate
butterbr_ opF = 
    mkOpcode "butterbr" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kfreq, kband, iskip) ->
                [ getConfA asig,  getConfK kfreq
                , getConfK kband, getConfI iskip ]


--------------------------------------------------------------------------------
-- Specialized filters

nlfilt :: Opcode6 ARate KRate KRate KRate KRate KRate -> Element ARate
nlfilt opF = 
    mkOpcode "nlfilt" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(ain, ka, kb, kd, kL, kC) ->
                [ getConfA ain,  getConfK ka
                , getConfK kb,   getConfK kd
                , getConfK kL,   getConfK kC ]


pareq :: Opcode5 ARate KRate IInit IInit IInit -> Element ARate
pareq opF =
    mkOpcode "pareq" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kc, iv, iq, imode) ->
                [ getConfA asig,   getConfK kc
                , getConfI iv,     getConfI iq
                , getConfI imode ]



dcblock :: Opcode1 ARate -> Element ARate
dcblock opF = 
    mkOpcode "dcblock" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \asig ->
                [ getConfA asig ]

dcblock_ :: Opcode2 ARate IInit -> Element ARate
dcblock_ opF = 
    mkOpcode "dcblock" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, ig) ->
                [ getConfA asig, getConfI ig ]


--------------------------------------------------------------------------------
-- Envelope modifiers

linen :: forall rate. (KA_Rate rate)
     => Opcode4 rate IInit IInit IInit -> Element rate
linen opF = 
    mkOpcode "linen" inspec [] (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(amp, irise, idur, idec) ->
                [ getConfUniv amp,  getConfI irise
                , getConfI idur,    getConfI idec ]

linenr :: forall rate. (KA_Rate rate)
      => Opcode4 rate IInit IInit IInit -> Element rate
linenr opF =
    mkOpcode "linenr" inspec [] (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(amp, irise, idec, iatdec) ->
                [ getConfUniv amp,   getConfI irise
                , getConfI idec,     getConfI iatdec ]

envlpx :: forall rate. (KA_Rate rate)
       => Int -> Opcode6 rate IInit IInit IInit IInit IInit -> Element rate
envlpx ifn opF = 
    mkOpcode "envlpx" inspec [ifn] (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(amp, irise, idur, idec, iatss, iatdec) -> 
                [ getConfUniv amp,   getConfI irise
                , getConfI idur,     getConfI idec
                , getConfI $ tablefn ifn,      getConfI iatss
                , getConfI iatdec ]

envlpxr :: forall rate. (KA_Rate rate)
        => Int -> Opcode6 rate IInit IInit IInit IInit IInit  -> Element rate
envlpxr ifn opF = 
    mkOpcode "envlpxr" inspec [ifn] (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(amp, irise, idur, idec, iatss, iatdec) ->
                [ getConfUniv amp,          getConfI irise
                , getConfI idur,            getConfI idec
                , getConfI $ tablefn ifn,   getConfI iatss
                , getConfI iatdec ]



--------------------------------------------------------------------------------
-- Amplitude modifiers

rms :: Opcode1 ARate -> Element KRate
rms opF = 
    mkOpcode "rms" inspec [] (Out1 K)
  where
    inspec = applyOpcode opF $ \asig ->
                [ getConfA asig ]

gain :: Opcode2 ARate KRate -> Element ARate
gain opF = 
    mkOpcode "gain" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, krms) ->
                [ getConfA asig, getConfK krms ] 

balance :: Opcode2 ARate ARate -> Element ARate
balance opF = 
    mkOpcode "balance" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, acomp) ->
                [ getConfA asig, getConfA acomp ]


dam :: Opcode6 ARate KRate IInit IInit IInit IInit -> Element ARate
dam opF = 
    mkOpcode "dam" inspec [] (Out1 A)
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
    mkOpcode "wrap" inspec [] (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(sig, low, high) ->
                [ getConfUniv sig, getConfUniv low
                , getConfUniv high ]


mirror :: forall rate. (Rate rate)
       => Opcode3 rate rate rate -> Element rate
mirror opF =
    mkOpcode "mirror" inspec [] (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(sig, low, high) ->
                [ getConfUniv sig, getConfUniv low
                , getConfUniv high ]

limit :: forall rate. (Rate rate)
      => Opcode3 rate rate rate -> Element rate
limit opF =
    mkOpcode "limit" inspec [] (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(sig, low, high) ->
                [ getConfUniv sig, getConfUniv low
                , getConfUniv high ]

--------------------------------------------------------------------------------
-- Delay

delayr :: Opcode1 IInit -> Element ARate
delayr opF = 
    mkOpcode "delayr" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \idlt ->
                [ getConfI idlt ]


-- No answer ...
delayw :: Opcode1 ARate -> Element rate
delayw opF = 
    mkOpcode "delayw" inspec [] Out0
  where
    inspec = applyOpcode opF $ \asigar ->
                 [ getConfA asigar ]


delay :: Opcode2 ARate IInit -> Element ARate
delay opF = 
    mkOpcode "delay" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, idlt) ->
                [ getConfA asig, getConfI idlt ]

delay1 :: Opcode1 ARate -> Element ARate
delay1 opF = 
    mkOpcode "delay1" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \asig ->
                [ getConfA asig ]


deltap :: Opcode1 KRate -> Element ARate
deltap opF = 
    mkOpcode "deltap" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \kdlt ->
                [ getConfK kdlt ]

-- | xdlt is seconds...
--
deltapi :: Opcode1 rate -> Element ARate
deltapi opF = 
    mkOpcode "deltapi" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \xdlt ->
                [ getConfUniv xdlt ]

-- | xnumsamps is presumably an integer...
--
deltapn :: Opcode1 rate -> Element ARate
deltapn opF = 
    mkOpcode "deltapn" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \xnumsamps ->
                [ getConfUniv xnumsamps ]


-- | xdlt is seconds...
--
deltap3 :: Opcode1 rate -> Element ARate
deltap3 opF = 
    mkOpcode "deltap3" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \xdlt ->
                [ getConfUniv xdlt ]

                
-- needs spcial typesig...


type MultitapConfig = ( Conf ARate, [(Conf IInit, Conf IInit)] )
                      
type MultitapOpcode = ElemRef -> PortDict -> Either FailMsg MultitapConfig


multitap :: MultitapOpcode -> Element ARate
multitap opF = 
    mkOpcode "multitap" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, xs) ->
                let fn (a,b) = [ getConfI a, getConfI b ]
                in (getConfA asig : concatMap fn xs)


vdelay :: Opcode3 ARate ARate IInit -> Element ARate
vdelay opF = 
    mkOpcode "vdelay" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, adel, imaxdel) ->
                [ getConfA asig,    getConfA adel
                , getConfI imaxdel ]

vdelay3 :: Opcode3 ARate ARate IInit -> Element ARate
vdelay3 opF = 
    mkOpcode "vdelay3" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, adel, imaxdel) ->
                [ getConfA asig,    getConfA adel
                , getConfI imaxdel ]

--------------------------------------------------------------------------------
-- Reverberation

reverb :: Opcode2 ARate KRate -> Element ARate
reverb opF = 
    mkOpcode "reverb" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kvrt) ->
                [ getConfA asig, getConfK kvrt ]

reverb2 :: Opcode3 ARate KRate KRate -> Element ARate
reverb2 opF = 
    mkOpcode "reverb2" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, ktime, khdif) ->
                [ getConfA asig, getConfK ktime
                , getConfK khdif ]

nreverb :: Opcode3 ARate KRate KRate -> Element ARate
nreverb opF = 
    mkOpcode "nreverb" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, ktime, khdif) ->
                [ getConfA asig, getConfK ktime
                , getConfK khdif ]

comb :: Opcode3 ARate KRate IInit -> Element ARate
comb opF = 
    mkOpcode "comb" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kvrt, ilpt) ->
                [ getConfA asig, getConfK kvrt
                , getConfI ilpt ]

alpass :: Opcode3 ARate KRate IInit -> Element ARate
alpass opF = 
    mkOpcode "alpass" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(asig, kvrt, ilpt) ->
                [ getConfA asig, getConfK kvrt
                , getConfI ilpt ]
               
nestedap :: Opcode5 ARate IInit IInit IInit IInit
         -> Element ARate
nestedap opF = 
    mkOpcode "nestedap" inspec [] (Out1 A)
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
-- 1 Table fn.
--
pan :: Int -> Opcode3 ARate KRate KRate -> Element ARate
pan ifn opF = 
    mkOpcode "pan" inspec [ifn] (OutN A 4)
  where
    inspec = applyOpcode opF $ \(asig, kx, ky) ->
                [ getConfA asig,    getConfK kx
                , getConfK ky,      getConfI $ tablefn ifn ]

pan2 :: Opcode2 ARate rate  -> Element ARate
pan2 opF = 
    mkOpcode "pan2" inspec [] (Out2 A)
  where
    inspec = applyOpcode opF $ \(ain, xp) ->
                [ getConfA ain,     getConfUniv xp ]

-- | 4 channel version of locsig.
--
locsig :: Opcode4 ARate KRate KRate KRate -> Element ARate
locsig opF = 
    mkOpcode "locsig" inspec [] (OutN A 4)
  where
    inspec = applyOpcode opF $ \(asig, kdegree, kdistance, kreverbsend) ->
                [ getConfA asig,      getConfK kdegree
                , getConfK kdistance, getConfK kreverbsend ]


-- | This is the 2 channel version of 'locsig'
--
locsig2 :: Opcode4 ARate KRate KRate KRate -> Element ARate
locsig2 opF =
    mkOpcode "locsig" inspec [] (Out2 A)
  where
    inspec = applyOpcode opF $ \(asig, kdegree, kdistance, kreverbsend) ->
                [ getConfA asig,      getConfK kdegree
                , getConfK kdistance, getConfK kreverbsend ]

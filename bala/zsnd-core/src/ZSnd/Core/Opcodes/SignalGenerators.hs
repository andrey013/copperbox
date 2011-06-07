{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes.SignalGenerators
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Csound opcodes.
-- 
-- Unlike Haskell, Csound allows optional arguments in opcode 
-- signatures. ZSnd models this by defining two versions of 
-- such opcodes:
-- 
-- a) The short version with no optional args uses the regular 
-- Csound name.
-- 
-- b) The fully specified version, which includes all optional
-- args, suffixes the Csound name with an underscore.
--
-- Note, there are many name clashes with Prelude. 
--
-- Also note, some opcodes are actually class methods to support 
-- rate overloading. However, whilst the class methods are 
-- exported, the class isn'\t. This is because the set of 
-- instances is finite - one or more of @IRate@, @KRate@ or
-- @ARate@.
-- 
--
--------------------------------------------------------------------------------

module ZSnd.Core.Opcodes.SignalGenerators
  (

  -- * Linear and exponential generators
    line
  , expon 
  , linseg
  , linsegr
  , expseg
  , expsegr
  , expsega
  , adsr
  , adsr_
  , madsr
  , madsr_
  , xadsr
  , xadsr_
  , mxadsr
  , mxadsr_

  -- * Table access
  , table
  , table_
  , tablei
  , tablei_
  , table3
  , table3_

  , oscil1
  , oscil1i
  , osciln

  -- * Phasors
  , phasor
  , phasor_
  , phasorbnk
  , phasorbnk_

  -- * Basic oscillators
  , oscil
  , oscili
  , oscil3
  , poscil
  , poscil3
  , lfo

  -- * Dynamic spectrum oscillators
  , buzz
  , buzz_
  , gbuzz
  , gbuzz_
  , vco

  -- * Additive synthesis / resynthesis
  , adsyn
  , adsynt
  , adsynt_
  , hsboscil
  , hsboscil_

  -- * FM synthesis
  , foscil
  , foscil_

  ) where


import ZSnd.Core.CsoundInst

--------------------------------------------------------------------------------
-- Linear and Exponential generators

class CLinear rate where
  line      :: Expr IR -> Expr IR -> Expr IR -> InstBuilder (Expr rate)
  expon     :: Expr IR -> Expr IR -> Expr IR -> InstBuilder (Expr rate)
  linseg    :: Expr IR -> Expr IR -> Expr IR -> [(Expr IR, Expr IR)]
                       -> InstBuilder (Expr rate)

  linsegr   :: Expr IR -> Expr IR -> Expr IR -> [(Expr IR, Expr IR)]
                       -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  expseg    :: Expr IR -> Expr IR -> Expr IR -> [(Expr IR, Expr IR)]
                       -> InstBuilder (Expr rate)

  expsegr   :: Expr IR -> Expr IR -> Expr IR -> [(Expr IR, Expr IR)]
                       -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)



instance CLinear KR where
  line ia idur ib = kopcode "line" [ getExpr ia, getExpr idur, getExpr ib ]

  expon ia idur ib = kopcode "expon" [ getExpr ia, getExpr idur, getExpr ib ]

  linseg ia idur ib xs = 
    kopcode "linseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  linsegr ia idur ib xs irel iz = 
    kopcode "linsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]

  expseg ia idur ib xs = 
    kopcode "expseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  expsegr ia idur ib xs irel iz = 
    kopcode "linsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]


instance CLinear AR where
  line ia idur ib = aopcode "line" [ getExpr ia, getExpr idur, getExpr ib ]

  expon ia idur ib = aopcode "expon" [ getExpr ia, getExpr idur, getExpr ib ]

  linseg ia idur ib xs = 
    aopcode "linseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  linsegr ia idur ib xs irel iz = 
    aopcode "linsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]

  expseg ia idur ib xs = 
    aopcode "expseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  expsegr ia idur ib xs irel iz = 
    aopcode "expsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]



expsega   :: Expr IR -> Expr IR -> Expr IR 
                        -> [(Expr IR, Expr IR)]
                        -> InstBuilder (Expr AR)
expsega ia idur ib xs = 
    aopcode "expsega" (getExpr ia : getExpr idur : getExpr ib : rest)
  where
    rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs


class CAdsr rate where
  adsr      :: Expr IR -> Expr IR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  adsr_     :: Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  madsr     :: Expr IR -> Expr IR -> Expr IR -> Expr IR
                       -> InstBuilder (Expr rate)

  madsr_    :: Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  xadsr     :: Expr IR -> Expr IR -> Expr IR -> Expr IR
                       -> InstBuilder (Expr rate)

  xadsr_    :: Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  mxadsr    :: Expr IR -> Expr IR -> Expr IR -> Expr IR
                       -> InstBuilder (Expr rate)

  mxadsr_   :: Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

instance CAdsr KR where
  adsr ia idec isl ir = 
    kopcode "adsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  adsr_ ia idec isl ir idel = 
    kopcode "adsr" [ getExpr ia, getExpr idec, getExpr isl
                   , getExpr ir, getExpr idel ]

  madsr ia idec isl ir = 
    kopcode "madsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  madsr_ ia idec isl ir idel = 
    kopcode "madsr" [ getExpr ia, getExpr idec, getExpr isl
                    , getExpr ir, getExpr idel ]

  xadsr ia idec isl ir = 
    kopcode "xadsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  xadsr_ ia idec isl ir idel = 
    kopcode "xadsr" [ getExpr ia, getExpr idec, getExpr isl
                    , getExpr ir, getExpr idel ]

  mxadsr ia idec isl ir = 
    kopcode "mxadsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  mxadsr_ ia idec isl ir idel = 
    kopcode "mxadsr" [ getExpr ia, getExpr idec, getExpr isl
                     , getExpr ir, getExpr idel ]

instance CAdsr AR where
  adsr ia idec isl ir = 
    aopcode "adsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  adsr_ ia idec isl ir idel = 
    aopcode "adsr" [ getExpr ia, getExpr idec, getExpr isl
                   , getExpr ir, getExpr idel ]

  madsr ia idec isl ir = 
    aopcode "madsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  madsr_ ia idec isl ir idel = 
    aopcode "madsr" [ getExpr ia, getExpr idec, getExpr isl
                    , getExpr ir, getExpr idel ]

  xadsr ia idec isl ir = 
    aopcode "xadsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  xadsr_ ia idec isl ir idel = 
    aopcode "xadsr" [ getExpr ia, getExpr idec, getExpr isl
                    , getExpr ir, getExpr idel ]

  mxadsr ia idec isl ir = 
    aopcode "mxadsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  mxadsr_ ia idec isl ir idel = 
    aopcode "mxadsr" [ getExpr ia, getExpr idec, getExpr isl
                     , getExpr ir, getExpr idel ]

--------------------------------------------------------------------------------
-- Table access

table :: Expr a -> Expr IR -> InstBuilder (Expr a)
table ndx ifn = opcode "table" [ getExpr ndx, getExpr ifn ]

table_ :: Expr a -> Expr IR -> Expr a -> Expr IR -> Expr IR    
       -> InstBuilder (Expr a)
table_ ndx ifn ixmode ixoff ixwrap = 
    opcode "table" [ getExpr ndx, getExpr ifn, getExpr ixmode
                   , getExpr ixoff, getExpr ixwrap ]


tablei :: Expr a -> Expr IR -> InstBuilder (Expr a)
tablei ndx ifn = opcode "tablei" [ getExpr ndx, getExpr ifn ]

tablei_ :: Expr a -> Expr IR -> Expr a -> Expr IR -> Expr IR
       -> InstBuilder (Expr a)
tablei_ ndx ifn ixmode ixoff ixwrap = 
    opcode "tablei" [ getExpr ndx, getExpr ifn, getExpr ixmode
                    , getExpr ixoff, getExpr ixwrap ]



table3 :: Expr a -> Expr IR -> InstBuilder (Expr a)
table3 ndx ifn = opcode "table" [ getExpr ndx, getExpr ifn ]

table3_ :: Expr a -> Expr IR -> Expr a -> Expr IR -> Expr IR
        -> InstBuilder (Expr a)
table3_ ndx ifn ixmode ixoff ixwrap = 
    opcode "table3" [ getExpr ndx, getExpr ifn, getExpr ixmode
                    , getExpr ixoff, getExpr ixwrap ]


oscil1 :: Expr IR -> Expr KR -> Expr IR -> Expr IR -> InstBuilder (Expr KR)
oscil1 idel kamp idur ifn = 
    kopcode "oscil1" [ getExpr idel, getExpr kamp, getExpr idur, getExpr ifn ]


oscil1i :: Expr IR -> Expr KR -> Expr IR -> Expr IR -> InstBuilder (Expr KR)
oscil1i idel kamp idur ifn = 
    kopcode "oscil1i" [ getExpr idel, getExpr kamp, getExpr idur, getExpr ifn ]

osciln :: Expr KR -> Expr IR -> Expr IR -> Expr IR -> InstBuilder (Expr KR)
osciln kamp ifrq ifn itimes = 
    kopcode "osciln" [ getExpr kamp, getExpr ifrq, getExpr ifn, getExpr itimes ]

--------------------------------------------------------------------------------
-- Phasors

class CPhasor rate where
  phasor      :: Expr rate -> InstBuilder (Expr rate)
  phasor_     :: Expr rate -> Expr IR -> InstBuilder (Expr rate)

  phasorbnk   :: Expr rate -> Expr KR -> Expr IR -> InstBuilder (Expr rate)
  phasorbnk_  :: Expr rate -> Expr KR -> Expr IR -> Expr IR 
                           -> InstBuilder (Expr rate)
 
instance CPhasor KR where
  phasor cps = kopcode "phasor" [getExpr cps]
  phasor_ cps iphs = kopcode "phasor" [getExpr cps, getExpr iphs]
   
  phasorbnk cps kindx icnt = 
    kopcode "phasorbnk" [ getExpr cps, getExpr kindx, getExpr icnt ]

  phasorbnk_ cps kindx icnt iphs = 
    kopcode "phasorbnk" [ getExpr cps, getExpr kindx
                        , getExpr icnt, getExpr iphs ]

 
instance CPhasor AR where
  phasor cps = aopcode "phasor" [getExpr cps]
  phasor_ cps iphs = aopcode "phasor" [getExpr cps, getExpr iphs]
   
  phasorbnk cps kindx icnt = 
    aopcode "phasorbnk" [ getExpr cps, getExpr kindx, getExpr icnt ]

  phasorbnk_ cps kindx icnt iphs = 
    aopcode "phasorbnk" [ getExpr cps, getExpr kindx
                        , getExpr icnt, getExpr iphs ]


--------------------------------------------------------------------------------
-- Basic oscillators

class COscil rate where
  oscil     :: Expr rate -> Expr rate -> Expr IR -> InstBuilder (Expr rate)

  oscil_    :: Expr rate -> Expr rate -> Expr IR -> Expr IR 
                         -> InstBuilder (Expr rate)

  oscili    :: Expr rate -> Expr rate -> Expr IR -> InstBuilder (Expr rate)
  oscili_   :: Expr rate -> Expr rate -> Expr IR -> Expr IR 
                         -> InstBuilder (Expr rate)

  oscil3    :: Expr rate -> Expr rate -> Expr IR -> InstBuilder (Expr rate)
  oscil3_   :: Expr rate -> Expr rate -> Expr IR -> Expr IR 
                         -> InstBuilder (Expr rate)

  poscil    :: Expr KR -> Expr KR -> Expr IR -> InstBuilder (Expr rate)
  poscil_   :: Expr KR -> Expr KR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  poscil3   :: Expr KR -> Expr KR -> Expr IR -> InstBuilder (Expr rate)
  poscil3_  :: Expr KR -> Expr KR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  lfo       :: Expr KR -> Expr KR -> InstBuilder (Expr rate)
  lfo_      :: Expr KR -> Expr KR -> Expr IR -> InstBuilder (Expr rate)

instance COscil KR where
  oscil amp cps ifn  = 
    kopcode "oscil" [getExpr amp, getExpr cps, getExpr ifn]

  oscil_ amp cps ifn iphs = 
    kopcode "oscil" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  oscili amp cps ifn = 
    kopcode "oscili" [getExpr amp, getExpr cps, getExpr ifn]

  oscili_ amp cps ifn iphs = 
    kopcode "oscili" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  oscil3 amp cps ifn = 
    kopcode "oscil3" [getExpr amp, getExpr cps, getExpr ifn]

  oscil3_ amp cps ifn iphs = 
    kopcode "oscil3" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  poscil amp cps ift = 
    kopcode "poscil" [getExpr amp, getExpr cps, getExpr ift]

  poscil_ amp cps ift iphs = 
    kopcode "poscil" [getExpr amp, getExpr cps, getExpr ift, getExpr iphs]

  poscil3 amp cps ift = 
    kopcode "poscil3" [getExpr amp, getExpr cps, getExpr ift]

  poscil3_ amp cps ift iphs = 
    kopcode "poscil3" [getExpr amp, getExpr cps, getExpr ift, getExpr iphs]

  lfo amp cps = 
    kopcode "lfo" [getExpr amp, getExpr cps]

  lfo_ amp cps itype = 
    kopcode "lfo" [getExpr amp, getExpr cps, getExpr itype]


instance COscil AR where
  oscil amp cps ifn = 
    aopcode "oscil" [getExpr amp, getExpr cps, getExpr ifn]

  oscil_ amp cps ifn iphs = 
    aopcode "oscil" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  oscili amp cps ifn = 
    aopcode "oscili" [getExpr amp, getExpr cps, getExpr ifn]

  oscili_ amp cps ifn iphs = 
    aopcode "oscili" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  oscil3 amp cps ifn = 
    aopcode "oscil3" [getExpr amp, getExpr cps, getExpr ifn]

  oscil3_ amp cps ifn iphs = 
    aopcode "oscil3" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  poscil amp cps ift = 
    aopcode "poscil" [getExpr amp, getExpr cps, getExpr ift]

  poscil_ amp cps ift iphs = 
    aopcode "poscil" [getExpr amp, getExpr cps, getExpr ift, getExpr iphs]

  poscil3 amp cps ift = 
    aopcode "poscil3" [getExpr amp, getExpr cps, getExpr ift]

  poscil3_ amp cps ift iphs = 
    aopcode "poscil3" [getExpr amp, getExpr cps, getExpr ift, getExpr iphs]

  lfo amp cps = 
    aopcode "lfo" [getExpr amp, getExpr cps]

  lfo_ amp cps itype = 
    aopcode "lfo" [getExpr amp, getExpr cps, getExpr itype]


--------------------------------------------------------------------------------
-- Dynamic spectrum oscillators

-- TODO - what does the x prefix convention mean for Csound?
-- Above we have interpreted it as audio rate, but is this 
-- correct?

buzz :: Expr a -> Expr a -> Expr KR -> Expr IR -> InstBuilder (Expr AR)
buzz xamp xcps knh ifn = 
    aopcode "buzz" [getExpr xamp, getExpr xcps, getExpr knh, getExpr ifn]

buzz_ :: Expr a -> Expr a -> Expr KR -> Expr IR -> Expr IR 
      -> InstBuilder (Expr AR)
buzz_ xamp xcps knh ifn iphs = 
    aopcode "buzz" [ getExpr xamp, getExpr xcps, getExpr knh
                   , getExpr ifn, getExpr iphs ]


gbuzz :: Expr a -> Expr a -> Expr KR -> Expr IR -> Expr KR -> Expr KR 
      -> InstBuilder (Expr AR)
gbuzz xamp xcps knh klh kr ifn = 
    aopcode "gbuzz" [ getExpr xamp, getExpr xcps, getExpr knh
                    , getExpr klh,  getExpr kr,   getExpr ifn ]

gbuzz_ :: Expr a -> Expr a -> Expr KR -> Expr IR 
       -> Expr KR -> Expr KR -> Expr IR
       -> InstBuilder (Expr AR)
gbuzz_ xamp xcps knh klh kr ifn iphs = 
    aopcode "gbuzz" [ getExpr xamp, getExpr xcps, getExpr knh
                    , getExpr klh,  getExpr kr,   getExpr ifn, getExpr iphs ]


vco :: Expr KR -> Expr KR -> Expr IR -> Expr KR -> Expr IR -> Expr IR 
    -> InstBuilder (Expr AR)
vco kamp kfqc iwave kpw ifn imaxd = 
    aopcode "gbuzz" [ getExpr kamp, getExpr kfqc, getExpr iwave
                    , getExpr kpw,  getExpr ifn,  getExpr imaxd ]

--------------------------------------------------------------------------------
-- Additive synthesis / resynthesis

adsyn :: Expr KR -> Expr KR -> Expr KR -> Expr IR -> InstBuilder (Expr AR)
adsyn kamod kfmod ksmod ifilcod = 
    aopcode "adsyn" [ getExpr kamod, getExpr kfmod, getExpr ksmod
                    , getExpr ifilcod ]


adsynt :: Expr KR -> Expr KR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
       -> InstBuilder (Expr AR)
adsynt kamp kcps iwfn ifreqfn iampfn icnt = 
    aopcode "adsynt" [ getExpr kamp, getExpr kcps, getExpr iwfn
                     , getExpr ifreqfn, getExpr iampfn, getExpr icnt ]

adsynt_ :: Expr KR -> Expr KR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
        -> Expr IR
        -> InstBuilder (Expr AR)
adsynt_ kamp kcps iwfn ifreqfn iampfn icnt iphs = 
    aopcode "adsynt" [ getExpr kamp, getExpr kcps, getExpr iwfn
                     , getExpr ifreqfn, getExpr iampfn, getExpr icnt
                     , getExpr iphs ]


hsboscil :: Expr KR -> Expr KR -> Expr KR -> Expr IR -> Expr IR -> Expr IR
         -> InstBuilder (Expr AR)
hsboscil kamp ktone kbrite ibasfreq iwfn ioctfn = 
    aopcode "hsboscil" [ getExpr kamp, getExpr ktone, getExpr kbrite
                       , getExpr ibasfreq, getExpr iwfn, getExpr ioctfn ]

hsboscil_ :: Expr KR -> Expr KR -> Expr KR -> Expr IR -> Expr IR -> Expr IR
          -> Expr IR
          -> InstBuilder (Expr AR)
hsboscil_ kamp ktone kbrite ibasfreq iwfn ioctfn iphs = 
    aopcode "hsboscil" [ getExpr kamp, getExpr ktone, getExpr kbrite
                       , getExpr ibasfreq, getExpr iwfn, getExpr ioctfn
                       , getExpr iphs ]

--------------------------------------------------------------------------------
-- FM Synthesis

foscil :: Expr a -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr IR
       -> InstBuilder (Expr AR)
foscil xamp kcps kcar kmod kndx ifn = 
    aopcode "foscil" [ getExpr xamp, getExpr kcps, getExpr kcar
                     , getExpr kmod, getExpr kndx, getExpr ifn ]


foscil_ :: Expr a -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr IR 
        -> Expr IR
        -> InstBuilder (Expr AR)
foscil_ xamp kcps kcar kmod kndx ifn iphs = 
    aopcode "foscil" [ getExpr xamp, getExpr kcps, getExpr kcar
                     , getExpr kmod, getExpr kndx, getExpr ifn
                     , getExpr iphs ]
  
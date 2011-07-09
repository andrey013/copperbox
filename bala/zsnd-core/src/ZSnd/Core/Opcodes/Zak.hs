{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes.Zak
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Zak opcodes.
--
--------------------------------------------------------------------------------

module ZSnd.Core.Opcodes.Zak
  (

    zakinit
  , ziw
  , zkw
  , zaw
  , zir
  , zkr
  , zar
  , zarg
  , zkmod
  , zamod
  , zkcl
  , zacl

  ) where


import ZSnd.Core.CsoundInst.Monadic
import ZSnd.Core.CsoundInst.Typed


-- Should Opcode0 builders be monadic computations instead?
-- There is no analogue to @alet@, @klet@ etc. or the need to 
-- reuse the opcode for both binding to a fresh opcode and 
-- reassigning to an existing one.
--

-- | 'zakinit' is an orchestra (not instrument) lvel statement.
--
zakinit :: Expr IInit -> Expr IInit -> BuildOrch ()
zakinit isizea isizek = writeSignalOrch $  
    Opcode0 "zakinit" [ getExprI isizea, getExprI isizek ] 

ziw :: Expr IInit -> Expr IInit -> BuildInst () 
ziw isig indx = writeSignalInst $  
    Opcode0 "ziw" [ getExprI isig, getExprI indx ] 

zkw :: Expr KRate -> Expr KRate -> BuildInst ()
zkw ksig kndx = writeSignalInst $ 
    Opcode0 "zkw" [ getExprK ksig, getExprK kndx ] 

zaw :: Expr ARate -> Expr KRate -> BuildInst ()
zaw asig kndx = writeSignalInst $  
    Opcode0 "zaw" [ getExprA asig, getExprK kndx ] 


zir :: Expr IInit -> Opcode1 IInit
zir indx = 
    Opcode1 "zir" [ getExprI indx ] 


zkr :: Expr KRate -> Opcode1 KRate
zkr kndx = 
    Opcode1 "zkr" [ getExprK kndx ] 

zar :: Expr KRate -> Opcode1 ARate
zar kndx = 
    Opcode1 "zar" [ getExprK kndx ] 

zarg :: Expr KRate -> Expr KRate -> Opcode1 ARate
zarg kndx kgain = 
    Opcode1 "zarg" [ getExprK kndx, getExprK kgain ] 


zkmod :: Expr KRate -> Expr KRate -> Opcode1 KRate
zkmod ksig kzkmod = 
    Opcode1 "zkmod" [ getExprK ksig, getExprK kzkmod ] 


zamod :: Expr ARate -> Expr KRate -> Opcode1 ARate
zamod asig kzkmod = 
    Opcode1 "zamod" [ getExprA asig, getExprK kzkmod ] 


zkcl :: Expr KRate -> Expr KRate -> BuildInst ()
zkcl kfirst klast = writeSignalInst $ 
    Opcode0 "zkcl" [ getExprK kfirst, getExprK klast ] 

zacl :: Expr KRate -> Expr KRate -> BuildInst ()
zacl kfirst klast = writeSignalInst $ 
    Opcode0 "zacl" [ getExprK kfirst, getExprK klast ] 

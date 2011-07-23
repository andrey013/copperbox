{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.Envelopes
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Envelope builders
--
--------------------------------------------------------------------------------

module Sound.FMSS.Envelopes
  (

    linsegEnvelope
  , expsegEnvelope

  ) where


import Sound.FMSS.AbstractSyntax

linsegEnvelope :: [(Double,Double)] -> (String,[SymDouble])
linsegEnvelope []            = ("linseg", [0])
linsegEnvelope (v@(_,a0):vs) = ("linseg", realToFrac a0 : work v vs)
  where
    work _      []               = [] 
    work (d0,_) (x@(d1,amp):xs)  = drn d1 d0 : realToFrac amp : work x xs


    drn d1 d0 = idur * realToFrac ((d1 - d0) / 100)



expsegEnvelope :: [(Double,Double)] -> (String,[SymDouble])
expsegEnvelope []            = ("expseg", [0.01])
expsegEnvelope (v@(_,a0):vs) = ("expseg", noZero a0 : work v vs)
  where
    work _      []               = [] 
    work (d0,_) (x@(d1,amp):xs)  = drn d1 d0 : noZero amp : work x xs

    drn d1 d0 = idur * realToFrac ((d1 - d0) / 100)

    noZero a | a == 0    = 0.01
             | otherwise = realToFrac a




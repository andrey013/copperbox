{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.RegAlloc
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Register allocation
--

module HMinCaml.RegAlloc  where

import HMinCaml.CompilerMonad
import HMinCaml.SparcAsmSyn

regAlloc :: Prog -> CM Prog
regAlloc _ = error $ "regAlloc undefined"


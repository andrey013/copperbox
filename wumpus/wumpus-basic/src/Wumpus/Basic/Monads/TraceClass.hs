{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.TraceClass
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Tracing class covers both cons and snoc tracing
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.TraceClass
  (

    TraceM(..)

 
  ) where


import Wumpus.Basic.Graphic


class TraceM  m u | m -> u where
  trace  :: Graphic u -> m ()


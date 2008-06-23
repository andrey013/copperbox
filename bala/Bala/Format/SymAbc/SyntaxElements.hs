{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymAbcPond.SyntaxElements
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A 'concrete' syntax built on the abstract syntax provided by the datatypes
--
--------------------------------------------------------------------------------

module Bala.Format.SymAbc.SyntaxElements where

import Bala.Format.Base.SymBase
import Bala.Format.SymAbc.Datatypes

--------------------------------------------------------------------------------
-- * Contexts for lists

fieldCtx :: (SymCList repr CT_Field) => repr (CList CT_Field)
fieldCtx = cNil

elementCtx :: (SymCList repr CT_Element) => repr (CList CT_Element)
elementCtx = cNil


--------------------------------------------------------------------------------

c_, d_, e_, f_, g_, a_, b_ :: (SymBaseNote repr) =>  repr (BaseNote CT_Element)
c_  = note C
d_  = note D
e_  = note E
f_  = note F
g_  = note G
a_  = note A
b_  = note B

c__, d__, e__, f__, g__, a__, b__ :: (SymBaseNote repr) =>  repr (BaseNote CT_Element)
c__  = note C2
d__  = note D2
e__  = note E2
f__  = note F2
g__  = note G2
a__  = note A2
b__  = note B2


-- rests 
z1, z2 :: (SymRest repr, SymAttrDuration repr) => repr (Rest CT_Element)
z1 = rest # dur 1
z2 = rest # dur 2


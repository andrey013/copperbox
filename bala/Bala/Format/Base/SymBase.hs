{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Base.SymBase
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations, multi-parameter typeclasses
--
-- Common functions and types for syntax trees represented in the 
-- final-tagless style.
--
--------------------------------------------------------------------------------

module Bala.Format.Base.SymBase (
  -- * Generic snoc list
  ListContext, SnocList, CSnocList(..), ( +++ ), optsnoc,

  -- * Attributes
  Attribute, CAttr(..), ( %% ), optAttr,

  -- * Prefix Attributes
  PrefixAttribute, CPrefixAttr(..), ( -%- ), optPrefixAttr,
  
  (<<),

  -- * Pretty printing
  P(..), printP
  
  
  ) where

import Bala.Base.Meter
import Text.PrettyPrint.Leijen


infixl 5 `snoc`

-- | A properly formed snoc list is heterogenous, but elements types must
-- all be instances of the same list context. 
class ListContext ctx a

-- | Represent lists as snoc lists so we can add to the right end.  
data SnocList ctx
class CSnocList repr ctx where
  snil :: repr (SnocList ctx)
  snoc :: (ListContext ctx a) 
              => repr (SnocList ctx) -> repr a ->  repr (SnocList ctx) 


infixl 5 +++

-- | (+++) - alias for snoc.
(+++) :: (ListContext ctx a, CSnocList repr ctx) 
      => repr (SnocList ctx) -> repr a ->  repr (SnocList ctx) 
(+++) es e = snoc es e

-- | @optsnoc@ - snoc a Maybe (repr elt) if it is Just elt.
-- If it is nothing return the snoc list.
optsnoc :: (ListContext ctx a, CSnocList repr ctx) 
        => repr (SnocList ctx) -> Maybe (repr a) ->  repr (SnocList ctx)
optsnoc es Nothing   = es
optsnoc es (Just e)  = snoc es e      

-- * Attributes
-- | Instances declares an attribute relation.
class Attribute elt attrib


class CAttr repr where
  attr        :: Attribute elt att => repr elt -> repr att -> repr elt


infixl 7 %%

-- | shorthand for attr
( %% ) :: (Attribute elt att, CAttr repr) => repr elt -> repr att -> repr elt
e %% a = attr e a

optAttr :: (CAttr repr, Attribute elt att) 
        => repr elt -> Maybe (repr att) -> repr elt
optAttr e Nothing   = e
optAttr e (Just a)  = e `attr` a


-- * Prefix Attributes
-- | Normally the P (pretty-print interpretation) prints all attributes as a 
-- suffix to their element. We meed a special case for prefix attributes 
-- (e.g. vertical placement in LilyPond, accidental in Abc).

class PrefixAttribute elt attrib
class CPrefixAttr repr where
  prefixAttr  :: PrefixAttribute elt att => repr att -> repr elt -> repr elt
  

infixl 7 -%-

-- | shorthand for prefixAttr
( -%- ) :: (PrefixAttribute elt att, CPrefixAttr repr) 
        => repr att -> repr elt -> repr elt
a -%- e = prefixAttr a e

optPrefixAttr :: (CPrefixAttr repr, PrefixAttribute elt att) 
        => Maybe (repr att) -> repr elt -> repr elt
optPrefixAttr Nothing  e    = e
optPrefixAttr (Just a) e    = a `prefixAttr` e


-- CAN WE DO WITHOUT << ?

infixr 6 << 

-- | Higher precedence version of the application operator ($).
(<<) ::(a -> b) -> a ->  b
f << a = f a 
  
  
--------------------------------------------------------------------------------
-- * Pretty printing

-- | To generate output we need a pretty printing interpretation.
newtype P a = P { unP :: Doc }

-- | Print a document with a unit argument. Documents taking () as an argument
-- do not have to be explicitly typed and avoid the monomorphism restriction.
printP :: (() -> P a) -> IO ()
printP e = let sdoc = renderPretty 0.8 80 (unP (e ())) in do
    putStr ((displayS sdoc []) ++ "\n")

instance CAttr P where
  attr e a              = P $ group $ unP e <> unP a 

instance CPrefixAttr P where
  prefixAttr a e        = P $ group $ unP a <> unP e 
    
  
instance Pretty MeterFraction where
  pretty mf = let (n,d) = unMeterFraction mf in 
              group $ int n <> char '/' <> int d


  

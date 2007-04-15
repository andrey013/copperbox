-- OLD - uses Language.Haskell.Syntax

module Ext.Haskell.HsCodeFragments where

import Ext.Haskell.HsSyntax

import Control.Monad
import Language.Haskell.Syntax

tuplePat :: Int -> PatS
tuplePat i = tupP (patvars i)

tupleExp :: Int -> ExpS
tupleExp i = tupE (expvars i)

constrPat :: String -> Int -> PatS
constrPat s i =  conP (mkQName s) (patvars i)

constrAppExp s 0 = conE (mkQName s)
constrAppExp s i = parenE $ apply $  (conE (mkQName s)) : (expvars i)
                        
apply [e]     = e
apply (e:es)  = appE e (apply es)                        

patvars :: Int -> [PatS]
patvars i = map (patvar) [1..i]

expvars :: Int -> [ExpS]
expvars i = map (expvar) [1..i]

patvar :: Int -> PatS
patvar x = varP (mkName $ 'x':show x)


expvar :: Int -> ExpS
expvar x = varE (mkQName $ 'x':show x)

returnS :: ExpS -> StmtS
returnS exp = noBindS (appE (varE (mkQName "return")) exp)

{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Main
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Main
--

module HMinCaml.Main where


import HMinCaml.Closure.Virtual   ( virtual )

import HMinCaml.KNormal.Alpha     ( alpha )
import HMinCaml.KNormal.Assoc     ( assoc )
import HMinCaml.KNormal.Beta      ( beta )
import HMinCaml.KNormal.ConstFold ( constFold )
import HMinCaml.KNormal.Elim      ( elim )
import HMinCaml.KNormal.Inline    ( inline )
import HMinCaml.KNormal.KNormalSyn ( Expr )
import HMinCaml.KNormal.KNormalToClosure   ( closure )

import HMinCaml.Parser    ( parseMinCaml )
import HMinCaml.Sparc.Emit      ( emit )
import HMinCaml.Sparc.RegAlloc  ( regAlloc )
import HMinCaml.Sparc.Simm13    ( simm13 )

import qualified HMinCaml.Syntax as Syntax

import HMinCaml.ToKNormal   ( knormal )

import HMinCaml.Typing    ( typing )


import Control.Monad

limit :: Int
limit = 1000

type AsmText = String


parseFile :: FilePath -> IO (Maybe Syntax.Expr)
parseFile path = do 
  ans <- parseMinCaml path
  case ans of 
    Left err -> putStrLn err >> return Nothing
    Right a -> return $ Just a

iter :: Int -> Expr ->Expr
iter 0 e = e
iter i e = let e' = (elim . constFold . inline limit . assoc . beta) e
           in if e' == e then e else iter (i-1) e'
               
               
compile :: Syntax.Expr -> AsmText
compile = emit . regAlloc   . simm13  . virtual   . closure 
               . iter limit . alpha   . knormal   . typing


                    
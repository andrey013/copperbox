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

import HMinCaml.Alpha     ( alpha )
import HMinCaml.Assoc     ( assoc )
import HMinCaml.Beta      ( beta )
import HMinCaml.Closure   ( closure )
import HMinCaml.CompilerMonad
import HMinCaml.ConstFold ( constFold )
import HMinCaml.Elim      ( elim )
import HMinCaml.Emit      ( emit )
import HMinCaml.KNormal   ( knormal, Expr )
import HMinCaml.Inline    ( inline )
import HMinCaml.Parser    ( parseMinCaml )
import HMinCaml.RegAlloc  ( regAlloc )
import HMinCaml.Simm13    ( simm13 )
import qualified HMinCaml.Syntax as Syntax
import HMinCaml.Typing    ( typing )
import HMinCaml.Virtual   ( virtual )

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

iter :: Int -> Expr -> CM Expr
iter 0 e = return e
iter i e = do e' <- (elim <=< constFold <=< inline <=< assoc <=< beta)  e
              if e' == e then return e else iter (i-1) e'
               
               
compile :: Syntax.Expr -> CM AsmText
compile e = (emit <=< regAlloc   <=< simm13 <=< virtual <=< closure 
                  <=< iter limit <=< alpha  <=< knormal <=< typing) e


                    
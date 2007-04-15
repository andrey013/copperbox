module Ext.Haskell.SplitCaseLC
  where

import Gen.LambdaCore.LambdaCoreAbsSyn

import RecLib

import Data.Maybe

-- goes from 1 to many
-- so cannot be Expr -> Expr
rewriteCase :: [Expr] -> Expr -> [Expr]
rewriteCase k a@(Let fname (Lam _ (Case v ms))) = k -- error $ "RC: " ++ show a
rewriteCase k a = (a:k)



splitCaseTrafo :: Program -> Maybe [Expr]
splitCaseTrafo = accumulate (always rewriteCase) []
  
splitCase :: Program -> Program
splitCase t = Program $ maybe [] reverse (splitCaseTrafo t)
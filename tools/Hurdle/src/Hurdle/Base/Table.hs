{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Base.Table
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Table formatting
--
--------------------------------------------------------------------------------

module Hurdle.Base.Table  where


import Text.PrettyPrint.JoinPrint    -- package: kangaroo

import Prelude hiding ( length, null )

data Align = ALeft | ACenter | ARight
  deriving (Eq,Ord,Show)

type FieldWidth = Int

type FieldProp  = (FieldWidth,Align)

-- Little trick to avoided prefixing with the sep
type Sep = Either Doc Doc

-- Essentially the 'typecase' printf...
-- Note an extra arg is threaded fr the separator



newtype Tablef t = Tablef { tablefApp :: Sep -> Doc -> t }

field :: FieldProp -> Tablef t -> Tablef (Doc -> t)
field prop k = Tablef $ \rator acc d -> case rator of
     Left  sep -> tablefApp k (Right sep) (format1 prop d)
     Right sep -> tablefApp k (Right sep) (acc <> sep <> (format1 prop d))


eot :: Tablef Doc
eot = Tablef $ \_ acc -> acc


table :: Doc -> Tablef t -> t
table sep p = (tablefApp p) (Left sep) empty


alignLeft       :: FieldWidth -> Tablef t -> Tablef (Doc -> t)
alignLeft w     = field (w,ALeft)

alignRight      :: FieldWidth -> Tablef t -> Tablef (Doc -> t)
alignRight w    = field (w,ARight)

alignCenter     :: FieldWidth -> Tablef t -> Tablef (Doc -> t)
alignCenter w   = field (w,ACenter)



format1 :: FieldProp -> Doc -> Doc 
format1 (w,align) d = step (length d) where
  step dw | dw > w    = truncateTo w align d
          | dw < w    = padTo w align d
          | otherwise = d

-- Note truncate with Center alignment truncates the right side
truncateTo :: Int -> Align -> Doc -> Doc
truncateTo w ALeft   d = truncr w d
truncateTo w _       d = truncl w d

-- truncateTo w ACenter d = let (a,b) = splitTwo w in truncl b $ truncr a d  

padTo :: Int -> Align -> Doc -> Doc
padTo w ALeft   d = padr w ' ' d
padTo w ACenter d = let (a,b) = splitTwo w in padl b ' ' $ padr a ' ' d  
padTo w ARight  d = padl w ' ' d

splitTwo :: Int -> (Int,Int)
splitTwo n | even n    = let h = n `div` 2 in (h,h)
           | otherwise = let h = n `div` 2 in (h+1,h)



{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.JoinPrint.Row
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Table formatting - TypeCase / printf style
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.JoinPrint.Row
  (
    FieldWidth
  , Rowf
  , row
  , endrow
  , alignLeft
  , alignRight
  , alignCenter

  ) where


import Text.PrettyPrint.JoinPrint.Core

import Prelude hiding ( length, null )

data Align = ALeft | ACenter | ARight
  deriving (Eq,Ord,Show)

type FieldWidth = Int

type FieldProp  = (FieldWidth,Align)

-- Little trick to avoided prefixing with the sep
type Sep = Either Doc Doc

-- Essentially the 'typecase' printf...
-- Note an extra arg is threaded fr the separator



newtype Rowf t = Rowf { rowfApp :: Sep -> Doc -> t }

field :: FieldProp -> Rowf t -> Rowf (Doc -> t)
field prop k = Rowf $ \rator acc d -> case rator of
     Left  sep -> rowfApp k (Right sep) (format1 prop d)
     Right sep -> rowfApp k (Right sep) (acc <> sep <> (format1 prop d))


endrow :: Rowf Doc
endrow = Rowf $ \_ acc -> acc


row :: Doc -> Rowf t -> t
row sep p = (rowfApp p) (Left sep) empty


alignLeft       :: FieldWidth -> Rowf t -> Rowf (Doc -> t)
alignLeft w     = field (w,ALeft)

alignRight      :: FieldWidth -> Rowf t -> Rowf (Doc -> t)
alignRight w    = field (w,ARight)

alignCenter     :: FieldWidth -> Rowf t -> Rowf (Doc -> t)
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



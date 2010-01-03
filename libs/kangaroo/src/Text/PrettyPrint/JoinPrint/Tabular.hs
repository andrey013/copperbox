{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Tabular
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Hex dumps
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.JoinPrint.Tabular
  ( 
    Align(..)
  , alignPad

  , columns2
  , columns3

  ) where


import Text.PrettyPrint.JoinPrint.Core




data Align = AlignLeft | AlignCenter | AlignRight
  deriving (Eq,Show)



alignPad :: Align -> Int -> Char -> Doc -> Doc
alignPad AlignLeft   = column_truncr_padr
alignPad AlignCenter = center_truncr
alignPad AlignRight  = column_truncr_padl


-- | A (long) zipWith and a foldr performed together.
--
longZipWith2 :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
longZipWith2 op xunit yunit = step where
    step (x:xs) (y:ys)    = (x `op` y)     : step xs ys
    step (x:xs) []        = (x `op` yunit) : step xs []
    step []     (y:ys)    = (xunit `op` y) : step [] ys
    step []     []        = []


longZipWith3 :: (a -> b -> c -> d) -> a -> b -> c -> [a] -> [b] -> [c] -> [d]
longZipWith3 op xunit yunit zunit = step where
    step (x:xs) (y:ys) (z:zs) = (op x y z) : step xs ys zs
    step (x:xs) (y:ys) []     = (op x y zunit) : step xs ys []
    step (x:xs) []     (z:zs) = (op x yunit z) : step xs [] zs
    step (x:xs) []     []     = (op x yunit zunit) : step xs [] []
    step []     (y:ys) (z:zs) = (op xunit y z) : step [] ys zs
    step []     (y:ys) []     = (op xunit y zunit) : step [] ys []
    step []     []     (z:zs) = (op xunit yunit z) : step [] [] zs
    step []     []     []     = []

-- | Build a 2 column table.
--
columns2 :: (Int,Align) -> (Int,Align) -> Doc -> [Doc] -> [Doc] -> Doc
columns2 (w1,a1) (w2,a2) sep xs ys = 
    vcat $ longZipWith2 op (spacer w1) (spacer w2) xs' ys'
  where
    op x y = x <> sep <> y
    xs'    = map (alignPad a1 w1 ' ') xs
    ys'    = map (alignPad a2 w2 ' ') ys


-- | Build a 3 column table.
--
columns3 :: (Int,Align) -> (Int,Align) -> (Int,Align)
         -> Doc
         -> [Doc] -> [Doc] -> [Doc]
         -> Doc
columns3 (w1,a1) (w2,a2) (w3,a3) sep xs ys zs =
    vcat $ longZipWith3 op (spacer w1) (spacer w2) (spacer w3) xs' ys' zs'
  where
    op x y z = x <> sep <> y <> sep <> z
    xs'      = map (alignPad a1 w1 ' ') xs
    ys'      = map (alignPad a2 w2 ' ') ys
    zs'      = map (alignPad a3 w3 ' ') zs

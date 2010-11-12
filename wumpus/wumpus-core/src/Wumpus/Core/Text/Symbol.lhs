{-# OPTIONS -Wall #-}

> ------------------------------------------------------------------------------
> -- |
> -- Module      :  Wumpus.Core.Text.Symbol
> -- Copyright   :  (c) Stephen Tetley 2010
> -- License     :  BSD3
> --
> -- Maintainer  :  stephen.tetley@gmail.com
> -- Stability   :  unstable
> -- Portability :  GHC
> --
> -- This is a stub. This module will be auto-generated soon...
> -- 
> ------------------------------------------------------------------------------

> module Wumpus.Core.Text.Symbol
>  ( 
>    symbol_encoding
>  ) where

> import Wumpus.Core.Text.Base

> import qualified Data.IntMap as IntMap


> symbol_encoding :: EncodingVector
> symbol_encoding = IntMap.fromAscList $
>     [( 0x0040, "at" )
>     ,( 0x0041, "A" )
>     ,( 0x0042, "B" )
>     ,( 0x0043, "C" )
>     ,( 0x0044, "D" )
>     ,( 0x0045, "E" )
>     ,( 0x0046, "F" )
>     ,( 0x0047, "G" )
>     ,( 0x0048, "H" )
>     ,( 0x0049, "I" )
>     ,( 0x004a, "J" )
>     ,( 0x004b, "K" )
>     ,( 0x004c, "L" )
>     ,( 0x004d, "M" )
>     ,( 0x004e, "N" )
>     ,( 0x004f, "O" )
>     ,( 0x0050, "P" )
>     ,( 0x0051, "Q" )
>     ,( 0x0052, "R" )
>     ,( 0x0053, "S" )
>     ,( 0x0054, "T" )
>     ,( 0x0055, "U" )
>     ,( 0x0056, "V" )
>     ,( 0x0057, "W" )
>     ,( 0x0058, "X" )
>     ,( 0x0059, "Y" )
>     ,( 0x005a, "Z" )
>     ]
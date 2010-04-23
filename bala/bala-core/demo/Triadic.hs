

module Triadic where

import Bala.Core.Invert
import Bala.Core.Z12


data Mode = Pve | Nve 
  deriving (Enum,Eq,Ord)

instance Show Mode where
  showsPrec _ Pve = showChar '+'
  showsPrec _ Nve = showChar '-'

-- Triad - root x mode
--
data Triad = Triad Z12 Mode
  deriving (Eq,Ord,Show)

data UTT = UTT Mode Z12 Z12 
  deriving (Eq,Ord,Show)

instance Invert UTT where
  invert (UTT Pve m n) = UTT Pve (invert m) (invert n)
  invert (UTT Nve m n) = UTT Nve (invert n) (invert m)  -- swapped (?)

demo1 = invert (UTT Nve 4 5)
demo2 = invert (UTT Nve 8 9)

moveMode :: Mode -> Triad -> Triad 
moveMode Nve (Triad r Pve) = Triad r Nve
moveMode Nve (Triad r Nve) = Triad r Pve
moveMode _   t             = t

act :: UTT -> Triad -> Triad
act (UTT Pve m _) (Triad root Pve) = Triad (m + root) Pve
act (UTT Pve _ n) (Triad root Nve) = Triad (n + root) Nve
act (UTT Nve m _) (Triad root Pve) = Triad (m + root) Nve
act (UTT Nve _ n) (Triad root Nve) = Triad (n + root) Pve

demo3 = (UTT Pve 4 3) `act` (Triad 0 Pve)
demo4 = (UTT Nve 4 3) `act` (Triad 0 Pve)
demo5 = (UTT Pve 4 3) `act` (Triad 0 Nve)
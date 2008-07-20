



module TabBase where

import Bala.Base


-- Tunings read left to right, but are numbered 6..1
data Tuning = Tuning Pitch Pitch Pitch Pitch Pitch Pitch
  deriving Show

instance Affi Tuning where 
  affi (Tuning u v w x y z) = hcatS $ map (affi . pitchLetter) [u,v,w,x,y,z]

-- | EADGBE - standard tuning
standard_tuning :: Tuning
standard_tuning = Tuning e3 a3 d4 g4 b4 e5

-- | DADGBE - drop D
drop_d :: [Pitch]
drop_d = [d3,a3,d4,g4,b4,e5]

-- | DGDGBE - drop D and G 
drop_d_and_g :: [Pitch]
drop_d_and_g = [d3,g3,d4,g4,b4,e5]


string :: Integral a => a -> Tuning -> Pitch
string 6 (Tuning u _ _ _ _ _) = u
string 5 (Tuning _ v _ _ _ _) = v
string 4 (Tuning _ _ w _ _ _) = w
string 3 (Tuning _ _ _ x _ _) = x
string 2 (Tuning _ _ _ _ y _) = y
string 1 (Tuning _ _ _ _ _ z) = z
string i _                    = error $ "Tuning has no string " ++ show i

capo :: Integral a => a -> Tuning -> Tuning
capo i (Tuning u v w x y z) = 
    let u' = capo' i u
        v' = capo' i v
        w' = capo' i w
        x' = capo' i x
        y' = capo' i y
        z' = capo' i z
    in Tuning u' v' w' x' y' z'
  where
    capo' i s = s `addSemi` (fromIntegral i)    


tabPitch :: Integral a => Tuning -> a -> a -> Pitch
tabPitch tu s i = let root = string s tu in root `addSemi` (fromIntegral i)




 
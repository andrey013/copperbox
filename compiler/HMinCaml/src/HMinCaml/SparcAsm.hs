

-- UUAGC 0.9.6 (SparcAsm.ag)


-- UUAGC 0.9.6 (SparcAsm.ag)


-- |
-- Module: HMinCaml.SparcAsm
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Sparc asm
--



module HMinCaml.SparcAsm where

import HMinCaml.Id
import qualified HMinCaml.M as M
import qualified HMinCaml.S as S
import HMinCaml.SparcAsmSyn
import HMinCaml.Type

import Data.Array.IArray




fletd :: Id -> Expr -> SparcT -> SparcT
fletd x e1 e2 = Let (x, TFloat) e1 e2

{-
-- ulet aka seq 
ulet :: Expr -> SparcT -> CM SparcT
ulet e1 e2  = (\ident -> Let (ident, TUnit) e1 e2) <$> gentmp TUnit
-}

regs :: Array Int Id
regs = listArray (0,length xs -1) xs where
    xs = ["%i2", "%i3", "%i4", "%i5",
          "%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7",
          "%o0", "%o1", "%o2", "%o3", "%o4", "%o5" ]
          
fregs :: Array Int Id
fregs = listArray (0,length xs -1) xs where
    xs = map (\i -> '%':'f': show (i * 2) ) [0..15::Int]

allregs :: [Id]                    
allregs = elems regs

allfregs :: [Id]
allfregs = elems fregs

-- Closure address
reg_cl :: Id
reg_cl = let (_,i) = bounds regs in regs!i

reg_sw :: Id
reg_sw = let (_,i) = bounds regs in regs!(i-1)

reg_swf :: Id
reg_swf = let (_,i) = bounds fregs in fregs!i

-- stack pointer
reg_sp :: Id
reg_sp = "%i0" 

-- heap pointer 
reg_hp :: Id
reg_hp = "%i1" 

--  return address
reg_ra :: Id 
reg_ra = "%o7" 

isReg :: Id -> Bool
isReg ('%':_)  = True
isReg _        = False

co_freg_table :: M.M Id Id
co_freg_table = M.addList xs M.empty where
  xs = map (\i -> ('%':'f':show (i*2), '%':'f':show (i*2+1))) [0..15::Int]  

-- "companion" freg
coFreg :: Id -> Id
coFreg freg = M.find' freg co_freg_table

removeAndUniq :: S.S Id -> [Id] -> [Id]
removeAndUniq set ls = step ls where
    step []     = []
    step (y:ys) | S.mem y set   = removeAndUniq set ys
                | otherwise     = removeAndUniq (S.add y set) ys

fv_idOrImm :: Id_or_Imm -> [Id]
fv_idOrImm (V x) = [x] 
fv_idOrImm  _    = []




sparcConcat :: SparcT -> (Id, Type) -> SparcT -> SparcT
sparcConcat (Ans expr)        xt e2 = Let xt expr e2
sparcConcat (Let yt expr e1') xt e2 = Let yt expr (sparcConcat e1' xt e2)
sparcConcat (Forget y e1')    xt e2 = Forget y (sparcConcat e1' xt e2)

align :: Int -> Int  
align i = if i `mod` 8 == 0 then i else i + 4


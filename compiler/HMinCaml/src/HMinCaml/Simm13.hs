

-- UUAGC 0.9.6 (Simm13.ag)


-- |
-- Module: HMinCaml.Simm13
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- 13-bit immediate optimisation
--


module HMinCaml.Simm13 where


import HMinCaml.Id
import qualified HMinCaml.M as M
import HMinCaml.SparcAsmSyn
import HMinCaml.Type

import Data.Bits




type Env = M.M Id Int

vfind :: Id_or_Imm -> Env -> Maybe Int
vfind (V i) env = M.find i env
vfind _     _   = Nothing

idsfind :: Id -> Id_or_Imm -> Env -> Maybe (Id,Int)
idsfind x (V y) env = maybe Nothing 
                            (either (\i -> Just (x,i)) (\i -> Just (y,i)))
                            (alt (M.find y env) (M.find x env)) 
idsfind _ _    _    = Nothing



alt :: Maybe a -> Maybe b -> Maybe (Either a b)
alt (Just a) _        = Just (Left a)
alt _        (Just b) = Just (Right b)
alt _        _        = Nothing






simm13 :: Prog -> Prog
simm13 prog = simm13_Syn_Prog synthesized
  where
    synthesized = wrap_Prog (sem_Prog prog) inherited
    inherited   = Inh_Prog { env_Inh_Prog = M.empty }

                        
-- Expr --------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : M.M Id Int
      synthesized attributes:
         copy                 : SELF 
         simm13               : Expr
   alternatives:
      alternative Add:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         visit 0:
            local copy        : _
      alternative CallCls:
         child lbl            : {Id}
         child xs             : {[Id]}
         child ys             : {[Id]}
         visit 0:
            local copy        : _
      alternative CallDir:
         child lbl            : {Label}
         child xs             : {[Id]}
         child ys             : {[Id]}
         visit 0:
            local copy        : _
      alternative Comment:
         child msg            : {String}
         visit 0:
            local copy        : _
      alternative FAddD:
         child lbl1           : {Id}
         child lbl2           : {Id}
         visit 0:
            local copy        : _
      alternative FDivD:
         child lbl1           : {Id}
         child lbl2           : {Id}
         visit 0:
            local copy        : _
      alternative FMovD:
         child lbl            : {Id}
         visit 0:
            local copy        : _
      alternative FMulD:
         child lbl1           : {Id}
         child lbl2           : {Id}
         visit 0:
            local copy        : _
      alternative FNegD:
         child lbl            : {Id}
         visit 0:
            local copy        : _
      alternative FSubD:
         child lbl1           : {Id}
         child lbl2           : {Id}
         visit 0:
            local copy        : _
      alternative IfEq:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         child ins1           : SparcT 
         child ins2           : SparcT 
         visit 0:
            local copy        : _
      alternative IfFEq:
         child lbl1           : {Id}
         child lbl2           : {Id}
         child ins1           : SparcT 
         child ins2           : SparcT 
         visit 0:
            local copy        : _
      alternative IfFLE:
         child lbl1           : {Id}
         child lbl2           : {Id}
         child ins1           : SparcT 
         child ins2           : SparcT 
         visit 0:
            local copy        : _
      alternative IfGE:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         child ins1           : SparcT 
         child ins2           : SparcT 
         visit 0:
            local copy        : _
      alternative IfLE:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         child ins1           : SparcT 
         child ins2           : SparcT 
         visit 0:
            local copy        : _
      alternative Ld:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         visit 0:
            local copy        : _
      alternative LdDF:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         visit 0:
            local copy        : _
      alternative Mov:
         child lbl            : {Id}
         visit 0:
            local copy        : _
      alternative Neg:
         child lbl            : {Id}
         visit 0:
            local copy        : _
      alternative Nop:
         visit 0:
            local copy        : _
      alternative Restore:
         child lbl            : {Id}
         visit 0:
            local copy        : _
      alternative SLL:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         visit 0:
            local copy        : _
      alternative Save:
         child lbl1           : {Id}
         child lbl2           : {Id}
         visit 0:
            local copy        : _
      alternative Set:
         child val            : {Int}
         visit 0:
            local copy        : _
      alternative SetL:
         child lbl            : {Id}
         visit 0:
            local copy        : _
      alternative St:
         child lbl1           : {Id}
         child lbl2           : {Id}
         child vlbl           : Id_or_Imm 
         visit 0:
            local copy        : _
      alternative StDF:
         child lbl1           : {Id}
         child lbl2           : {Id}
         child vlbl           : Id_or_Imm 
         visit 0:
            local copy        : _
      alternative Sub:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         visit 0:
            local copy        : _
-}
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Add _lbl _vlbl )  =
    (sem_Expr_Add _lbl (sem_Id_or_Imm _vlbl ) )
sem_Expr (CallCls _lbl _xs _ys )  =
    (sem_Expr_CallCls _lbl _xs _ys )
sem_Expr (CallDir _lbl _xs _ys )  =
    (sem_Expr_CallDir _lbl _xs _ys )
sem_Expr (Comment _msg )  =
    (sem_Expr_Comment _msg )
sem_Expr (FAddD _lbl1 _lbl2 )  =
    (sem_Expr_FAddD _lbl1 _lbl2 )
sem_Expr (FDivD _lbl1 _lbl2 )  =
    (sem_Expr_FDivD _lbl1 _lbl2 )
sem_Expr (FMovD _lbl )  =
    (sem_Expr_FMovD _lbl )
sem_Expr (FMulD _lbl1 _lbl2 )  =
    (sem_Expr_FMulD _lbl1 _lbl2 )
sem_Expr (FNegD _lbl )  =
    (sem_Expr_FNegD _lbl )
sem_Expr (FSubD _lbl1 _lbl2 )  =
    (sem_Expr_FSubD _lbl1 _lbl2 )
sem_Expr (IfEq _lbl _vlbl _ins1 _ins2 )  =
    (sem_Expr_IfEq _lbl (sem_Id_or_Imm _vlbl ) (sem_SparcT _ins1 ) (sem_SparcT _ins2 ) )
sem_Expr (IfFEq _lbl1 _lbl2 _ins1 _ins2 )  =
    (sem_Expr_IfFEq _lbl1 _lbl2 (sem_SparcT _ins1 ) (sem_SparcT _ins2 ) )
sem_Expr (IfFLE _lbl1 _lbl2 _ins1 _ins2 )  =
    (sem_Expr_IfFLE _lbl1 _lbl2 (sem_SparcT _ins1 ) (sem_SparcT _ins2 ) )
sem_Expr (IfGE _lbl _vlbl _ins1 _ins2 )  =
    (sem_Expr_IfGE _lbl (sem_Id_or_Imm _vlbl ) (sem_SparcT _ins1 ) (sem_SparcT _ins2 ) )
sem_Expr (IfLE _lbl _vlbl _ins1 _ins2 )  =
    (sem_Expr_IfLE _lbl (sem_Id_or_Imm _vlbl ) (sem_SparcT _ins1 ) (sem_SparcT _ins2 ) )
sem_Expr (Ld _lbl _vlbl )  =
    (sem_Expr_Ld _lbl (sem_Id_or_Imm _vlbl ) )
sem_Expr (LdDF _lbl _vlbl )  =
    (sem_Expr_LdDF _lbl (sem_Id_or_Imm _vlbl ) )
sem_Expr (Mov _lbl )  =
    (sem_Expr_Mov _lbl )
sem_Expr (Neg _lbl )  =
    (sem_Expr_Neg _lbl )
sem_Expr (Nop )  =
    (sem_Expr_Nop )
sem_Expr (Restore _lbl )  =
    (sem_Expr_Restore _lbl )
sem_Expr (SLL _lbl _vlbl )  =
    (sem_Expr_SLL _lbl (sem_Id_or_Imm _vlbl ) )
sem_Expr (Save _lbl1 _lbl2 )  =
    (sem_Expr_Save _lbl1 _lbl2 )
sem_Expr (Set _val )  =
    (sem_Expr_Set _val )
sem_Expr (SetL _lbl )  =
    (sem_Expr_SetL _lbl )
sem_Expr (St _lbl1 _lbl2 _vlbl )  =
    (sem_Expr_St _lbl1 _lbl2 (sem_Id_or_Imm _vlbl ) )
sem_Expr (StDF _lbl1 _lbl2 _vlbl )  =
    (sem_Expr_StDF _lbl1 _lbl2 (sem_Id_or_Imm _vlbl ) )
sem_Expr (Sub _lbl _vlbl )  =
    (sem_Expr_Sub _lbl (sem_Id_or_Imm _vlbl ) )
-- semantic domain
type T_Expr  = (M.M Id Int) ->
               ( Expr,(M.M Id Int),Expr)
data Inh_Expr  = Inh_Expr {env_Inh_Expr :: M.M Id Int}
data Syn_Expr  = Syn_Expr {copy_Syn_Expr :: Expr,env_Syn_Expr :: M.M Id Int,simm13_Syn_Expr :: Expr}
wrap_Expr :: T_Expr  ->
             Inh_Expr  ->
             Syn_Expr 
wrap_Expr sem (Inh_Expr _lhsIenv )  =
    (let ( _lhsOcopy,_lhsOenv,_lhsOsimm13) =
             (sem _lhsIenv )
     in  (Syn_Expr _lhsOcopy _lhsOenv _lhsOsimm13 ))
sem_Expr_Add :: Id ->
                T_Id_or_Imm  ->
                T_Expr 
sem_Expr_Add lbl_ vlbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _vlblIcopy :: Id_or_Imm
              -- "Simm13.ag"(line 76, column 15)
              _lhsOsimm13 =
                  maybe _copy
                        (\(x,y) -> Add x (C y))
                        (idsfind lbl_ _vlblIcopy _lhsIenv)
              -- self rule
              _copy =
                  Add lbl_ _vlblIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              ( _vlblIcopy) =
                  (vlbl_ )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_CallCls :: Id ->
                    ([Id]) ->
                    ([Id]) ->
                    T_Expr 
sem_Expr_CallCls lbl_ xs_ ys_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  CallCls lbl_ xs_ ys_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_CallDir :: Label ->
                    ([Id]) ->
                    ([Id]) ->
                    T_Expr 
sem_Expr_CallDir lbl_ xs_ ys_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  CallDir lbl_ xs_ ys_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_Comment :: String ->
                    T_Expr 
sem_Expr_Comment msg_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  Comment msg_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_FAddD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FAddD lbl1_ lbl2_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  FAddD lbl1_ lbl2_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_FDivD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FDivD lbl1_ lbl2_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  FDivD lbl1_ lbl2_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_FMovD :: Id ->
                  T_Expr 
sem_Expr_FMovD lbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  FMovD lbl_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_FMulD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FMulD lbl1_ lbl2_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  FMulD lbl1_ lbl2_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_FNegD :: Id ->
                  T_Expr 
sem_Expr_FNegD lbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  FNegD lbl_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_FSubD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FSubD lbl1_ lbl2_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  FSubD lbl1_ lbl2_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_IfEq :: Id ->
                 T_Id_or_Imm  ->
                 T_SparcT  ->
                 T_SparcT  ->
                 T_Expr 
sem_Expr_IfEq lbl_ vlbl_ ins1_ ins2_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _ins1Oenv :: (M.M Id Int)
              _ins2Oenv :: (M.M Id Int)
              _vlblIcopy :: Id_or_Imm
              _ins1Icopy :: SparcT
              _ins1Ienv :: (M.M Id Int)
              _ins1Isimm13 :: SparcT
              _ins2Icopy :: SparcT
              _ins2Ienv :: (M.M Id Int)
              _ins2Isimm13 :: SparcT
              -- "Simm13.ag"(line 104, column 15)
              _lhsOsimm13 =
                  maybe (IfEq lbl_ _vlblIcopy _ins1Isimm13 _ins2Isimm13)
                        (\(x,y) -> IfEq x (C y) _ins1Isimm13 _ins2Isimm13)
                        (idsfind lbl_ _vlblIcopy _lhsIenv)
              -- self rule
              _copy =
                  IfEq lbl_ _vlblIcopy _ins1Icopy _ins2Icopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _ins2Ienv
              -- copy rule (down)
              _ins1Oenv =
                  _lhsIenv
              -- copy rule (chain)
              _ins2Oenv =
                  _ins1Ienv
              ( _vlblIcopy) =
                  (vlbl_ )
              ( _ins1Icopy,_ins1Ienv,_ins1Isimm13) =
                  (ins1_ _ins1Oenv )
              ( _ins2Icopy,_ins2Ienv,_ins2Isimm13) =
                  (ins2_ _ins2Oenv )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_IfFEq :: Id ->
                  Id ->
                  T_SparcT  ->
                  T_SparcT  ->
                  T_Expr 
sem_Expr_IfFEq lbl1_ lbl2_ ins1_ ins2_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _ins1Oenv :: (M.M Id Int)
              _ins2Oenv :: (M.M Id Int)
              _ins1Icopy :: SparcT
              _ins1Ienv :: (M.M Id Int)
              _ins1Isimm13 :: SparcT
              _ins2Icopy :: SparcT
              _ins2Ienv :: (M.M Id Int)
              _ins2Isimm13 :: SparcT
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  IfFEq lbl1_ lbl2_ _ins1Icopy _ins2Icopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _ins2Ienv
              -- copy rule (down)
              _ins1Oenv =
                  _lhsIenv
              -- copy rule (chain)
              _ins2Oenv =
                  _ins1Ienv
              ( _ins1Icopy,_ins1Ienv,_ins1Isimm13) =
                  (ins1_ _ins1Oenv )
              ( _ins2Icopy,_ins2Ienv,_ins2Isimm13) =
                  (ins2_ _ins2Oenv )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_IfFLE :: Id ->
                  Id ->
                  T_SparcT  ->
                  T_SparcT  ->
                  T_Expr 
sem_Expr_IfFLE lbl1_ lbl2_ ins1_ ins2_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _ins1Oenv :: (M.M Id Int)
              _ins2Oenv :: (M.M Id Int)
              _ins1Icopy :: SparcT
              _ins1Ienv :: (M.M Id Int)
              _ins1Isimm13 :: SparcT
              _ins2Icopy :: SparcT
              _ins2Ienv :: (M.M Id Int)
              _ins2Isimm13 :: SparcT
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  IfFLE lbl1_ lbl2_ _ins1Icopy _ins2Icopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _ins2Ienv
              -- copy rule (down)
              _ins1Oenv =
                  _lhsIenv
              -- copy rule (chain)
              _ins2Oenv =
                  _ins1Ienv
              ( _ins1Icopy,_ins1Ienv,_ins1Isimm13) =
                  (ins1_ _ins1Oenv )
              ( _ins2Icopy,_ins2Ienv,_ins2Isimm13) =
                  (ins2_ _ins2Oenv )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_IfGE :: Id ->
                 T_Id_or_Imm  ->
                 T_SparcT  ->
                 T_SparcT  ->
                 T_Expr 
sem_Expr_IfGE lbl_ vlbl_ ins1_ ins2_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _ins1Oenv :: (M.M Id Int)
              _ins2Oenv :: (M.M Id Int)
              _vlblIcopy :: Id_or_Imm
              _ins1Icopy :: SparcT
              _ins1Ienv :: (M.M Id Int)
              _ins1Isimm13 :: SparcT
              _ins2Icopy :: SparcT
              _ins2Ienv :: (M.M Id Int)
              _ins2Isimm13 :: SparcT
              -- "Simm13.ag"(line 112, column 15)
              _lhsOsimm13 =
                  maybe (IfGE lbl_ _vlblIcopy _ins1Isimm13 _ins2Isimm13)
                        (\(x,y) -> IfGE x (C y) _ins1Isimm13 _ins2Isimm13)
                        (idsfind lbl_ _vlblIcopy _lhsIenv)
              -- self rule
              _copy =
                  IfGE lbl_ _vlblIcopy _ins1Icopy _ins2Icopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _ins2Ienv
              -- copy rule (down)
              _ins1Oenv =
                  _lhsIenv
              -- copy rule (chain)
              _ins2Oenv =
                  _ins1Ienv
              ( _vlblIcopy) =
                  (vlbl_ )
              ( _ins1Icopy,_ins1Ienv,_ins1Isimm13) =
                  (ins1_ _ins1Oenv )
              ( _ins2Icopy,_ins2Ienv,_ins2Isimm13) =
                  (ins2_ _ins2Oenv )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_IfLE :: Id ->
                 T_Id_or_Imm  ->
                 T_SparcT  ->
                 T_SparcT  ->
                 T_Expr 
sem_Expr_IfLE lbl_ vlbl_ ins1_ ins2_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _ins1Oenv :: (M.M Id Int)
              _ins2Oenv :: (M.M Id Int)
              _vlblIcopy :: Id_or_Imm
              _ins1Icopy :: SparcT
              _ins1Ienv :: (M.M Id Int)
              _ins1Isimm13 :: SparcT
              _ins2Icopy :: SparcT
              _ins2Ienv :: (M.M Id Int)
              _ins2Isimm13 :: SparcT
              -- "Simm13.ag"(line 108, column 15)
              _lhsOsimm13 =
                  maybe (IfLE lbl_ _vlblIcopy _ins1Isimm13 _ins2Isimm13)
                        (\(x,y) -> IfLE x (C y) _ins1Isimm13 _ins2Isimm13)
                        (idsfind lbl_ _vlblIcopy _lhsIenv)
              -- self rule
              _copy =
                  IfLE lbl_ _vlblIcopy _ins1Icopy _ins2Icopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _ins2Ienv
              -- copy rule (down)
              _ins1Oenv =
                  _lhsIenv
              -- copy rule (chain)
              _ins2Oenv =
                  _ins1Ienv
              ( _vlblIcopy) =
                  (vlbl_ )
              ( _ins1Icopy,_ins1Ienv,_ins1Isimm13) =
                  (ins1_ _ins1Oenv )
              ( _ins2Icopy,_ins2Ienv,_ins2Isimm13) =
                  (ins2_ _ins2Oenv )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_Ld :: Id ->
               T_Id_or_Imm  ->
               T_Expr 
sem_Expr_Ld lbl_ vlbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _vlblIcopy :: Id_or_Imm
              -- "Simm13.ag"(line 88, column 15)
              _lhsOsimm13 =
                  maybe _copy
                        (\i -> Ld lbl_ (C i))
                        (vfind _vlblIcopy _lhsIenv)
              -- self rule
              _copy =
                  Ld lbl_ _vlblIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              ( _vlblIcopy) =
                  (vlbl_ )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_LdDF :: Id ->
                 T_Id_or_Imm  ->
                 T_Expr 
sem_Expr_LdDF lbl_ vlbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _vlblIcopy :: Id_or_Imm
              -- "Simm13.ag"(line 96, column 15)
              _lhsOsimm13 =
                  maybe _copy
                        (\i -> LdDF lbl_ (C i))
                        (vfind _vlblIcopy _lhsIenv)
              -- self rule
              _copy =
                  LdDF lbl_ _vlblIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              ( _vlblIcopy) =
                  (vlbl_ )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_Mov :: Id ->
                T_Expr 
sem_Expr_Mov lbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  Mov lbl_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_Neg :: Id ->
                T_Expr 
sem_Expr_Neg lbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  Neg lbl_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_Nop :: T_Expr 
sem_Expr_Nop  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  Nop
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_Restore :: Id ->
                    T_Expr 
sem_Expr_Restore lbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  Restore lbl_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_SLL :: Id ->
                T_Id_or_Imm  ->
                T_Expr 
sem_Expr_SLL lbl_ vlbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _vlblIcopy :: Id_or_Imm
              -- "Simm13.ag"(line 84, column 15)
              _lhsOsimm13 =
                  maybe _copy
                        (\i -> SLL lbl_ (C i))
                        (vfind _vlblIcopy _lhsIenv)
              -- self rule
              _copy =
                  SLL lbl_ _vlblIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              ( _vlblIcopy) =
                  (vlbl_ )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_Save :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_Save lbl1_ lbl2_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  Save lbl1_ lbl2_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_Set :: Int ->
                T_Expr 
sem_Expr_Set val_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  Set val_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_SetL :: Id ->
                 T_Expr 
sem_Expr_SetL lbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              -- "Simm13.ag"(line 117, column 15)
              _lhsOsimm13 =
                  _copy
              -- self rule
              _copy =
                  SetL lbl_
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_St :: Id ->
               Id ->
               T_Id_or_Imm  ->
               T_Expr 
sem_Expr_St lbl1_ lbl2_ vlbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _vlblIcopy :: Id_or_Imm
              -- "Simm13.ag"(line 92, column 15)
              _lhsOsimm13 =
                  maybe _copy
                        (\i -> St lbl1_ lbl2_ (C i))
                        (vfind _vlblIcopy _lhsIenv)
              -- self rule
              _copy =
                  St lbl1_ lbl2_ _vlblIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              ( _vlblIcopy) =
                  (vlbl_ )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_StDF :: Id ->
                 Id ->
                 T_Id_or_Imm  ->
                 T_Expr 
sem_Expr_StDF lbl1_ lbl2_ vlbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _vlblIcopy :: Id_or_Imm
              -- "Simm13.ag"(line 100, column 15)
              _lhsOsimm13 =
                  maybe _copy
                        (\i -> StDF lbl1_ lbl2_ (C i))
                        (vfind _vlblIcopy _lhsIenv)
              -- self rule
              _copy =
                  StDF lbl1_ lbl2_ _vlblIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              ( _vlblIcopy) =
                  (vlbl_ )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Expr_Sub :: Id ->
                T_Id_or_Imm  ->
                T_Expr 
sem_Expr_Sub lbl_ vlbl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Expr
              _lhsOcopy :: Expr
              _lhsOenv :: (M.M Id Int)
              _vlblIcopy :: Id_or_Imm
              -- "Simm13.ag"(line 80, column 15)
              _lhsOsimm13 =
                  maybe _copy
                        (\i -> Sub lbl_ (C i))
                        (vfind _vlblIcopy _lhsIenv)
              -- self rule
              _copy =
                  Sub lbl_ _vlblIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
              ( _vlblIcopy) =
                  (vlbl_ )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
-- FloatConst --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Tuple:
         child label          : {Label}
         child value          : {Float}
         visit 0:
            local copy        : _
-}
-- cata
sem_FloatConst :: FloatConst  ->
                  T_FloatConst 
sem_FloatConst ( label,value)  =
    (sem_FloatConst_Tuple label value )
-- semantic domain
type T_FloatConst  = ( FloatConst)
data Inh_FloatConst  = Inh_FloatConst {}
data Syn_FloatConst  = Syn_FloatConst {copy_Syn_FloatConst :: FloatConst}
wrap_FloatConst :: T_FloatConst  ->
                   Inh_FloatConst  ->
                   Syn_FloatConst 
wrap_FloatConst sem (Inh_FloatConst )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_FloatConst _lhsOcopy ))
sem_FloatConst_Tuple :: Label ->
                        Float ->
                        T_FloatConst 
sem_FloatConst_Tuple label_ value_  =
    (let _lhsOcopy :: FloatConst
         -- self rule
         _copy =
             (label_,value_)
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
-- FloatConsts -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : FloatConst 
         child tl             : FloatConsts 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_FloatConsts :: FloatConsts  ->
                   T_FloatConsts 
sem_FloatConsts list  =
    (Prelude.foldr sem_FloatConsts_Cons sem_FloatConsts_Nil (Prelude.map sem_FloatConst list) )
-- semantic domain
type T_FloatConsts  = ( FloatConsts)
data Inh_FloatConsts  = Inh_FloatConsts {}
data Syn_FloatConsts  = Syn_FloatConsts {copy_Syn_FloatConsts :: FloatConsts}
wrap_FloatConsts :: T_FloatConsts  ->
                    Inh_FloatConsts  ->
                    Syn_FloatConsts 
wrap_FloatConsts sem (Inh_FloatConsts )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_FloatConsts _lhsOcopy ))
sem_FloatConsts_Cons :: T_FloatConst  ->
                        T_FloatConsts  ->
                        T_FloatConsts 
sem_FloatConsts_Cons hd_ tl_  =
    (let _lhsOcopy :: FloatConsts
         _hdIcopy :: FloatConst
         _tlIcopy :: FloatConsts
         -- self rule
         _copy =
             (:) _hdIcopy _tlIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _hdIcopy) =
             (hd_ )
         ( _tlIcopy) =
             (tl_ )
     in  ( _lhsOcopy))
sem_FloatConsts_Nil :: T_FloatConsts 
sem_FloatConsts_Nil  =
    (let _lhsOcopy :: FloatConsts
         -- self rule
         _copy =
             []
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
-- Fundef ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : M.M Id Int
      synthesized attributes:
         copy                 : SELF 
         simm13               : Fundef
   alternatives:
      alternative Fundef:
         child name           : {Label}
         child args           : {[Id]}
         child fargs          : {[Id]}
         child body           : SparcT 
         child ret            : Type 
         visit 0:
            local copy        : _
-}
-- cata
sem_Fundef :: Fundef  ->
              T_Fundef 
sem_Fundef (Fundef _name _args _fargs _body _ret )  =
    (sem_Fundef_Fundef _name _args _fargs (sem_SparcT _body ) (sem_Type _ret ) )
-- semantic domain
type T_Fundef  = (M.M Id Int) ->
                 ( Fundef,(M.M Id Int),Fundef)
data Inh_Fundef  = Inh_Fundef {env_Inh_Fundef :: M.M Id Int}
data Syn_Fundef  = Syn_Fundef {copy_Syn_Fundef :: Fundef,env_Syn_Fundef :: M.M Id Int,simm13_Syn_Fundef :: Fundef}
wrap_Fundef :: T_Fundef  ->
               Inh_Fundef  ->
               Syn_Fundef 
wrap_Fundef sem (Inh_Fundef _lhsIenv )  =
    (let ( _lhsOcopy,_lhsOenv,_lhsOsimm13) =
             (sem _lhsIenv )
     in  (Syn_Fundef _lhsOcopy _lhsOenv _lhsOsimm13 ))
sem_Fundef_Fundef :: Label ->
                     ([Id]) ->
                     ([Id]) ->
                     T_SparcT  ->
                     T_Type  ->
                     T_Fundef 
sem_Fundef_Fundef name_ args_ fargs_ body_ ret_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Fundef
              _lhsOcopy :: Fundef
              _lhsOenv :: (M.M Id Int)
              _bodyOenv :: (M.M Id Int)
              _bodyIcopy :: SparcT
              _bodyIenv :: (M.M Id Int)
              _bodyIsimm13 :: SparcT
              _retIcopy :: Type
              -- "Simm13.ag"(line 121, column 15)
              _lhsOsimm13 =
                  Fundef name_ args_ fargs_ _bodyIsimm13 _retIcopy
              -- self rule
              _copy =
                  Fundef name_ args_ fargs_ _bodyIcopy _retIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _bodyIenv
              -- copy rule (down)
              _bodyOenv =
                  _lhsIenv
              ( _bodyIcopy,_bodyIenv,_bodyIsimm13) =
                  (body_ _bodyOenv )
              ( _retIcopy) =
                  (ret_ )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
-- Fundefs -----------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : M.M Id Int
      synthesized attributes:
         copy                 : SELF 
         simm13               : [Fundef]
   alternatives:
      alternative Cons:
         child hd             : Fundef 
         child tl             : Fundefs 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Fundefs :: Fundefs  ->
               T_Fundefs 
sem_Fundefs list  =
    (Prelude.foldr sem_Fundefs_Cons sem_Fundefs_Nil (Prelude.map sem_Fundef list) )
-- semantic domain
type T_Fundefs  = (M.M Id Int) ->
                  ( Fundefs,(M.M Id Int),([Fundef]))
data Inh_Fundefs  = Inh_Fundefs {env_Inh_Fundefs :: M.M Id Int}
data Syn_Fundefs  = Syn_Fundefs {copy_Syn_Fundefs :: Fundefs,env_Syn_Fundefs :: M.M Id Int,simm13_Syn_Fundefs :: [Fundef]}
wrap_Fundefs :: T_Fundefs  ->
                Inh_Fundefs  ->
                Syn_Fundefs 
wrap_Fundefs sem (Inh_Fundefs _lhsIenv )  =
    (let ( _lhsOcopy,_lhsOenv,_lhsOsimm13) =
             (sem _lhsIenv )
     in  (Syn_Fundefs _lhsOcopy _lhsOenv _lhsOsimm13 ))
sem_Fundefs_Cons :: T_Fundef  ->
                    T_Fundefs  ->
                    T_Fundefs 
sem_Fundefs_Cons hd_ tl_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: ([Fundef])
              _lhsOcopy :: Fundefs
              _lhsOenv :: (M.M Id Int)
              _hdOenv :: (M.M Id Int)
              _tlOenv :: (M.M Id Int)
              _hdIcopy :: Fundef
              _hdIenv :: (M.M Id Int)
              _hdIsimm13 :: Fundef
              _tlIcopy :: Fundefs
              _tlIenv :: (M.M Id Int)
              _tlIsimm13 :: ([Fundef])
              -- use rule "Simm13.ag"(line 59, column 29)
              _lhsOsimm13 =
                  _hdIsimm13 : _tlIsimm13
              -- self rule
              _copy =
                  (:) _hdIcopy _tlIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _tlIenv
              -- copy rule (down)
              _hdOenv =
                  _lhsIenv
              -- copy rule (chain)
              _tlOenv =
                  _hdIenv
              ( _hdIcopy,_hdIenv,_hdIsimm13) =
                  (hd_ _hdOenv )
              ( _tlIcopy,_tlIenv,_tlIsimm13) =
                  (tl_ _tlOenv )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_Fundefs_Nil :: T_Fundefs 
sem_Fundefs_Nil  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: ([Fundef])
              _lhsOcopy :: Fundefs
              _lhsOenv :: (M.M Id Int)
              -- use rule "Simm13.ag"(line 59, column 29)
              _lhsOsimm13 =
                  []
              -- self rule
              _copy =
                  []
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (chain)
              _lhsOenv =
                  _lhsIenv
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
-- Id_or_Imm ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative C:
         child value          : {Int}
         visit 0:
            local copy        : _
      alternative V:
         child ident          : {Id}
         visit 0:
            local copy        : _
-}
-- cata
sem_Id_or_Imm :: Id_or_Imm  ->
                 T_Id_or_Imm 
sem_Id_or_Imm (C _value )  =
    (sem_Id_or_Imm_C _value )
sem_Id_or_Imm (V _ident )  =
    (sem_Id_or_Imm_V _ident )
-- semantic domain
type T_Id_or_Imm  = ( Id_or_Imm)
data Inh_Id_or_Imm  = Inh_Id_or_Imm {}
data Syn_Id_or_Imm  = Syn_Id_or_Imm {copy_Syn_Id_or_Imm :: Id_or_Imm}
wrap_Id_or_Imm :: T_Id_or_Imm  ->
                  Inh_Id_or_Imm  ->
                  Syn_Id_or_Imm 
wrap_Id_or_Imm sem (Inh_Id_or_Imm )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_Id_or_Imm _lhsOcopy ))
sem_Id_or_Imm_C :: Int ->
                   T_Id_or_Imm 
sem_Id_or_Imm_C value_  =
    (let _lhsOcopy :: Id_or_Imm
         -- self rule
         _copy =
             C value_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Id_or_Imm_V :: Id ->
                   T_Id_or_Imm 
sem_Id_or_Imm_V ident_  =
    (let _lhsOcopy :: Id_or_Imm
         -- self rule
         _copy =
             V ident_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
-- LabeledType -------------------------------------------------
{-
   alternatives:
      alternative Tuple:
         child argId          : {Label}
         child argType        : Type 
-}
-- cata
sem_LabeledType :: LabeledType  ->
                   T_LabeledType 
sem_LabeledType ( argId,argType)  =
    (sem_LabeledType_Tuple argId (sem_Type argType ) )
-- semantic domain
type T_LabeledType  = ( )
data Inh_LabeledType  = Inh_LabeledType {}
data Syn_LabeledType  = Syn_LabeledType {}
wrap_LabeledType :: T_LabeledType  ->
                    Inh_LabeledType  ->
                    Syn_LabeledType 
wrap_LabeledType sem (Inh_LabeledType )  =
    (let ( ) =
             (sem )
     in  (Syn_LabeledType ))
sem_LabeledType_Tuple :: Label ->
                         T_Type  ->
                         T_LabeledType 
sem_LabeledType_Tuple argId_ argType_  =
    (let _argTypeIcopy :: Type
         ( _argTypeIcopy) =
             (argType_ )
     in  ( ))
-- OptType -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Just:
         child just           : Type 
         visit 0:
            local copy        : _
      alternative Nothing:
         visit 0:
            local copy        : _
-}
-- cata
sem_OptType :: OptType  ->
               T_OptType 
sem_OptType (Prelude.Just x )  =
    (sem_OptType_Just (sem_Type x ) )
sem_OptType Prelude.Nothing  =
    sem_OptType_Nothing
-- semantic domain
type T_OptType  = ( OptType)
data Inh_OptType  = Inh_OptType {}
data Syn_OptType  = Syn_OptType {copy_Syn_OptType :: OptType}
wrap_OptType :: T_OptType  ->
                Inh_OptType  ->
                Syn_OptType 
wrap_OptType sem (Inh_OptType )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_OptType _lhsOcopy ))
sem_OptType_Just :: T_Type  ->
                    T_OptType 
sem_OptType_Just just_  =
    (let _lhsOcopy :: OptType
         _justIcopy :: Type
         -- self rule
         _copy =
             Just _justIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _justIcopy) =
             (just_ )
     in  ( _lhsOcopy))
sem_OptType_Nothing :: T_OptType 
sem_OptType_Nothing  =
    (let _lhsOcopy :: OptType
         -- self rule
         _copy =
             Nothing
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
-- Prog --------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : M.M Id Int
      synthesized attribute:
         simm13               : Prog
   alternatives:
      alternative Prog:
         child floats         : FloatConsts 
         child fundefs        : Fundefs 
         child instr          : SparcT 
-}
-- cata
sem_Prog :: Prog  ->
            T_Prog 
sem_Prog (Prog _floats _fundefs _instr )  =
    (sem_Prog_Prog (sem_FloatConsts _floats ) (sem_Fundefs _fundefs ) (sem_SparcT _instr ) )
-- semantic domain
type T_Prog  = (M.M Id Int) ->
               ( (M.M Id Int),Prog)
data Inh_Prog  = Inh_Prog {env_Inh_Prog :: M.M Id Int}
data Syn_Prog  = Syn_Prog {env_Syn_Prog :: M.M Id Int,simm13_Syn_Prog :: Prog}
wrap_Prog :: T_Prog  ->
             Inh_Prog  ->
             Syn_Prog 
wrap_Prog sem (Inh_Prog _lhsIenv )  =
    (let ( _lhsOenv,_lhsOsimm13) =
             (sem _lhsIenv )
     in  (Syn_Prog _lhsOenv _lhsOsimm13 ))
sem_Prog_Prog :: T_FloatConsts  ->
                 T_Fundefs  ->
                 T_SparcT  ->
                 T_Prog 
sem_Prog_Prog floats_ fundefs_ instr_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: Prog
              _lhsOenv :: (M.M Id Int)
              _fundefsOenv :: (M.M Id Int)
              _instrOenv :: (M.M Id Int)
              _floatsIcopy :: FloatConsts
              _fundefsIcopy :: Fundefs
              _fundefsIenv :: (M.M Id Int)
              _fundefsIsimm13 :: ([Fundef])
              _instrIcopy :: SparcT
              _instrIenv :: (M.M Id Int)
              _instrIsimm13 :: SparcT
              -- "Simm13.ag"(line 66, column 15)
              _lhsOsimm13 =
                  Prog _floatsIcopy _fundefsIsimm13 _instrIsimm13
              -- copy rule (up)
              _lhsOenv =
                  _instrIenv
              -- copy rule (down)
              _fundefsOenv =
                  _lhsIenv
              -- copy rule (chain)
              _instrOenv =
                  _fundefsIenv
              ( _floatsIcopy) =
                  (floats_ )
              ( _fundefsIcopy,_fundefsIenv,_fundefsIsimm13) =
                  (fundefs_ _fundefsOenv )
              ( _instrIcopy,_instrIenv,_instrIsimm13) =
                  (instr_ _instrOenv )
          in  ( _lhsOenv,_lhsOsimm13)))
-- SparcT ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         env                  : M.M Id Int
      synthesized attributes:
         copy                 : SELF 
         simm13               : SparcT
   alternatives:
      alternative Ans:
         child expr           : Expr 
         visit 0:
            local copy        : _
      alternative Forget:
         child ident          : {Id}
         child body           : SparcT 
         visit 0:
            local copy        : _
      alternative Let:
         child tyid           : TypeId 
         child expr           : Expr 
         child body           : SparcT 
         visit 0:
            local copy        : _
-}
-- cata
sem_SparcT :: SparcT  ->
              T_SparcT 
sem_SparcT (Ans _expr )  =
    (sem_SparcT_Ans (sem_Expr _expr ) )
sem_SparcT (Forget _ident _body )  =
    (sem_SparcT_Forget _ident (sem_SparcT _body ) )
sem_SparcT (Let _tyid _expr _body )  =
    (sem_SparcT_Let (sem_TypeId _tyid ) (sem_Expr _expr ) (sem_SparcT _body ) )
-- semantic domain
type T_SparcT  = (M.M Id Int) ->
                 ( SparcT,(M.M Id Int),SparcT)
data Inh_SparcT  = Inh_SparcT {env_Inh_SparcT :: M.M Id Int}
data Syn_SparcT  = Syn_SparcT {copy_Syn_SparcT :: SparcT,env_Syn_SparcT :: M.M Id Int,simm13_Syn_SparcT :: SparcT}
wrap_SparcT :: T_SparcT  ->
               Inh_SparcT  ->
               Syn_SparcT 
wrap_SparcT sem (Inh_SparcT _lhsIenv )  =
    (let ( _lhsOcopy,_lhsOenv,_lhsOsimm13) =
             (sem _lhsIenv )
     in  (Syn_SparcT _lhsOcopy _lhsOenv _lhsOsimm13 ))
sem_SparcT_Ans :: T_Expr  ->
                  T_SparcT 
sem_SparcT_Ans expr_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: SparcT
              _lhsOcopy :: SparcT
              _lhsOenv :: (M.M Id Int)
              _exprOenv :: (M.M Id Int)
              _exprIcopy :: Expr
              _exprIenv :: (M.M Id Int)
              _exprIsimm13 :: Expr
              -- "Simm13.ag"(line 69, column 15)
              _lhsOsimm13 =
                  Ans _exprIsimm13
              -- self rule
              _copy =
                  Ans _exprIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _exprIenv
              -- copy rule (down)
              _exprOenv =
                  _lhsIenv
              ( _exprIcopy,_exprIenv,_exprIsimm13) =
                  (expr_ _exprOenv )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_SparcT_Forget :: Id ->
                     T_SparcT  ->
                     T_SparcT 
sem_SparcT_Forget ident_ body_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: SparcT
              _lhsOcopy :: SparcT
              _lhsOenv :: (M.M Id Int)
              _bodyOenv :: (M.M Id Int)
              _bodyIcopy :: SparcT
              _bodyIenv :: (M.M Id Int)
              _bodyIsimm13 :: SparcT
              -- "Simm13.ag"(line 73, column 15)
              _lhsOsimm13 =
                  Forget ident_ _bodyIsimm13
              -- self rule
              _copy =
                  Forget ident_ _bodyIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _bodyIenv
              -- copy rule (down)
              _bodyOenv =
                  _lhsIenv
              ( _bodyIcopy,_bodyIenv,_bodyIsimm13) =
                  (body_ _bodyOenv )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
sem_SparcT_Let :: T_TypeId  ->
                  T_Expr  ->
                  T_SparcT  ->
                  T_SparcT 
sem_SparcT_Let tyid_ expr_ body_  =
    (\ _lhsIenv ->
         (let _lhsOsimm13 :: SparcT
              _lhsOcopy :: SparcT
              _lhsOenv :: (M.M Id Int)
              _exprOenv :: (M.M Id Int)
              _bodyOenv :: (M.M Id Int)
              _tyidIargId :: Id
              _tyidIcopy :: TypeId
              _exprIcopy :: Expr
              _exprIenv :: (M.M Id Int)
              _exprIsimm13 :: Expr
              _bodyIcopy :: SparcT
              _bodyIenv :: (M.M Id Int)
              _bodyIsimm13 :: SparcT
              -- "Simm13.ag"(line 71, column 15)
              _lhsOsimm13 =
                  undefined
              -- self rule
              _copy =
                  Let _tyidIcopy _exprIcopy _bodyIcopy
              -- self rule
              _lhsOcopy =
                  _copy
              -- copy rule (up)
              _lhsOenv =
                  _bodyIenv
              -- copy rule (down)
              _exprOenv =
                  _lhsIenv
              -- copy rule (chain)
              _bodyOenv =
                  _exprIenv
              ( _tyidIargId,_tyidIcopy) =
                  (tyid_ )
              ( _exprIcopy,_exprIenv,_exprIsimm13) =
                  (expr_ _exprOenv )
              ( _bodyIcopy,_bodyIenv,_bodyIsimm13) =
                  (body_ _bodyOenv )
          in  ( _lhsOcopy,_lhsOenv,_lhsOsimm13)))
-- Type --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative TArray:
         child ty             : Type 
         visit 0:
            local copy        : _
      alternative TBool:
         visit 0:
            local copy        : _
      alternative TFloat:
         visit 0:
            local copy        : _
      alternative TFun:
         child args           : Types 
         child retTy          : Type 
         visit 0:
            local copy        : _
      alternative TInt:
         visit 0:
            local copy        : _
      alternative TTuple:
         child tys            : Types 
         visit 0:
            local copy        : _
      alternative TUnit:
         visit 0:
            local copy        : _
      alternative TVar:
         child optTy          : OptType 
         visit 0:
            local copy        : _
-}
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type (TArray _ty )  =
    (sem_Type_TArray (sem_Type _ty ) )
sem_Type (TBool )  =
    (sem_Type_TBool )
sem_Type (TFloat )  =
    (sem_Type_TFloat )
sem_Type (TFun _args _retTy )  =
    (sem_Type_TFun (sem_Types _args ) (sem_Type _retTy ) )
sem_Type (TInt )  =
    (sem_Type_TInt )
sem_Type (TTuple _tys )  =
    (sem_Type_TTuple (sem_Types _tys ) )
sem_Type (TUnit )  =
    (sem_Type_TUnit )
sem_Type (TVar _optTy )  =
    (sem_Type_TVar (sem_OptType _optTy ) )
-- semantic domain
type T_Type  = ( Type)
data Inh_Type  = Inh_Type {}
data Syn_Type  = Syn_Type {copy_Syn_Type :: Type}
wrap_Type :: T_Type  ->
             Inh_Type  ->
             Syn_Type 
wrap_Type sem (Inh_Type )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_Type _lhsOcopy ))
sem_Type_TArray :: T_Type  ->
                   T_Type 
sem_Type_TArray ty_  =
    (let _lhsOcopy :: Type
         _tyIcopy :: Type
         -- self rule
         _copy =
             TArray _tyIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _tyIcopy) =
             (ty_ )
     in  ( _lhsOcopy))
sem_Type_TBool :: T_Type 
sem_Type_TBool  =
    (let _lhsOcopy :: Type
         -- self rule
         _copy =
             TBool
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Type_TFloat :: T_Type 
sem_Type_TFloat  =
    (let _lhsOcopy :: Type
         -- self rule
         _copy =
             TFloat
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Type_TFun :: T_Types  ->
                 T_Type  ->
                 T_Type 
sem_Type_TFun args_ retTy_  =
    (let _lhsOcopy :: Type
         _argsIcopy :: Types
         _retTyIcopy :: Type
         -- self rule
         _copy =
             TFun _argsIcopy _retTyIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _argsIcopy) =
             (args_ )
         ( _retTyIcopy) =
             (retTy_ )
     in  ( _lhsOcopy))
sem_Type_TInt :: T_Type 
sem_Type_TInt  =
    (let _lhsOcopy :: Type
         -- self rule
         _copy =
             TInt
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Type_TTuple :: T_Types  ->
                   T_Type 
sem_Type_TTuple tys_  =
    (let _lhsOcopy :: Type
         _tysIcopy :: Types
         -- self rule
         _copy =
             TTuple _tysIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _tysIcopy) =
             (tys_ )
     in  ( _lhsOcopy))
sem_Type_TUnit :: T_Type 
sem_Type_TUnit  =
    (let _lhsOcopy :: Type
         -- self rule
         _copy =
             TUnit
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Type_TVar :: T_OptType  ->
                 T_Type 
sem_Type_TVar optTy_  =
    (let _lhsOcopy :: Type
         _optTyIcopy :: OptType
         -- self rule
         _copy =
             TVar _optTyIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _optTyIcopy) =
             (optTy_ )
     in  ( _lhsOcopy))
-- TypeId ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         argId                : Id
         copy                 : SELF 
   alternatives:
      alternative Tuple:
         child argId          : {Id}
         child argType        : Type 
         visit 0:
            local copy        : _
-}
-- cata
sem_TypeId :: TypeId  ->
              T_TypeId 
sem_TypeId ( argId,argType)  =
    (sem_TypeId_Tuple argId (sem_Type argType ) )
-- semantic domain
type T_TypeId  = ( Id,TypeId)
data Inh_TypeId  = Inh_TypeId {}
data Syn_TypeId  = Syn_TypeId {argId_Syn_TypeId :: Id,copy_Syn_TypeId :: TypeId}
wrap_TypeId :: T_TypeId  ->
               Inh_TypeId  ->
               Syn_TypeId 
wrap_TypeId sem (Inh_TypeId )  =
    (let ( _lhsOargId,_lhsOcopy) =
             (sem )
     in  (Syn_TypeId _lhsOargId _lhsOcopy ))
sem_TypeId_Tuple :: Id ->
                    T_Type  ->
                    T_TypeId 
sem_TypeId_Tuple argId_ argType_  =
    (let _lhsOargId :: Id
         _lhsOcopy :: TypeId
         _argTypeIcopy :: Type
         -- "./TypeDEFS.ag"(line 32, column 15)
         _lhsOargId =
             argId_
         -- self rule
         _copy =
             (argId_,_argTypeIcopy)
         -- self rule
         _lhsOcopy =
             _copy
         ( _argTypeIcopy) =
             (argType_ )
     in  ( _lhsOargId,_lhsOcopy))
-- TypeIds -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         argId                : [Id]
         copy                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : TypeId 
         child tl             : TypeIds 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_TypeIds :: TypeIds  ->
               T_TypeIds 
sem_TypeIds list  =
    (Prelude.foldr sem_TypeIds_Cons sem_TypeIds_Nil (Prelude.map sem_TypeId list) )
-- semantic domain
type T_TypeIds  = ( ([Id]),TypeIds)
data Inh_TypeIds  = Inh_TypeIds {}
data Syn_TypeIds  = Syn_TypeIds {argId_Syn_TypeIds :: [Id],copy_Syn_TypeIds :: TypeIds}
wrap_TypeIds :: T_TypeIds  ->
                Inh_TypeIds  ->
                Syn_TypeIds 
wrap_TypeIds sem (Inh_TypeIds )  =
    (let ( _lhsOargId,_lhsOcopy) =
             (sem )
     in  (Syn_TypeIds _lhsOargId _lhsOcopy ))
sem_TypeIds_Cons :: T_TypeId  ->
                    T_TypeIds  ->
                    T_TypeIds 
sem_TypeIds_Cons hd_ tl_  =
    (let _lhsOargId :: ([Id])
         _lhsOcopy :: TypeIds
         _hdIargId :: Id
         _hdIcopy :: TypeId
         _tlIargId :: ([Id])
         _tlIcopy :: TypeIds
         -- use rule "./TypeDEFS.ag"(line 28, column 32)
         _lhsOargId =
             _hdIargId : _tlIargId
         -- self rule
         _copy =
             (:) _hdIcopy _tlIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _hdIargId,_hdIcopy) =
             (hd_ )
         ( _tlIargId,_tlIcopy) =
             (tl_ )
     in  ( _lhsOargId,_lhsOcopy))
sem_TypeIds_Nil :: T_TypeIds 
sem_TypeIds_Nil  =
    (let _lhsOargId :: ([Id])
         _lhsOcopy :: TypeIds
         -- use rule "./TypeDEFS.ag"(line 28, column 32)
         _lhsOargId =
             []
         -- self rule
         _copy =
             []
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOargId,_lhsOcopy))
-- Types -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Type 
         child tl             : Types 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Types :: Types  ->
             T_Types 
sem_Types list  =
    (Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list) )
-- semantic domain
type T_Types  = ( Types)
data Inh_Types  = Inh_Types {}
data Syn_Types  = Syn_Types {copy_Syn_Types :: Types}
wrap_Types :: T_Types  ->
              Inh_Types  ->
              Syn_Types 
wrap_Types sem (Inh_Types )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_Types _lhsOcopy ))
sem_Types_Cons :: T_Type  ->
                  T_Types  ->
                  T_Types 
sem_Types_Cons hd_ tl_  =
    (let _lhsOcopy :: Types
         _hdIcopy :: Type
         _tlIcopy :: Types
         -- self rule
         _copy =
             (:) _hdIcopy _tlIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _hdIcopy) =
             (hd_ )
         ( _tlIcopy) =
             (tl_ )
     in  ( _lhsOcopy))
sem_Types_Nil :: T_Types 
sem_Types_Nil  =
    (let _lhsOcopy :: Types
         -- self rule
         _copy =
             []
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))


-- UUAGC 0.9.6 (Emit.ag)


-- |
-- Module: HMinCaml.Emit
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Emit Sparc Asm
--


module HMinCaml.Emit where

import HMinCaml.CompilerMonad
import HMinCaml.Float
import HMinCaml.Id
import HMinCaml.S
import HMinCaml.SparcAsmSyn
import HMinCaml.Type


import Control.Monad.State
import Control.Monad.Writer
import Text.PrettyPrint.Leijen

data Dest = Tail | NonTail Id

dest :: b -> (Id -> b) -> Dest -> b
dest tk _   Tail        = tk
dest _  ntk (NonTail l) = ntk l

tab :: String -> Doc
tab s = indent 4 (text s) 




emit :: Prog -> CM String 
emit _ = error $ "emit undefined" 

-- Expr --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
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
type T_Expr  = ( Expr)
data Inh_Expr  = Inh_Expr {}
data Syn_Expr  = Syn_Expr {copy_Syn_Expr :: Expr}
wrap_Expr :: T_Expr  ->
             Inh_Expr  ->
             Syn_Expr 
wrap_Expr sem (Inh_Expr )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_Expr _lhsOcopy ))
sem_Expr_Add :: Id ->
                T_Id_or_Imm  ->
                T_Expr 
sem_Expr_Add lbl_ vlbl_  =
    (let _lhsOcopy :: Expr
         _vlblIcopy :: Id_or_Imm
         -- self rule
         _copy =
             Add lbl_ _vlblIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _vlblIcopy) =
             (vlbl_ )
     in  ( _lhsOcopy))
sem_Expr_CallCls :: Id ->
                    ([Id]) ->
                    ([Id]) ->
                    T_Expr 
sem_Expr_CallCls lbl_ xs_ ys_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             CallCls lbl_ xs_ ys_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_CallDir :: Label ->
                    ([Id]) ->
                    ([Id]) ->
                    T_Expr 
sem_Expr_CallDir lbl_ xs_ ys_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             CallDir lbl_ xs_ ys_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_Comment :: String ->
                    T_Expr 
sem_Expr_Comment msg_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             Comment msg_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_FAddD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FAddD lbl1_ lbl2_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             FAddD lbl1_ lbl2_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_FDivD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FDivD lbl1_ lbl2_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             FDivD lbl1_ lbl2_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_FMovD :: Id ->
                  T_Expr 
sem_Expr_FMovD lbl_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             FMovD lbl_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_FMulD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FMulD lbl1_ lbl2_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             FMulD lbl1_ lbl2_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_FNegD :: Id ->
                  T_Expr 
sem_Expr_FNegD lbl_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             FNegD lbl_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_FSubD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FSubD lbl1_ lbl2_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             FSubD lbl1_ lbl2_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_IfEq :: Id ->
                 T_Id_or_Imm  ->
                 T_SparcT  ->
                 T_SparcT  ->
                 T_Expr 
sem_Expr_IfEq lbl_ vlbl_ ins1_ ins2_  =
    (let _lhsOcopy :: Expr
         _vlblIcopy :: Id_or_Imm
         _ins1Icopy :: SparcT
         _ins2Icopy :: SparcT
         -- self rule
         _copy =
             IfEq lbl_ _vlblIcopy _ins1Icopy _ins2Icopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _vlblIcopy) =
             (vlbl_ )
         ( _ins1Icopy) =
             (ins1_ )
         ( _ins2Icopy) =
             (ins2_ )
     in  ( _lhsOcopy))
sem_Expr_IfFEq :: Id ->
                  Id ->
                  T_SparcT  ->
                  T_SparcT  ->
                  T_Expr 
sem_Expr_IfFEq lbl1_ lbl2_ ins1_ ins2_  =
    (let _lhsOcopy :: Expr
         _ins1Icopy :: SparcT
         _ins2Icopy :: SparcT
         -- self rule
         _copy =
             IfFEq lbl1_ lbl2_ _ins1Icopy _ins2Icopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _ins1Icopy) =
             (ins1_ )
         ( _ins2Icopy) =
             (ins2_ )
     in  ( _lhsOcopy))
sem_Expr_IfFLE :: Id ->
                  Id ->
                  T_SparcT  ->
                  T_SparcT  ->
                  T_Expr 
sem_Expr_IfFLE lbl1_ lbl2_ ins1_ ins2_  =
    (let _lhsOcopy :: Expr
         _ins1Icopy :: SparcT
         _ins2Icopy :: SparcT
         -- self rule
         _copy =
             IfFLE lbl1_ lbl2_ _ins1Icopy _ins2Icopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _ins1Icopy) =
             (ins1_ )
         ( _ins2Icopy) =
             (ins2_ )
     in  ( _lhsOcopy))
sem_Expr_IfGE :: Id ->
                 T_Id_or_Imm  ->
                 T_SparcT  ->
                 T_SparcT  ->
                 T_Expr 
sem_Expr_IfGE lbl_ vlbl_ ins1_ ins2_  =
    (let _lhsOcopy :: Expr
         _vlblIcopy :: Id_or_Imm
         _ins1Icopy :: SparcT
         _ins2Icopy :: SparcT
         -- self rule
         _copy =
             IfGE lbl_ _vlblIcopy _ins1Icopy _ins2Icopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _vlblIcopy) =
             (vlbl_ )
         ( _ins1Icopy) =
             (ins1_ )
         ( _ins2Icopy) =
             (ins2_ )
     in  ( _lhsOcopy))
sem_Expr_IfLE :: Id ->
                 T_Id_or_Imm  ->
                 T_SparcT  ->
                 T_SparcT  ->
                 T_Expr 
sem_Expr_IfLE lbl_ vlbl_ ins1_ ins2_  =
    (let _lhsOcopy :: Expr
         _vlblIcopy :: Id_or_Imm
         _ins1Icopy :: SparcT
         _ins2Icopy :: SparcT
         -- self rule
         _copy =
             IfLE lbl_ _vlblIcopy _ins1Icopy _ins2Icopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _vlblIcopy) =
             (vlbl_ )
         ( _ins1Icopy) =
             (ins1_ )
         ( _ins2Icopy) =
             (ins2_ )
     in  ( _lhsOcopy))
sem_Expr_Ld :: Id ->
               T_Id_or_Imm  ->
               T_Expr 
sem_Expr_Ld lbl_ vlbl_  =
    (let _lhsOcopy :: Expr
         _vlblIcopy :: Id_or_Imm
         -- self rule
         _copy =
             Ld lbl_ _vlblIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _vlblIcopy) =
             (vlbl_ )
     in  ( _lhsOcopy))
sem_Expr_LdDF :: Id ->
                 T_Id_or_Imm  ->
                 T_Expr 
sem_Expr_LdDF lbl_ vlbl_  =
    (let _lhsOcopy :: Expr
         _vlblIcopy :: Id_or_Imm
         -- self rule
         _copy =
             LdDF lbl_ _vlblIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _vlblIcopy) =
             (vlbl_ )
     in  ( _lhsOcopy))
sem_Expr_Mov :: Id ->
                T_Expr 
sem_Expr_Mov lbl_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             Mov lbl_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_Neg :: Id ->
                T_Expr 
sem_Expr_Neg lbl_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             Neg lbl_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_Nop :: T_Expr 
sem_Expr_Nop  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             Nop
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_Restore :: Id ->
                    T_Expr 
sem_Expr_Restore lbl_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             Restore lbl_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_SLL :: Id ->
                T_Id_or_Imm  ->
                T_Expr 
sem_Expr_SLL lbl_ vlbl_  =
    (let _lhsOcopy :: Expr
         _vlblIcopy :: Id_or_Imm
         -- self rule
         _copy =
             SLL lbl_ _vlblIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _vlblIcopy) =
             (vlbl_ )
     in  ( _lhsOcopy))
sem_Expr_Save :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_Save lbl1_ lbl2_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             Save lbl1_ lbl2_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_Set :: Int ->
                T_Expr 
sem_Expr_Set val_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             Set val_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_SetL :: Id ->
                 T_Expr 
sem_Expr_SetL lbl_  =
    (let _lhsOcopy :: Expr
         -- self rule
         _copy =
             SetL lbl_
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
sem_Expr_St :: Id ->
               Id ->
               T_Id_or_Imm  ->
               T_Expr 
sem_Expr_St lbl1_ lbl2_ vlbl_  =
    (let _lhsOcopy :: Expr
         _vlblIcopy :: Id_or_Imm
         -- self rule
         _copy =
             St lbl1_ lbl2_ _vlblIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _vlblIcopy) =
             (vlbl_ )
     in  ( _lhsOcopy))
sem_Expr_StDF :: Id ->
                 Id ->
                 T_Id_or_Imm  ->
                 T_Expr 
sem_Expr_StDF lbl1_ lbl2_ vlbl_  =
    (let _lhsOcopy :: Expr
         _vlblIcopy :: Id_or_Imm
         -- self rule
         _copy =
             StDF lbl1_ lbl2_ _vlblIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _vlblIcopy) =
             (vlbl_ )
     in  ( _lhsOcopy))
sem_Expr_Sub :: Id ->
                T_Id_or_Imm  ->
                T_Expr 
sem_Expr_Sub lbl_ vlbl_  =
    (let _lhsOcopy :: Expr
         _vlblIcopy :: Id_or_Imm
         -- self rule
         _copy =
             Sub lbl_ _vlblIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _vlblIcopy) =
             (vlbl_ )
     in  ( _lhsOcopy))
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
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Fundef:
         child name           : {Label}
         child args           : {[Id]}
         child fargs          : {[Id]}
         child body           : SparcT 
         child ret            : {Type}
         visit 0:
            local copy        : _
-}
-- cata
sem_Fundef :: Fundef  ->
              T_Fundef 
sem_Fundef (Fundef _name _args _fargs _body _ret )  =
    (sem_Fundef_Fundef _name _args _fargs (sem_SparcT _body ) _ret )
-- semantic domain
type T_Fundef  = ( Fundef)
data Inh_Fundef  = Inh_Fundef {}
data Syn_Fundef  = Syn_Fundef {copy_Syn_Fundef :: Fundef}
wrap_Fundef :: T_Fundef  ->
               Inh_Fundef  ->
               Syn_Fundef 
wrap_Fundef sem (Inh_Fundef )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_Fundef _lhsOcopy ))
sem_Fundef_Fundef :: Label ->
                     ([Id]) ->
                     ([Id]) ->
                     T_SparcT  ->
                     Type ->
                     T_Fundef 
sem_Fundef_Fundef name_ args_ fargs_ body_ ret_  =
    (let _lhsOcopy :: Fundef
         _bodyIcopy :: SparcT
         -- self rule
         _copy =
             Fundef name_ args_ fargs_ _bodyIcopy ret_
         -- self rule
         _lhsOcopy =
             _copy
         ( _bodyIcopy) =
             (body_ )
     in  ( _lhsOcopy))
-- Fundefs -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
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
type T_Fundefs  = ( Fundefs)
data Inh_Fundefs  = Inh_Fundefs {}
data Syn_Fundefs  = Syn_Fundefs {copy_Syn_Fundefs :: Fundefs}
wrap_Fundefs :: T_Fundefs  ->
                Inh_Fundefs  ->
                Syn_Fundefs 
wrap_Fundefs sem (Inh_Fundefs )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_Fundefs _lhsOcopy ))
sem_Fundefs_Cons :: T_Fundef  ->
                    T_Fundefs  ->
                    T_Fundefs 
sem_Fundefs_Cons hd_ tl_  =
    (let _lhsOcopy :: Fundefs
         _hdIcopy :: Fundef
         _tlIcopy :: Fundefs
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
sem_Fundefs_Nil :: T_Fundefs 
sem_Fundefs_Nil  =
    (let _lhsOcopy :: Fundefs
         -- self rule
         _copy =
             []
         -- self rule
         _lhsOcopy =
             _copy
     in  ( _lhsOcopy))
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
-- Prog --------------------------------------------------------
{-
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
type T_Prog  = ( )
data Inh_Prog  = Inh_Prog {}
data Syn_Prog  = Syn_Prog {}
wrap_Prog :: T_Prog  ->
             Inh_Prog  ->
             Syn_Prog 
wrap_Prog sem (Inh_Prog )  =
    (let ( ) =
             (sem )
     in  (Syn_Prog ))
sem_Prog_Prog :: T_FloatConsts  ->
                 T_Fundefs  ->
                 T_SparcT  ->
                 T_Prog 
sem_Prog_Prog floats_ fundefs_ instr_  =
    (let _floatsIcopy :: FloatConsts
         _fundefsIcopy :: Fundefs
         _instrIcopy :: SparcT
         ( _floatsIcopy) =
             (floats_ )
         ( _fundefsIcopy) =
             (fundefs_ )
         ( _instrIcopy) =
             (instr_ )
     in  ( ))
-- SparcT ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
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
         child tyid           : {TypeId}
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
    (sem_SparcT_Let _tyid (sem_Expr _expr ) (sem_SparcT _body ) )
-- semantic domain
type T_SparcT  = ( SparcT)
data Inh_SparcT  = Inh_SparcT {}
data Syn_SparcT  = Syn_SparcT {copy_Syn_SparcT :: SparcT}
wrap_SparcT :: T_SparcT  ->
               Inh_SparcT  ->
               Syn_SparcT 
wrap_SparcT sem (Inh_SparcT )  =
    (let ( _lhsOcopy) =
             (sem )
     in  (Syn_SparcT _lhsOcopy ))
sem_SparcT_Ans :: T_Expr  ->
                  T_SparcT 
sem_SparcT_Ans expr_  =
    (let _lhsOcopy :: SparcT
         _exprIcopy :: Expr
         -- self rule
         _copy =
             Ans _exprIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _exprIcopy) =
             (expr_ )
     in  ( _lhsOcopy))
sem_SparcT_Forget :: Id ->
                     T_SparcT  ->
                     T_SparcT 
sem_SparcT_Forget ident_ body_  =
    (let _lhsOcopy :: SparcT
         _bodyIcopy :: SparcT
         -- self rule
         _copy =
             Forget ident_ _bodyIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _bodyIcopy) =
             (body_ )
     in  ( _lhsOcopy))
sem_SparcT_Let :: TypeId ->
                  T_Expr  ->
                  T_SparcT  ->
                  T_SparcT 
sem_SparcT_Let tyid_ expr_ body_  =
    (let _lhsOcopy :: SparcT
         _exprIcopy :: Expr
         _bodyIcopy :: SparcT
         -- self rule
         _copy =
             Let tyid_ _exprIcopy _bodyIcopy
         -- self rule
         _lhsOcopy =
             _copy
         ( _exprIcopy) =
             (expr_ )
         ( _bodyIcopy) =
             (body_ )
     in  ( _lhsOcopy))
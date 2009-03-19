

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
import HMinCaml.SparcAsm
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
   alternatives:
      alternative Add:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
      alternative CallCls:
         child lbl            : {Id}
         child xs             : {[Id]}
         child ys             : {[Id]}
      alternative CallDir:
         child lbl            : {Label}
         child xs             : {[Id]}
         child ys             : {[Id]}
      alternative Comment:
         child msg            : {String}
      alternative FAddD:
         child lbl1           : {Id}
         child lbl2           : {Id}
      alternative FDivD:
         child lbl1           : {Id}
         child lbl2           : {Id}
      alternative FMovD:
         child lbl            : {Id}
      alternative FMulD:
         child lbl1           : {Id}
         child lbl2           : {Id}
      alternative FNegD:
         child lbl            : {Id}
      alternative FSubD:
         child lbl1           : {Id}
         child lbl2           : {Id}
      alternative IfEq:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         child ins1           : SparcT 
         child ins2           : SparcT 
      alternative IfFEq:
         child lbl1           : {Id}
         child lbl2           : {Id}
         child ins1           : SparcT 
         child ins2           : SparcT 
      alternative IfFLE:
         child lbl1           : {Id}
         child lbl2           : {Id}
         child ins1           : SparcT 
         child ins2           : SparcT 
      alternative IfGE:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         child ins1           : SparcT 
         child ins2           : SparcT 
      alternative IfLE:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
         child ins1           : SparcT 
         child ins2           : SparcT 
      alternative Ld:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
      alternative LdDF:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
      alternative Mov:
         child lbl            : {Id}
      alternative Neg:
         child lbl            : {Id}
      alternative Nop:
      alternative Restore:
         child lbl            : {Id}
      alternative SLL:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
      alternative Save:
         child lbl1           : {Id}
         child lbl2           : {Id}
      alternative Set:
         child val            : {Int}
      alternative SetL:
         child lbl            : {Id}
      alternative St:
         child lbl1           : {Id}
         child lbl2           : {Id}
         child vlbl           : Id_or_Imm 
      alternative StDF:
         child lbl1           : {Id}
         child lbl2           : {Id}
         child vlbl           : Id_or_Imm 
      alternative Sub:
         child lbl            : {Id}
         child vlbl           : Id_or_Imm 
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
type T_Expr  = ( )
data Inh_Expr  = Inh_Expr {}
data Syn_Expr  = Syn_Expr {}
wrap_Expr :: T_Expr  ->
             Inh_Expr  ->
             Syn_Expr 
wrap_Expr sem (Inh_Expr )  =
    (let ( ) =
             (sem )
     in  (Syn_Expr ))
sem_Expr_Add :: Id ->
                T_Id_or_Imm  ->
                T_Expr 
sem_Expr_Add lbl_ vlbl_  =
    (let 
     in  ( ))
sem_Expr_CallCls :: Id ->
                    ([Id]) ->
                    ([Id]) ->
                    T_Expr 
sem_Expr_CallCls lbl_ xs_ ys_  =
    (let 
     in  ( ))
sem_Expr_CallDir :: Label ->
                    ([Id]) ->
                    ([Id]) ->
                    T_Expr 
sem_Expr_CallDir lbl_ xs_ ys_  =
    (let 
     in  ( ))
sem_Expr_Comment :: String ->
                    T_Expr 
sem_Expr_Comment msg_  =
    (let 
     in  ( ))
sem_Expr_FAddD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FAddD lbl1_ lbl2_  =
    (let 
     in  ( ))
sem_Expr_FDivD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FDivD lbl1_ lbl2_  =
    (let 
     in  ( ))
sem_Expr_FMovD :: Id ->
                  T_Expr 
sem_Expr_FMovD lbl_  =
    (let 
     in  ( ))
sem_Expr_FMulD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FMulD lbl1_ lbl2_  =
    (let 
     in  ( ))
sem_Expr_FNegD :: Id ->
                  T_Expr 
sem_Expr_FNegD lbl_  =
    (let 
     in  ( ))
sem_Expr_FSubD :: Id ->
                  Id ->
                  T_Expr 
sem_Expr_FSubD lbl1_ lbl2_  =
    (let 
     in  ( ))
sem_Expr_IfEq :: Id ->
                 T_Id_or_Imm  ->
                 T_SparcT  ->
                 T_SparcT  ->
                 T_Expr 
sem_Expr_IfEq lbl_ vlbl_ ins1_ ins2_  =
    (let 
     in  ( ))
sem_Expr_IfFEq :: Id ->
                  Id ->
                  T_SparcT  ->
                  T_SparcT  ->
                  T_Expr 
sem_Expr_IfFEq lbl1_ lbl2_ ins1_ ins2_  =
    (let 
     in  ( ))
sem_Expr_IfFLE :: Id ->
                  Id ->
                  T_SparcT  ->
                  T_SparcT  ->
                  T_Expr 
sem_Expr_IfFLE lbl1_ lbl2_ ins1_ ins2_  =
    (let 
     in  ( ))
sem_Expr_IfGE :: Id ->
                 T_Id_or_Imm  ->
                 T_SparcT  ->
                 T_SparcT  ->
                 T_Expr 
sem_Expr_IfGE lbl_ vlbl_ ins1_ ins2_  =
    (let 
     in  ( ))
sem_Expr_IfLE :: Id ->
                 T_Id_or_Imm  ->
                 T_SparcT  ->
                 T_SparcT  ->
                 T_Expr 
sem_Expr_IfLE lbl_ vlbl_ ins1_ ins2_  =
    (let 
     in  ( ))
sem_Expr_Ld :: Id ->
               T_Id_or_Imm  ->
               T_Expr 
sem_Expr_Ld lbl_ vlbl_  =
    (let 
     in  ( ))
sem_Expr_LdDF :: Id ->
                 T_Id_or_Imm  ->
                 T_Expr 
sem_Expr_LdDF lbl_ vlbl_  =
    (let 
     in  ( ))
sem_Expr_Mov :: Id ->
                T_Expr 
sem_Expr_Mov lbl_  =
    (let 
     in  ( ))
sem_Expr_Neg :: Id ->
                T_Expr 
sem_Expr_Neg lbl_  =
    (let 
     in  ( ))
sem_Expr_Nop :: T_Expr 
sem_Expr_Nop  =
    (let 
     in  ( ))
sem_Expr_Restore :: Id ->
                    T_Expr 
sem_Expr_Restore lbl_  =
    (let 
     in  ( ))
sem_Expr_SLL :: Id ->
                T_Id_or_Imm  ->
                T_Expr 
sem_Expr_SLL lbl_ vlbl_  =
    (let 
     in  ( ))
sem_Expr_Save :: Id ->
                 Id ->
                 T_Expr 
sem_Expr_Save lbl1_ lbl2_  =
    (let 
     in  ( ))
sem_Expr_Set :: Int ->
                T_Expr 
sem_Expr_Set val_  =
    (let 
     in  ( ))
sem_Expr_SetL :: Id ->
                 T_Expr 
sem_Expr_SetL lbl_  =
    (let 
     in  ( ))
sem_Expr_St :: Id ->
               Id ->
               T_Id_or_Imm  ->
               T_Expr 
sem_Expr_St lbl1_ lbl2_ vlbl_  =
    (let 
     in  ( ))
sem_Expr_StDF :: Id ->
                 Id ->
                 T_Id_or_Imm  ->
                 T_Expr 
sem_Expr_StDF lbl1_ lbl2_ vlbl_  =
    (let 
     in  ( ))
sem_Expr_Sub :: Id ->
                T_Id_or_Imm  ->
                T_Expr 
sem_Expr_Sub lbl_ vlbl_  =
    (let 
     in  ( ))
-- FloatConst --------------------------------------------------
{-
   alternatives:
      alternative Tuple:
         child label          : {Label}
         child value          : {Float}
-}
-- cata
sem_FloatConst :: FloatConst  ->
                  T_FloatConst 
sem_FloatConst ( label,value)  =
    (sem_FloatConst_Tuple label value )
-- semantic domain
type T_FloatConst  = ( )
data Inh_FloatConst  = Inh_FloatConst {}
data Syn_FloatConst  = Syn_FloatConst {}
wrap_FloatConst :: T_FloatConst  ->
                   Inh_FloatConst  ->
                   Syn_FloatConst 
wrap_FloatConst sem (Inh_FloatConst )  =
    (let ( ) =
             (sem )
     in  (Syn_FloatConst ))
sem_FloatConst_Tuple :: Label ->
                        Float ->
                        T_FloatConst 
sem_FloatConst_Tuple label_ value_  =
    (let 
     in  ( ))
-- FloatConsts -------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : FloatConst 
         child tl             : FloatConsts 
      alternative Nil:
-}
-- cata
sem_FloatConsts :: FloatConsts  ->
                   T_FloatConsts 
sem_FloatConsts list  =
    (Prelude.foldr sem_FloatConsts_Cons sem_FloatConsts_Nil (Prelude.map sem_FloatConst list) )
-- semantic domain
type T_FloatConsts  = ( )
data Inh_FloatConsts  = Inh_FloatConsts {}
data Syn_FloatConsts  = Syn_FloatConsts {}
wrap_FloatConsts :: T_FloatConsts  ->
                    Inh_FloatConsts  ->
                    Syn_FloatConsts 
wrap_FloatConsts sem (Inh_FloatConsts )  =
    (let ( ) =
             (sem )
     in  (Syn_FloatConsts ))
sem_FloatConsts_Cons :: T_FloatConst  ->
                        T_FloatConsts  ->
                        T_FloatConsts 
sem_FloatConsts_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_FloatConsts_Nil :: T_FloatConsts 
sem_FloatConsts_Nil  =
    (let 
     in  ( ))
-- Fundef ------------------------------------------------------
{-
   alternatives:
      alternative Fundef:
         child name           : {Label}
         child args           : {[Id]}
         child fargs          : {[Id]}
         child body           : SparcT 
         child ret            : {Type}
-}
-- cata
sem_Fundef :: Fundef  ->
              T_Fundef 
sem_Fundef (Fundef _name _args _fargs _body _ret )  =
    (sem_Fundef_Fundef _name _args _fargs (sem_SparcT _body ) _ret )
-- semantic domain
type T_Fundef  = ( )
data Inh_Fundef  = Inh_Fundef {}
data Syn_Fundef  = Syn_Fundef {}
wrap_Fundef :: T_Fundef  ->
               Inh_Fundef  ->
               Syn_Fundef 
wrap_Fundef sem (Inh_Fundef )  =
    (let ( ) =
             (sem )
     in  (Syn_Fundef ))
sem_Fundef_Fundef :: Label ->
                     ([Id]) ->
                     ([Id]) ->
                     T_SparcT  ->
                     Type ->
                     T_Fundef 
sem_Fundef_Fundef name_ args_ fargs_ body_ ret_  =
    (let 
     in  ( ))
-- Fundefs -----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Fundef 
         child tl             : Fundefs 
      alternative Nil:
-}
-- cata
sem_Fundefs :: Fundefs  ->
               T_Fundefs 
sem_Fundefs list  =
    (Prelude.foldr sem_Fundefs_Cons sem_Fundefs_Nil (Prelude.map sem_Fundef list) )
-- semantic domain
type T_Fundefs  = ( )
data Inh_Fundefs  = Inh_Fundefs {}
data Syn_Fundefs  = Syn_Fundefs {}
wrap_Fundefs :: T_Fundefs  ->
                Inh_Fundefs  ->
                Syn_Fundefs 
wrap_Fundefs sem (Inh_Fundefs )  =
    (let ( ) =
             (sem )
     in  (Syn_Fundefs ))
sem_Fundefs_Cons :: T_Fundef  ->
                    T_Fundefs  ->
                    T_Fundefs 
sem_Fundefs_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_Fundefs_Nil :: T_Fundefs 
sem_Fundefs_Nil  =
    (let 
     in  ( ))
-- Id_or_Imm ---------------------------------------------------
{-
   alternatives:
      alternative C:
         child value          : {Int}
      alternative V:
         child ident          : {Id}
-}
-- cata
sem_Id_or_Imm :: Id_or_Imm  ->
                 T_Id_or_Imm 
sem_Id_or_Imm (C _value )  =
    (sem_Id_or_Imm_C _value )
sem_Id_or_Imm (V _ident )  =
    (sem_Id_or_Imm_V _ident )
-- semantic domain
type T_Id_or_Imm  = ( )
data Inh_Id_or_Imm  = Inh_Id_or_Imm {}
data Syn_Id_or_Imm  = Syn_Id_or_Imm {}
wrap_Id_or_Imm :: T_Id_or_Imm  ->
                  Inh_Id_or_Imm  ->
                  Syn_Id_or_Imm 
wrap_Id_or_Imm sem (Inh_Id_or_Imm )  =
    (let ( ) =
             (sem )
     in  (Syn_Id_or_Imm ))
sem_Id_or_Imm_C :: Int ->
                   T_Id_or_Imm 
sem_Id_or_Imm_C value_  =
    (let 
     in  ( ))
sem_Id_or_Imm_V :: Id ->
                   T_Id_or_Imm 
sem_Id_or_Imm_V ident_  =
    (let 
     in  ( ))
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
    (let 
     in  ( ))
-- SparcT ------------------------------------------------------
{-
   alternatives:
      alternative Ans:
         child expr           : Expr 
      alternative Forget:
         child ident          : {Id}
         child body           : SparcT 
      alternative Let:
         child tyid           : {TypeId}
         child expr           : Expr 
         child body           : SparcT 
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
type T_SparcT  = ( )
data Inh_SparcT  = Inh_SparcT {}
data Syn_SparcT  = Syn_SparcT {}
wrap_SparcT :: T_SparcT  ->
               Inh_SparcT  ->
               Syn_SparcT 
wrap_SparcT sem (Inh_SparcT )  =
    (let ( ) =
             (sem )
     in  (Syn_SparcT ))
sem_SparcT_Ans :: T_Expr  ->
                  T_SparcT 
sem_SparcT_Ans expr_  =
    (let 
     in  ( ))
sem_SparcT_Forget :: Id ->
                     T_SparcT  ->
                     T_SparcT 
sem_SparcT_Forget ident_ body_  =
    (let 
     in  ( ))
sem_SparcT_Let :: TypeId ->
                  T_Expr  ->
                  T_SparcT  ->
                  T_SparcT 
sem_SparcT_Let tyid_ expr_ body_  =
    (let 
     in  ( ))
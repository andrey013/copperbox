-- do not edit; automatically generated by UU.AG

module Gen.OutputAsdl
  ( outputAsdl
  ) where

import Base.PrimitiveTypes
import Base.AsdlConcreteSyn

import Util.PPExt

import PPrint




outputAsdl :: AsdlSpec -> Doc
outputAsdl spec = vsep $ pp_Syn_AsdlSpec synthesized
  where synthesized = wrap_AsdlSpec (sem_AsdlSpec spec) inherited
        inherited   = Inh_AsdlSpec



       
-- AltThree ----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for AltThree.Alt1:

-}
{-
   local variables for AltThree.Alt2:

-}
{-
   local variables for AltThree.Alt3:

-}
-- semantic domain
type T_AltThree = ( (Doc))
-- cata
sem_AltThree :: (AltThree) ->
                (T_AltThree)
sem_AltThree ((Alt1 (_a))) =
    (sem_AltThree_Alt1 ((sem_ModuleDefn (_a))))
sem_AltThree ((Alt2 (_a))) =
    (sem_AltThree_Alt2 ((sem_PrimModule (_a))))
sem_AltThree ((Alt3 (_a))) =
    (sem_AltThree_Alt3 ((sem_ViewDefn (_a))))
data Inh_AltThree = Inh_AltThree {}
data Syn_AltThree = Syn_AltThree {pp_Syn_AltThree :: Doc}
wrap_AltThree :: (T_AltThree) ->
                 (Inh_AltThree) ->
                 (Syn_AltThree)
wrap_AltThree (sem) ((Inh_AltThree )) =
    let ( s1) =
            (sem )
    in  (Syn_AltThree (s1))
sem_AltThree_Alt1 :: (T_ModuleDefn) ->
                     (T_AltThree)
sem_AltThree_Alt1 (a_) =
    let _lhsOpp :: (Doc)
        _aIpp :: (Doc)
        ( _aIpp) =
            (a_ )
        -- "OutputAsdl.ag"(line 32, column 17)
        (_lhsOpp@_) =
            _aIpp
    in  ( _lhsOpp)
sem_AltThree_Alt2 :: (T_PrimModule) ->
                     (T_AltThree)
sem_AltThree_Alt2 (a_) =
    let _lhsOpp :: (Doc)
        _aIpp :: (Doc)
        ( _aIpp) =
            (a_ )
        -- "OutputAsdl.ag"(line 33, column 17)
        (_lhsOpp@_) =
            _aIpp
    in  ( _lhsOpp)
sem_AltThree_Alt3 :: (T_ViewDefn) ->
                     (T_AltThree)
sem_AltThree_Alt3 (a_) =
    let _lhsOpp :: (Doc)
        _aIpp :: (Doc)
        ( _aIpp) =
            (a_ )
        -- "OutputAsdl.ag"(line 34, column 17)
        (_lhsOpp@_) =
            _aIpp
    in  ( _lhsOpp)
-- AsdlPrim ----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for AsdlPrim.TyIdentifier:

-}
{-
   local variables for AsdlPrim.TyInt:

-}
{-
   local variables for AsdlPrim.TyRef:

-}
{-
   local variables for AsdlPrim.TyString:

-}
{-
   local variables for AsdlPrim.TyUnit:

-}
-- semantic domain
type T_AsdlPrim = ( (Doc))
-- cata
sem_AsdlPrim :: (AsdlPrim) ->
                (T_AsdlPrim)
sem_AsdlPrim ((TyIdentifier )) =
    (sem_AsdlPrim_TyIdentifier )
sem_AsdlPrim ((TyInt )) =
    (sem_AsdlPrim_TyInt )
sem_AsdlPrim ((TyRef (_name))) =
    (sem_AsdlPrim_TyRef (_name))
sem_AsdlPrim ((TyString )) =
    (sem_AsdlPrim_TyString )
sem_AsdlPrim ((TyUnit )) =
    (sem_AsdlPrim_TyUnit )
data Inh_AsdlPrim = Inh_AsdlPrim {}
data Syn_AsdlPrim = Syn_AsdlPrim {pp_Syn_AsdlPrim :: Doc}
wrap_AsdlPrim :: (T_AsdlPrim) ->
                 (Inh_AsdlPrim) ->
                 (Syn_AsdlPrim)
wrap_AsdlPrim (sem) ((Inh_AsdlPrim )) =
    let ( s1) =
            (sem )
    in  (Syn_AsdlPrim (s1))
sem_AsdlPrim_TyIdentifier :: (T_AsdlPrim)
sem_AsdlPrim_TyIdentifier  =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 85, column 21)
        (_lhsOpp@_) =
            text "identifier"
    in  ( _lhsOpp)
sem_AsdlPrim_TyInt :: (T_AsdlPrim)
sem_AsdlPrim_TyInt  =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 84, column 21)
        (_lhsOpp@_) =
            text "int"
    in  ( _lhsOpp)
sem_AsdlPrim_TyRef :: (String) ->
                      (T_AsdlPrim)
sem_AsdlPrim_TyRef (name_) =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 86, column 21)
        (_lhsOpp@_) =
            text name_
    in  ( _lhsOpp)
sem_AsdlPrim_TyString :: (T_AsdlPrim)
sem_AsdlPrim_TyString  =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 83, column 21)
        (_lhsOpp@_) =
            text "string"
    in  ( _lhsOpp)
sem_AsdlPrim_TyUnit :: (T_AsdlPrim)
sem_AsdlPrim_TyUnit  =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 82, column 21)
        (_lhsOpp@_) =
            empty
    in  ( _lhsOpp)
-- AsdlSpec ----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : [Doc]

-}
{-
   local variables for AsdlSpec.Cons:

-}
{-
   local variables for AsdlSpec.Nil:

-}
-- semantic domain
type T_AsdlSpec = ( ([Doc]))
-- cata
sem_AsdlSpec :: (AsdlSpec) ->
                (T_AsdlSpec)
sem_AsdlSpec (list) =
    (Prelude.foldr (sem_AsdlSpec_Cons) (sem_AsdlSpec_Nil) ((Prelude.map sem_AltThree list)))
data Inh_AsdlSpec = Inh_AsdlSpec {}
data Syn_AsdlSpec = Syn_AsdlSpec {pp_Syn_AsdlSpec :: [Doc]}
wrap_AsdlSpec :: (T_AsdlSpec) ->
                 (Inh_AsdlSpec) ->
                 (Syn_AsdlSpec)
wrap_AsdlSpec (sem) ((Inh_AsdlSpec )) =
    let ( s1) =
            (sem )
    in  (Syn_AsdlSpec (s1))
sem_AsdlSpec_Cons :: (T_AltThree) ->
                     (T_AsdlSpec) ->
                     (T_AsdlSpec)
sem_AsdlSpec_Cons (hd_) (tl_) =
    let _lhsOpp :: ([Doc])
        _hdIpp :: (Doc)
        _tlIpp :: ([Doc])
        ( _hdIpp) =
            (hd_ )
        ( _tlIpp) =
            (tl_ )
        -- use rule
        (_lhsOpp@_) =
            _hdIpp : _tlIpp
    in  ( _lhsOpp)
sem_AsdlSpec_Nil :: (T_AsdlSpec)
sem_AsdlSpec_Nil  =
    let _lhsOpp :: ([Doc])
        -- use rule
        (_lhsOpp@_) =
            []
    in  ( _lhsOpp)
-- AsdlType ----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for AsdlType.Prod:

-}
{-
   local variables for AsdlType.Sum:

-}
-- semantic domain
type T_AsdlType = ( (Doc))
-- cata
sem_AsdlType :: (AsdlType) ->
                (T_AsdlType)
sem_AsdlType ((Prod (_fields))) =
    (sem_AsdlType_Prod ((sem_Fields (_fields))))
sem_AsdlType ((Sum (_constrs) (_opt_attribs))) =
    (sem_AsdlType_Sum ((sem_Constrs (_constrs))) ((sem_OptAttribs (_opt_attribs))))
data Inh_AsdlType = Inh_AsdlType {}
data Syn_AsdlType = Syn_AsdlType {pp_Syn_AsdlType :: Doc}
wrap_AsdlType :: (T_AsdlType) ->
                 (Inh_AsdlType) ->
                 (Syn_AsdlType)
wrap_AsdlType (sem) ((Inh_AsdlType )) =
    let ( s1) =
            (sem )
    in  (Syn_AsdlType (s1))
sem_AsdlType_Prod :: (T_Fields) ->
                     (T_AsdlType)
sem_AsdlType_Prod (fields_) =
    let _lhsOpp :: (Doc)
        _fieldsIpp :: ([Doc])
        ( _fieldsIpp) =
            (fields_ )
        -- "OutputAsdl.ag"(line 62, column 17)
        (_lhsOpp@_) =
            equals <+> tupled _fieldsIpp
    in  ( _lhsOpp)
sem_AsdlType_Sum :: (T_Constrs) ->
                    (T_OptAttribs) ->
                    (T_AsdlType)
sem_AsdlType_Sum (constrs_) (opt_attribs_) =
    let _lhsOpp :: (Doc)
        _constrsIpp :: ([Doc])
        ( _constrsIpp) =
            (constrs_ )
        -- "OutputAsdl.ag"(line 57, column 17)
        (_lhsOpp@_) =
            encloseSepO (equals <> space)
                        empty
                        (text "| ")
                        _constrsIpp
    in  ( _lhsOpp)
-- Attribs -----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:

-}
{-
   local variables for Attribs.Cons:

-}
{-
   local variables for Attribs.Nil:

-}
-- semantic domain
type T_Attribs = ( )
-- cata
sem_Attribs :: (Attribs) ->
               (T_Attribs)
sem_Attribs (list) =
    (Prelude.foldr (sem_Attribs_Cons) (sem_Attribs_Nil) ((Prelude.map sem_Field list)))
data Inh_Attribs = Inh_Attribs {}
data Syn_Attribs = Syn_Attribs {}
wrap_Attribs :: (T_Attribs) ->
                (Inh_Attribs) ->
                (Syn_Attribs)
wrap_Attribs (sem) ((Inh_Attribs )) =
    let 
    in  (Syn_Attribs )
sem_Attribs_Cons :: (T_Field) ->
                    (T_Attribs) ->
                    (T_Attribs)
sem_Attribs_Cons (hd_) (tl_) =
    let _hdIpp :: (Doc)
        ( _hdIpp) =
            (hd_ )
    in  ( )
sem_Attribs_Nil :: (T_Attribs)
sem_Attribs_Nil  =
    let 
    in  ( )
-- Cardinality -------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for Cardinality.One:

-}
{-
   local variables for Cardinality.Opt:

-}
{-
   local variables for Cardinality.Zom:

-}
-- semantic domain
type T_Cardinality = ( (Doc))
-- cata
sem_Cardinality :: (Cardinality) ->
                   (T_Cardinality)
sem_Cardinality ((One )) =
    (sem_Cardinality_One )
sem_Cardinality ((Opt )) =
    (sem_Cardinality_Opt )
sem_Cardinality ((Zom )) =
    (sem_Cardinality_Zom )
data Inh_Cardinality = Inh_Cardinality {}
data Syn_Cardinality = Syn_Cardinality {pp_Syn_Cardinality :: Doc}
wrap_Cardinality :: (T_Cardinality) ->
                    (Inh_Cardinality) ->
                    (Syn_Cardinality)
wrap_Cardinality (sem) ((Inh_Cardinality )) =
    let ( s1) =
            (sem )
    in  (Syn_Cardinality (s1))
sem_Cardinality_One :: (T_Cardinality)
sem_Cardinality_One  =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 90, column 17)
        (_lhsOpp@_) =
            empty
    in  ( _lhsOpp)
sem_Cardinality_Opt :: (T_Cardinality)
sem_Cardinality_Opt  =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 91, column 17)
        (_lhsOpp@_) =
            char '?'
    in  ( _lhsOpp)
sem_Cardinality_Zom :: (T_Cardinality)
sem_Cardinality_Zom  =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 92, column 17)
        (_lhsOpp@_) =
            char '*'
    in  ( _lhsOpp)
-- Constr ------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for Constr.Constr:

-}
-- semantic domain
type T_Constr = ( (Doc))
-- cata
sem_Constr :: (Constr) ->
              (T_Constr)
sem_Constr ((Constr (_label) (_fields))) =
    (sem_Constr_Constr (_label) ((sem_Fields (_fields))))
data Inh_Constr = Inh_Constr {}
data Syn_Constr = Syn_Constr {pp_Syn_Constr :: Doc}
wrap_Constr :: (T_Constr) ->
               (Inh_Constr) ->
               (Syn_Constr)
wrap_Constr (sem) ((Inh_Constr )) =
    let ( s1) =
            (sem )
    in  (Syn_Constr (s1))
sem_Constr_Constr :: (String) ->
                     (T_Fields) ->
                     (T_Constr)
sem_Constr_Constr (label_) (fields_) =
    let _lhsOpp :: (Doc)
        _fieldsIpp :: ([Doc])
        ( _fieldsIpp) =
            (fields_ )
        -- "OutputAsdl.ag"(line 66, column 17)
        (_lhsOpp@_) =
            text label_
              <> encloseSepO lparen rparen commaspace _fieldsIpp
    in  ( _lhsOpp)
-- Constrs -----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : [Doc]

-}
{-
   local variables for Constrs.Cons:

-}
{-
   local variables for Constrs.Nil:

-}
-- semantic domain
type T_Constrs = ( ([Doc]))
-- cata
sem_Constrs :: (Constrs) ->
               (T_Constrs)
sem_Constrs (list) =
    (Prelude.foldr (sem_Constrs_Cons) (sem_Constrs_Nil) ((Prelude.map sem_Constr list)))
data Inh_Constrs = Inh_Constrs {}
data Syn_Constrs = Syn_Constrs {pp_Syn_Constrs :: [Doc]}
wrap_Constrs :: (T_Constrs) ->
                (Inh_Constrs) ->
                (Syn_Constrs)
wrap_Constrs (sem) ((Inh_Constrs )) =
    let ( s1) =
            (sem )
    in  (Syn_Constrs (s1))
sem_Constrs_Cons :: (T_Constr) ->
                    (T_Constrs) ->
                    (T_Constrs)
sem_Constrs_Cons (hd_) (tl_) =
    let _lhsOpp :: ([Doc])
        _hdIpp :: (Doc)
        _tlIpp :: ([Doc])
        ( _hdIpp) =
            (hd_ )
        ( _tlIpp) =
            (tl_ )
        -- use rule
        (_lhsOpp@_) =
            _hdIpp : _tlIpp
    in  ( _lhsOpp)
sem_Constrs_Nil :: (T_Constrs)
sem_Constrs_Nil  =
    let _lhsOpp :: ([Doc])
        -- use rule
        (_lhsOpp@_) =
            []
    in  ( _lhsOpp)
-- Definition --------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for Definition.Def:

-}
-- semantic domain
type T_Definition = ( (Doc))
-- cata
sem_Definition :: (Definition) ->
                  (T_Definition)
sem_Definition ((Def (_name) (_dtype))) =
    (sem_Definition_Def (_name) ((sem_AsdlType (_dtype))))
data Inh_Definition = Inh_Definition {}
data Syn_Definition = Syn_Definition {pp_Syn_Definition :: Doc}
wrap_Definition :: (T_Definition) ->
                   (Inh_Definition) ->
                   (Syn_Definition)
wrap_Definition (sem) ((Inh_Definition )) =
    let ( s1) =
            (sem )
    in  (Syn_Definition (s1))
sem_Definition_Def :: (String) ->
                      (T_AsdlType) ->
                      (T_Definition)
sem_Definition_Def (name_) (dtype_) =
    let _lhsOpp :: (Doc)
        _dtypeIpp :: (Doc)
        ( _dtypeIpp) =
            (dtype_ )
        -- "OutputAsdl.ag"(line 54, column 17)
        (_lhsOpp@_) =
            fill 10 (text name_) <+> align _dtypeIpp
    in  ( _lhsOpp)
-- Definitions -------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : [Doc]

-}
{-
   local variables for Definitions.Cons:

-}
{-
   local variables for Definitions.Nil:

-}
-- semantic domain
type T_Definitions = ( ([Doc]))
-- cata
sem_Definitions :: (Definitions) ->
                   (T_Definitions)
sem_Definitions (list) =
    (Prelude.foldr (sem_Definitions_Cons) (sem_Definitions_Nil) ((Prelude.map sem_Definition list)))
data Inh_Definitions = Inh_Definitions {}
data Syn_Definitions = Syn_Definitions {pp_Syn_Definitions :: [Doc]}
wrap_Definitions :: (T_Definitions) ->
                    (Inh_Definitions) ->
                    (Syn_Definitions)
wrap_Definitions (sem) ((Inh_Definitions )) =
    let ( s1) =
            (sem )
    in  (Syn_Definitions (s1))
sem_Definitions_Cons :: (T_Definition) ->
                        (T_Definitions) ->
                        (T_Definitions)
sem_Definitions_Cons (hd_) (tl_) =
    let _lhsOpp :: ([Doc])
        _hdIpp :: (Doc)
        _tlIpp :: ([Doc])
        ( _hdIpp) =
            (hd_ )
        ( _tlIpp) =
            (tl_ )
        -- use rule
        (_lhsOpp@_) =
            _hdIpp : _tlIpp
    in  ( _lhsOpp)
sem_Definitions_Nil :: (T_Definitions)
sem_Definitions_Nil  =
    let _lhsOpp :: ([Doc])
        -- use rule
        (_lhsOpp@_) =
            []
    in  ( _lhsOpp)
-- Field -------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for Field.Field:

-}
-- semantic domain
type T_Field = ( (Doc))
-- cata
sem_Field :: (Field) ->
             (T_Field)
sem_Field ((Field (_qual) (_ptype) (_card) (_opt_name))) =
    (sem_Field_Field ((sem_OptQualifier (_qual))) ((sem_AsdlPrim (_ptype))) ((sem_Cardinality (_card))) ((sem_OptIdentifier (_opt_name))))
data Inh_Field = Inh_Field {}
data Syn_Field = Syn_Field {pp_Syn_Field :: Doc}
wrap_Field :: (T_Field) ->
              (Inh_Field) ->
              (Syn_Field)
wrap_Field (sem) ((Inh_Field )) =
    let ( s1) =
            (sem )
    in  (Syn_Field (s1))
sem_Field_Field :: (T_OptQualifier) ->
                   (T_AsdlPrim) ->
                   (T_Cardinality) ->
                   (T_OptIdentifier) ->
                   (T_Field)
sem_Field_Field (qual_) (ptype_) (card_) (opt_name_) =
    let _lhsOpp :: (Doc)
        _qualIpp :: (Doc)
        _ptypeIpp :: (Doc)
        _cardIpp :: (Doc)
        _opt_nameIpp :: (Doc)
        ( _qualIpp) =
            (qual_ )
        ( _ptypeIpp) =
            (ptype_ )
        ( _cardIpp) =
            (card_ )
        ( _opt_nameIpp) =
            (opt_name_ )
        -- "OutputAsdl.ag"(line 70, column 17)
        (_lhsOpp@_) =
            _qualIpp <> _ptypeIpp <> _cardIpp <> _opt_nameIpp
    in  ( _lhsOpp)
-- Fields ------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : [Doc]

-}
{-
   local variables for Fields.Cons:

-}
{-
   local variables for Fields.Nil:

-}
-- semantic domain
type T_Fields = ( ([Doc]))
-- cata
sem_Fields :: (Fields) ->
              (T_Fields)
sem_Fields (list) =
    (Prelude.foldr (sem_Fields_Cons) (sem_Fields_Nil) ((Prelude.map sem_Field list)))
data Inh_Fields = Inh_Fields {}
data Syn_Fields = Syn_Fields {pp_Syn_Fields :: [Doc]}
wrap_Fields :: (T_Fields) ->
               (Inh_Fields) ->
               (Syn_Fields)
wrap_Fields (sem) ((Inh_Fields )) =
    let ( s1) =
            (sem )
    in  (Syn_Fields (s1))
sem_Fields_Cons :: (T_Field) ->
                   (T_Fields) ->
                   (T_Fields)
sem_Fields_Cons (hd_) (tl_) =
    let _lhsOpp :: ([Doc])
        _hdIpp :: (Doc)
        _tlIpp :: ([Doc])
        ( _hdIpp) =
            (hd_ )
        ( _tlIpp) =
            (tl_ )
        -- use rule
        (_lhsOpp@_) =
            _hdIpp : _tlIpp
    in  ( _lhsOpp)
sem_Fields_Nil :: (T_Fields)
sem_Fields_Nil  =
    let _lhsOpp :: ([Doc])
        -- use rule
        (_lhsOpp@_) =
            []
    in  ( _lhsOpp)
-- ImportStmt --------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for ImportStmt.ImportStmt:

-}
-- semantic domain
type T_ImportStmt = ( (Doc))
-- cata
sem_ImportStmt :: (ImportStmt) ->
                  (T_ImportStmt)
sem_ImportStmt ((ImportStmt (_name))) =
    (sem_ImportStmt_ImportStmt (_name))
data Inh_ImportStmt = Inh_ImportStmt {}
data Syn_ImportStmt = Syn_ImportStmt {pp_Syn_ImportStmt :: Doc}
wrap_ImportStmt :: (T_ImportStmt) ->
                   (Inh_ImportStmt) ->
                   (Syn_ImportStmt)
wrap_ImportStmt (sem) ((Inh_ImportStmt )) =
    let ( s1) =
            (sem )
    in  (Syn_ImportStmt (s1))
sem_ImportStmt_ImportStmt :: (String) ->
                             (T_ImportStmt)
sem_ImportStmt_ImportStmt (name_) =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 50, column 17)
        (_lhsOpp@_) =
            text "imports" <+> text name_
    in  ( _lhsOpp)
-- ImportStmts -------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : [Doc]

-}
{-
   local variables for ImportStmts.Cons:

-}
{-
   local variables for ImportStmts.Nil:

-}
-- semantic domain
type T_ImportStmts = ( ([Doc]))
-- cata
sem_ImportStmts :: (ImportStmts) ->
                   (T_ImportStmts)
sem_ImportStmts (list) =
    (Prelude.foldr (sem_ImportStmts_Cons) (sem_ImportStmts_Nil) ((Prelude.map sem_ImportStmt list)))
data Inh_ImportStmts = Inh_ImportStmts {}
data Syn_ImportStmts = Syn_ImportStmts {pp_Syn_ImportStmts :: [Doc]}
wrap_ImportStmts :: (T_ImportStmts) ->
                    (Inh_ImportStmts) ->
                    (Syn_ImportStmts)
wrap_ImportStmts (sem) ((Inh_ImportStmts )) =
    let ( s1) =
            (sem )
    in  (Syn_ImportStmts (s1))
sem_ImportStmts_Cons :: (T_ImportStmt) ->
                        (T_ImportStmts) ->
                        (T_ImportStmts)
sem_ImportStmts_Cons (hd_) (tl_) =
    let _lhsOpp :: ([Doc])
        _hdIpp :: (Doc)
        _tlIpp :: ([Doc])
        ( _hdIpp) =
            (hd_ )
        ( _tlIpp) =
            (tl_ )
        -- use rule
        (_lhsOpp@_) =
            _hdIpp : _tlIpp
    in  ( _lhsOpp)
sem_ImportStmts_Nil :: (T_ImportStmts)
sem_ImportStmts_Nil  =
    let _lhsOpp :: ([Doc])
        -- use rule
        (_lhsOpp@_) =
            []
    in  ( _lhsOpp)
-- ModuleDefn --------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for ModuleDefn.Module:

-}
-- semantic domain
type T_ModuleDefn = ( (Doc))
-- cata
sem_ModuleDefn :: (ModuleDefn) ->
                  (T_ModuleDefn)
sem_ModuleDefn ((Module (_name) (_importlist) (_defs))) =
    (sem_ModuleDefn_Module (_name) ((sem_ImportStmts (_importlist))) ((sem_Definitions (_defs))))
data Inh_ModuleDefn = Inh_ModuleDefn {}
data Syn_ModuleDefn = Syn_ModuleDefn {pp_Syn_ModuleDefn :: Doc}
wrap_ModuleDefn :: (T_ModuleDefn) ->
                   (Inh_ModuleDefn) ->
                   (Syn_ModuleDefn)
wrap_ModuleDefn (sem) ((Inh_ModuleDefn )) =
    let ( s1) =
            (sem )
    in  (Syn_ModuleDefn (s1))
sem_ModuleDefn_Module :: (String) ->
                         (T_ImportStmts) ->
                         (T_Definitions) ->
                         (T_ModuleDefn)
sem_ModuleDefn_Module (name_) (importlist_) (defs_) =
    let _lhsOpp :: (Doc)
        _importlistIpp :: ([Doc])
        _defsIpp :: ([Doc])
        ( _importlistIpp) =
            (importlist_ )
        ( _defsIpp) =
            (defs_ )
        -- "OutputAsdl.ag"(line 37, column 17)
        (_lhsOpp@_) =
            text "module" <+> text name_
              <> encloseSepO (text " (") rparen space _importlistIpp
              <+> lbrace
              <$> indent 2 (encloseSep line line line _defsIpp)
              <$> rbrace
    in  ( _lhsOpp)
-- ModuleDefns -------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:

-}
{-
   local variables for ModuleDefns.Cons:

-}
{-
   local variables for ModuleDefns.Nil:

-}
-- semantic domain
type T_ModuleDefns = ( )
-- cata
sem_ModuleDefns :: (ModuleDefns) ->
                   (T_ModuleDefns)
sem_ModuleDefns (list) =
    (Prelude.foldr (sem_ModuleDefns_Cons) (sem_ModuleDefns_Nil) ((Prelude.map sem_ModuleDefn list)))
data Inh_ModuleDefns = Inh_ModuleDefns {}
data Syn_ModuleDefns = Syn_ModuleDefns {}
wrap_ModuleDefns :: (T_ModuleDefns) ->
                    (Inh_ModuleDefns) ->
                    (Syn_ModuleDefns)
wrap_ModuleDefns (sem) ((Inh_ModuleDefns )) =
    let 
    in  (Syn_ModuleDefns )
sem_ModuleDefns_Cons :: (T_ModuleDefn) ->
                        (T_ModuleDefns) ->
                        (T_ModuleDefns)
sem_ModuleDefns_Cons (hd_) (tl_) =
    let _hdIpp :: (Doc)
        ( _hdIpp) =
            (hd_ )
    in  ( )
sem_ModuleDefns_Nil :: (T_ModuleDefns)
sem_ModuleDefns_Nil  =
    let 
    in  ( )
-- OptAttribs --------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:

-}
{-
   local variables for OptAttribs.Just:

-}
{-
   local variables for OptAttribs.Nothing:

-}
-- semantic domain
type T_OptAttribs = ( )
-- cata
sem_OptAttribs :: (OptAttribs) ->
                  (T_OptAttribs)
sem_OptAttribs ((Prelude.Just (x))) =
    (sem_OptAttribs_Just ((sem_Attribs (x))))
sem_OptAttribs (Prelude.Nothing) =
    sem_OptAttribs_Nothing
data Inh_OptAttribs = Inh_OptAttribs {}
data Syn_OptAttribs = Syn_OptAttribs {}
wrap_OptAttribs :: (T_OptAttribs) ->
                   (Inh_OptAttribs) ->
                   (Syn_OptAttribs)
wrap_OptAttribs (sem) ((Inh_OptAttribs )) =
    let 
    in  (Syn_OptAttribs )
sem_OptAttribs_Just :: (T_Attribs) ->
                       (T_OptAttribs)
sem_OptAttribs_Just (just_) =
    let 
    in  ( )
sem_OptAttribs_Nothing :: (T_OptAttribs)
sem_OptAttribs_Nothing  =
    let 
    in  ( )
-- OptIdentifier -----------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for OptIdentifier.Just:

-}
{-
   local variables for OptIdentifier.Nothing:

-}
-- semantic domain
type T_OptIdentifier = ( (Doc))
-- cata
sem_OptIdentifier :: (OptIdentifier) ->
                     (T_OptIdentifier)
sem_OptIdentifier ((Prelude.Just (x))) =
    (sem_OptIdentifier_Just (x))
sem_OptIdentifier (Prelude.Nothing) =
    sem_OptIdentifier_Nothing
data Inh_OptIdentifier = Inh_OptIdentifier {}
data Syn_OptIdentifier = Syn_OptIdentifier {pp_Syn_OptIdentifier :: Doc}
wrap_OptIdentifier :: (T_OptIdentifier) ->
                      (Inh_OptIdentifier) ->
                      (Syn_OptIdentifier)
wrap_OptIdentifier (sem) ((Inh_OptIdentifier )) =
    let ( s1) =
            (sem )
    in  (Syn_OptIdentifier (s1))
sem_OptIdentifier_Just :: (String) ->
                          (T_OptIdentifier)
sem_OptIdentifier_Just (just_) =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 77, column 17)
        (_lhsOpp@_) =
            space <> text just_
    in  ( _lhsOpp)
sem_OptIdentifier_Nothing :: (T_OptIdentifier)
sem_OptIdentifier_Nothing  =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 78, column 17)
        (_lhsOpp@_) =
            empty
    in  ( _lhsOpp)
-- OptQualifier ------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for OptQualifier.Just:

-}
{-
   local variables for OptQualifier.Nothing:

-}
-- semantic domain
type T_OptQualifier = ( (Doc))
-- cata
sem_OptQualifier :: (OptQualifier) ->
                    (T_OptQualifier)
sem_OptQualifier ((Prelude.Just (x))) =
    (sem_OptQualifier_Just (x))
sem_OptQualifier (Prelude.Nothing) =
    sem_OptQualifier_Nothing
data Inh_OptQualifier = Inh_OptQualifier {}
data Syn_OptQualifier = Syn_OptQualifier {pp_Syn_OptQualifier :: Doc}
wrap_OptQualifier :: (T_OptQualifier) ->
                     (Inh_OptQualifier) ->
                     (Syn_OptQualifier)
wrap_OptQualifier (sem) ((Inh_OptQualifier )) =
    let ( s1) =
            (sem )
    in  (Syn_OptQualifier (s1))
sem_OptQualifier_Just :: (String) ->
                         (T_OptQualifier)
sem_OptQualifier_Just (just_) =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 73, column 17)
        (_lhsOpp@_) =
            text just_ <> char '.'
    in  ( _lhsOpp)
sem_OptQualifier_Nothing :: (T_OptQualifier)
sem_OptQualifier_Nothing  =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 74, column 17)
        (_lhsOpp@_) =
            empty
    in  ( _lhsOpp)
-- PrimModule --------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for PrimModule.PrimModule:

-}
-- semantic domain
type T_PrimModule = ( (Doc))
-- cata
sem_PrimModule :: (PrimModule) ->
                  (T_PrimModule)
sem_PrimModule ((PrimModule (_name) (_prims))) =
    (sem_PrimModule_PrimModule (_name) (_prims))
data Inh_PrimModule = Inh_PrimModule {}
data Syn_PrimModule = Syn_PrimModule {pp_Syn_PrimModule :: Doc}
wrap_PrimModule :: (T_PrimModule) ->
                   (Inh_PrimModule) ->
                   (Syn_PrimModule)
wrap_PrimModule (sem) ((Inh_PrimModule )) =
    let ( s1) =
            (sem )
    in  (Syn_PrimModule (s1))
sem_PrimModule_PrimModule :: (String) ->
                             ([String]) ->
                             (T_PrimModule)
sem_PrimModule_PrimModule (name_) (prims_) =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 43, column 17)
        (_lhsOpp@_) =
            text "PrimModule to do"
    in  ( _lhsOpp)
-- ViewDecl ----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:

-}
{-
   local variables for ViewDecl.View_Many_to_Many:

-}
{-
   local variables for ViewDecl.View_Many_to_One:

-}
{-
   local variables for ViewDecl.View_One_to_Many:

-}
{-
   local variables for ViewDecl.View_Plain:

-}
-- semantic domain
type T_ViewDecl = ( )
-- cata
sem_ViewDecl :: (ViewDecl) ->
                (T_ViewDecl)
sem_ViewDecl ((View_Many_to_Many (_entities) (_kv_pairs))) =
    (sem_ViewDecl_View_Many_to_Many (_entities) (_kv_pairs))
sem_ViewDecl ((View_Many_to_One (_entities) (_kv_pair))) =
    (sem_ViewDecl_View_Many_to_One (_entities) ((sem_ViewPair (_kv_pair))))
sem_ViewDecl ((View_One_to_Many (_entity) (_kv_pairs))) =
    (sem_ViewDecl_View_One_to_Many ((sem_ViewEntity (_entity))) (_kv_pairs))
sem_ViewDecl ((View_Plain (_entity) (_kv_pair))) =
    (sem_ViewDecl_View_Plain ((sem_ViewEntity (_entity))) ((sem_ViewPair (_kv_pair))))
data Inh_ViewDecl = Inh_ViewDecl {}
data Syn_ViewDecl = Syn_ViewDecl {}
wrap_ViewDecl :: (T_ViewDecl) ->
                 (Inh_ViewDecl) ->
                 (Syn_ViewDecl)
wrap_ViewDecl (sem) ((Inh_ViewDecl )) =
    let 
    in  (Syn_ViewDecl )
sem_ViewDecl_View_Many_to_Many :: ([ViewEntity]) ->
                                  ([ViewPair]) ->
                                  (T_ViewDecl)
sem_ViewDecl_View_Many_to_Many (entities_) (kv_pairs_) =
    let 
    in  ( )
sem_ViewDecl_View_Many_to_One :: ([ViewEntity]) ->
                                 (T_ViewPair) ->
                                 (T_ViewDecl)
sem_ViewDecl_View_Many_to_One (entities_) (kv_pair_) =
    let 
    in  ( )
sem_ViewDecl_View_One_to_Many :: (T_ViewEntity) ->
                                 ([ViewPair]) ->
                                 (T_ViewDecl)
sem_ViewDecl_View_One_to_Many (entity_) (kv_pairs_) =
    let 
    in  ( )
sem_ViewDecl_View_Plain :: (T_ViewEntity) ->
                           (T_ViewPair) ->
                           (T_ViewDecl)
sem_ViewDecl_View_Plain (entity_) (kv_pair_) =
    let 
    in  ( )
-- ViewDecls ---------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:

-}
{-
   local variables for ViewDecls.Cons:

-}
{-
   local variables for ViewDecls.Nil:

-}
-- semantic domain
type T_ViewDecls = ( )
-- cata
sem_ViewDecls :: (ViewDecls) ->
                 (T_ViewDecls)
sem_ViewDecls (list) =
    (Prelude.foldr (sem_ViewDecls_Cons) (sem_ViewDecls_Nil) ((Prelude.map sem_ViewDecl list)))
data Inh_ViewDecls = Inh_ViewDecls {}
data Syn_ViewDecls = Syn_ViewDecls {}
wrap_ViewDecls :: (T_ViewDecls) ->
                  (Inh_ViewDecls) ->
                  (Syn_ViewDecls)
wrap_ViewDecls (sem) ((Inh_ViewDecls )) =
    let 
    in  (Syn_ViewDecls )
sem_ViewDecls_Cons :: (T_ViewDecl) ->
                      (T_ViewDecls) ->
                      (T_ViewDecls)
sem_ViewDecls_Cons (hd_) (tl_) =
    let 
    in  ( )
sem_ViewDecls_Nil :: (T_ViewDecls)
sem_ViewDecls_Nil  =
    let 
    in  ( )
-- ViewDefn ----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      pp                   : Doc

-}
{-
   local variables for ViewDefn.View:

-}
-- semantic domain
type T_ViewDefn = ( (Doc))
-- cata
sem_ViewDefn :: (ViewDefn) ->
                (T_ViewDefn)
sem_ViewDefn ((View (_lang_id) (_decls))) =
    (sem_ViewDefn_View (_lang_id) ((sem_ViewDecls (_decls))))
data Inh_ViewDefn = Inh_ViewDefn {}
data Syn_ViewDefn = Syn_ViewDefn {pp_Syn_ViewDefn :: Doc}
wrap_ViewDefn :: (T_ViewDefn) ->
                 (Inh_ViewDefn) ->
                 (Syn_ViewDefn)
wrap_ViewDefn (sem) ((Inh_ViewDefn )) =
    let ( s1) =
            (sem )
    in  (Syn_ViewDefn (s1))
sem_ViewDefn_View :: (String) ->
                     (T_ViewDecls) ->
                     (T_ViewDefn)
sem_ViewDefn_View (lang_id_) (decls_) =
    let _lhsOpp :: (Doc)
        -- "OutputAsdl.ag"(line 46, column 17)
        (_lhsOpp@_) =
            text "View to do"
    in  ( _lhsOpp)
-- ViewEntity --------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:

-}
{-
   local variables for ViewEntity.ConstrView:

-}
{-
   local variables for ViewEntity.ModuleView:

-}
{-
   local variables for ViewEntity.TypeView:

-}
-- semantic domain
type T_ViewEntity = ( )
-- cata
sem_ViewEntity :: (ViewEntity) ->
                  (T_ViewEntity)
sem_ViewEntity ((ConstrView (_module_qual) (_con_id))) =
    (sem_ViewEntity_ConstrView (_module_qual) (_con_id))
sem_ViewEntity ((ModuleView (_name))) =
    (sem_ViewEntity_ModuleView (_name))
sem_ViewEntity ((TypeView (_module_qual) (_type_id))) =
    (sem_ViewEntity_TypeView (_module_qual) (_type_id))
data Inh_ViewEntity = Inh_ViewEntity {}
data Syn_ViewEntity = Syn_ViewEntity {}
wrap_ViewEntity :: (T_ViewEntity) ->
                   (Inh_ViewEntity) ->
                   (Syn_ViewEntity)
wrap_ViewEntity (sem) ((Inh_ViewEntity )) =
    let 
    in  (Syn_ViewEntity )
sem_ViewEntity_ConstrView :: (String) ->
                             (String) ->
                             (T_ViewEntity)
sem_ViewEntity_ConstrView (module_qual_) (con_id_) =
    let 
    in  ( )
sem_ViewEntity_ModuleView :: (String) ->
                             (T_ViewEntity)
sem_ViewEntity_ModuleView (name_) =
    let 
    in  ( )
sem_ViewEntity_TypeView :: (String) ->
                           (String) ->
                           (T_ViewEntity)
sem_ViewEntity_TypeView (module_qual_) (type_id_) =
    let 
    in  ( )
-- ViewPair ----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:

-}
{-
   local variables for ViewPair.Tuple:

-}
-- semantic domain
type T_ViewPair = ( )
-- cata
sem_ViewPair :: (ViewPair) ->
                (T_ViewPair)
sem_ViewPair (( prop,value)) =
    (sem_ViewPair_Tuple (prop) (value))
data Inh_ViewPair = Inh_ViewPair {}
data Syn_ViewPair = Syn_ViewPair {}
wrap_ViewPair :: (T_ViewPair) ->
                 (Inh_ViewPair) ->
                 (Syn_ViewPair)
wrap_ViewPair (sem) ((Inh_ViewPair )) =
    let 
    in  (Syn_ViewPair )
sem_ViewPair_Tuple :: (Property) ->
                      (TextValue) ->
                      (T_ViewPair)
sem_ViewPair_Tuple (prop_) (value_) =
    let 
    in  ( )



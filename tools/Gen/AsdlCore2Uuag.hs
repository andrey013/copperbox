{-# OPTIONS -fglasgow-exts #-}
-- do not edit; automatically generated by UU.AG

module Gen.AsdlCore2Uuag 
  ( transformToUuag
  ) where


import Base.Lib
import qualified Gen.UuagAbsSyn as AG
import Util.Naming

import Data.Either



transformToUuag :: AsdlSpec -> AG.Ag
transformToUuag spec = toElems_Syn_AsdlSpec synthesized
    where synthesized = wrap_AsdlSpec (sem_AsdlSpec spec) inherited
          inherited   = Inh_AsdlSpec


buildBody :: [(Cardinality, Maybe String, String)] -> AG.TypeBody
buildBody [(Zom,Nothing,conid)]     = AG.TB_List (AG.NamedType $ haskellName conid)
buildBody [(Opt,Nothing,conid)]     = AG.TB_Maybe (AG.NamedType $ haskellName conid)
buildBody xs                        = AG.TB_Tuple (tupleFields xs)


typeFromField (AG.LabelledField _ typ)   = typ
typeFromField (AG.ConstrField conid)     = AG.NamedType conid
        
buildLabelledField :: (Cardinality, String, String) -> AG.Field
buildLabelledField (Zom,name,tyname)  
  = AG.LabelledField name (AG.CodeBlock $ "[" ++ haskellName tyname ++ "]")
buildLabelledField (Opt,name,tyname)  
  = AG.LabelledField name (AG.CodeBlock $ "(Maybe " ++ haskellName tyname ++ ")")        
buildLabelledField (_,name,tyname)  
  = AG.LabelledField name (AG.NamedType $ haskellName tyname)



nameLabel :: [(Cardinality, Maybe String, String)] -> [(Cardinality, String, String)]
nameLabel xs = reverse $ fst $ foldl fn ([],1) xs
  where fn (acc,i) (c,Nothing,t) = ((c,'x':show i,t):acc,i+1)
        fn (acc,i) (c,Just s, t) = ((c,s,t):acc,i+1)
  

tupleFields :: [(Cardinality, Maybe String, String)] -> [AG.Field]
tupleFields xs = map buildLabelledField (nameLabel xs)
 


                       
  
-- AsdlPrim ----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      typename             : String

-}
{-
   local variables for AsdlPrim.TyExternalPrim:

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
type T_AsdlPrim = ( (String))
-- cata
sem_AsdlPrim :: (AsdlPrim) ->
                (T_AsdlPrim)
sem_AsdlPrim ((TyExternalPrim (_name))) =
    (sem_AsdlPrim_TyExternalPrim (_name))
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
data Syn_AsdlPrim = Syn_AsdlPrim {typename_Syn_AsdlPrim :: String}
wrap_AsdlPrim :: (T_AsdlPrim) ->
                 (Inh_AsdlPrim) ->
                 (Syn_AsdlPrim)
wrap_AsdlPrim (sem) ((Inh_AsdlPrim )) =
    let ( s1) =
            (sem )
    in  (Syn_AsdlPrim (s1))
sem_AsdlPrim_TyExternalPrim :: (String) ->
                               (T_AsdlPrim)
sem_AsdlPrim_TyExternalPrim (name_) =
    let _lhsOtypename :: (String)
        -- "AsdlCore2Uuag.ag"(line 76, column 23)
        (_lhsOtypename@_) =
            name_
    in  ( _lhsOtypename)
sem_AsdlPrim_TyIdentifier :: (T_AsdlPrim)
sem_AsdlPrim_TyIdentifier  =
    let _lhsOtypename :: (String)
        -- "AsdlCore2Uuag.ag"(line 74, column 23)
        (_lhsOtypename@_) =
            "String"
    in  ( _lhsOtypename)
sem_AsdlPrim_TyInt :: (T_AsdlPrim)
sem_AsdlPrim_TyInt  =
    let _lhsOtypename :: (String)
        -- "AsdlCore2Uuag.ag"(line 73, column 23)
        (_lhsOtypename@_) =
            "Int"
    in  ( _lhsOtypename)
sem_AsdlPrim_TyRef :: (String) ->
                      (T_AsdlPrim)
sem_AsdlPrim_TyRef (name_) =
    let _lhsOtypename :: (String)
        -- "AsdlCore2Uuag.ag"(line 75, column 23)
        (_lhsOtypename@_) =
            name_
    in  ( _lhsOtypename)
sem_AsdlPrim_TyString :: (T_AsdlPrim)
sem_AsdlPrim_TyString  =
    let _lhsOtypename :: (String)
        -- "AsdlCore2Uuag.ag"(line 72, column 23)
        (_lhsOtypename@_) =
            "String"
    in  ( _lhsOtypename)
sem_AsdlPrim_TyUnit :: (T_AsdlPrim)
sem_AsdlPrim_TyUnit  =
    let _lhsOtypename :: (String)
        -- "AsdlCore2Uuag.ag"(line 71, column 23)
        (_lhsOtypename@_) =
            "()"
    in  ( _lhsOtypename)
-- AsdlSpec ----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      toElems              : [AG.Elem]

-}
{-
   local variables for AsdlSpec.AsdlSpec:

-}
-- semantic domain
type T_AsdlSpec = ( ([AG.Elem]))
-- cata
sem_AsdlSpec :: (AsdlSpec) ->
                (T_AsdlSpec)
sem_AsdlSpec ((AsdlSpec (_decls))) =
    (sem_AsdlSpec_AsdlSpec ((sem_Decls (_decls))))
data Inh_AsdlSpec = Inh_AsdlSpec {}
data Syn_AsdlSpec = Syn_AsdlSpec {toElems_Syn_AsdlSpec :: [AG.Elem]}
wrap_AsdlSpec :: (T_AsdlSpec) ->
                 (Inh_AsdlSpec) ->
                 (Syn_AsdlSpec)
wrap_AsdlSpec (sem) ((Inh_AsdlSpec )) =
    let ( s1) =
            (sem )
    in  (Syn_AsdlSpec (s1))
sem_AsdlSpec_AsdlSpec :: (T_Decls) ->
                         (T_AsdlSpec)
sem_AsdlSpec_AsdlSpec (decls_) =
    let _lhsOtoElems :: ([AG.Elem])
        _declsItoElems :: ([[AG.Elem]])
        ( _declsItoElems) =
            (decls_ )
        -- "AsdlCore2Uuag.ag"(line 22, column 17)
        (_lhsOtoElems@_) =
            concat _declsItoElems
    in  ( _lhsOtoElems)
-- AsdlType ----------------------------------------------------
{-
   inherited attributes:
      name                 : String

   chained attributes:

   synthesised attributes:
      conv                 : AG.Elem

-}
{-
   local variables for AsdlType.Prod:

-}
{-
   local variables for AsdlType.Sum:

-}
-- semantic domain
type T_AsdlType = (String) ->
                  ( (AG.Elem))
-- cata
sem_AsdlType :: (AsdlType) ->
                (T_AsdlType)
sem_AsdlType ((Prod (_fields))) =
    (sem_AsdlType_Prod ((sem_Fields (_fields))))
sem_AsdlType ((Sum (_constrs))) =
    (sem_AsdlType_Sum ((sem_Constrs (_constrs))))
data Inh_AsdlType = Inh_AsdlType {name_Inh_AsdlType :: String}
data Syn_AsdlType = Syn_AsdlType {conv_Syn_AsdlType :: AG.Elem}
wrap_AsdlType :: (T_AsdlType) ->
                 (Inh_AsdlType) ->
                 (Syn_AsdlType)
wrap_AsdlType (sem) ((Inh_AsdlType (i1))) =
    let ( s1) =
            (sem (i1))
    in  (Syn_AsdlType (s1))
sem_AsdlType_Prod :: (T_Fields) ->
                     (T_AsdlType)
sem_AsdlType_Prod (fields_) =
    \ _lhsIname ->
        let _lhsOconv :: (AG.Elem)
            _fieldsIftriple :: ([(Cardinality, Maybe String, String)])
            ( _fieldsIftriple) =
                (fields_ )
            -- "AsdlCore2Uuag.ag"(line 45, column 17)
            (_lhsOconv@_) =
                AG.Type (haskellName _lhsIname)
                        (buildBody _fieldsIftriple)
        in  ( _lhsOconv)
sem_AsdlType_Sum :: (T_Constrs) ->
                    (T_AsdlType)
sem_AsdlType_Sum (constrs_) =
    \ _lhsIname ->
        let _lhsOconv :: (AG.Elem)
            _constrsItoDataAlt :: ([AG.DataAlt])
            ( _constrsItoDataAlt) =
                (constrs_ )
            -- "AsdlCore2Uuag.ag"(line 43, column 17)
            (_lhsOconv@_) =
                AG.Data (haskellName _lhsIname) _constrsItoDataAlt
        in  ( _lhsOconv)
-- Cardinality -------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      identity             : SELF

-}
{-
   local variables for Cardinality.One:
      identity

-}
{-
   local variables for Cardinality.Opt:
      identity

-}
{-
   local variables for Cardinality.Zom:
      identity

-}
-- semantic domain
type T_Cardinality = ( (Cardinality))
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
data Syn_Cardinality = Syn_Cardinality {identity_Syn_Cardinality :: Cardinality}
wrap_Cardinality :: (T_Cardinality) ->
                    (Inh_Cardinality) ->
                    (Syn_Cardinality)
wrap_Cardinality (sem) ((Inh_Cardinality )) =
    let ( s1) =
            (sem )
    in  (Syn_Cardinality (s1))
sem_Cardinality_One :: (T_Cardinality)
sem_Cardinality_One  =
    let _lhsOidentity :: (Cardinality)
        -- self rule
        (_identity@_) =
            One
        -- self rule
        (_lhsOidentity@_) =
            _identity
    in  ( _lhsOidentity)
sem_Cardinality_Opt :: (T_Cardinality)
sem_Cardinality_Opt  =
    let _lhsOidentity :: (Cardinality)
        -- self rule
        (_identity@_) =
            Opt
        -- self rule
        (_lhsOidentity@_) =
            _identity
    in  ( _lhsOidentity)
sem_Cardinality_Zom :: (T_Cardinality)
sem_Cardinality_Zom  =
    let _lhsOidentity :: (Cardinality)
        -- self rule
        (_identity@_) =
            Zom
        -- self rule
        (_lhsOidentity@_) =
            _identity
    in  ( _lhsOidentity)
-- Constr ------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      toDataAlt            : AG.DataAlt

-}
{-
   local variables for Constr.Constr:

-}
-- semantic domain
type T_Constr = ( (AG.DataAlt))
-- cata
sem_Constr :: (Constr) ->
              (T_Constr)
sem_Constr ((Constr (_label) (_fields))) =
    (sem_Constr_Constr (_label) ((sem_Fields (_fields))))
data Inh_Constr = Inh_Constr {}
data Syn_Constr = Syn_Constr {toDataAlt_Syn_Constr :: AG.DataAlt}
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
    let _lhsOtoDataAlt :: (AG.DataAlt)
        _fieldsIftriple :: ([(Cardinality, Maybe String, String)])
        ( _fieldsIftriple) =
            (fields_ )
        -- "AsdlCore2Uuag.ag"(line 53, column 17)
        (_lhsOtoDataAlt@_) =
            let xs::(AG.Fields)
                xs = map buildLabelledField (nameLabel _fieldsIftriple)
            in  AG.DataAlt label_ xs
    in  ( _lhsOtoDataAlt)
-- Constrs -----------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      toDataAlt            : [AG.DataAlt]

-}
{-
   local variables for Constrs.Cons:

-}
{-
   local variables for Constrs.Nil:

-}
-- semantic domain
type T_Constrs = ( ([AG.DataAlt]))
-- cata
sem_Constrs :: (Constrs) ->
               (T_Constrs)
sem_Constrs (list) =
    (Prelude.foldr (sem_Constrs_Cons) (sem_Constrs_Nil) ((Prelude.map sem_Constr list)))
data Inh_Constrs = Inh_Constrs {}
data Syn_Constrs = Syn_Constrs {toDataAlt_Syn_Constrs :: [AG.DataAlt]}
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
    let _lhsOtoDataAlt :: ([AG.DataAlt])
        _hdItoDataAlt :: (AG.DataAlt)
        _tlItoDataAlt :: ([AG.DataAlt])
        ( _hdItoDataAlt) =
            (hd_ )
        ( _tlItoDataAlt) =
            (tl_ )
        -- use rule
        (_lhsOtoDataAlt@_) =
            _hdItoDataAlt : _tlItoDataAlt
    in  ( _lhsOtoDataAlt)
sem_Constrs_Nil :: (T_Constrs)
sem_Constrs_Nil  =
    let _lhsOtoDataAlt :: ([AG.DataAlt])
        -- use rule
        (_lhsOtoDataAlt@_) =
            []
    in  ( _lhsOtoDataAlt)
-- Decl --------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      toElems              : [AG.Elem]

-}
{-
   local variables for Decl.Module:

-}
-- semantic domain
type T_Decl = ( ([AG.Elem]))
-- cata
sem_Decl :: (Decl) ->
            (T_Decl)
sem_Decl ((Module (_name) (_defs))) =
    (sem_Decl_Module (_name) ((sem_Definitions (_defs))))
data Inh_Decl = Inh_Decl {}
data Syn_Decl = Syn_Decl {toElems_Syn_Decl :: [AG.Elem]}
wrap_Decl :: (T_Decl) ->
             (Inh_Decl) ->
             (Syn_Decl)
wrap_Decl (sem) ((Inh_Decl )) =
    let ( s1) =
            (sem )
    in  (Syn_Decl (s1))
sem_Decl_Module :: (String) ->
                   (T_Definitions) ->
                   (T_Decl)
sem_Decl_Module (name_) (defs_) =
    let _lhsOtoElems :: ([AG.Elem])
        _defsItoElem :: ([AG.Elem])
        ( _defsItoElem) =
            (defs_ )
        -- "AsdlCore2Uuag.ag"(line 30, column 17)
        (_lhsOtoElems@_) =
            _defsItoElem
    in  ( _lhsOtoElems)
-- Decls -------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      toElems              : [[AG.Elem]]

-}
{-
   local variables for Decls.Cons:

-}
{-
   local variables for Decls.Nil:

-}
-- semantic domain
type T_Decls = ( ([[AG.Elem]]))
-- cata
sem_Decls :: (Decls) ->
             (T_Decls)
sem_Decls (list) =
    (Prelude.foldr (sem_Decls_Cons) (sem_Decls_Nil) ((Prelude.map sem_Decl list)))
data Inh_Decls = Inh_Decls {}
data Syn_Decls = Syn_Decls {toElems_Syn_Decls :: [[AG.Elem]]}
wrap_Decls :: (T_Decls) ->
              (Inh_Decls) ->
              (Syn_Decls)
wrap_Decls (sem) ((Inh_Decls )) =
    let ( s1) =
            (sem )
    in  (Syn_Decls (s1))
sem_Decls_Cons :: (T_Decl) ->
                  (T_Decls) ->
                  (T_Decls)
sem_Decls_Cons (hd_) (tl_) =
    let _lhsOtoElems :: ([[AG.Elem]])
        _hdItoElems :: ([AG.Elem])
        _tlItoElems :: ([[AG.Elem]])
        ( _hdItoElems) =
            (hd_ )
        ( _tlItoElems) =
            (tl_ )
        -- use rule
        (_lhsOtoElems@_) =
            _hdItoElems : _tlItoElems
    in  ( _lhsOtoElems)
sem_Decls_Nil :: (T_Decls)
sem_Decls_Nil  =
    let _lhsOtoElems :: ([[AG.Elem]])
        -- use rule
        (_lhsOtoElems@_) =
            []
    in  ( _lhsOtoElems)
-- Definition --------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      toElem               : AG.Elem

-}
{-
   local variables for Definition.Def:

-}
-- semantic domain
type T_Definition = ( (AG.Elem))
-- cata
sem_Definition :: (Definition) ->
                  (T_Definition)
sem_Definition ((Def (_name) (_dtype))) =
    (sem_Definition_Def (_name) ((sem_AsdlType (_dtype))))
data Inh_Definition = Inh_Definition {}
data Syn_Definition = Syn_Definition {toElem_Syn_Definition :: AG.Elem}
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
    let _lhsOtoElem :: (AG.Elem)
        _dtypeIconv :: (AG.Elem)
        _dtypeOname :: (String)
        ( _dtypeIconv) =
            (dtype_ (_dtypeOname))
        -- "AsdlCore2Uuag.ag"(line 38, column 17)
        (_dtypeOname@_) =
            name_
        -- "AsdlCore2Uuag.ag"(line 37, column 17)
        (_lhsOtoElem@_) =
            _dtypeIconv
    in  ( _lhsOtoElem)
-- Definitions -------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      toElem               : [AG.Elem]

-}
{-
   local variables for Definitions.Cons:

-}
{-
   local variables for Definitions.Nil:

-}
-- semantic domain
type T_Definitions = ( ([AG.Elem]))
-- cata
sem_Definitions :: (Definitions) ->
                   (T_Definitions)
sem_Definitions (list) =
    (Prelude.foldr (sem_Definitions_Cons) (sem_Definitions_Nil) ((Prelude.map sem_Definition list)))
data Inh_Definitions = Inh_Definitions {}
data Syn_Definitions = Syn_Definitions {toElem_Syn_Definitions :: [AG.Elem]}
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
    let _lhsOtoElem :: ([AG.Elem])
        _hdItoElem :: (AG.Elem)
        _tlItoElem :: ([AG.Elem])
        ( _hdItoElem) =
            (hd_ )
        ( _tlItoElem) =
            (tl_ )
        -- use rule
        (_lhsOtoElem@_) =
            _hdItoElem : _tlItoElem
    in  ( _lhsOtoElem)
sem_Definitions_Nil :: (T_Definitions)
sem_Definitions_Nil  =
    let _lhsOtoElem :: ([AG.Elem])
        -- use rule
        (_lhsOtoElem@_) =
            []
    in  ( _lhsOtoElem)
-- Field -------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      ftriple              : (Cardinality, Maybe String, String)

-}
{-
   local variables for Field.Field:

-}
-- semantic domain
type T_Field = ( ((Cardinality, Maybe String, String)))
-- cata
sem_Field :: (Field) ->
             (T_Field)
sem_Field ((Field (_qual) (_ptype) (_card) (_opt_name))) =
    (sem_Field_Field ((sem_OptQualifier (_qual))) ((sem_AsdlPrim (_ptype))) ((sem_Cardinality (_card))) ((sem_OptIdentifier (_opt_name))))
data Inh_Field = Inh_Field {}
data Syn_Field = Syn_Field {ftriple_Syn_Field :: (Cardinality, Maybe String, String)}
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
    let _lhsOftriple :: ((Cardinality, Maybe String, String))
        _ptypeItypename :: (String)
        _cardIidentity :: (Cardinality)
        _opt_nameIidentity :: (OptIdentifier)
        ( _ptypeItypename) =
            (ptype_ )
        ( _cardIidentity) =
            (card_ )
        ( _opt_nameIidentity) =
            (opt_name_ )
        -- "AsdlCore2Uuag.ag"(line 65, column 7)
        (_lhsOftriple@_) =
            (_cardIidentity , _opt_nameIidentity, _ptypeItypename)
    in  ( _lhsOftriple)
-- Fields ------------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      ftriple              : [(Cardinality, Maybe String, String)]

-}
{-
   local variables for Fields.Cons:

-}
{-
   local variables for Fields.Nil:

-}
-- semantic domain
type T_Fields = ( ([(Cardinality, Maybe String, String)]))
-- cata
sem_Fields :: (Fields) ->
              (T_Fields)
sem_Fields (list) =
    (Prelude.foldr (sem_Fields_Cons) (sem_Fields_Nil) ((Prelude.map sem_Field list)))
data Inh_Fields = Inh_Fields {}
data Syn_Fields = Syn_Fields {ftriple_Syn_Fields :: [(Cardinality, Maybe String, String)]}
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
    let _lhsOftriple :: ([(Cardinality, Maybe String, String)])
        _hdIftriple :: ((Cardinality, Maybe String, String))
        _tlIftriple :: ([(Cardinality, Maybe String, String)])
        ( _hdIftriple) =
            (hd_ )
        ( _tlIftriple) =
            (tl_ )
        -- use rule
        (_lhsOftriple@_) =
            _hdIftriple : _tlIftriple
    in  ( _lhsOftriple)
sem_Fields_Nil :: (T_Fields)
sem_Fields_Nil  =
    let _lhsOftriple :: ([(Cardinality, Maybe String, String)])
        -- use rule
        (_lhsOftriple@_) =
            []
    in  ( _lhsOftriple)
-- OptIdentifier -----------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:
      identity             : SELF

-}
{-
   local variables for OptIdentifier.Just:
      identity

-}
{-
   local variables for OptIdentifier.Nothing:
      identity

-}
-- semantic domain
type T_OptIdentifier = ( (OptIdentifier))
-- cata
sem_OptIdentifier :: (OptIdentifier) ->
                     (T_OptIdentifier)
sem_OptIdentifier ((Prelude.Just (x))) =
    (sem_OptIdentifier_Just (x))
sem_OptIdentifier (Prelude.Nothing) =
    sem_OptIdentifier_Nothing
data Inh_OptIdentifier = Inh_OptIdentifier {}
data Syn_OptIdentifier = Syn_OptIdentifier {identity_Syn_OptIdentifier :: OptIdentifier}
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
    let _lhsOidentity :: (OptIdentifier)
        -- self rule
        (_identity@_) =
            Just just_
        -- self rule
        (_lhsOidentity@_) =
            _identity
    in  ( _lhsOidentity)
sem_OptIdentifier_Nothing :: (T_OptIdentifier)
sem_OptIdentifier_Nothing  =
    let _lhsOidentity :: (OptIdentifier)
        -- self rule
        (_identity@_) =
            Nothing
        -- self rule
        (_lhsOidentity@_) =
            _identity
    in  ( _lhsOidentity)
-- OptQualifier ------------------------------------------------
{-
   inherited attributes:

   chained attributes:

   synthesised attributes:

-}
{-
   local variables for OptQualifier.Just:

-}
{-
   local variables for OptQualifier.Nothing:

-}
-- semantic domain
type T_OptQualifier = ( )
-- cata
sem_OptQualifier :: (OptQualifier) ->
                    (T_OptQualifier)
sem_OptQualifier ((Prelude.Just (x))) =
    (sem_OptQualifier_Just (x))
sem_OptQualifier (Prelude.Nothing) =
    sem_OptQualifier_Nothing
data Inh_OptQualifier = Inh_OptQualifier {}
data Syn_OptQualifier = Syn_OptQualifier {}
wrap_OptQualifier :: (T_OptQualifier) ->
                     (Inh_OptQualifier) ->
                     (Syn_OptQualifier)
wrap_OptQualifier (sem) ((Inh_OptQualifier )) =
    let 
    in  (Syn_OptQualifier )
sem_OptQualifier_Just :: (String) ->
                         (T_OptQualifier)
sem_OptQualifier_Just (just_) =
    let 
    in  ( )
sem_OptQualifier_Nothing :: (T_OptQualifier)
sem_OptQualifier_Nothing  =
    let 
    in  ( )



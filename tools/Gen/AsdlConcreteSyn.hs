-- do not edit; automatically generated by UU.AG

module Gen.AsdlConcreteSyn where

import Base.PrimitiveTypes




type Property = String
type TextValue = String

-- Alternative type for a files contents.
-- (Naturally) it looses the order they appeared in the file

data Defns = Defns 
  { tdefs :: [ModuleDefn]
  , prims :: [PrimModule]
  , views :: [ViewDefn]
  }


-- AltThree ----------------------------------------------------
data AltThree = Alt1 (ModuleDefn)
              | Alt2 (PrimModule)
              | Alt3 (ViewDefn)
              deriving ( Show)
-- AsdlPrim ----------------------------------------------------
data AsdlPrim = TyIdentifier 
              | TyInt 
              | TyRef (String)
              | TyString 
              | TyUnit 
              deriving ( Show)
-- AsdlSpec ----------------------------------------------------
type AsdlSpec = [(AltThree)]
-- AsdlType ----------------------------------------------------
data AsdlType = Prod (Fields)
              | Sum (Constrs) (OptAttribs)
              deriving ( Show)
-- Attribs -----------------------------------------------------
type Attribs = [(Field)]
-- Cardinality -------------------------------------------------
data Cardinality = One 
                 | Opt 
                 | Zom 
                 deriving ( Eq,Ord,Show)
-- Constr ------------------------------------------------------
data Constr = Constr (String) (Fields)
            deriving ( Show)
-- Constrs -----------------------------------------------------
type Constrs = [(Constr)]
-- Definition --------------------------------------------------
data Definition = Def (String) (AsdlType)
                deriving ( Show)
-- Definitions -------------------------------------------------
type Definitions = [(Definition)]
-- Field -------------------------------------------------------
data Field = Field (OptQualifier) (AsdlPrim) (Cardinality) (OptIdentifier)
           deriving ( Show)
-- Fields ------------------------------------------------------
type Fields = [(Field)]
-- ImportStmt --------------------------------------------------
data ImportStmt = ImportStmt (String)
                deriving ( Show)
-- ImportStmts -------------------------------------------------
type ImportStmts = [(ImportStmt)]
-- ModuleDefn --------------------------------------------------
data ModuleDefn = Module (String) (ImportStmts) (Definitions)
                deriving ( Show)
-- ModuleDefns -------------------------------------------------
type ModuleDefns = [(ModuleDefn)]
-- OptAttribs --------------------------------------------------
type OptAttribs = (Maybe (Attribs))
-- OptIdentifier -----------------------------------------------
type OptIdentifier = (Maybe (String))
-- OptQualifier ------------------------------------------------
type OptQualifier = (Maybe (String))
-- PrimModule --------------------------------------------------
data PrimModule = PrimModule (String) ([String])
                deriving ( Show)
-- ViewDecl ----------------------------------------------------
data ViewDecl = View_Many_to_Many ([ViewEntity]) ([ViewPair])
              | View_Many_to_One ([ViewEntity]) (ViewPair)
              | View_One_to_Many (ViewEntity) ([ViewPair])
              | View_Plain (ViewEntity) (ViewPair)
              deriving ( Show)
-- ViewDecls ---------------------------------------------------
type ViewDecls = [(ViewDecl)]
-- ViewDefn ----------------------------------------------------
data ViewDefn = View (String) (ViewDecls)
              deriving ( Show)
-- ViewEntity --------------------------------------------------
data ViewEntity = ConstrView (String) (String)
                | ModuleView (String)
                | TypeView (String) (String)
                deriving ( Show)
-- ViewPair ----------------------------------------------------
type ViewPair = ( (Property),(TextValue))



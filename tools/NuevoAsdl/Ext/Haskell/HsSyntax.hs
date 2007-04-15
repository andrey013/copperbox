-- OLD - uses Language.Haskell.Syntax

module Ext.Haskell.HsSyntax where

import Control.Monad.State
import Data.Char
import Data.List
import Language.Haskell.Syntax





data St = St { src_loc :: SrcLoc } 
  deriving (Show)

location = do {loc <- gets src_loc; return loc}

-- what combinators for whitespace ?

whiteNewline :: CodeMonad ()
whiteNewline = do 
  loc <- gets src_loc
  let line = 1 + (srcLine loc)
  modify (\s -> s { src_loc=loc{srcLine=line} }) 



type CodeMonad a = State St a

st0 = St { src_loc = SrcLoc {srcFilename="<unknown>", srcLine=0, srcColumn=0}}

runCode a = evalState a st0


wrapList []  = Nothing
wrapList xs  = Just xs
        
        
----------------------------------------------------------
-- Type synonyms
----------------------------------------------------------

type S a = State St a

type ModuleS        = State St HsModule
type ModuleNameS    = State St Module
type ImportS        = State St HsImportDecl

type PatS           = State St HsPat
type FieldPatS      = State St HsPatField
type ExpS           = State St HsExp
type DeclS          = State St HsDecl
type MatchS         = State St HsMatch
type RhsS           = State St HsRhs
type GuardS         = State St HsGuardedAlts
type StmtS          = State St HsStmt

type GuardedAltS    = State St HsGuardedAlt
type NameS          = State St HsName
type QOpS           = State St HsQOp
type OpS            = State St HsOp
type AltS           = State St HsAlt
type TypeS          = State St HsType
type QTypeS         = State St HsQualType
type CxtS           = State St HsContext
type ConS           = State St HsConDecl
type BangTypeS      = State St HsBangType 
type VarBangTypeS   = State St ([HsName], HsBangType) 

----------------------------------------------------------
-- Lowercase pattern syntax functions
----------------------------------------------------------

intPrimL    :: Integer -> HsLiteral
intPrimL    = HsIntPrim
floatPrimL  :: Rational -> HsLiteral
floatPrimL  = HsFloatPrim
doublePrimL :: Rational -> HsLiteral
doublePrimL = HsDoublePrim
integerL    :: Integer -> HsLiteral
integerL    = HsInt
charL       :: Char -> HsLiteral
charL       = HsChar
stringL     :: String -> HsLiteral
stringL     = HsString
rationalL   :: Rational -> HsLiteral
rationalL   = HsFrac


intL    :: Int -> HsLiteral
intL    = HsInt . fromIntegral



litP :: HsLiteral -> PatS
litP l = return (HsPLit l)

varP :: HsName -> PatS
varP v = return (HsPVar v)

tupP :: [PatS] -> PatS
tupP ps = do { ps1 <- sequence ps; return (HsPTuple ps1)}

conP :: HsQName -> [PatS] -> PatS
conP n ps = do ps' <- sequence ps
               return (HsPApp n ps')
               
infixP :: PatS -> HsQName -> PatS -> PatS
infixP p1 n p2 = do p1' <- p1
                    p2' <- p2
                    return (HsPInfixApp p1' n p2')
                   
tildeP :: PatS -> PatS
tildeP p = do p' <- p
              return (HsPIrrPat p')
              
asP :: HsName -> PatS -> PatS
asP n p = do p' <- p
             return (HsPAsPat n p')
             
wildP :: PatS
wildP = return HsPWildCard

recP :: HsQName -> [FieldPatS] -> PatS
recP n fps = do fps' <- sequence fps
                return (HsPRec n fps')
                
listP :: [PatS] -> PatS
listP ps = do ps' <- sequence ps
              return (HsPList ps')
              
-- no sigP

fieldPat :: HsQName -> PatS -> FieldPatS
fieldPat n p = do p' <- p
                  return (HsPFieldPat n p')
                  
-------------------------------------------------------------------------------
--     Stmt

bindS :: PatS -> ExpS -> StmtS
bindS p e = do whiteNewline
               loc <- location 
               liftM2 (HsGenerator loc) p e


letS :: [DeclS] -> StmtS
letS ds = do { ds1 <- sequence ds; return (HsLetStmt ds1) }

noBindS :: ExpS -> StmtS
noBindS e = do { e1 <- e; return (HsQualifier e1) }

-- no parS

-------------------------------------------------------------------------------
--     Range


                         
-------------------------------------------------------------------------------
--     Rhs (aka Body)

normalRhs :: ExpS -> RhsS
normalRhs e = do { e1 <- e; return (HsUnGuardedRhs e1) }

guardedRhs :: [S HsGuardedRhs] -> RhsS
guardedRhs ges = do { ges' <- sequence ges; return (HsGuardedRhss ges') }

-------------------------------------------------------------------------------
--     Guard

normalG :: ExpS -> GuardS
normalG e = do { e1 <- e; return (HsUnGuardedAlt e1) }

normalGE :: ExpS -> ExpS -> S (HsGuardedAlts, HsExp)
normalGE g e = do { g1 <- g; e1 <- e; return (HsUnGuardedAlt g1, e1) }

patG :: [GuardedAltS] -> GuardS
patG ss = do { ss' <- sequence ss; return (HsGuardedAlts ss') }

patGE :: [GuardedAltS] -> ExpS -> S (HsGuardedAlts, HsExp)
patGE ss e = do { ss' <- sequence ss;
                  e'  <- e;
                  return (HsGuardedAlts ss', e') }
                  
-------------------------------------------------------------------------------
--     Match (no clause)


match :: HsName -> [PatS] -> RhsS -> [DeclS] -> MatchS
match n ps r ds = do { loc <- location;
                        -- n' <- n;
                        ps' <- sequence ps;
                        r' <- r;
                        ds' <- sequence ds;
                        return (HsMatch loc n ps' r' ds') }
                      
---------------------------------------------------------------------------
--     Exp

dyn :: String -> S HsExp 
dyn s = return (HsVar (mkQName s))

global :: HsQName -> ExpS
global s = return (HsVar s)

varE :: HsQName -> ExpS
varE s = return (HsVar s)

conE :: HsQName -> ExpS
conE s =  return (HsCon s)

litE :: HsLiteral -> ExpS
litE c = return (HsLit c)

appE :: ExpS -> ExpS -> ExpS
appE x y = do { a <- x; b <- y; return (HsApp a b)}


infixE :: ExpS -> QOpS -> ExpS -> ExpS
infixE x s y = do { a <- x; s' <- s; b <- y;
                    return (HsInfixApp a s' b)}
                    
lamE :: [PatS] -> ExpS -> ExpS
lamE ps e = do loc <- location
               ps' <- sequence ps
               e' <- e
               return (HsLambda loc ps' e')

lam1E :: PatS -> ExpS -> ExpS    -- Single-arg lambda
lam1E p e = lamE [p] e

tupE :: [ExpS] -> ExpS
tupE es = do { es1 <- sequence es; return (HsTuple es1)}

condE :: ExpS -> ExpS -> ExpS -> ExpS
condE x y z =  do { a <- x; b <- y; c <- z; return (HsIf a b c)}

letE :: [DeclS] -> ExpS -> ExpS
letE ds e = do { ds2 <- sequence ds; e2 <- e; return (HsLet ds2 e2) }

caseE :: ExpS -> [AltS] -> ExpS
caseE e ms = do { e1 <- e; ms1 <- sequence ms; return (HsCase e1 ms1) } 

doE :: [StmtS] -> ExpS
doE ss = do { ss1 <- sequence ss; return (HsDo ss1) } 

compE :: ExpS -> [StmtS] -> ExpS
compE e ss = do { e' <- e; ss1 <- sequence ss; return (HsListComp e' ss1) } 

{-
arithSeqE :: RangeS -> ExpS
arithSeqE r = do { r' <- r; return (ArithSeqE r') }  
-}                      

fromE :: ExpS -> ExpS
fromE x = do { a <- x; return (HsEnumFrom a) }  

fromThenE :: ExpS -> ExpS -> ExpS
fromThenE x y = do { a <- x; b <- y; return (HsEnumFromThen a b) }  

fromToE :: ExpS -> ExpS -> ExpS
fromToE x y = do { a <- x; b <- y; return (HsEnumFromTo a b) }  

fromThenToE :: ExpS -> ExpS -> ExpS -> ExpS
fromThenToE x y z = do { a <- x; b <- y; c <- z;
                         return (HsEnumFromThenTo a b c) } 
                         
listE :: [ExpS] -> ExpS
listE es = do { es1 <- sequence es; return (HsList es1) }

parenE x = do { a <- x; return (HsParen a)}


sigE :: ExpS -> QTypeS -> ExpS
sigE e t = do { loc <- location; 
                e1 <- e; t1 <- t; 
                return (HsExpTypeSig loc e1 t1) }

recConE :: HsQName -> [S HsFieldUpdate] -> ExpS
recConE c fs = do { flds <- sequence fs; return (HsRecConstr c flds) }

recUpdE :: ExpS -> [S HsFieldUpdate] -> ExpS
recUpdE e fs = do { e1 <- e; flds <- sequence fs; return (HsRecUpdate e1 flds) }

stringE :: String -> ExpS
stringE = litE . stringL

fieldExp :: HsQName -> ExpS -> S HsFieldUpdate
fieldExp s e = do { e' <- e; return (HsFieldUpdate s e') }

-------------------------------------------------------------------------------
--     Dec

-- valD

funD :: [MatchS] -> DeclS
funD cs = 
 do { cs1 <- sequence cs
    ; return (HsFunBind cs1)
    }
    
tyD :: HsName -> [HsName] -> TypeS -> DeclS
tyD tc tvs rhs = do { loc <- location; 
                      rhs1 <- rhs; 
                      return (HsTypeDecl loc tc tvs rhs1) }


dataD :: CxtS -> HsName -> [HsName] -> [ConS] -> [HsQName] -> DeclS
dataD ctxt tc tvs cons derivs =
  do
    loc <- location
    ctxt1 <- ctxt
    cons1 <- sequence cons
    return (HsDataDecl loc ctxt1 tc tvs cons1 derivs)
        
newtypeD :: CxtS -> HsName -> [HsName] -> ConS -> [HsQName] -> DeclS
newtypeD ctxt tc tvs con derivs =
  do
    loc <- location
    ctxt1 <- ctxt
    con1 <- con
    return (HsNewTypeDecl loc ctxt1 tc tvs con1 derivs)
    
    
classD :: CxtS -> HsName -> [HsName] -> [DeclS] -> DeclS
classD ctxt cls tvs decs =
  do 
    loc <- location
    decs1 <- sequence decs
    ctxt1 <- ctxt
    return (HsClassDecl loc ctxt1 cls tvs decs1)

instanceD :: CxtS -> HsQName -> [TypeS] -> [DeclS] -> DeclS
instanceD ctxt cls tvs decs =
  do 
    loc <- location
    ctxt1 <- ctxt
    tvs1 <- sequence tvs
    decs1 <- sequence decs
    return (HsInstDecl loc ctxt1 cls tvs1 decs1)

sigD :: [HsName] -> QTypeS -> DeclS
sigD funs ty = do { loc <- location;
                    liftM (HsTypeSig loc funs) $ ty }
    


normalC :: HsName -> [BangTypeS] -> ConS
normalC con strtys = do { loc <- location; 
                          liftM (HsConDecl loc con) $ sequence strtys }

recC :: HsName -> [VarBangTypeS] -> ConS
recC con varstrtys = do { loc <- location; 
                          liftM (HsRecDecl loc con) $ sequence varstrtys}



-------------------------------------------------------------------------------
--     Type

qualT :: S HsContext -> TypeS -> S HsQualType
qualT ctx ty = do {ctx1 <- ctx;
                   ty1 <- ty;
                   return (HsQualType ctx1 ty1) }

funT :: TypeS -> TypeS -> TypeS
funT x y  = do { a <- x; b <- y; return (HsTyFun a b) }
                 
                 
tupleT :: [TypeS] -> TypeS
tupleT ts = do { ts1 <- sequence ts;
                 return (HsTyTuple ts1) }


varT :: HsName -> TypeS
varT = return . HsTyVar

conT :: HsQName -> TypeS
conT = return . HsTyCon

appT :: TypeS -> TypeS -> TypeS
appT t1 t2 = do
           t1' <- t1
           t2' <- t2
           return $ HsTyApp t1' t2'
                   
empty_ctx :: S HsContext    
empty_ctx = return []

varOp :: HsName -> OpS
varOp s = return (HsVarOp s)

conOp :: HsName -> OpS
conOp s = return (HsConOp s)


qvarOp :: HsQName -> QOpS
qvarOp s = return (HsQVarOp s)

qconOp :: HsQName -> QOpS
qconOp s = return (HsQConOp s)

cvarName :: HsName -> S HsCName 
cvarName s = return (HsVarName s)

cconName :: HsName -> S HsCName 
cconName s = return (HsVarName s) 

moduleD mn exs ids ds = do
  loc <- location
  mn1 <- mn
  exs1 <- sequence exs
  ids1 <- sequence ids
  ds1 <- sequence ds
  return $ HsModule loc mn1 (wrapList exs1) ids1 ds1 

moduleN :: String -> ModuleNameS
moduleN s = return (Module s)


importD :: ModuleNameS -> Bool -> Maybe (ModuleNameS) -> Maybe (S (Bool, [HsImportSpec]))
          -> ImportS
importD nm qual (Just as) (Just sps) = do
  loc <- location
  nm1 <- nm
  as1 <- as
  sps1 <- sps
  return $  HsImportDecl 
              { importLoc       = loc
              , importModule    = nm1
              , importQualified = qual
              , importAs        = Just as1
              , importSpecs     = Just sps1
              }
  

importD nm qual Nothing (Just sps) = do
  loc <- location
  nm1 <- nm
  sps1 <- sps
  return $  HsImportDecl 
              { importLoc       = loc
              , importModule    = nm1
              , importQualified = qual
              , importAs        = Nothing
              , importSpecs     = Just sps1
              }
                
importD nm qual (Just as) Nothing = do
  loc <- location
  nm1 <- nm
  as1 <- as
  return $  HsImportDecl 
              { importLoc       = loc
              , importModule    = nm1
              , importQualified = qual
              , importAs        = Just as1
              , importSpecs     = Nothing
              }

importD nm qual Nothing Nothing = do
  loc <- location
  nm1 <- nm
  return $  HsImportDecl 
              { importLoc       = loc
              , importModule    = nm1
              , importQualified = qual
              , importAs        = Nothing
              , importSpecs     = Nothing
              }   
              
                           
mkName :: String -> HsName
mkName s@(x:_)
  | isLetter x  = HsIdent s
  |otherwise    = HsSymbol s
  
  
mkQName :: String -> HsQName
mkQName s = case elts of 
          (n,"") -> UnQual(HsIdent n)
          (n,q) -> Qual (Module q) (HsIdent n)        
  where
    elts = let (one,rest) = foldr sep ("",[]) s
           in remake $ one:rest
    
    remake []   = error "empty name"
    remake [a]  = (a,[])
    remake xs   = (last xs, dotSep $ take ((length xs) - 1) xs)
    
    dotSep = concat . (intersperse ".")
            
    sep '.' (xs,yss) = ("",xs:yss)
    sep x   (xs,yss) = (x:xs,yss)
    
    

{-# OPTIONS -fglasgow-exts #-}

module Ext.Haskell.CodeGen where

import Ext.Haskell.HsSyntax
import Ext.Haskell.HsCodeFragments

import Data.List
import Language.Haskell.Syntax

import Base.Lib 

numzip :: Int -> [a] -> [(Int,a)]       
numzip off xs = zip ys xs
  where ys = [off..off+(length xs)]    




writePickleSig :: String -> DeclS 
writePickleSig name = do 
  td <- mkTypeDesc 
  sigD [fun_name] td
  where fun_name = mkName $ "write" ++ name
        mkTypeDesc = return $ qualT empty_ctx (funT (conT $ mkQName name)
                                               (appT (conT $ mkQName "CM")
                                               (conT unit_con_name)))



readPickleSig :: String -> DeclS 
readPickleSig name = do 
  td <- mkTypeDesc 
  sigD [fun_name] td
  where fun_name = mkName $ "read" ++ name
        mkTypeDesc = return $ qualT empty_ctx (appT (conT $ mkQName "CM")
                                                    (conT $ mkQName name))
                                               
                                                 
tuplePicklerD :: String -> [(String, Cardinality)] -> DeclS                                               
tuplePicklerD name ty_elts = do
  ps <- mkPats
  rhs <- mkRhs
  funD [match funname ps rhs []] 
  where 
    funname = mkName ("write" ++ name)
    mkPats = return [tuplePat (length ty_elts)]
    mkRhs  = return $ normalRhs (doE (writeTypeStmtsS ty_elts))
    
constrPicklerD :: String -> String -> Int -> [(String, Cardinality)] -> DeclS
constrPicklerD name constr_name tag_no ty_elts = do
  ps <- mkPats
  rhs <- mkRhs
  funD [match funname ps rhs []] 
  where 
    funname = mkName ("write" ++ name)
    mkPats = return [constrPat constr_name (length ty_elts)]
              
    mkRhs  = return $ normalRhs (doE (writeTagS tag_no : writeTypeStmtsS ty_elts))


dtypeReadD :: String -> [(String,[(String, Cardinality)])] -> DeclS
dtypeReadD name defs  = do   
  rhs <- mkRhs
  funD [match funname [] rhs inners ]
  where
    funname = mkName ("read" ++ name)    
    mkRhs = return $ normalRhs (doE [s1,s2])
    s1 = bindS (varP (mkName "i")) (varE (mkQName "read_tag"))
    s2 = noBindS (appE (varE (mkQName $ "read" ++ name ++ "'")) 
                       (varE (mkQName "i")))
    inners = map (uncurry (flip (mkConstrReadD name))) (numzip 1 defs)


mkConstrReadD :: String -> (String,[(String, Cardinality)]) -> Int -> DeclS
mkConstrReadD name (cname,xs) tag_no = do
  pats <- mkPat
  rhs <- mkRhs
  funD [match funname pats rhs [] ]
  where
    funname = mkName $ "read" ++ name ++ "'"
    mkPat = return [litP (intL tag_no)]
    mkRhs = return $ normalRhs (doE (readTypeStmtsS xs ++ [ret]))
    ret = returnS (constrAppExp cname (length xs))

tupleReadD :: String -> [(String, Cardinality)] -> DeclS 
tupleReadD name ty_elts = do
  rhs <- mkRhs
  funD [match funname [] rhs [] ]
  where 
    funname = mkName ("read" ++ name)
    mkRhs = return $ normalRhs (doE (readTypeStmtsS ty_elts ++ 
                                      [returnS (tupleExp (length ty_elts))]))


writeTypeStmtsS tns = map (uncurry (flip writeTypeS)) (numzip 1 tns)
    
writeTagS :: Int -> StmtS
writeTagS i = noBindS (appE (varE (mkQName "write_tag")) 
                            (litE (intL i)))
                            
writeTypeS :: (String, Cardinality) -> Int -> StmtS                            
writeTypeS (s,One) i = noBindS (appE (varE (mkQName s)) 
                               (varE (mkQName $ 'x': show i)))                            
writeTypeS (s,Zom) i = noBindS (appE (varE $ mkQName "plist") 
                                     (appE (varE (mkQName s)) 
                                           (varE (mkQName $ 'x': show i)))) 
                               
writeTypeS (s,Opt) i = noBindS (appE (varE $ mkQName "pmaybe") 
                                     (appE (varE (mkQName s)) 
                                           (varE (mkQName $ 'x': show i))))                                
{-
constrUnpicklerD = do
  funD [match funname
-}  
      
constrUnpicklerInnerD name constr_name ty_elts tag_no = do
  ps <- pats
  rhs <- rhside
  funD [match funname ps rhs [] ] 
  where
    funname = mkName ("write" ++ name ++ "'")
    pats = return [litP (intL tag_no)]
    rhside = return $ 
                normalRhs (doE (readTypeStmtsS ty_elts 
                                  ++ [conretS constr_name (length ty_elts)]))
    
conretS cname i = error "now" 
   
readTypeStmtsS tns = map (uncurry (flip readTypeS)) (numzip 1 tns)
    
readTypeS :: (String,Cardinality) -> Int -> StmtS 
readTypeS (s,One) i = bindS (varP (mkName $ 'x': show i))
                            (varE (mkQName s))
                      
readTypeS (s,Zom) i = bindS (varP (mkName $ 'x': show i))
                            (appE (varE $ mkQName "ulist")
                                  (varE (mkQName s)))

readTypeS (s,Opt) i = bindS (varP (mkName $ 'x': show i))
                            (appE (varE $ mkQName "umaybe")
                                  (varE (mkQName s)))                                  
                                       
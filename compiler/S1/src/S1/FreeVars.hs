{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

-- seems NoMonomorphismRestriction is crucial to getting KYB to work...

module S1.FreeVars where

import S1.KureUtils
import S1.Syntax
-- import S1.SyntaxK

import Language.KURE
import Language.KURE.Boilerplate

import Control.Monad.Identity
import Data.Set


$(kureYourBoilerplate ''SynGeneric [(''Identity,''())])


freeVariables :: Expr -> Set Name
freeVariables = runIdentityTranslate freevars



freevars :: Translate Identity () Expr (Set Name) 
freevars = var <+ fn <+ letd <+ app <+ crushU (promoteU freevars)
  where
    var   = varG >-> translate (\ (Var x)   -> return (singleton x))
    fn    = fnG  >-> translate (\ (Fn n a) -> do 
                fa <- apply freevars a
                return (n `delete` fa))               
    letd  = letG >-> translate (\ (Let n a b) -> do 
                fa <- apply freevars a
                fb <- apply freevars b
                return $ fa `union` (n `delete` fb))
    app   = appG >-> translate (\ (App a b) -> do 
                fa <- apply freevars a
                fb <- apply freevars b
                return $ fa `union` fb)
                                
                                                                
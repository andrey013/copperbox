{-# OPTIONS -fglasgow-exts #-}


module Compiler.Views.ViewBase
  ( ViewTable
  , VwElt (..)        -- (2DO) pottentially this should be private
  , empty_view_table
  , buildViewTable
  , textEntry
  , boolEntry
  , intEntry
  ) where


import Base.AsdlConcreteSyn
import Compiler.Language
import Util.ParseExt


import qualified Data.Map as Map

class ViewKey a where vkey :: a -> String
  
class ViewElt a where velt :: a -> VwElt   

type ViewTable  = Map.Map (Lang,VwElt,String) String

data VwElt
  = Vw_Type String
  | Vw_Constr String
  | Vw_Module String
  deriving (Eq,Ord,Show)

instance ViewElt VwElt where velt = id
  
instance ViewElt ViewEntity where
  velt (TypeView qual name)   = Vw_Type (qual ++ "." ++ name)
  velt (ConstrView qual name) = Vw_Constr (qual ++ "." ++ name)
  velt (ModuleView name)      = Vw_Module name

instance ViewKey String where vkey = id


empty_view_table = Map.empty
    
--------------------------------------------------------------------------------
-- lookup view entries
--------------------------------------------------------------------------------



-- do a more general call on AnyLang if the specific lang fails
viewEntry :: (ViewElt a, ViewKey b) => 
    Lang -> a -> b -> (String -> Maybe ans) -> ViewTable -> Maybe ans

viewEntry AnyLang k1 k2 f vt = let a = lookup' AnyLang k1 k2 vt
                               in appf f a
viewEntry lang    k1 k2 f vt = let a = lookup' lang k1 k2 vt
                               in case a of
                                Just s -> f s
                                Nothing -> viewEntry AnyLang k1 k2 f vt


lookup' :: (ViewElt a,ViewKey b) => Lang -> a -> b -> ViewTable -> Maybe String
lookup' l k1 k2 vt = Map.lookup (l, velt k1, vkey k2) vt

textEntry :: (ViewElt a,ViewKey b) => Lang -> a -> b -> ViewTable -> Maybe String
textEntry l k1 k2 vt = viewEntry l k1 k2 (Just) vt

boolEntry :: (ViewElt a,ViewKey b) => Lang -> a -> b -> ViewTable -> Maybe Bool  
boolEntry l k1 k2 vt = viewEntry l k1 k2 fn vt
  where fn "true"   = Just True
        fn "false"  = Just False
        fn _        = Nothing

intEntry :: (ViewElt a,ViewKey b) => Lang -> a -> b -> ViewTable -> Maybe Int 
intEntry l k1 k2 vt = viewEntry l k1 k2 fn vt
  where fn = readIntMaybe

appf :: (String -> Maybe a) -> Maybe String -> Maybe a
appf f Nothing  = Nothing
appf f (Just s) = f s


--------------------------------------------------------------------------------
-- construct the view table
--------------------------------------------------------------------------------


buildViewTable :: [ViewDefn] -> ViewTable
buildViewTable vs = foldr addView Map.empty vs

addView :: ViewDefn -> ViewTable -> ViewTable
addView (View lang_id  decls) vt = foldr add' vt (concatMap simplifyViewDecl decls)
  where
    lang = findLang lang_id
  
    add' :: (ViewEntity, ViewPair) -> ViewTable -> ViewTable
    add' (entity,(view_prop,view_text)) vt 
      = addViewEntry lang entity view_prop view_text vt
    


-- should we do the vkey, velt coercions here, or outside?
addViewEntry :: (ViewElt a,ViewKey b) => Lang -> a -> b -> String -> ViewTable -> ViewTable
addViewEntry l k1 k2 text vt = Map.insert (l, velt k1, vkey k2) text vt     

simplifyViewDecl :: ViewDecl -> [(ViewEntity, ViewPair)]
simplifyViewDecl (View_Plain  x y)          = [(x,y)]
simplifyViewDecl (View_Many_to_One xs y)    = [(x,y) | x <- xs]
simplifyViewDecl (View_One_to_Many x ys)    = [(x,y) | y <- ys]
simplifyViewDecl (View_Many_to_Many xs ys)  = [(x,y) | x <- xs , y <- ys ]



  

findLang  "All"             = AnyLang
findLang  "Asdl"            = Asdl_1_2
findLang  "Doc"             = Html
findLang  "Html"            = Html
findLang  "OCaml"           = OCaml 
findLang  "Uuag"            = Uuag    
findLang  _                 = NoLang 

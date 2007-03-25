

module ParseSyntax where

import System.IO
import Language.Haskell.Syntax
import Language.Haskell.Parser

main = go "Sexp.hs"


go fl = do
  text <- readFile fl
  let res = parseModule text
  putStr $ show res

{-

(HsPVar (HsIdent "val1")) 
  (HsUnGuardedRhs (HsApp (HsApp (HsCon (UnQual (HsIdent "Cons"))) 
                                (HsVar (UnQual (HsIdent "nil")))) 
                         (HsCon (UnQual (HsIdent "Nil")))))

-}  
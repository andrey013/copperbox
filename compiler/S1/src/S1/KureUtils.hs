
module S1.KureUtils where

import Language.KURE

import Control.Monad.Identity

runIdentityTranslate :: Translate Identity () a b -> a -> b
runIdentityTranslate f a = runIdentity $ do 
        Right (b,(),_) <- runTranslate f () a
        return $ b
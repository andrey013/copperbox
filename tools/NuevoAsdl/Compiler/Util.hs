

module Compiler.Util where

import Control.Monad.Trans
import System.Exit


type ResultE a = Either String a 


exitIfLeft :: (MonadIO m) => m (Either String a) -> m a
exitIfLeft action = do
  ans <- action
  case ans of
    Left msg -> do { liftIO $ putStrLn $ "Fatal Error: " ++ msg
                   ; liftIO $ exitFailure }
    Right a -> return a

sequenceM_failure :: (MonadIO m) => [m (Either String a)] -> m [a]
sequenceM_failure fs = process [] fs
  where process xs []     = return $ reverse xs
        process xs (f:fs) = do { x <- exitIfLeft f
                               ; process (x:xs) fs }
    
foldM_failure :: (MonadIO m) => a -> [(a -> m (Either String a))] -> m a
foldM_failure init fs = process init fs
  where process a []      = return a
        process a (f:fs)  = do { a' <- exitIfLeft $ f a
                               ; process a' fs }


module LambdaCore.Translate where

import Base.Lib
import Gen.LambdaCore.LambdaCoreAbsSyn 

funname :: Definition -> String
funname (Def n _) = "write" ++ n


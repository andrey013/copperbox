
module SexprPkl 
  (writeSexpr
  , readSexpr
  ) where


import AsdlBase
import Sexpr


writeSexpr :: Sexpr -> CM ()
writeSexpr (Int x1) = do
  write_tag 1
  pinteger (fromIntegral x1)
             
writeSexpr (String x1) = do
  write_tag 2
  pstring x1
             
writeSexpr (Symbol x1) = do
  write_tag 3
  pstring x1
             
writeSexpr (Cons x1 x2) = do
  write_tag 4
  writeSexpr x1
  writeSexpr x2
             
writeSexpr (Nil) = do
  write_tag 5

            

readSexpr :: CM Sexpr     
readSexpr = do 
  i <- read_tag
  readSexpr' i
  
  where
    readSexpr' 1 = do
      x1 <- uint
      return (Int x1))

    readSexpr' 2 = do 
      x1 <- ustring
      return (String x1)
                    
    readSexpr' 3 = do 
      x1 <- ustring
      return (Symbol x1)
                    
    readSexpr' 4 = do 
      x1 <- readSexpr
      x2 <- readSexpr
      return (Cons x1 x2)
                    
    readSexpr' 5 = do 
      return (Nil)
                
    readSexpr' i = error $ "readSexpr' " ++ show i
              
demo = readPickle "sexp.asdlpickle" readSexpr

demo2 = do 
  a <- readPickle "sexp.asdlpickle" readSexpr
  writePickle "sexp2.asdlpickle" writeSexpr a
  b <- readPickle "sexp2.asdlpickle" readSexpr
  return b
  


  


instance Pickle Sexpr where
  pickle = writeSexpr
  
  unpickle = readSexpr  
    

  

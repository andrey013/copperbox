{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

-- |
-- Module: HMinCaml.Emit
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Emit Sparc Asm
--

module HMinCaml.Emit where


import HMinCaml.Float
import HMinCaml.Id
import HMinCaml.S
import HMinCaml.SparcAsm

import Control.Monad.State
import Control.Monad.Writer
import Text.Printf


data EmitState = EmitState 
      { stackset :: S Id
      , stackmap :: [Id]
      }

initial_state :: EmitState
initial_state = EmitState empty []

save :: MonadState EmitState m => Id -> m ()
save x  = do 
    stkset <- gets stackset
    modify $ \s -> s {stackset= x `add` stkset }   
    stkmap <- gets stackmap
    when (not $ x `elem` stkmap) (modify $ \s -> s {stackmap= x:stkmap } )

stackclear :: MonadState EmitState m => m () 
stackclear = put initial_state
  
-- not so useful...  
tellprintf :: (MonadWriter String m, PrintfArg a)
           => String -> a -> m ()
tellprintf fmts = tell . printf fmts

ppId_or_Imm :: Id_or_Imm -> String
ppId_or_Imm (V x) = x
ppId_or_Imm (C i) = show i

data Dest = Tail | NonTail Id
  deriving (Eq,Show)
  
emitG :: (MonadState EmitState m, MonadWriter String m) 
      => Dest -> SparcT -> m ()   
emitG dest (Ans expr)           = emitG' dest expr
emitG dest (Let (x,_t) expr e)  = do emitG' (NonTail x) expr
                                     emitG dest e
emitG dest (Forget _ _)         = error $ "emitG - Forget" 

emitG' :: (MonadState EmitState m, MonadWriter String m) 
       => Dest -> Expr -> m ()
emitG' (NonTail _)  Nop       = return ()

emitG' _ _    = return ()

emitH :: (MonadState EmitState m, MonadWriter String m) => Fundef -> m ()

emitH (Fundef (L x)_ _ e _) = do
    tellprintf "%s:\n" x
    stackclear
    emitG Tail e
  

emitF :: (MonadState EmitState m, MonadWriter String m) => Prog -> m ()
emitF (Prog pdata fundefs e) = do
    -- "generating assembly...@.";
    tell ".section\t\".rodata\"\n"
    tell ".align\t8\n"
    
    mapM_
      (\ ((L x), d) -> do
                tellprintf "%s:\t! " x 
                tellprintf "%f\n" d
                tellprintf "\t.long\t0x%lx\n" (gethi d)
                tellprintf "\t.long\t0x%lx\n" (getlo d)
                return ())
      pdata

    tell ".section\t\".text\"\n"
    mapM_ emitH fundefs
    tell  ".global\tmin_caml_start\n"
    tell  "min_caml_start:\n"
    tell  "\tsave\t%%sp, -112, %%sp\n"
    stackclear
    emitG (NonTail "%g0") e
    tell  "\tret\n"
    tell  "\trestore\n"


         
  
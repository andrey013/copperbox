--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymLilyPond.Pretty
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty printer for a subset of LilyPond files
--
--------------------------------------------------------------------------------

module Bala.Format.SymLilyPond.Pretty where

import Bala.Format.SymLilyPond.Datatypes

import Text.PrettyPrint.Leijen


newtype P a = P { unP :: Doc }

putDocP x = putDoc $ unP (x ())


ppcommand :: String -> Doc
ppcommand =  text . ('\\' :)


instance SymE P where
  cmd s a     = P $ (text $ '\\' : s) <+> (unP a)
  emp         = P $ empty
  (##) l r    = P $ (unP l) <+> (unP r)

instance SymMaybe P where
  just a  = P $ unP a
  nothing = P $ empty  
  
  
    
instance SymRest P where
  rest a = P $ group $ char 'r' <> unP a
  
instance SymDuration P where
  dur i = P $ int i
  dot i = P $ group $ int i <> char '.'
    
instance SymPitch P where
  pitch a = P $ text $ show a   
  
instance SymNote P where
  note p d = P $  group $ unP p <> unP d 

instance SymChord P where
  chord xs d = P $ (angles $ hsep $ map unP xs) <> unP d
  
instance SymCmdTime P where
  cmdTime n d = P $ ppcommand "time" <+> group (int d) <> char '/' <> (int n)  
  
instance SymCmdZero P where
  cmdZero s = P $ ppcommand s  


-- ties (6.5.1)
instance SymTie P where
  tie         = P $ char '~'
  

-- slurs (6.5.2)
instance SymSlur P where
  openSlur    = P $ char '('
  closeSlur   = P $ char ')'
  
  
-- phrasing slurs (6.5.3)
instance SymPhrasingSlur P where
  openPhrasingSlur    = P $ ppcommand "("
  closePhrasingSlur   = P $ ppcommand ")"
  
  

-- beams (6.5.6)  
instance SymBeam P where
  openBeam  = P $ char '['
  closeBeam = P $ char ']'
  
    

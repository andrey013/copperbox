{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Inst.ClickNew
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Instrument defs patterned after the Click modular router language.
--
--------------------------------------------------------------------------------

module ZSnd.Core.Inst.ClickNew
  (


    ElemRef
  , PortDict
  , InputConfig
  , CStmt(..)
  , UElement(..)
--  , UBinding(..)
  , OutConf(..)
  , TagVar(..)
  , DataRate(..)

  , FailMsg

  , mkElemRef
  , translateDesc

  ) where

import ZSnd.Core.Inst.Prim
import ZSnd.Core.Utils.FormatCombinators


import Control.Applicative
import Data.List ( sortBy, union )
import qualified Data.Map       as M
import Data.Monoid

import Prelude hiding ( lookup )



type FailMsg = String



-- | map from port to TagVar of its parent.
--
type PortDict = M.Map (ElemRef,Int) TagVar


-- | This is dynamic as the InConfs can be a result of 
-- /port assignment/.
--
type InputConfig = ElemRef -> PortDict ->  Either FailMsg [Expr]



--
-- We want to keep the /Prim/ language and generate output by 
-- pretty printing it.
--
-- The Click language should be transformed to the Prim language.
--


newtype ElemRef = ElemRef { getElemRef :: Int }
  deriving (Eq,Ord)

instance Show ElemRef where
  showsPrec p = showsPrec p . getElemRef



type PortNum  = Int


--
-- > To add back:          | DeclV ElemRef UBinding
--
data CStmt = DeclE ElemRef UElement
           | Conn  (ElemRef, PortNum) (ElemRef, PortNum)
           | Out   ElemRef

instance Show CStmt where
  show = show . format


-- | UElement - universally typed element. This is a UGen.
--
data UElement = UElement
       { elt_name   :: String           -- the opcode name
       , elt_inputs :: InputConfig
       , elt_out    :: OutConf
       }

instance Show UElement where
  show = show . format





{-

-- TODO - re-introduce...

-- | UBinding - universally typed let binding.
--
-- Potential this should include scope - local or global.
--
data UBinding = UBinding
      { bind_equation   :: InConf
      , bind_out_rate   :: DataRate
      }
  deriving (Eq,Ord,Show)

-}
      


-- | It is assumed no opcodes actually generate multiple
-- output at different data rates...
--
data OutConf = Out0 
             | Out1 DataRate 
             | Out2 DataRate
             | OutN DataRate Int
  deriving (Eq,Ord,Show)


mkElemRef :: Int -> ElemRef
mkElemRef = ElemRef




--------------------------------------------------------------------------------
-- Translation


-- | Translate a Click instrument.
--
translateDesc :: Int -> [CStmt] -> Either FailMsg PrimInst
translateDesc i stmts = case runTransMonad $ mapM transStmt stmts of
  Left err -> Left err
  Right xs -> Right $ PrimInst i xs




unions :: Eq a => [[a]] -> [a]
unions []       = []
unions [xs]     = xs
unions (xs:xss) = xs `union` (unions xss)

elookup :: Ord key => FailMsg -> key -> M.Map key a -> Either FailMsg a
elookup msg key m = maybe (Left msg) Right $ M.lookup key m

newtype TransMonad a = TM { 
          getTM :: St -> AccDecls -> Either FailMsg (a, St, AccDecls) }


data St = St
      { i_num    :: Int
      , k_num    :: Int
      , a_num    :: Int
      , pa_count :: Int         -- count the port assignments
      }
  deriving Show

data AccDecls = AccDecls 
      { acc_decls           :: M.Map ElemRef UElement
      , acc_tagvars         :: M.Map ElemRef [TagVar]
      , acc_port_assigns    :: M.Map (ElemRef,Int) TagVar
      , acc_last_assign     :: M.Map ElemRef Int
      , acc_outs            :: LastList ElemRef
      , acc_dependencies    :: M.Map ElemRef [ElemRef]
      }
  deriving Show

accdZero :: AccDecls
accdZero = AccDecls mempty mempty mempty mempty emptyLL mempty

instance Functor TransMonad where
  fmap f ma = TM $ \s ac -> fmap post $ getTM ma s ac
    where
      post (a,s1,ac1) = (f a, s1,ac1)
                              

instance Applicative TransMonad where
  pure a    = TM $ \s ac -> Right (a,s,ac)
  mf <*> ma = TM $ \s ac -> case getTM mf s ac of
      Left err -> Left err 
      Right (f,s1,ac1) -> case getTM ma s1 ac1 of
        Left err2 -> Left err2
        Right (a,s2,ac2) -> Right (f a, s2, ac2)


instance Monad TransMonad where
  return a  = TM $ \s ac -> Right (a, s, ac)
  ma >>= k  = TM $ \s ac -> case getTM ma s ac of 
      Left err -> Left err
      Right (a,s1,ac1) -> (getTM . k) a s1 ac1

runTransMonad :: TransMonad a -> Either FailMsg [Stmt]
runTransMonad ma = getTM ma (St 0 0 0 0) accdZero >>= sk
  where
    sk (_,_,ac) = buildResult ac


--------------------------------------------------------------------------------
-- Post processing to build a list of Stmts

buildResult :: AccDecls -> Either FailMsg [Stmt]
buildResult ac = extractPlan ac >>= mapM (makeStmt `flip` ac)


-- Get a list of elemRefs reachable from the outputs
-- ordered according to last_port_assignment.
--
extractPlan :: AccDecls -> Either FailMsg [ElemRef]
extractPlan ac = findDeps outs >>= (orderByAssign `flip` ac)
  where
    outs = unLast $ acc_outs ac

    findDeps :: [ElemRef] -> Either FailMsg [ElemRef]
    findDeps xs = unions <$> mapM (dependencies `flip` ac) xs


makeStmt :: ElemRef -> AccDecls -> Either FailMsg Stmt
makeStmt eref ac = 
    (\(name,exps) tvs -> Opcode tvs name exps)  
        <$> (elookup fk1 eref (acc_decls ac) >>= sk1) 
        <*> elookup fk2 eref (acc_tagvars ac)
  where
    fk1 = "error - could not find element in decls " ++ show [eref]
    fk2 = "error - could not find tag vars for " ++ show [eref]
    sk1 :: UElement -> Either FailMsg (String,[Expr])
    sk1 ue =  let find = elt_inputs ue 
                  name = elt_name ue
              in fmap (\a -> (name,a)) $ find eref $ acc_port_assigns ac


orderByAssign :: [ElemRef] -> AccDecls -> Either FailMsg [ElemRef]
orderByAssign xs (AccDecls { acc_last_assign = m }) = 
    post <$> mapM (\dref -> fmap (sk dref) $ elookup fk dref m) xs
  where
    
    fk        = "error - element with no recorded port assignment."
    sk dref i = (i,dref)
    post      = map snd . sortBy (\(a,_) (b,_) -> a `compare` b)

-- | Note - answer list doesn\'t need to preserve order when 
-- merging, we get the order from last_assign timestamps.
-- 
dependencies :: ElemRef -> AccDecls -> Either FailMsg [ElemRef]
dependencies eref ac = step [eref] (dependencies1 eref [] ac)
  where
    step _  (Left err) = Left err 
    step xs (Right []) = Right xs 
    step xs (Right ys) = merge $ map (\y1 -> dependencies1 y1 xs ac) ys

    merge []           = Right []
    merge (Left err:_) = Left err
    merge (Right xs:xss) = case merge xss of
                             Right ys -> Right $ union xs ys
                             Left err -> Left err
                         


-- | Note - we detect cycles here.
--
dependencies1 :: ElemRef -> [ElemRef] -> AccDecls -> Either FailMsg [ElemRef]
dependencies1 eref prevs ac = 
    case M.lookup eref (acc_dependencies ac) of
      Nothing -> Right []
      Just xs -> checkdups (eref:prevs) xs
  where
    checkdups _      [] = Right []      -- fast path! for empty case
    checkdups []     ys = Right ys
    checkdups (x:xs) ys | elem x ys = Left "error - cyclic port assignments."
                        | otherwise = checkdups xs ys


--------------------------------------------------------------------------------
-- Traversing and building...

newLocVar :: DataRate -> TransMonad TagVar
newLocVar rt = TM $ \s ac -> let (a,s1) = step rt s in Right (a,s1,ac)
  where
    step I s = let i = i_num s in (LocVar I (i+1), s { i_num = i + 1 })
    step K s = let i = k_num s in (LocVar K (i+1), s { k_num = i + 1 })
    step A s = let i = a_num s in (LocVar A (i+1), s { a_num = i + 1 })




bindDeclE :: ElemRef -> UElement -> [TagVar] -> TransMonad ()
bindDeclE eref elt outs = TM $ \s ac -> 
    let decls1 = M.insert eref elt (acc_decls ac)
        ports1 = foldr (\(tv,i) mac -> M.insert (eref,i) tv mac)
                       (acc_port_assigns ac)
                       (zip outs [0..]) 
        lasts1 = M.insert eref (pa_count s) (acc_last_assign ac)
        tagvs1 = M.insert eref outs (acc_tagvars ac)
    in Right ((), s, ac { acc_decls        = decls1
                        , acc_port_assigns = ports1 
                        , acc_last_assign  = lasts1
                        , acc_tagvars      = tagvs1 } )


addOut :: ElemRef -> TransMonad ()
addOut eref = TM $ \s ac -> 
    let outs1 = snocLL (acc_outs ac) eref
    in Right ((),s, ac { acc_outs = outs1 })


-- | Assign ports...
--
-- > to-ElemRef gets updated with pa_count
-- > pa_count is incremented
-- 
assignPort :: (ElemRef, PortNum) -> (ElemRef, PortNum) -> TransMonad ()
assignPort a (eref_to,_) = TM $ \s ac ->
    let pa = 1 + pa_count s 
        lasts1 = M.insert eref_to pa (acc_last_assign ac)
    in Right ((), s { pa_count = pa} 
                , ac { acc_last_assign = lasts1 }) 


-- | The @last_port_count@ on an object (principally an Out) can
-- be forced so it will be printed later or last.
--
-- Note - this functions raises a fail if the ElemRef is not 
-- already in the dict.
-- 
forcePortCount :: ElemRef -> TransMonad ()
forcePortCount dref = TM $ \s ac -> 
    elookup fk dref (acc_last_assign ac) >>= sk ac s

  where
    fk = "error - missing declaration ref."
    sk ac s _ = let pa     = pa_count s
                    lasts1 = M.insert dref (pa+1) (acc_last_assign ac)
                in return ((), s { pa_count = pa+1}
                             , ac { acc_last_assign = lasts1 })


-- A Decl generates an opcode Stmt - it may have unassigned ports
-- which need filling in, so a \"delayed\" dictionary of opcode 
-- stmts is built. 
--
-- The delayed dictionary tracks port assignements - the stmts 
-- that are filled in first will be printed first.
--
--

transStmt :: CStmt -> TransMonad ()
transStmt (DeclE dref elt)      = do
    ovars <- outVars $ elt_out elt
    bindDeclE dref elt ovars 


transStmt (Conn a b)            = assignPort a b  

transStmt (Out dref)            = do 
   forcePortCount dref
   addOut dref



outVars :: OutConf -> TransMonad [TagVar]
outVars Out0        = pure []
outVars (Out1 rt)   = (\a -> [a]) <$> newLocVar rt
outVars (Out2 rt)   = (\a b -> [a,b]) <$> newLocVar rt <*> newLocVar rt
outVars (OutN rt n) = countA n (newLocVar rt)



countA :: Applicative f => Int -> f a -> f [a]
countA i _  | i <= 0 = pure []
countA i ma          = (:) <$> ma <*> countA (i-1) ma





newtype LastList a = LL { getLL :: [a] }

instance Show a => Show (LastList a) where
  show = show . unLast

emptyLL :: LastList a
emptyLL = LL []

snocLL :: Eq a => LastList a -> a -> LastList a
snocLL ll a = LL $ a : filter (/=a) (getLL ll)

unLast :: LastList a -> [a]
unLast = reverse . getLL



--------------------------------------------------------------------------------
-- Format instances (useful for debugging)

instance Format CStmt where
  format (DeclE name elt)       = 
      format name <+> text "::" <+> format elt <> char ';'

--  format (DeclV name bin)       = 
--      format name <+> text ":=" <+> format bin <> char ';'
    

  format (Conn (v1,i1) (v2,i2)) = 
      format v1 <> brackets (int i1) <+> text "->" 
               <+> brackets (int i2) <> format v2 <> char ';'
               

  format (Out v1)               = text "==>" <+> format v1 <> char ';'
     

instance Format UElement where
  format (UElement name _ _)    = text name

-- instance Format UBinding where
--   format (UBinding eqn _)       = format eqn

instance Format ElemRef where
  format (ElemRef i) = text "var" <> int i 

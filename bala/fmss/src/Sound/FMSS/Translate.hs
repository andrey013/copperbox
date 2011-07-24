{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.Translate
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translate to to the abstract syntax.
--
--------------------------------------------------------------------------------

module Sound.FMSS.Translate
  (
    
    ErrMsg

  , translate     

  , outputVar
 
  ) where

import Sound.FMSS.AbstractSyntax
import Sound.FMSS.Datatypes
import Sound.FMSS.Utils.HList

import Control.Applicative hiding ( Const )
import qualified Data.IntMap as IM
import Data.List ( foldl', delete )
import Data.Monoid

type ErrMsg = String


-- | The relationships are one-to-many.
--
-- Note - storing modulator backlints as Ints is more convenient 
-- for finding mod-order. It is less convenient than Varid for 
-- printing.
--
data Env = Env 
      { modulator_backlinks   :: IM.IntMap [Int]
      , carrier_backlinks     :: IM.IntMap [Int]
      , modulator_backcycles  :: IM.IntMap [(VarId,ExprF)]
      , carrier_backcycles    :: IM.IntMap [(VarId,ExprF)]
      }

      --  Note - env needs cycles adding...


newtype Trans a = Trans { getTrans :: Env -> Either ErrMsg a }

bimapE :: (a -> c) ->  (b -> d) -> Either a b -> Either c d
bimapE f _ (Left a) = Left (f a)
bimapE _ g (Right b) = Right (g b)


instance Functor Trans where
  fmap f mf = Trans $ \r -> bimapE id f $ getTrans mf r


instance Applicative Trans where
  pure a    = Trans $ \_ -> Right a
  mf <*> ma = Trans $ \r -> getTrans mf r >>= \f -> 
                            getTrans ma r >>= \a -> 
                            Right (f a)

instance Monad Trans where
  return a  = Trans $ \_ -> Right a
  m >>= k   = Trans $ \r -> getTrans m r >>= \a -> getTrans (k a) r
  fail msg  = Trans $ \_ -> Left msg


runTrans :: Env -> Trans a -> Either ErrMsg a
runTrans = flip getTrans 

translate :: FMSynth -> Either ErrMsg Instr
translate (FMSynth { fm_instr_num = inst_num, fm_decls    = decls
                   , fm_synth_body = body,    fm_sinetbl  = tablenum
                   , fm_out       = out1 }) = 
    runTrans env $ do 
      gen_decls     <- stdDecls tablenum mods cars
      cyc_inits     <- mapM cycleInit cycles
      mod_stmts     <- modulators mods
      car_stmts     <- carriers cars
      out_stmt      <- outStatement out1
      return $ Instr { instr_number      = inst_num
                     , instr_irate_decls = gen_decls ++ decls 
                     , instr_body        = concat [ cyc_inits
                                                  , mod_stmts 
                                                  , car_stmts
                                                  , [out_stmt] ]
                     }
                   
  where
    mods    = synth_mods body
    cars    = synth_cars body
    links   = synth_links body
    cycles  = synth_cycles body
    modnums = map oscilNum mods
    carnums = map oscilNum cars
    env     = buildLinkTables modnums carnums links cycles
   


-- | Note - all modulator indexes need populating (even if empty)
-- for the mod-order algorithm.
--
insertLink :: Int -> Int -> IM.IntMap [Int] -> IM.IntMap [Int]
insertLink ix n imap = let ns = queryLink ix imap 
                       in IM.insert ix (n:ns) imap

queryLink :: Int -> IM.IntMap [Int] -> [Int]
queryLink ix imap = maybe [] id $ IM.lookup ix imap


-- | Doesn\'t care about every indexes being populated.
--
insertCycle :: Int -> (VarId,ExprF) -> IM.IntMap [(VarId,ExprF)] 
            -> IM.IntMap [(VarId,ExprF)]
insertCycle ix cyc imap = let cs = queryCycle ix imap 
                          in IM.insert ix (cyc:cs) imap

queryCycle :: Int -> IM.IntMap [(VarId,ExprF)] -> [(VarId,ExprF)]
queryCycle ix imap = maybe [] id $ IM.lookup ix imap



-- | Note - @mods@ needs populating with all @modnums@ even if 
-- there are no backlinks. This lets the mod-order algorithm take
-- dependency free modualtors first.
--
buildLinkTables :: [Int] -> [Int] -> [Link] -> [Cycle] -> Env
buildLinkTables modnums carnums links cycles = 
    let mods0       = IM.fromList $ map (\a -> (a,[])) modnums
        cars0       = IM.fromList $ map (\a -> (a,[])) carnums    
        (mods,cars)   = foldr fl (mods0,cars0) links
        (mcycs,ccycs) = foldr fc (mempty,mempty) cycles 
    in Env { modulator_backlinks   = mods
           , carrier_backlinks     = cars
           , modulator_backcycles  = mcycs
           , carrier_backcycles    = ccycs 
           }
  where
    fl (ModMod a b) (mm,cm) = (insertLink b a mm, cm) 
    fl (ModCar a b) (mm,cm) = (mm, insertLink b a cm) 
       
    fc (CycModMod a b eF) (mm,cm) = let varid = outputCyclePhmName MODULATOR a
                                    in (insertCycle b (varid,eF) mm, cm)

    fc (CycCarCar a b eF) (mm,cm) = let varid = outputCyclePhmName CARRIER a
                                    in (mm, insertCycle b (varid,eF) cm)

    fc (CycCarMod a b eF) (mm,cm) = let varid = outputCyclePhmName CARRIER a
                                    in (insertCycle b (varid,eF) mm, cm)


asks :: (Env -> a) -> Trans a
asks f = Trans $ \r -> Right (f r)


modBacklinks :: Int -> Trans [Int]
modBacklinks i = (queryLink i) <$> asks modulator_backlinks

carBacklinks :: Int -> Trans [Int]
carBacklinks i = (queryLink i) <$> asks carrier_backlinks

modBackcycles :: Int -> Trans [(VarId,ExprF)]
modBackcycles i = (queryCycle i) <$> asks modulator_backcycles

carBackcycles :: Int -> Trans [(VarId,ExprF)]
carBackcycles i = (queryCycle i) <$> asks carrier_backcycles


stdDecls :: Int -> [Modulator] -> [Carrier] -> Trans [Decl]
stdDecls sinetab xs ys = return $ 
    declProlog sinetab ++ map freqIDecl_m xs ++ map freqIDecl_c ys

cycleInit :: Cycle -> Trans Stmt
cycleInit (CycModMod i _ _) = 
    let varid = outputCyclePhmName MODULATOR i in return $ Init varid 0

cycleInit (CycCarCar i _ _) = 
    let varid = outputCyclePhmName CARRIER i in return $ Init varid 0

cycleInit (CycCarMod i _ _) =
    let varid = outputCyclePhmName CARRIER i in return $ Init varid 0



carriers :: [Carrier] -> Trans [Stmt]
carriers xs = fmap concat $ mapM emitCarrier xs

outStatement  :: Stmt -> Trans Stmt
outStatement = return 


-- | Assigns p3, p4 and p5 to their regular fields 
-- (dur, amp and freq).
--
-- Table num for the sine wave table is a param.
-- 
declProlog :: Int -> [Decl]
declProlog sinetab = 
    [ Decl "isinetbl"  (Const $ CsInt sinetab)
    , Decl "idur"      (PField 3)
    , Decl "iamp"      (PField 4)
    , Decl "ifreq"     (PField 5)
    ]


freqIDecl_m :: Modulator -> Decl
freqIDecl_m osc = 
    freqIDecl (inputFreqName MODULATOR $ oscilNum osc) (oscilFreq osc)

freqIDecl_c :: Carrier -> Decl
freqIDecl_c osc = 
    freqIDecl (inputFreqName CARRIER $ oscilNum osc) (oscilFreq osc)

freqIDecl :: String -> OscilFreq -> Decl
freqIDecl varid hz = Decl varid (mkExpr hz)
  where
    mkExpr (FixedFreq d)  = Const $ CsDouble d
    mkExpr (BaseScaler d) = VarE "ifreq" * Const (CsDouble d)





--------------------------------------------------------------------------------

-- TODO - links ...

--
-- Modulator order can be resolved by looking at just the Mod-Mod 
-- links.
--
-- Carriers are always printed last. As their only input is from 
-- modulators (or themselves), they are always fully
-- \"saturated\" - i.e. all arguments are resolved before use.
--
--

modulators :: [Modulator] -> Trans [Stmt]
modulators mods = 
    let mtable = IM.fromList $ map (\a -> (oscilNum a, a)) mods
    in modOrder >>= \ixs -> stepM mtable ixs emptyH
  where
    err_msg i   = "Error - unresolved modulator " ++ (show i)

    find i im   = maybe (fail $ err_msg i) return $ IM.lookup i im
                    

    stepM _  []      ac = return (toListH ac)
    stepM im (i:ixs) ac = do { m1    <- find i im
                             ; stmts <- fmap fromListH $ emitModulator m1
                             ; stepM im ixs (ac `appendH` stmts)
                             }


--------------------------------------------------------------------------------
-- Find modulator order

-- WARNING - this is not adequate for e.g. Algo 15.

type LinkMap = [(Int,[Int])]

challenge :: LinkMap -> Maybe Int
challenge []          = Nothing
challenge ((i,[]):_)  = Just i
challenge (_:xs)      = challenge xs


prune :: Int -> LinkMap -> LinkMap
prune ix = step
  where
    step [] = []
    step ((n,ns):xs) | n == ix = xs
                     | otherwise = let ns' = delete ix ns in (n,ns') :xs

modOrder :: Trans [Int]
modOrder = fmap IM.toList (asks modulator_backlinks) >>= modOrder1 
  

-- | Every modualtor has an entry in the link map, even if it has
-- no parents.
--
modOrder1 :: LinkMap -> Trans [Int]
modOrder1 links = level links emptyH
  where
    level [] ac = return $ toListH ac
    level xs ac = case challenge xs of
                    Nothing -> mkErr $ map fst xs
                    Just ix -> let xs' = prune ix xs 
                               in level xs' (ac `snocH` ix)

    mkErr ixs   = fail $ "modulator order - these modulators cannot be resolved "
                       ++ show ixs


-- Note - carrier order can be numeric order as there 
-- are no dependencies.

--------------------------------------------------------------------------------


emitModulator :: Modulator -> Trans [Stmt]
emitModulator osc = 
    (\links cycs -> concat [ [mkS1 links] 
                           , [mkS2 $ map fst cycs]
                           , pps 
                           , mkPhms cycs 
                           ])
      <$> modBacklinks ix <*> modBackcycles ix
  where
    ix     = oscilNum osc
    cmt    = "modulator " ++ show ix
    mkS1   = \ns -> CommentS cmt $ phasorCall_m ix ns
    mkS2   = \cs -> tableiCall_m ix cs
    pps    = postproSig_m osc
    mkPhms = map (phaseAssign_m `flip` ix)


emitCarrier :: Carrier -> Trans [Stmt]
emitCarrier osc = 
    (\links cycs -> concat [ [mkS1 links]
                           , [mkS2 $ map fst cycs]
                           , pps
                           , mkPhms cycs
                           ])
      <$> carBacklinks ix <*> carBackcycles ix
  where
    ix     = oscilNum osc
    cmt    = "carrier " ++ show ix
    mkS1   = \ns -> CommentS cmt $ phasorCall_c ix ns
    mkS2   = \cs -> tableiCall_c ix cs
    pps    = postproSig_c osc
    mkPhms = map (phaseAssign_c `flip` ix)

-- | Note this is too simple, rhs could be an expr...
--
phasorCall_m :: Int -> [Int] -> Stmt
phasorCall_m i xs = Phasor (outputPhaseName MODULATOR i) expr 
  where
    expr    = foldl' fn (VarE $ inputFreqName MODULATOR i) xs
    fn ac n = ac + VarE (outputSignalName MODULATOR n)

phasorCall_c :: Int -> [Int] -> Stmt
phasorCall_c i xs = Phasor (outputPhaseName CARRIER i) expr 
  where
    expr    = foldl' fn (VarE $ inputFreqName CARRIER i) xs
    fn ac n = ac + VarE (outputSignalName MODULATOR n)


tableiCall_m :: Int -> [VarId] -> Stmt 
tableiCall_m i xs = Tablei (outputSignalName MODULATOR i) expr
  where
    expr       = foldl' fn (VarE $ outputPhaseName MODULATOR i) xs
    fn ac name = ac + VarE name

tableiCall_c :: Int -> [VarId] -> Stmt 
tableiCall_c i xs = Tablei (outputSignalName CARRIER i) expr 
  where
    expr       = foldl' fn (VarE $ outputPhaseName CARRIER i) xs
    fn ac name = ac + VarE name


-- | Note - the list really encodes a Maybe value
--
postproSig_m :: Modulator -> [Stmt]
postproSig_m osc = case oscilPostpro osc of
    Nothing -> []
    Just fn -> let varid = outputSignalName MODULATOR (oscilNum osc)
               in [ Assign varid (fn $ VarE varid) ]


postproSig_c :: Carrier -> [Stmt]
postproSig_c osc = case oscilPostpro osc of
    Nothing -> []
    Just fn -> let varid = outputSignalName CARRIER (oscilNum osc)
               in [ Assign varid (fn $ VarE varid) ]


phaseAssign_m :: (VarId,ExprF) -> Int -> Stmt
phaseAssign_m (varid,exprF) i = 
    Assign varid (exprF $ VarE $ outputSignalName MODULATOR i)



phaseAssign_c :: (VarId,ExprF) -> Int -> Stmt
phaseAssign_c (varid,exprF) i = 
    Assign varid (exprF $ VarE $ outputSignalName CARRIER i)

--------------------------------------------------------------------------------
-- Names

data OscilType = CARRIER | MODULATOR
  deriving (Eq,Show)

shortName :: OscilType -> String
shortName CARRIER   = "car"
shortName MODULATOR = "mod"

-- | Of form:
--
-- > 'i' ("mod" | "car") <num> "freq"
--
inputFreqName :: OscilType -> Int -> String
inputFreqName ot i = 'i' : shortName ot ++ show i ++ "freq"



-- | Of form:
--
-- > 'a' ("mod" | "car") <num> "phs"
--
outputPhaseName :: OscilType -> Int -> String
outputPhaseName ot i = 'a' : shortName ot ++ show i ++ "phs"



-- | Of form:
--
-- > 'a' ("mod" | "car") <num> "sig"
--
outputSignalName :: OscilType -> Int -> String
outputSignalName ot i = 'a' : shortName ot ++ show i ++ "sig"


-- | Of form:
--
-- > 'a' ("mod" | "car") <num> "phm"
--
outputCyclePhmName :: OscilType -> Int -> String
outputCyclePhmName ot i = 'a' : shortName ot ++ show i ++ "phm"


outputVar :: Carrier -> Expr
outputVar osc = VarE $ outputSignalName CARRIER (oscilNum osc)
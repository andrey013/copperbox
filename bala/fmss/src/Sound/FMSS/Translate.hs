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
 
  ) where

import Sound.FMSS.AbstractSyntax
import Sound.FMSS.Datatypes
import Sound.FMSS.Utils.HList

import Control.Applicative hiding ( Const )
import qualified Data.IntMap as IM
import Data.List ( foldl', delete )

type ErrMsg = String


-- | The relationships are one-to-many.
--
data Env = Env 
      { modulator_backlinks :: IM.IntMap [Int]
      , carrier_backlinks   :: IM.IntMap [Int]
      }


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
translate (FMSynth { fm_instr_num = inst_num, fm_envelopes = envlps
                   , fm_mods      = mods,     fm_cars      = cars
                   , fm_sinetbl   = tablenum, fm_links     = links
                   , fm_out       = out1 }) = 
    runTrans env $ do 
      decls         <- stdDecls tablenum mods cars
      envlp_stmts   <- envelopes envlps
      mod_stmts     <- modulators mods
      car_stmts     <- carriers cars
      out_stmt      <- outStatement out1
      return $ Instr { instr_num         = inst_num
                     , instr_irate_decls = decls
                     , instr_body        = concat [ envlp_stmts 
                                                  , mod_stmts 
                                                  , car_stmts
                                                  , [out_stmt] ]
                     }
                   
  where
    modnums = map mosc_num mods
    carnums = map cosc_num cars
    env     = buildLinkTables modnums carnums links
   


-- | Note - this relies on key being populated.
--
insertLink :: Int -> Int -> IM.IntMap [Int] -> IM.IntMap [Int]
insertLink ix n imap = IM.adjust (n:) ix imap

queryLink :: Int -> IM.IntMap [Int] -> [Int]
queryLink ix imap = maybe [] id $ IM.lookup ix imap


buildLinkTables :: [Int] -> [Int] -> [Link] -> Env
buildLinkTables modnums carnums links = 
    foldr fn (Env mods cars) links
  where
    mods            = IM.fromList $ map (\a -> (a,[])) modnums
    cars            = IM.fromList $ map (\a -> (a,[])) carnums

    fn (ModMod a b) = (\s u -> s { modulator_backlinks = insertLink b a u }) 
                        <*> modulator_backlinks

    fn (ModCar a b) = (\s u -> s { carrier_backlinks = insertLink b a u }) 
                        <*> carrier_backlinks
    
    fn _            = id


asks :: (Env -> a) -> Trans a
asks f = Trans $ \r -> Right (f r)


modBacklinks :: Int -> Trans [Int]
modBacklinks i = (queryLink i) <$> asks modulator_backlinks

carBacklinks :: Int -> Trans [Int]
carBacklinks i = (queryLink i) <$> asks carrier_backlinks



stdDecls :: Int -> [ModulatorOsc] -> [CarrierOsc] -> Trans [Decl]
stdDecls sinetab xs ys = return $ 
    declProlog sinetab ++ map freqIDecl_m xs ++ map freqIDecl_c ys

envelopes :: [EnvelopeSpec] -> Trans [Stmt]
envelopes = return . map envelope 


carriers :: [CarrierOsc] -> Trans [Stmt]
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


freqIDecl_m :: ModulatorOsc -> Decl
freqIDecl_m osc = freqIDecl (inputFreqName_m $ mosc_num osc) (mosc_freq osc)

freqIDecl_c :: CarrierOsc -> Decl
freqIDecl_c osc = freqIDecl (inputFreqName_c $ cosc_num osc) (cosc_freq osc)

freqIDecl :: String -> OscilFreq -> Decl
freqIDecl varid hz = Decl varid (mkExpr hz)
  where
    mkExpr (FixedFreq d)  = Const $ CsDouble d
    mkExpr (BaseScaler d) = VarE "ifreq" * Const (CsDouble d)



envelope :: EnvelopeSpec -> Stmt
envelope (EnvelopeSpec varid mb_cmt opco doc) =
    maybe ans (\cmt -> CommentS cmt ans) mb_cmt
  where 
    ans = Envelope varid opco doc
    
 
    



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

modulators :: [ModulatorOsc] -> Trans [Stmt]
modulators mods = 
    let mtable = IM.fromList $ map (\a -> (mosc_num a, a)) mods
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


emitModulator :: ModulatorOsc -> Trans [Stmt]
emitModulator osc@(ModulatorOsc { mosc_num = ix }) = 
    (\links -> mkS1 links : s2 : xs)
      <$> modBacklinks ix
  where
    cmt   = "modulator " ++ show ix
    mkS1  = \ns -> CommentS cmt $ phasorCall_m ix ns
    s2    = tableiCall_m osc
    xs    = postproSig_m osc
   


emitCarrier :: CarrierOsc -> Trans [Stmt]
emitCarrier osc@(CarrierOsc { cosc_num = ix }) = 
    (\links -> mkS1 links : s2 : xs)
      <$> carBacklinks ix
  where
    cmt   = "carrier " ++ show (cosc_num osc)
    mkS1  = \ns -> CommentS cmt $ phasorCall_c ix ns
    s2    = tableiCall_c osc
    xs    = postproSig_c osc

-- | Note this is too simple, rhs could be an expr...
--
phasorCall_m :: Int -> [Int] -> Stmt
phasorCall_m i xs = Phasor (outputPhaseName_m i) expr 
  where
    expr    = foldl' fn (VarE $ inputFreqName_m i) xs
    fn ac n = ac + VarE (outputSignalName_m n)

phasorCall_c :: Int -> [Int] -> Stmt
phasorCall_c i xs = Phasor (outputPhaseName_c i) expr 
  where
    expr    = foldl' fn (VarE $ inputFreqName_c i) xs
    fn ac n = ac + VarE (outputSignalName_m n)


tableiCall_m :: ModulatorOsc -> Stmt 
tableiCall_m osc = 
    let i = mosc_num osc
    in Tablei (outputSignalName_m i) (VarE $ outputPhaseName_m i)

tableiCall_c :: CarrierOsc -> Stmt 
tableiCall_c osc = 
    let i = cosc_num osc
    in Tablei (outputSignalName_c i) (VarE $ outputPhaseName_c i)


-- | Note - the list really encodes a Maybe value
--
postproSig_m :: ModulatorOsc -> [Stmt]
postproSig_m osc = case mosc_postpro osc of
    Nothing -> []
    Just fn -> let varid = outputSignalName_m (mosc_num osc)
               in [ Assign varid (fn $ VarE varid) ]


postproSig_c :: CarrierOsc -> [Stmt]
postproSig_c osc = case cosc_postpro osc of
    Nothing -> []
    Just fn -> let varid = outputSignalName_c (cosc_num osc)
               in [ Assign varid (fn $ VarE varid) ]


-- Could get these just by number ...

-- | Of form:
--
-- > 'i' ("mod" | "car") <num> "freq"
--
inputFreqName_m :: Int -> String
inputFreqName_m i = "imod" ++ show i ++ "freq"


inputFreqName_c :: Int -> String
inputFreqName_c i   = "icar" ++ show i ++ "freq"

-- | Of form:
--
-- > 'a' ("mod" | "car") <num> "phs"
--
outputPhaseName_m :: Int -> String
outputPhaseName_m i = "amod" ++ show i ++ "phs"

outputPhaseName_c :: Int -> String
outputPhaseName_c i = "acar" ++ show i ++ "phs"


-- | Of form:
--
-- > 'a' ("mod" | "car") <num> "sig"
--
outputSignalName_m :: Int -> String
outputSignalName_m i = "amod" ++ show i ++ "sig"

outputSignalName_c :: Int -> String
outputSignalName_c i = "acar" ++ show i ++ "sig"


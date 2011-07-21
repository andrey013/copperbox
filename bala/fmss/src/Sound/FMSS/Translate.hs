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
  , decls
  , modulators
  , carriers
 
  ) where

import Sound.FMSS.AbstractSyntax
import Sound.FMSS.Datatypes
import Sound.FMSS.Utils.FormatCombinators
import Sound.FMSS.Utils.HList

import Data.List ( delete )
import qualified Data.IntMap as IM

type ErrMsg = String

translate :: FMSynth -> Either ErrMsg Instr
translate fmsyn = 
   let irate_decls = decls fmsyn
       mods_or_err = modulators fmsyn
       cars        = carriers fmsyn
   in case mods_or_err of 
       Right mods -> Right $ Instr { instr_num         = fm_instr_num fmsyn
                                   , instr_irate_decls = irate_decls
                                   , instr_body        = mods ++ cars }
                   
       Left err -> Left err


decls :: FMSynth -> [Decl]
decls (FMSynth {fm_sinetbl=sinetab, fm_mods = xs, fm_cars = ys }) = 
    declProlog sinetab ++ map freqIDecl_m xs ++ map freqIDecl_c ys


carriers :: FMSynth -> [Stmt]
carriers (FMSynth { fm_cars = xs }) = concatMap emitCarrier xs


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
freqIDecl_m osc = freqIDecl (inputFreqName_m osc) (mosc_freq osc)

freqIDecl_c :: CarrierOsc -> Decl
freqIDecl_c osc = freqIDecl (inputFreqName_c osc) (cosc_freq osc)

freqIDecl :: String -> OscilFreq -> Decl
freqIDecl varid hz = Decl varid (mkExpr hz)
  where
    mkExpr (FixedFreq d)  = Const $ CsDouble d
    mkExpr (BaseScaler d) = VarE "ifreq" `mult` Const (CsDouble d)


kampenvInit :: Maybe (String,Doc) -> Maybe Stmt
kampenvInit Nothing          = Nothing
kampenvInit (Just (ss,body)) = Just $ Envelope "kampenv" ss body

--------------------------------------------------------------------------------

--
-- Modulator order can be resolved by looking at just the Mod-Mod 
-- links.
--
-- Carriers are always printed last. As their only input is from 
-- modulators (or themselves), they are always fully
-- \"saturated\" - i.e. all arguments are resolved before use.
--
--

modulators :: FMSynth -> Either ErrMsg [Stmt]
modulators (FMSynth { fm_mods = mods, fm_links = links}) = do
    ixs <- modOrder (map mosc_num mods) $ modmodLinks links
    let mt = foldr (\a ac -> IM.insert (mosc_num a) a ac) IM.empty mods
    stepM mt ixs emptyH
  where
    err_msg i = "Error - unresolved modulator " ++ (show i)

    stepM _  []      ac = return (toListH ac)
    stepM im (i:ixs) ac = case IM.lookup i im of
                            Just o -> let outs = fromListH $ emitModulator o
                                      in stepM im ixs (ac `appendH` outs)
                            Nothing -> Left (err_msg i)

                            
        

type LinkMap = IM.IntMap Int

modmodLinks :: [Link] -> LinkMap
modmodLinks = foldr fn IM.empty
  where
    fn (ModMod i j) ac = IM.insert j i ac  -- note - flipped
    fn _            ac = ac

isDest :: Int -> LinkMap -> Bool
isDest = IM.member


-- need to account for mods that route straight to carriers...
modOrder :: [Int] -> LinkMap -> Either ErrMsg [Int]
modOrder ixs links = level ixs emptyH
  where
    err_msg = "Error - unresolved links."
    level []   ac               = return (toListH ac)
    level keys ac = case step1 keys of 
                         Just x -> level (delete x keys) (ac `snocH` x)
                         Nothing -> Left err_msg
               

    step1 (y:ys) | isDest y links = step1 ys 
                 | otherwise      = Just y
    step1 []                      = Nothing





emitModulator :: ModulatorOsc -> [Stmt]
emitModulator osc = 
    [ CommentS cmt $ phasorCall_m osc, tableiCall_m osc ]
  where
    cmt = "modulator " ++ show (mosc_num osc)


emitCarrier :: CarrierOsc -> [Stmt]
emitCarrier osc = 
    [ CommentS cmt $ phasorCall_c osc, tableiCall_c osc ]
  where
    cmt = "carrier " ++ show (cosc_num osc)


-- | Note this is too simple, rhs could be an expr...
--
phasorCall_m :: ModulatorOsc -> Stmt
phasorCall_m osc = 
    Phasor (outputPhaseName_m osc) (VarE $ inputFreqName_m osc)

phasorCall_c :: CarrierOsc -> Stmt
phasorCall_c osc = 
    Phasor (outputPhaseName_c osc) (VarE $ inputFreqName_c osc)


tableiCall_m :: ModulatorOsc -> Stmt 
tableiCall_m osc = 
    Tablei (outputSignalName_m osc) (VarE $ outputPhaseName_m osc)

tableiCall_c :: CarrierOsc -> Stmt 
tableiCall_c osc = 
    Tablei (outputSignalName_c osc) (VarE $ outputPhaseName_c osc)


-- | Of form:
--
-- > 'i' ("mod" | "car") <num> "freq"
--
inputFreqName_m :: ModulatorOsc -> String
inputFreqName_m (ModulatorOsc i _) = "imod" ++ show i ++ "freq"


inputFreqName_c :: CarrierOsc -> String
inputFreqName_c (CarrierOsc i _)   = "icar" ++ show i ++ "freq"

-- | Of form:
--
-- > 'a' ("mod" | "car") <num> "phs"
--
outputPhaseName_m :: ModulatorOsc -> String
outputPhaseName_m (ModulatorOsc i _) = "amod" ++ show i ++ "phs"

outputPhaseName_c :: CarrierOsc -> String
outputPhaseName_c (CarrierOsc i _)   = "acar" ++ show i ++ "phs"


-- | Of form:
--
-- > 'a' ("mod" | "car") <num> "sig"
--
outputSignalName_m :: ModulatorOsc -> String
outputSignalName_m (ModulatorOsc i _) = "amod" ++ show i ++ "sig"

outputSignalName_c :: CarrierOsc -> String
outputSignalName_c (CarrierOsc i _)   = "acar" ++ show i ++ "sig"


{-
assignDoc :: String -> Doc -> Doc
assignDoc name val = padr 11 (text name) <+> char '=' <+> val


-- |
opcodeDoc :: String -> String -> [Doc] -> Doc
opcodeDoc name opcode args = 
    padr 11 (text name) <+> padr 9 (text opcode) <+> commaSep args

-}
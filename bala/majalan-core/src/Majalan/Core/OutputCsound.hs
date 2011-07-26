{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.OutputCsound
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output Csound - scores (.sco) or unified orchestra and score 
-- files (.csd). 
--
--------------------------------------------------------------------------------

module Majalan.Core.OutputCsound
  (

    writeSco
  , writeUnifiedFile
  , writeUnifiedScore

  , CsoundFlags(..)
  , flags_rt_audio

  , flags_wav_file_out
  , flags_aiff_file_out
  , flags_display

  ) where

import Majalan.Core.CSDDoc
-- import Majalan.Core.CsoundScore
import Majalan.Core.ScoreInternal
import Majalan.Core.Utils.FormatCombinators
import Majalan.Core.Utils.JoinList hiding ( empty )

import Control.Applicative hiding ( empty )




writeSco :: FilePath -> PrimScore -> IO ()
writeSco file_path xs = writeFile file_path $ show $ buildScoreDoc xs




writeUnifiedFile :: FilePath -> CsoundFlags -> [String] -> PrimScore -> IO ()
writeUnifiedFile file_path flags insts sco = 
    writeFile file_path $ show $ csound_synthesizer dflags (mkOrch insts) dsco
  where
    dflags = cs_options [getCsoundFlags flags]
    mkOrch = cs_instruments . map (vcat . map text . lines)
    dsco   = cs_score $ buildScoreDoc sco



-- | Write a unified @.csd@ file with a pre-written orchestra
-- file.
--
writeUnifiedScore :: FilePath -> CsoundFlags -> FilePath -> PrimScore -> IO ()
writeUnifiedScore out_path flags orch_file sco = do
    orch <- readFile orch_file
    writeFile out_path $ show $ csound_synthesizer dflags (mkOrch orch) dsco
  where
    dflags = cs_options [getCsoundFlags flags]
    mkOrch = cs_instruments . (\a -> [a]) . vcat . map text . lines
    dsco   = cs_score $ buildScoreDoc sco




data CsoundFlags = CsoundFlags { getCsoundFlags :: String }
  deriving (Eq,Ord,Show)

flags_rt_audio :: CsoundFlags
flags_rt_audio = CsoundFlags "-odac     ;;; RT audio out"


-- | Note - file path is passed to the shell.
-- 
flags_wav_file_out :: FilePath -> CsoundFlags
flags_wav_file_out wav = CsoundFlags $ body 
  where
    body = "-o " ++ wav ++ " -W"


-- | Note - file path is passed to the shell.
-- 
flags_aiff_file_out :: FilePath -> CsoundFlags
flags_aiff_file_out aiff = CsoundFlags $ body 
  where
    body = "-o " ++ aiff ++ " -A"


-- | Enable display for @display@ opcode, no audio is generated.
-- 
flags_display :: CsoundFlags
flags_display = CsoundFlags $ body 
  where
    body = "--displays"



--------------------------------------------------------------------------------
-- Printing scores

buildScoreDoc :: PrimScore -> Doc
buildScoreDoc = evalScoMonad . score


data St = St 
      { cummulative_time    :: Double 
      , scale_factor        :: Double
      , carry_props         :: InstStmtProps
      }

st_zero :: St
st_zero = St { cummulative_time = 0 
             , scale_factor     = 1
             , carry_props      = false_inst 
             }

false_inst :: InstStmtProps
false_inst = InstStmtProps { inst_num     = (-1)
                           , inst_dur     = (-1.0)
                           , inst_params  = [] } 


-- | ScoMonad is a State monad.
-- 
-- State tracks onset time and \"carry\" printing.
--
newtype ScoMonad a = ScoMonad { getScoMonad :: St -> (a,St) }



instance Functor ScoMonad where
  fmap f ma = ScoMonad $ \s -> let (a, s1) = getScoMonad ma s in (f a, s1)


instance Applicative ScoMonad where
  pure a    = ScoMonad $ \s -> (a, s)
  mf <*> ma = ScoMonad $ \s -> let (f,s1) = getScoMonad mf s
                                   (a,s2) = getScoMonad ma s1
                               in (f a,s2)

instance Monad ScoMonad where
  return a  = ScoMonad $ \s -> (a, s)
  ma >>= k  = ScoMonad $ \s -> let (a,s1) = getScoMonad ma s
                                   (b,s2) = (getScoMonad . k) a s1
                               in (b, s2)


evalScoMonad :: ScoMonad a -> a
evalScoMonad ma = fst $ getScoMonad ma st_zero


-- | This is for nesting in scores...
--
localBumpLocale :: Double -> Double -> ScoMonad a -> ScoMonad a
localBumpLocale tx sx ma = ScoMonad $ \s -> 
    let t0 = cummulative_time s
        sf = scale_factor s
    in getScoMonad ma (s { cummulative_time = t0 + tx, scale_factor = sf * sx })


-- | This is for contiguous notes...
--
nextOnsetTime :: Double -> ScoMonad Double
nextOnsetTime dt = ScoMonad $ \s -> let t0 = cummulative_time s
                                        sf = scale_factor s
                                        t1 = t0 + sf * dt
                                    in (t1, s { cummulative_time = t1 } )




-- | This is for Scores at the same level within the tree...
--
resetOnsetTime :: Double -> ScoMonad ()
resetOnsetTime t0 = ScoMonad $ \s -> ((), s { cummulative_time = t0 } )


-- | This is Scores at the same level within the tree...
--
getOnsetTime :: ScoMonad Double
getOnsetTime = ScoMonad $ \s -> (cummulative_time s, s)


zeroInst :: ScoMonad ()
zeroInst = ScoMonad $ \s -> ((), s { carry_props = false_inst })

-- | Return a list of rendered pfields - dot indicates same.
-- 
-- Obviously if the instrument changes all fields are /fresh/.
--
-- It\'s not enforced but the same instrument should always have 
-- pfield lists of the same length.
--
pfieldDiffs :: InstStmtProps -> ScoMonad [Doc]
pfieldDiffs new@(InstStmtProps i _ ps) = ScoMonad $ \s ->
    if i == (inst_num $ carry_props s)
       then let ans = diff (inst_params $ carry_props s) ps
            in (ans, s { carry_props = new })
       else (map field ps, s {carry_props = new})
  where
    field                 = padr 10 . format

    diff (x:xs) (y:ys)    = step x y
      where
        step (CsInt a)    (CsInt b) 
             | a == b     = (padr 10 $ char '.') : diff xs ys

        step (CsDouble a) (CsDouble b)
             | a `tEQ` b  = (padr 10 $ char '.') : diff xs ys

        step _             v2 = (padr 10 $ format v2) : diff xs ys

    -- These cases shouldn't match...
    diff []     ys     = map field ys
    diff xs     []     = map field xs


--------------------------------------------------------------------------------
-- Translation

score :: PrimScore -> ScoMonad Doc
score sco = flat <$> scoreSections sco 
  where
    flat []     = empty
    flat [d]    = d `vconcat` char 'e' 
    flat (d:ds) = d `vconcat` (char 'b' <+> int 0) `vconcat` flat ds


scoreSections :: PrimScore -> ScoMonad [Doc]
scoreSections (PrimLeaf loc@(_,_,gs) ones) = bracketLocale loc $ 
    (\gens a -> [ gens `vconcat` a] ) <$> (fmap vcat $ mapM genStmt gs)
                                      <*> (zeroInst >> oneConcat primitive ones)

scoreSections (PrimScore loc ones)         = 
    bracketLocale loc $ getOnsetTime >>= \t0 -> step t0 (viewl ones) 
  where
    step _  EmptyL    = return []
    step t0 (a :< as) = (++) <$> scoreSections a 
                             <*> (resetOnsetTime t0 >> step t0 (viewl as))


bracketLocale :: Locale -> ScoMonad a -> ScoMonad a
bracketLocale (_,frm,_) ma = let ogin = frame_origin frm
                                 sf   = frame_scaling frm
                             in localBumpLocale ogin sf ma

oneConcat :: (a -> ScoMonad Doc) -> JoinList a -> ScoMonad Doc
oneConcat fn ones = outstep (viewl ones)
  where
    outstep (e :< rest)   = fn e >>= \a -> instep a (viewl rest)
    outstep EmptyL        = return empty
    
    instep ac EmptyL      = return ac
    instep ac (e :< rest) = fn e >>= \a -> instep (ac `vconcat` a) (viewl rest)

genStmt :: GenStmt -> ScoMonad Doc
genStmt (GenStmt ix sz gen args) =
   (\ot -> cs_sco_table_stmt ix ot sz gen args) <$> nextOnsetTime 0
    

primitive :: PrimStmt -> ScoMonad Doc
primitive (InstStmt dt props) =
    (\ot ps -> cs_sco_inst_stmt (inst_num props) ot (inst_dur props) ps) 
      <$> nextOnsetTime dt <*> pfieldDiffs props


infix 4 `tEQ`

-- | Tolerant equality - helper function for defining Eq instances
-- that use tolerance.
--
-- Note - the definition actually needs Ord which is 
-- unfortunate (as Ord is /inaccurate/).
--
tEQ :: Double -> Double -> Bool
tEQ a b = (abs (a-b)) < 0.000001


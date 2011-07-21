{-# OPTIONS -Wall #-}

module Demo01 where

import Sound.FMSS.AbstractSyntax
import Sound.FMSS.Datatypes
import Sound.FMSS.Translate
import Sound.FMSS.Utils.FormatCombinators


demo01 :: IO ()
demo01 = print $ expsegEnvelope [ (0,0), (0.1,1), (10,0.6), (25,0.3)
                                , (50,0.15), (90,0.1), (100,0)
                                ]

env1 :: (String,Doc)
env1 = linearEnvelope [ (0,0), (0.1,1), (10,0.6), (25,0.3)
                      , (50,0.15), (90,0.1), (100,0)
                      ]

osc1 :: CarrierOsc
osc1 = CarrierOsc 6 (BaseScaler 2.4)


dummy1 = either print print $ (fmap (format) $ translate fm1)

fm1 :: FMSynth
fm1 = FMSynth { fm_instr_num  = 1
              , fm_sinetbl    = 1
              , fm_amp_env    = Just env1
              , fm_mods       = [om]
              , fm_cars       = [oc]
              , fm_links      = [ModCar 1 1]
              }

  where
    om = ModulatorOsc 1 (BaseScaler 1.4)
    oc = CarrierOsc 1  (BaseScaler 1.0)

demo02 = do 
    print $ vcat $ map format $ decls fm1

demo03 = do 
    print $ vcat $ map format $ carriers fm1

demo04 = do 
    print $ fmap (vcat . map format) $ modulators fm1

linearEnvelope :: [(Double,Double)] -> (String,Doc)
linearEnvelope []            = ("linseg", int 0)
linearEnvelope (v@(_,a0):vs) = ("linseg", commaSep (dtrunc a0 : work v vs))
  where
    work _      []               = [] 
    work (d0,_) (x@(d1,amp):xs)  = drn d1 d0 : dtrunc amp : work x xs


    drn d1 d0 = text "idur*" <> dtrunc ((d1 - d0) / 100)



expsegEnvelope :: [(Double,Double)] -> Doc
expsegEnvelope []            = text "expseg .01"
expsegEnvelope (v@(_,a0):vs) = 
    text "expseg" <+> commaSep (ppamp a0 : work v vs)
  where
    work _      []               = [] 
    work (d0,_) (x@(d1,amp):xs)  = drn d1 d0 : ppamp amp : work x xs


    drn d1 d0 = text "idur*" <> dtrunc ((d1 - d0) / 100)

    ppamp a | a == 0    = text ".01"
            | otherwise = dtrunc a



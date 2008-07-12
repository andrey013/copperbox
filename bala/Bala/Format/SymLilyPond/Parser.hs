{-# LANGUAGE Rank2Types #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymLilyPond.Parser
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  Rank 2 types.
--
-- Parser for a subset of LilyPond files
--
--------------------------------------------------------------------------------

module Bala.Format.SymLilyPond.Parser where

import Bala.Format.Base.SymBase
import Bala.Format.SymLilyPond.Datatypes
import Bala.Format.SymLilyPond.SyntaxElements

import Bala.Base.BaseExtra

import Data.List (sortBy)
import Prelude hiding (break)
import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad

import Text.ParserCombinators.Parsec hiding (space)


--------------------------------------------------------------------------------
-- * Helpers 

fchoice :: (c -> Parser b) -> [(c, a)] -> Parser a
fchoice f xs = choice $ map (interp f) xs
  where
    interp f (a,b) = b <$ f a

longestChoice :: (String -> Parser b) ->  [(String,a)] -> Parser a
longestChoice = withLongestString
  
attrParse :: (Attribute elt att, CAttr repr) 
          => Parser (repr att)
          -> repr elt 
          -> Parser (repr elt)
attrParse p elt = option elt (do {att <- p; return (elt %% att)})

-- For Haddock infix operators starting with # need spacing.

infixl 6 ##

-- | @##@ - attribute bind - chain attribute parsers like bind (>>=),
-- but the steps in the chain are optional.
( ## ) :: (Attribute elt att, CAttr repr) 
       => Parser (repr elt) 
       -> Parser (repr att)
       -> Parser (repr elt) 
( ## ) p pa = p >>= attrParse pa


-- | Placeholder
noBlock :: (CBlock repr) => Parser (repr Block)
noBlock = undefined <$ (lexChar '{') <*> (lexChar '}')


meterFraction :: Parser MeterFraction
meterFraction = (//) <$> (int <* char '/') <*> lexeme int


many1Cat :: (CSnocList repr ctx, ListContext ctx a) 
         => repr (SnocList ctx) 
         -> Parser (repr a) 
         -> Parser (repr (SnocList ctx))
many1Cat cxnil p = do 
    a <- p
    step1 p (snoc cxnil a)
  where
    step1 p acc = do
      a <- optparse p    
      case a of      
        Nothing -> return acc
        Just a  -> step1 p (snoc acc a)


--------------------------------------------------------------------------------
-- * Lexer combinators

-- | Parse a command token. 
-- Implementation note using try is essential, otherwise we might the 
-- forward slash will be consumed.
command :: String -> CharParser st String
command ss = lexeme $ try $ string ('\\':ss)

--------------------------------------------------------------------------------
-- * Knot tying
-- NOTE this will need some thought...

data Para repr = Para { 
      parseDuration :: Parser (repr Duration)
    , parsePitch    :: Parser (repr Pitch) 
    }



  
--------------------------------------------------------------------------------
-- * Basic notation (6)
-- ** Pitches (6.1)
-- *** Normal pitches (6.1.1)


  

-- | Pitch, plus attributes (knot tied).
pitchA :: (CPitch repr,
           CAttr repr, 
           CAccidental repr, 
           CMicroTone repr,
           COctaveSpec repr) 
       => Parser (repr Pitch)
pitchA = lexeme $ pPitch ## pAccidental ## pMicroTone ## pOctaveSpec 


pPitch :: (CPitch repr) => Parser (repr Pitch)
pPitch = choice $ map fn xs
  where
    fn (ch,cnstr) = cnstr <$ char ch   
    xs = [('c',      _c),
          ('d',      _d),
          ('e',      _e),
          ('f',      _f),
          ('g',      _g),
          ('a',      _a),
          ('b',      _b)]
    
   
pOctaveSpec :: (COctaveSpec repr) => Parser (repr OctaveSpec)
pOctaveSpec = pRaised <|> pLowered
  where
    pRaised  = raised  <$> counting1 (char '\'')
    pLowered = lowered <$> counting1 (char ',')
    

pNote :: (CNote repr) => Para repr -> Parser (repr Note)
pNote px = note <$> (parsePitch px)

--------------------------------------------------------------------------------
-- *** Accidentals (6.1.2)
pAccidental :: (CAccidental repr) => Parser (repr Accidental)
pAccidental= longestChoice lexString $
  [ ("isis",  doubleSharp)
  , ("eses",  doubleFlat)
  , ("is",    sharp)
  , ("es",    flat) 
  ]


--------------------------------------------------------------------------------
-- *** Cautionary accidentals (6.1.3)

pCautionaryAccidental :: (CCautionaryAccidental repr) 
                      => Parser (repr CautionaryAccidental)
pCautionaryAccidental= fchoice lexChar $ 
  [('!', reminderAccidental), ('^', cautionaryAccidental)]
  
  
--------------------------------------------------------------------------------
-- *** Micro tones (6.1.4)

pMicroTone :: (CMicroTone repr) => Parser (repr MicroTone)
pMicroTone = longestChoice string $ 
  [("ih", halfFlat), ("eh", halfFlat)] 


--------------------------------------------------------------------------------
-- *** Relative octaves (6.1.6)

pRelative :: (CCmdRelative repr, CBlock repr)
          => Para repr -> Parser (repr CmdRelative)
pRelative px = relative <$> (command "relative" *> (parsePitch px)) <*> noBlock
  
  
      
--------------------------------------------------------------------------------
-- *** Rests (6.1.9)

pRest :: (CRest repr) => Parser (repr Rest)
pRest = rest <$ lexChar 'r'


--------------------------------------------------------------------------------  
-- *** Skips (6.1.10)

pSkipDuration :: (CSkipDuration repr, CDuration repr) 
              => Para repr -> Parser (repr SkipDuration)
pSkipDuration px = skipDuration <$> (char 's' *> (parseDuration px))
  
--------------------------------------------------------------------------------  
-- ** Rhythms (6.2)
-- *** Durations (6.2.1)



pDuration :: CDuration repr => Parser (repr Duration)
pDuration = duration <$> int



--------------------------------------------------------------------------------
-- *** Augmentation dots (6.2.2)

pDotted :: (CDotted repr) => Parser (repr Dotted)
pDotted = dotted <$> counting1 (char '.')  

--------------------------------------------------------------------------------
-- *** Tuplets (6.2.3)

-- pTimes :: (CCmdTimes repr, CBlock repr)
--       => Para repr ->  Parser (repr (CmdTimes ctx))
pTimes px = times <$> (command "times" *> meterFraction) 
                  <*> braces (many1Cat elementCtx (pNote px))  


--------------------------------------------------------------------------------
-- ** Mutliple notes at once (6.3)
-- *** Chords (6.3.1)

pChord :: (CChord repr) => Para repr -> Parser (repr Chord)
pChord px = chord <$> angles (many1 (parsePitch px))


--------------------------------------------------------------------------------
-- ** Staff notation (6.4)
-- *** Key signature (6.4.2)

pKeyType :: (CCmdKeyType repr) => Parser (repr CmdKeyType) 
pKeyType = longestChoice command $ 
  [ ("major",       major)
  , ("minor",       minor)
  , ("ionian",      ionian)
  , ("locrian",     locrian)
  , ("aeolian",     aeolian)
  , ("mixolydian",  mixolydian)
  , ("lydian",      lydian)
  , ("phrygian",    phrygian)
  , ("dorian",      dorian)
  ]
          
--------------------------------------------------------------------------------
-- ** Connecting notes (6.5)
-- *** Manual beams (6.5.6)

pOpenBeam, pCloseBeam :: (CBeam repr) => Parser (repr Beam) 
pOpenBeam   = openBeam <$ lexChar '['
pCloseBeam  = closeBeam <$ lexChar ']'

--------------------------------------------------------------------------------
-- ** Expressive marks (6.6)
-- ***  Articulations (6.6.1)

pVerticalPlacement  :: (CVerticalPlacement repr) 
                    => Parser (repr VerticalPlacement) 
pVerticalPlacement = fchoice char $
  [ ('^', verticalPlacement VAbove)
  , ('_', verticalPlacement VBelow)
  , ('-', verticalPlacement VDefault)
  ] 

pCmdArticulation  :: (CCmdArticulation repr) 
                  => Parser (repr CmdArticulation)
pCmdArticulation = longestChoice command $ 
  [ ("accent",              accent)
  , ("marcato",             marcato)
  , ("staccatissimo",       staccatissimo)
  , ("espressivo",          espressivo)
  , ("staccato",            staccato)
  , ("tenuto",              tenuto)
  , ("portato",             portato)
  , ("upbow",               upbow)
  , ("downbow",             downbow)
  , ("flageolet",           flageolet)
  , ("thumb",               thumb)
  , ("lheel",               lheel)
  , ("rheel",               rheel)
  , ("ltoe",                ltoe)
  , ("rtoe",                rtoe)
  , ("open",                open)
  , ("stopped",             stopped)
  , ("turn",                turn)
  , ("reverseturn",         reverseturn)
  , ("trill",               trill)
  , ("prall",               prall)
  , ("mordent",             mordent)
  , ("prallprall",          prallprall)
  , ("prallmordent",        prallmordent)
  , ("upprall",             upprall)
  , ("downprall",           downprall)
  , ("upmordent",           upmordent)
  , ("downmordent",         downmordent)
  , ("pralldown",           pralldown)
  , ("prallup",             prallup)
  , ("lineprall",           lineprall)
  , ("signumcongruentiae",  signumcongruentiae)
  , ("shortfermata",        shortfermata)
  , ("fermata",             fermata)
  , ("longfermata",         longfermata)
  , ("verylongfermata",     verylongfermata)
  , ("segno",               segno)
  , ("coda",                coda)
  , ("varcoda",             varcoda) 
  ]
    
    
--------------------------------------------------------------------------------
-- *** Dynamics (6.6.3)
-- nullary commands (use underscore suffix _ for commands)

pCmdDynamic :: (CCmdDynamic repr) 
                  => Parser (repr CmdDynamic)
pCmdDynamic = longestChoice command $
  [ ("ppppp",           ppppp_)
  , ("pppp",            pppp_)
  , ("ppp",             ppp_)
  , ("pp",              pp_)
  , ("p",               piano)
  , ("mp",              mp_)
  , ("mf",              mf_)
  , ("fp",              fp_)
  , ("ffff",            ffff_)
  , ("fff",             fff_)  
  , ("ff",              ff_)
  , ("f",               forte)
  , ("sff",             sff_)
  , ("sfz",             sfz_)
  , ("sf",              sf_)
  , ("spp",             spp_)
  , ("sp",              sp_)
  , ("rfz",             rfz_)
  , ("<",               openCrescendo)
  , (">",               openDecrescendo)
  , ("!",               closeDynamic)
  , ("cr",              cr_)
  , ("decr",            decr_)
  , ("dynamicUp",       dynamicUp)
  , ("dynamicDown",     dynamicDown)
  , ("dynamicNeutral",  dynamicNeutral)
  ]
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- * Instrument-specific notation (7)

-- ** Vocal music (7.3)

-- *** Melismata (7.3.5)
pMelismata :: (CMelismata repr) => Parser (repr Melismata) 
pMelismata = longestChoice command $
  [("melisma", melisma), ("melismaEnd", melismaEnd)]
  
  
--------------------------------------------------------------------------------
-- ** Rhythmic music (7.4)

-- *** Entering percussion (7.4.2)

pDrumPitchName :: CDrumPitchName repr => Parser (repr DrumPitchName)
pDrumPitchName = longestChoice lexString $ 
  [ ("acousticbassdrum",  acousticbassdrum)
  , ("bassdrum",          bassdrum)
  , ("hisidestick",       hisidestick)
  , ("sidestick",         sidestick)
  , ("losidestick",       losidestick)
  , ("acousticsnare",     acousticsnare)
  , ("snare",             snare)
  , ("handclap",          handclap)
  , ("electricsnare",     electricsnare)
  , ("lowfloortom",       lowfloortom)
  , ("closedhihat",       closedhihat)
  , ("hihat",             hihat)
  , ("highfloortom",      highfloortom)
  , ("pedalhihat",        pedalhihat)
  , ("lowtom",            lowtom)
  , ("openhihat",         openhihat)
  , ("halfopenhihat",     halfopenhihat)
  , ("lowmidtom",         lowmidtom)
  , ("himidtom",          himidtom)
  , ("crashcymbala",      crashcymbala)
  , ("crashcymbal",       crashcymbal)
  , ("hightom",           hightom)
  , ("ridecymbala",       ridecymbala)
  , ("ridecymbal",        ridecymbal)
  , ("chinesecymbal",     chinesecymbal)
  , ("ridebell",          ridebell) 
  , ("tambourine",        tambourine)
  , ("splashcymbal",      splashcymbal)
  , ("cowbell",           cowbell)
  , ("crashcymbalb",      crashcymbalb)
  , ("vibraslap",         vibraslap)
  , ("ridecymbalb",       ridecymbalb)
  , ("mutehibongo",       mutehibongo)
  , ("hibongo",           hibongo)
  , ("openhibongo",       openhibongo)
  , ("mutelobongo",       mutelobongo)
  , ("lobongo",           lobongo)
  , ("openlobongo",       openlobongo)
  , ("mutehiconga",       mutehiconga)
  , ("muteloconga",       muteloconga)
  , ("openhiconga",       openhiconga)
  , ("hiconga",           hiconga)
  , ("openloconga",       openloconga)
  , ("loconga",           loconga)
  , ("hitimbale",         hitimbale)
  , ("lotimbale",         lotimbale)
  , ("hiagogo",           hiagogo)
  , ("loagogo",           loagogo)
  , ("cabasa",            cabasa)
  , ("maracas",           maracas)
  , ("shortwhistle",      shortwhistle)
  , ("longwhistle",       longwhistle)
  , ("shortguiro",        shortguiro)
  , ("longguiro",         longguiro)
  , ("guiro",             guiro)
  , ("claves",            claves)
  , ("hiwoodblock",       hiwoodblock)
  , ("lowoodblock",       lowoodblock)
  , ("mutecuica",         mutecuica)
  , ("opencuica",         opencuica)
  , ("mutetriangle",      mutetriangle)
  , ("triangle",          triangle)
  , ("opentriangle",      opentriangle)
  , ("oneup",             oneup)
  , ("twoup",             twoup)
  , ("threeup",           threeup)
  , ("fourup",            fourup)
  , ("fiveup",            fiveup)
  , ("onedown",           onedown)
  , ("twodown",           twodown)
  , ("threedown",         threedown)
  , ("fourdown",          fourdown)
  , ("fivedown",          fivedown)
  , ("bda",               bda)
  , ("bd",                bd)
  , ("ssh",               ssh)
  , ("ss",                ss)
  , ("ssl",               ssl)
  , ("sna",               sna)
  , ("sn",                sn)
  , ("hc",                hc)
  , ("sne",               sne)
  , ("tomfl",             tomfl)
  , ("hhc",               hhc)
  , ("hh",                hh)
  , ("tomfh",             tomfh)
  , ("hhp",               hhp)
  , ("toml",              toml)
  , ("hho",               hho)
  , ("hhho",              hhho)
  , ("tomml",             tomml)
  , ("tommh",             tommh)
  , ("cymca",             cymca)
  , ("cymc",              cymc)
  , ("tomh",              tomh)
  , ("cymra",             cymra)
  , ("cymr",              cymr)
  , ("cymch",             cymch)
  , ("rb",                rb)
  , ("tamb",              tamb)
  , ("cyms",              cyms)
  , ("cb",                cb)
  , ("cymcb",             cymcb)
  , ("vibs",              vibs)
  , ("cymrb",             cymrb)
  , ("bohm",              bohm)
  , ("boh",               boh)
  , ("boho",              boho)
  , ("bolm",              bolm)
  , ("bol",               bol)
  , ("bolo",              bolo)
  , ("cghm",              cghm)
  , ("cglm",              cglm)
  , ("cgho",              cgho)
  , ("cgh",               cgh)
  , ("cglo",              cglo)
  , ("cgl",               cgl)
  , ("timh",              timh)
  , ("timl",              timl)
  , ("agh",               agh)
  , ("agl",               agl)
  , ("cab",               cab)
  , ("mar",               mar)
  , ("whs",               whs)
  , ("whl",               whl)
  , ("guis",              guis)
  , ("guil",              guil)
  , ("gui",               gui)
  , ("cl",                cl)
  , ("wbh",               wbh)
  , ("wbl",               wbl)
  , ("cuim",              cuim)
  , ("cuio",              cuio)
  , ("trim",              trim)
  , ("tri",               tri)
  , ("trio",              trio)
  , ("tt",                tt)
  , ("ua",                ua)
  , ("ub",                ub)
  , ("uc",                uc)
  , ("ud",                ud)
  , ("ue",                ue)
  , ("da",                da)
  , ("db",                db)
  , ("dc",                dc)
  , ("dd",                dd)
  , ("de",                de)
  ] 
  
     
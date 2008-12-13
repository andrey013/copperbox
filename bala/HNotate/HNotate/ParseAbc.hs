{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.ParseAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- ParseAbc - parse Abc files for template holes.
--
--------------------------------------------------------------------------------

module HNotate.ParseAbc where

import HNotate.CommonUtils
import HNotate.Duration
import qualified HNotate.FPList as Fpl
import HNotate.MusicRepDatatypes
import HNotate.NotateMonad
import HNotate.ParserBase
import HNotate.Pitch
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>), empty)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromJust)
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (space)


abcExprParse :: ExprParser
abcExprParse path =  
    (liftIO $ readFile path) >>= (o1 . preprocessAbc) 
                             >>= (o2 . parse parseAbc path)
                             >>= eitherSkM (o3 . translateAbcScore) 
  where
    o1 = textoutput 3 "Postprocessed Abc source..."
    o2 = witness    3 "Parsed Abc source..."
    o3 = witness    3 "Abc translated to expression representation..."

       
--------------------------------------------------------------------------------
-- translate


translateAbcScore :: AbcScore -> [Expr]
translateAbcScore (AbcScore xs) = catMaybes $ map transAbcTune xs

transAbcTune :: AbcTune -> Maybe Expr
transAbcTune (AbcTune _ xs) = maybe Nothing fn (transExprs xs)
  where
    fn expr = Just $ Let LetNone expr


-- The complicated bit...  turning a list into a tree.
-- Branches should end in output-directives, if they don't then they get pruned. 

transExprs :: [AbcExpr] -> Maybe Expr
transExprs = tree `flip` id where 
  tree []                         k = k Nothing
  
  -- last element - (ideally) should be an output command  
  tree [AbcOutput mo]             k = k $ Just (Do (transMetaOutput mo))
  -- last element - 'prune' the tree if it is a binding
  tree [AbcFieldBinding _]        k = k Nothing
  tree [AbcMetaBinding _]         k = k Nothing
  
  tree (AbcOutput o:xs)           k = k $ tree xs fn where
      -- New continuation function for rest of input.
      -- If 'fn' gets Nothing, then the tree to the right was pruned 
      -- (as it contained no further output commands), so 'fn'
      -- returns a 'single output' (Do).  
      -- If there are more output commands in the right-hand side of 
      -- the list - (Just expr) - then we have a sequential-do (SDo) 
      fn = maybe (Just $ Do (transMetaOutput o))  --  
                 (\expr -> Just $ DoExpr (transMetaOutput o) expr)
        
  tree (AbcFieldBinding b:xs)     k = k $ tree xs fn where
      -- New continuation function, if the right-hand side of is empty
      -- then this new binding is redundant - pass the Nothing on. 
      -- Otherwise _nest_.
      fn = maybe Nothing (\expr -> Just $ Let (transAbcField b) expr)       

  tree (AbcMetaBinding b:xs)     k = k $ tree xs fn where
      -- Logic as per AbcFieldBinding.
      fn = maybe Nothing (\expr -> Just $ Let (transMetaBinding b) expr)   
      
      

             
transAbcField :: AbcField -> Binding
transAbcField (AbcKey (Just key)) = LetKey key
transAbcField (AbcKey Nothing)    = LetNone
transAbcField (AbcMeter m)        = LetMeter m




--------------------------------------------------------------------------------
-- preprocess

-- Doesn't work if metadirectives '#% output: tune' 
-- aren't on their own line
preprocessAbc :: String -> String
preprocessAbc = 
    unlines . map (++ ";") . filter (`isPrefixedBy` abc_prefixes) . lines
  where
    isPrefixedBy ss pres = any (`isPrefixOf` ss) pres  
    
    
abc_prefixes :: [String]
abc_prefixes = ["X:", "%#", "M:", "K:"]

--------------------------------------------------------------------------------
-- parse (works on postprocessed source)

parseAbc :: Parser AbcScore
parseAbc = AbcScore <$> many parseAbcTune


parseAbcTune :: Parser AbcTune
parseAbcTune = AbcTune <$> tuneStart <*> many abcExpr

tuneStart :: Parser Int
tuneStart = field 'X' int

abcExpr :: Parser AbcExpr
abcExpr = choice [fieldUpdate, metaExpr]

fieldUpdate :: Parser AbcExpr
fieldUpdate = AbcFieldBinding <$> choice 
    [ keyField, meterField]
    
keyField :: Parser AbcField
keyField = AbcKey <$> field 'K' (keySig <|> badKeySig)
    
meterField :: Parser AbcField
meterField = AbcMeter <$> field 'M' timeSig
  
metaExpr :: Parser AbcExpr
metaExpr = (try $ symbol "%#") *> metas <* symbol ";"
  where
    metas             = choice [abcOutputDef, abcMeterPattern, abcPartial]
    
    abcOutputDef      = AbcOutput      <$> metaOutput
    abcMeterPattern   = AbcMetaBinding <$> metaMeterPattern
    abcPartial        = AbcMetaBinding <$> metaPartial
    



timeSig :: Parser Meter
timeSig = TimeSig <$> int <*> (char '/' *> int)


-- TODO accidentals 
-- see ABC 2.0 spec - Klezmer (Ahavoh Rabboh) / Arabic music (Maqam Hedjaz)

-- K:<tonic> <mode> <accidentals>
-- Key fields may also have clef designators
keySig :: Parser (Maybe Key)
keySig = (\lbl mode accs -> Just $ Key lbl mode accs) 
    <$> lexeme keyPitchLabel <*> option Major (lexeme abcMode) 
            <*> ((many accidentalPitch) <* whiteSpace <* optionMaybe clefDesignator)

-- Pitch labels in key signatures have the form 
-- <upper-pitchletter> (<optional-#> | <optional-b>)
keyPitchLabel :: Parser PitchLabel
keyPitchLabel = PitchLabel <$> pitchLetter <*> option Nat sharpflat
  where
    sharpflat = choice [ Sharp <$ symbol "#", Flat <$ symbol "b"] 

-- e.g. ^f _b _c -- all labels should have a single accidental prefix
-- which saves lookahead problems
accidentalPitch :: Parser PitchLabel
accidentalPitch = (flip PitchLabel) <$> accidental <*> pitchLetter
  where 
    accidental = choice [ Sharp <$ symbol "^", Flat <$ symbol "_"]


-- Key fields might only have designators
badKeySig :: Parser (Maybe Key)
badKeySig = Nothing <$ clefDesignator 


clefDesignator :: Parser  ()
clefDesignator = () <$ (try clefstart *> stringTill (symbol ";"))
  where
    clefstart = choice $ map symbol $
      [ "clef", "treble", "alto", "tenor", "bass", "perc", "none",
        "+8", "-8" , "transpose", "middle" ]

abcDuration :: Parser Duration
abcDuration = (\n d -> convRatio (n%d)) <$> int <*> (char '/' *> int)


abcPitch :: Parser Pitch
abcPitch = build <$> abcAccidental <*> basenote <*> octaveDiff
  where build a (i,nt) od  = Pitch nt a (i + od)



-- C is middle C (C4)
-- c is one octve above (C5)
-- C, is (C3)
-- c' is (C5)

-- c, or C' shouldn't occur in Abc files but will be normalized anyway


pitchLetter :: Parser PitchLetter
pitchLetter = (fromJust . fromLChar) 
    <$> satisfy ((flip elem) "CDEFGABcdefgab")
    
basenote :: Parser (Int,PitchLetter)
basenote = build <$> satisfy ((flip elem) "CDEFGABcdefgab")
  where 
    build ch        = (octave ch, fromJust $ fromLChar ch) -- match guaranteed
    octave ch       = if isUpper ch then 4 else 5



abcAccidental :: Parser Accidental
abcAccidental = option Nat (choice [dblSharp, sharp, dblFlat, flat, natural])
  where dblSharp = DoubleSharp <$ string "^^" 
        sharp    = Sharp       <$ string "^"  
        dblFlat  = DoubleFlat  <$ string "__" 
        flat     = Flat        <$ string "_"  
        natural  = Nat         <$ string "="
        
octaveDiff :: Parser Int    
octaveDiff = option 0 (choice [octaveLow, octaveHigh])
  where octaveLow   = negate  <$> counting1 (char ',')
        octaveHigh  = counting1 (char '\'')
        
        
abcMode :: Parser Mode
abcMode = choice 
    [ major, lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian, 
      minor {- minor must be last - to respect longest match -} 
    ]
  where minor       = Minor      <$ choice 
              [leading3 'm' 'i' 'n', "M" <$ charDrop 'M', "m" <$ charDrop 'm']
        major       = Major      <$ leading3 'm' 'a' 'j'
        lydian      = Lydian     <$ leading3 'l' 'y' 'd'
        ionian      = Ionian     <$ leading3 'i' 'o' 'n'
        mixolydian  = Mixolydian <$ leading3 'm' 'i' 'x'
        dorian      = Dorian     <$ leading3 'd' 'o' 'r'
        aeolian     = Aeolian    <$ leading3 'a' 'e' 'o'
        phrygian    = Phrygian   <$ leading3 'p' 'h' 'r'
        locrian     = Locrian    <$ leading3 'l' 'o' 'c' 

--------------------------------------------------------------------------------
-- Parse the text for the water and holes so we can fill the holes

    
abcTextSource :: Parser ParsedTemplate
abcTextSource = Fpl.zipbuild <$> stringTill recognizeIsland <*> many islandWater


islandWater :: Parser (SrcLoc,String)
islandWater = (\(loc,_) ss -> (loc,ss)) 
    <$> (try $ withLoc recognizeIsland) <*> stringTill (eof <|> recognizeIsland)

recognizeIsland :: Parser ()
recognizeIsland = () <$ try (lexeme (symbol "%#") *> lexeme (symbol "output")
                                                  *> uptoNewline)
  where
    uptoNewline = do 
      tok <- eitherparse anyChar eof
      case tok of
        Left '\n' -> return ()
        Left _    -> uptoNewline 
        Right _   -> return ()
        
         
                      
                      
--------------------------------------------------------------------------------
-- utility parsers  


field :: Char -> Parser a -> Parser a
field c p = (try $ lexeme (symbol [c,':'])) *> p <* symbol ";"

-- Abc looks only at the first 3 characters of a mode specification     
leading3 :: Char -> Char -> Char -> Parser String   
leading3 a b c = lexeme $ try (caten <$> p a <*> p b <*> p c <*> many letter)
  where caten x1 x2 x3 xs = x1:x2:x3:xs
        p ch = choice [char ch, char $ toUpper ch] 

                
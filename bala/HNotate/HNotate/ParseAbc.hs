
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
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.ParserBase
import HNotate.Pitch
import HNotate.TemplateDatatypes

import Control.Applicative hiding (many, optional, (<|>), empty)
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Sequence hiding (reverse)
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (space)



-- FilePath -> NotateT IO (Either ParseError [Expr])
abcExprParse :: ExprParser
abcExprParse file_path = 
    abcFileParse file_path >>=
    either (return . Left) (return . Right . translateAbcScore)  
    
abcFileParse :: FilePath -> NotateT IO (Either ParseError AbcScore)
abcFileParse path = 
    liftIO (readFile path) >>= return . parse parseAbc path . preprocessAbc
--------------------------------------------------------------------------------
-- translate


translateAbcScore :: AbcScore -> [Expr]
translateAbcScore (AbcScore xs) = catMaybes $ map transAbcTune xs

transAbcTune :: AbcTune -> Maybe Expr
transAbcTune (AbcTune i xs) = maybe Nothing fn (transExprs xs)
  where
    fn expr = Just $ Let LetNone expr

transExprs :: [AbcExpr] -> Maybe Expr
transExprs xs = tree xs id where 
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
                 (\expr -> Just $ SDo (transMetaOutput o) expr)
        
  tree (AbcFieldBinding b:xs)     k = k $ tree xs fn where
      -- New continuation function, if the right-hand side of is empty
      -- then this new binding is redundant - pass the Nothing on. 
      -- Otherwise _nest_.
      fn = maybe Nothing (\expr -> Just $ Let (transAbcField b) expr)       

  tree (AbcMetaBinding b:xs)     k = k $ tree xs fn where
      -- Logic as per AbcFieldBinding.
      fn = maybe Nothing (\expr -> Just $ Let (transMetaBinding b) expr)   
      
      

             
transAbcField :: AbcField -> Binding
transAbcField (AbcKey key) = LetKey key
transAbcField (AbcMeter m) = LetMeter m




--------------------------------------------------------------------------------
-- preprocess

-- Doesn't work if metadirectives '#% output: tune' 
-- aren't on their own line
preprocessAbc :: String -> String
preprocessAbc = 
    unlines . filter (`isPrefixedBy` abc_prefixes) . lines
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
keyField = AbcKey <$> field 'K' keySig
    
meterField :: Parser AbcField
meterField = AbcMeter <$> field 'M' timeSig
  
metaExpr :: Parser AbcExpr
metaExpr = (try $ symbol "%#") >> choice [abcOutputDef, abcMeterPattern]
  where
    abcOutputDef      = AbcOutput <$> metaOutput
    abcMeterPattern   = AbcMetaBinding <$> metaMeterPattern
    



timeSig :: GenParser Char st Meter
timeSig = TimeSig <$> int <*> (char '/' *> int)


-- TODO accidentals 
-- see ABC 2.0 spec - Klezmer (Ahavoh Rabboh) / Arabic music (Maqam Hedjaz)

-- This has an error - might consume characters from the next line...
-- "K:C \nM:4/4" -- the M for meter will be consumed as M for minor 
keySig :: GenParser Char st Key
keySig = (\(Pitch l a _) m -> Key (PitchLabel l a) m) 
    <$> lexeme abcPitch <*> option Major abcMode


abcDuration :: GenParser Char st Duration
abcDuration = (\n d -> convRatio (n%d)) <$> int <*> (char '/' *> int)


abcPitch :: GenParser Char st Pitch
abcPitch = build <$> abcAccidental <*> basenote <*> octaveDiff
  where build a (i,nt) od  = Pitch nt a (i + od)



-- C is middle C (C4)
-- c is one octve above (C5)
-- C, is (C3)
-- c' is (C5)

-- c, or C' shouldn't occur in Abc files but will be normalized anyway


basenote :: GenParser Char st (Int,PitchLetter)
basenote = build <$> satisfy ((flip elem) "CDEFGABcdefgab")
  where build ch        = (octave ch, letter $ toUpper ch)
        octave ch       = if isUpper ch then 4 else 5
        
        letter 'C'      = C;   letter 'D'       = D;
        letter 'E'      = E;   letter 'F'       = F;
        letter 'G'      = G;   letter 'A'       = A;
        letter 'B'      = B;   letter _         = error "bassenote"

abcAccidental :: GenParser Char st Accidental
abcAccidental = option Nat (choice [dblSharp, sharp, dblFlat, flat, natural])
  where dblSharp = DoubleSharp <$ string "^^" 
        sharp    = Sharp       <$ string "^"  
        dblFlat  = DoubleFlat  <$ string "__" 
        flat     = Flat        <$ string "_"  
        natural  = Nat         <$ string "="
        
octaveDiff :: GenParser Char st Int    
octaveDiff = option 0 (choice [octaveLow, octaveHigh])
  where octaveLow   = negate  <$> counting1 (char ',')
        octaveHigh  = counting1 (char '\'')
        
        
abcMode :: GenParser Char st Mode
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

abcTextChunks :: TextChunkParser
abcTextChunks = collectWaterAcc (metaOutput)
  where 
    metaOutput = (,,) <$> lexeme (symbol "%#") 
                      <*> lexeme (symbol "output")
                      <*> uptoNewline ""                    
    
    -- Important - must not consume the newline
    -- Use direct recursion as 'many' parser can't be used 
    -- (empty string restriction). 
    uptoNewline cca = do 
        at_end <- option False (True <$ eof) 
        case at_end of
          True -> return (reverse cca)
          False -> optparse (satisfy (/='\n')) >>=
                   maybe (return $ reverse cca) (\c -> uptoNewline (c:cca))  

                   
    


--------------------------------------------------------------------------------
-- utility parsers  


field :: Char -> Parser a -> Parser a
field c p = (try $ lexeme (symbol [c,':'])) >> p

-- Abc looks only at the first 3 characters of a mode specification     
leading3 :: Char -> Char -> Char -> GenParser Char st String   
leading3 a b c = lexeme $ try (caten <$> p a <*> p b <*> p c <*> many letter)
  where caten a b c xs = a:b:c:xs
        p a = choice [char a, char $ toUpper a] 

                
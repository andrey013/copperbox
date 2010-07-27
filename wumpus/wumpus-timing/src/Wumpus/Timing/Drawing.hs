{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.Drawing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Timing.Drawing
  (
    Posn(..)
  )
  where

import Wumpus.Timing.Alphabet
import Wumpus.Timing.STraceMonad

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.SVGColours

import MonadLib                         -- package: monadLib

data Posn = High
          | Mid
          | Low
  deriving (Eq,Ord,Show)


data DrawingState = DrawingState 
      { line_colour :: DRGB
      , fill_colour :: DRGB
      , x_pos       :: Double
      , y_pos       :: Posn
      }

data DrawingConfig = DrawingConfig { half_height :: !Double }

newtype DrawingM a = DrawingM 
          { getDrawingM ::  StateT  DrawingState
                          ( ReaderT DrawingConfig  
                          ( STraceT DPrimitive  Id)) a }


instance Functor DrawingM where
  fmap f = DrawingM . fmap f . getDrawingM

instance Monad DrawingM where
  return a = DrawingM $ return a
  m >>= k  = DrawingM $ getDrawingM m >>= getDrawingM . k


instance StateM DrawingM DrawingState where
  get   = DrawingM $ get
  set c = DrawingM $ set c


updateYPos :: Transition -> DrawingM ()
updateYPos T_Zero = return ()
updateYPos HM     = sets_ (\s -> s { y_pos = Mid  })
updateYPos HL     = sets_ (\s -> s { y_pos = Low  })
updateYPos MH     = sets_ (\s -> s { y_pos = High })
updateYPos MHC    = sets_ (\s -> s { y_pos = Mid  })
updateYPos ML     = sets_ (\s -> s { y_pos = Low  })
updateYPos LH     = sets_ (\s -> s { y_pos = High })
updateYPos LHC    = sets_ (\s -> s { y_pos = High })
updateYPos LM     = sets_ (\s -> s { y_pos = Mid  })

updateColour :: Instruction -> DrawingM ()
updateColour (Line _ rgb)     = sets_  (\s -> s { line_colour = rgb   }) 
updateColour (CData _ fill _) = sets_  (\s -> s { line_colour = black
                                                , fill_colour = fill  })
updateColour (OData _ fill _) = sets_  (\s -> s { line_colour = black
                                                , fill_colour = fill  })
updateColour (Clock _ _)      = sets_  (\s -> s { line_colour = black })
updateColour (Metastasis _)   = sets_  (\s -> s { line_colour = brown }) 
updateColour (Space _)        = return () 
updateColour Glitch           = return ()


-- Update colour before drawing, update transition after 
-- drawing...
--
step1 :: (Instruction,Transition) -> DrawingM ()
step1 (ins,trans) = updateColour ins >> draw ins >> updateYPos trans


draw :: Instruction -> DrawingM ()
draw _ = return ()


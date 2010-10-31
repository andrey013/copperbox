{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Base types for Drawing Objects, Graphics / Images (a Graphic 
-- that also returns an answer), etc.
-- 
-- Notes on prefix and suffix names:
--
-- Function types suffixed @F@ are functions from same-to-same, e.g.:
--
-- > type Point2F u = Point2 u -> Point2 u
--
-- Functional types subfixed @R@ are functions from some static 
-- context to the answer type (c.f the ReaderMonad), e.g.:
--
-- > newtype DrawingR a = DrawingR { getDrawingR :: DrawingContext -> a }
--
-- The suffix @M@ is used for classes defining monadic actions.
--
-- The prefix @Loc@ indicates a functional type 
-- /from Point2 to something.../
-- 
-- The prefix @ThetaLoc@ indicates a functional type 
-- /from Direction (radian) then Point to something.../
--
-- \*\* WARNING \*\* - some names are expected to change.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Base
  (

    DrawingR(..) -- temporarily open

  , LocDrawingR
  , ThetaLocDrawingR
  , ConnectorDrawingR
  , DLocDrawingR
  , DThetaLocDrawingR
  , DConnectorDrawingR


  -- * Graphic  
  , Graphic
  , LocGraphic
  , ThetaLocGraphic
  , ConnectorGraphic

  , DGraphic
  , DLocGraphic
  , DThetaLocGraphic
  , DConnectorGraphic


  -- * Image
  , Image
  , LocImage
  , ThetaLocImage
  , ConnectorImage

  , DImage
  , DLocImage
  , DThetaLocImage
  , DConnectorImage


  -- * Run functions
  , runDrawingR
  , runGraphic
  , runLocGraphic
  , runImage
  , runLocImage

  -- * Extractors
  , drawingCtx
  , locCtx
  , locPoint
  , thetaLocCtx
  , thetaLocAng
  , thetaLocPoint
  , connCtx
  , connStart
  , connEnd

  -- * Combinators
  , moveLoc

  , promote
  , raise
  , compose

  , cardinalprime
  , idstarstar

  -- * Pre-transformers
  , trafo2
  , trafo3a
  , trafo3b

  -- * Cruft

  , Point2F
  , DPoint2F

  , DrawingTrafoF 
  , GraphicTrafoF

  , superiorGraphic
  , anteriorGraphic  

  , xlinkGraphic



  , ImageTrafoF
  , intoImageTrafo
  , imageTrafoDrawing
  , imageTrafoGraphic

  , intoImage
  , intoLocImage
  , xlinkImage

  , VecGraphic
  , DVecGraphic

  , intoVecGraphic
  , intoConnectorImage
  , intoThetaLocImage


  ) where

import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.Prim
import Wumpus.Basic.Utils.Combinators
-- import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core


import Control.Applicative
import Data.Monoid


--------------------------------------------------------------------------------
--

-- | Drawings in Wumpus-Basic have an implicit /graphics state/ 
-- the @DrawingContext@, the most primitive building block is 
-- a function from the DrawingContext to some polymorphic answer.
-- 
-- This functional type is represented concretely as @DrawingR@.
-- 
-- > DrawingR :: DrawingContext -> a 
--
newtype DrawingR a = DrawingR { getDrawingR :: DrawingContext -> a }


type LocDrawingR        u a = DrawingR (Point2 u -> a)

type ThetaLocDrawingR   u a = DrawingR (Radian -> Point2 u -> a)

type ConnectorDrawingR  u a = DrawingR (Point2 u -> Point2 u -> a)



type DLocDrawingR a         = LocDrawingR       Double a
type DThetaLocDrawingR a    = ThetaLocDrawingR  Double a
type DConnectorDrawingR a   = ConnectorDrawingR Double a


--------------------------------------------------------------------------------
-- Graphic

-- Simple drawing - produce a primitive, access the DrawingContext
-- if required.
--
type Graphic u      = DrawingR (PrimGraphic u)

-- | Commonly graphics take a start point as well as a drawing 
-- context.
-- 
-- Here they are called a LocGraphic - graphic with a (starting) 
-- location.
--
type LocGraphic u   = LocDrawingR u (PrimGraphic u)


-- | A function from @angle * point -> graphic@
--
type ThetaLocGraphic u          = ThetaLocDrawingR u (PrimGraphic u)

-- | ConnectorGraphic is a connector drawn between two points 
-- contructing a Graphic.
--
type ConnectorGraphic u         = ConnectorDrawingR u (PrimGraphic u)




type DGraphic           = Graphic Double
type DLocGraphic        = LocGraphic Double
type DThetaLocGraphic   = ThetaLocGraphic Double
type DConnectorGraphic  = ConnectorGraphic Double


type instance DUnit (Graphic u) = u


--------------------------------------------------------------------------------
-- Image

-- | Images return a value as well as drawing. A /node/ is a 
-- typical example - nodes are drawing but the also support 
-- taking anchor points.
--
type Image u a          = DrawingR (a, PrimGraphic u)

type LocImage u a       = LocDrawingR u (a,PrimGraphic u)

type ThetaLocImage u a  = ThetaLocDrawingR u (a,PrimGraphic u)

-- | ConnectorImage is a connector drawn between two points 
-- constructing an Image.
--
-- Usually the answer type of a ConnectorImage will be a Path so
-- the Points ar @midway@, @atstart@ etc. can be taken on it.
--
type ConnectorImage u a = ConnectorDrawingR u (a, PrimGraphic u)





type DImage a           = Image Double a
type DLocImage a        = LocImage Double a
type DThetaLocImage a   = ThetaLocImage Double a 
type DConnectorImage a  = ConnectorImage Double a



type instance DUnit (Image u a) = u






--------------------------------------------------------------------------------
-- DrawingR instances

instance Functor DrawingR where
  fmap f ma = DrawingR $ \ctx -> f $ getDrawingR ma ctx 


instance OPlus a => OPlus (DrawingR a)  where
  fa `oplus` fb = DrawingR $ \ctx -> 
                      getDrawingR fa ctx `oplus` getDrawingR fb ctx

-- The monoid instance seems sensible...
--
instance Monoid a => Monoid (DrawingR a) where 
  mempty          = DrawingR $ \_   -> mempty
  fa `mappend` fb = DrawingR $ \ctx -> 
                      getDrawingR fa ctx `mappend` getDrawingR fb ctx

-- Applicative

instance Applicative DrawingR where
  pure a    = DrawingR $ \_   -> a
  mf <*> ma = DrawingR $ \ctx -> let f = getDrawingR mf ctx
                                     a = getDrawingR ma ctx
                                 in f a

-- Monad 

instance Monad DrawingR where
  return a  = DrawingR $ \_   -> a
  ma >>= k  = DrawingR $ \ctx -> let a = getDrawingR ma ctx
                                 in (getDrawingR . k) a ctx 


instance DrawingCtxM DrawingR where
  askDC           = DrawingR $ \ctx -> ctx
  localize upd df = DrawingR $ \ctx -> getDrawingR df (upd ctx)
  

--------------------------------------------------------------------------------
-- Graphic instances


-- Affine instances

instance (Real u, Floating u) => Rotate (Graphic u) where
  rotate ang = liftA (rotate ang) 


instance (Real u, Floating u) => RotateAbout (Graphic u) where
  rotateAbout ang pt = liftA (rotateAbout ang pt)


instance Num u => Scale (Graphic u) where
  scale sx sy = liftA (scale sx sy)


instance Num u => Translate (Graphic u) where
  translate dx dy = liftA (translate dx dy)

--------------------------------------------------------------------------------
-- Image instances

-- Affine instances

instance (Real u, Floating u, Rotate a, DUnit a ~ u) => 
    Rotate (Image u a) where
  rotate ang = liftA (prod (rotate ang) (rotate ang))


instance (Real u, Floating u, RotateAbout a, DUnit a ~ u) => 
    RotateAbout (Image u a) where
  rotateAbout ang pt = liftA (prod (rotateAbout ang pt) (rotateAbout ang pt))


instance (Num u, Scale a, DUnit a ~ u) => Scale (Image u a) where
  scale sx sy = liftA (prod (scale sx sy) (scale sx sy))


instance (Num u, Translate a, DUnit a ~ u) => Translate (Image u a) where
  translate dx dy = liftA (prod (translate dx dy) (translate dx dy))



--------------------------------------------------------------------------------
-- Run functions



-- | Run a /Drawing Function/ with the supplied /Drawing Context/.
--
runDrawingR :: DrawingContext -> DrawingR a -> a
runDrawingR ctx df = getDrawingR df ctx



runGraphic :: DrawingContext -> Graphic u -> PrimGraphic u
runGraphic ctx gf = (getDrawingR gf) ctx


runLocGraphic :: DrawingContext -> LocGraphic u -> Point2 u -> PrimGraphic u
runLocGraphic ctx gf = (getDrawingR gf) ctx


runImage :: DrawingContext -> Image u a -> (a, PrimGraphic u)
runImage ctx img = (getDrawingR img) ctx

runLocImage :: DrawingContext -> LocImage u a -> Point2 u -> (a, PrimGraphic u)
runLocImage ctx img = (getDrawingR img) ctx



--------------------------------------------------------------------------------
-- extractors 

drawingCtx      :: DrawingR DrawingContext
drawingCtx      = DrawingR $ \ctx -> ctx


locCtx          :: LocDrawingR u DrawingContext
locCtx          = DrawingR $ \ctx _  -> ctx

locPoint        :: LocDrawingR u (Point2 u)
locPoint        = DrawingR $ \_ pt -> pt


thetaLocCtx     :: ThetaLocDrawingR u DrawingContext
thetaLocCtx     = DrawingR $ \ctx _ _ -> ctx

thetaLocAng     :: ThetaLocDrawingR u Radian
thetaLocAng     = DrawingR $ \_ ang _ -> ang

thetaLocPoint   :: ThetaLocDrawingR u (Point2 u)
thetaLocPoint   = DrawingR $ \_ _ pt -> pt


connCtx         :: ConnectorDrawingR u DrawingContext
connCtx         = DrawingR $ \ctx _ _ -> ctx

connStart       :: ConnectorDrawingR u (Point2 u) 
connStart       = DrawingR $ \_ pt _ -> pt

connEnd         :: ConnectorDrawingR u (Point2 u) 
connEnd         = DrawingR $ \_ _ pt -> pt

--------------------------------------------------------------------------------
-- Combinators

moveLoc :: (Point2 u -> Point2 u) -> LocDrawingR u a -> LocDrawingR u a
moveLoc pf mf =  trafo2 pf mf
   



promote :: (a -> DrawingR b) -> DrawingR (a -> b)
promote f = DrawingR $ \ctx a -> getDrawingR (f a) ctx

-- | Type specialized @pure@.
--
raise :: (a -> b) -> DrawingR (a -> b) 
raise = pure



compose :: DrawingR (b -> c) -> DrawingR (a -> b) -> DrawingR (a -> c)
compose f g = DrawingR $ \ctx a -> getDrawingR f ctx (getDrawingR g ctx a)


-- cardinal'  :: (a -> r1 -> ans) -> (r2 -> a) -> r1 -> r2 -> ans

-- | This is a /Cardinal-prime/
--
-- (b -> ctx -> c) -> (a -> b) -> ctx -> a -> c
--
cardinalprime :: (b -> DrawingR c) -> (a -> b) -> DrawingR (a -> c)
cardinalprime f g = promote f `compose` (raise g)


-- idstarstar  :: (r1 -> r2 -> ans) -> r1 -> r2 -> ans
--
-- | /id**/
--
-- > (ctx -> a -> b) -> a -> ctx -> b
--
idstarstar :: DrawingR (a -> b) -> a -> DrawingR b
idstarstar mf a = DrawingR $ \ctx -> getDrawingR mf ctx a


--------------------------------------------------------------------------------
-- Pre-transformers

-- The primary /pre-transformation/ function is @localize@, here 
-- it is without the newtype wrapper or the specific types:
--
-- > fn :: (r -> r) -> (r -> a) -> r -> a
-- > fn f g = \ctx -> g (f ctx)
--
-- This a a type restricted version of reverse appliction, the T
-- combinator sometimes called queer, sometings @(#)@ in Haskell.
-- In Wumpus-Basic it is @localize@ which is overloaded for the
-- DrawingR newtype and Drawing monads.
--
-- > localizeDR :: (DrawingContext -> DrawingContext) -> DrawingR a -> DrawingR a
-- > localizeDR = localize
--
-- Figuartively speaking this is an opposite of the fmap function 
-- instance. Instead of (post-) transforming the output, ... 
-- (pre-) transforms  the input.
-- 
-- Due to DrawingR functionals being stacked in a particular way
-- @localize@ always pre-transforms the DrawingContext regardless
-- of the arity of the functional:
--
-- > localizeDR2 :: (DrawingContext -> DrawingContext) 
-- >             -> DrawingR (r -> a) -> DrawingR (r -> a)
-- > localizeDR2 = localize
-- 

trafo2 :: (r1 -> a) -> DrawingR (a -> b) -> DrawingR (r1 -> b)
trafo2 f mf = DrawingR $ \ctx a -> getDrawingR mf ctx (f a)

trafo3a :: (r1 -> a) -> DrawingR (a -> r2 -> b) -> DrawingR (r1 -> r2 -> b)
trafo3a f mf = DrawingR $ \ctx a b -> getDrawingR mf ctx (f a) b

trafo3b :: (r2 -> a) -> DrawingR (r1 -> a -> b) -> DrawingR (r1 -> r2 -> b)
trafo3b f mf = DrawingR $ \ctx a b -> getDrawingR mf ctx a (f b)



--------------------------------------------------------------------------------



type DrawingTrafoF a = DrawingR a -> DrawingR a



-- | Point transformation function.
--
type Point2F u = Point2 u -> Point2 u

type DPoint2F = Point2F Double





xlinkGraphic :: XLink -> Graphic u -> Graphic u
xlinkGraphic xlink gf = DrawingR $ \ctx -> 
    let a = runGraphic ctx gf 
    in primGraphic $ xlinkGroup xlink [getPrimGraphic a]





type GraphicTrafoF u = Graphic u -> Graphic u

anteriorGraphic :: Graphic u -> GraphicTrafoF u
anteriorGraphic = anterior

superiorGraphic :: Graphic u -> GraphicTrafoF u
superiorGraphic = superior




--------------------------------------------------------------------------------



--------------------------------------------------------------------------------







intoImage :: DrawingR a -> Graphic u -> Image u a
intoImage f g = forkA f g




type ImageTrafoF u a = Image u a -> Image u a




intoImageTrafo :: DrawingTrafoF a -> GraphicTrafoF u -> ImageTrafoF u a
intoImageTrafo df gf img = img >>= \(a,prim) -> 
    intoImage (df $ pure a) (gf $ pure prim)

imageTrafoDrawing :: DrawingTrafoF a -> ImageTrafoF u a
imageTrafoDrawing df = intoImageTrafo df id

imageTrafoGraphic :: GraphicTrafoF u -> ImageTrafoF u a
imageTrafoGraphic gf = intoImageTrafo id gf




intoLocImage :: LocDrawingR u a -> LocGraphic u -> LocImage u a
intoLocImage f g = 
    DrawingR $ \ctx pt -> (getDrawingR f ctx pt, getDrawingR g ctx pt)


xlinkImage :: XLink -> Image u a -> Image u a
xlinkImage xlink img = DrawingR $ \ctx -> 
    let (a,pg) = runImage ctx img 
    in (a, primGraphic $ xlinkGroup xlink [getPrimGraphic pg])


--------------------------------------------------------------------------------

type VecGraphic u = LocImage u (Point2 u)

type DVecGraphic = VecGraphic Double


-- does the point transformer need Ctx?

intoVecGraphic :: (Point2 u -> Point2 u) -> LocGraphic u -> VecGraphic u
intoVecGraphic f g = undefined -- \pt -> intoImage (pure $ f pt) (g pt)

--------------------------------------------------------------------------------
--





intoConnectorImage :: ConnectorDrawingR u a 
                   -> ConnectorGraphic u 
                   -> ConnectorImage u a
intoConnectorImage f g = undefined -- forkA (f p1 p2) (g p1 p2)






intoThetaLocImage :: ThetaLocDrawingR u a 
                  -> ThetaLocGraphic u 
                  -> ThetaLocImage u a
intoThetaLocImage f g = undefined -- forkA (f theta pt) (g theta pt) 



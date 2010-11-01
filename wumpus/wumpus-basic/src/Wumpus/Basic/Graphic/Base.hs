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
  , LocThetaDrawingR
  , ConnectorDrawingR
  , DLocDrawingR
  , DLocThetaDrawingR
  , DConnectorDrawingR


  -- * Graphic  
  , Graphic
  , LocGraphic
  , LocThetaGraphic
  , ConnectorGraphic

  , DGraphic
  , DLocGraphic
  , DLocThetaGraphic
  , DConnectorGraphic


  -- * Image
  , Image
  , LocImage
  , LocThetaImage
  , ConnectorImage

  , DImage
  , DLocImage
  , DLocThetaImage
  , DConnectorImage


  -- * Run functions
  , runDrawingR
  , runGraphic
  , runLocGraphic
  , runImage
  , runLocImage

  -- * Extractors
  , drawingCtx
  , queryDrawing
  , locCtx
  , locPoint
  , locThetaCtx
  , locThetaAng
  , locThetaPoint
  , connCtx
  , connStart
  , connEnd

  -- * Combinators
  , moveLoc

  , wrap
  , wrap1
  , wrap2

  , promote1
  , promote2

  , raise
  , raise1
  , raise2

  , static1 
  , static2
  , dblstatic

  , compose

  , bind
  , bind1
  , bind2

  , cardinalprime
  , idstarstar


  -- * Pre-transformers
  , trafo1
  , trafo2a
  , trafo2b

  -- * Dropping answers
  , extrGraphic
  , extrLocGraphic

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
  , intoLocThetaImage


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

type LocThetaDrawingR   u a = LocDrawingR u (Radian -> a)

type ConnectorDrawingR  u a = LocDrawingR u (Point2 u -> a)



type DLocDrawingR a         = LocDrawingR       Double a
type DLocThetaDrawingR a    = LocThetaDrawingR  Double a
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


-- | A function from @point * angle -> graphic@
--
type LocThetaGraphic u          = LocThetaDrawingR u (PrimGraphic u)

-- | ConnectorGraphic is a connector drawn between two points 
-- contructing a Graphic.
--
type ConnectorGraphic u         = ConnectorDrawingR u (PrimGraphic u)




type DGraphic           = Graphic Double
type DLocGraphic        = LocGraphic Double
type DLocThetaGraphic   = LocThetaGraphic Double
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

type LocThetaImage u a  = LocThetaDrawingR u (a,PrimGraphic u)

-- | ConnectorImage is a connector drawn between two points 
-- constructing an Image.
--
-- Usually the answer type of a ConnectorImage will be a Path so
-- the Points ar @midway@, @atstart@ etc. can be taken on it.
--
type ConnectorImage u a = ConnectorDrawingR u (a, PrimGraphic u)





type DImage a           = Image Double a
type DLocImage a        = LocImage Double a
type DLocThetaImage a   = LocThetaImage Double a 
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

queryDrawing    :: (DrawingContext -> a) -> DrawingR a
queryDrawing f  = DrawingR $ \ctx -> f ctx

locCtx          :: LocDrawingR u DrawingContext
locCtx          = DrawingR $ \ctx _  -> ctx

locPoint        :: LocDrawingR u (Point2 u)
locPoint        = DrawingR $ \_ pt -> pt


locThetaCtx     :: LocThetaDrawingR u DrawingContext
locThetaCtx     = DrawingR $ \ctx _ _ -> ctx

locThetaAng     :: LocThetaDrawingR u Radian
locThetaAng     = DrawingR $ \_ _ ang -> ang

locThetaPoint   :: LocThetaDrawingR u (Point2 u)
locThetaPoint   = DrawingR $ \_ pt _ -> pt


connCtx         :: ConnectorDrawingR u DrawingContext
connCtx         = DrawingR $ \ctx _ _ -> ctx

connStart       :: ConnectorDrawingR u (Point2 u) 
connStart       = DrawingR $ \_ pt _ -> pt

connEnd         :: ConnectorDrawingR u (Point2 u) 
connEnd         = DrawingR $ \_ _ pt -> pt

--------------------------------------------------------------------------------
-- Combinators

moveLoc :: (Point2 u -> Point2 u) -> LocDrawingR u a -> LocDrawingR u a
moveLoc = trafo1


-- | Lift a pure value into a Drawing. The Drawing Context is 
-- /ignored/.
--
-- > ans -> (ctx -> ans)
--
-- This is the same as the 'raise' combinator at when raising into
-- the drawing Context. But the /arity family/ of wrap combinators 
-- is different.
--
wrap :: a -> DrawingR a
wrap = pure


-- | Lift a pure value into a Drawing, ignoring both the Drawing 
-- Context and the functional argument (e.g. start point for a 
-- @LocDrawing@).
--
-- > ans -> (ctx -> r1 -> ans)
--
wrap1 :: a -> DrawingR (r1 -> a)
wrap1 = pure . pure

-- | Lift a pure value into a Drawing, ignoring both the Drawing 
-- Context and the two functional arguments (e.g. start point and 
-- theta for a @LocThetaDrawing@).
--
-- > ans -> (ctx -> r1 -> r2 -> ans)
--
wrap2 :: a -> DrawingR (r1 -> r2 -> a)
wrap2 = pure . pure . pure

   

-- | Promote a one argument drawing function into a one argument 
-- drawing /functional/.
--
-- The type signature is probably more illustrative of the 
-- operation than this description:
--
-- > (r1 -> ctx -> ans) -> (ctx -> r1 -> ans)
--
-- This is essentially the @cardinal@ combinator - @flip@
-- in Haskell.
--
promote1 :: (r1 -> DrawingR ans) -> DrawingR (r1 -> ans)
promote1 f = DrawingR $ \ctx a -> getDrawingR (f a) ctx


-- | Promote a two argument drawing function into a two argument 
-- drawing /functional/.
--
-- The type signature is probably more illustrative of the 
-- operation than this description:
--
-- > (r1 -> r2 -> ctx -> ans) -> (ctx -> r1 -> r2 -> ans)
--
promote2 :: (r1 -> r2 -> DrawingR ans) -> DrawingR (r1 -> r2 -> ans)
promote2 df = DrawingR $ \ctx a b -> getDrawingR (df a b) ctx


-- | Lift a value into a Drawing.
--
-- > ans -> (ctx -> ans)
--
-- Essentially this is the @kestrel@ combinator - @const@ in Haskell.
--
raise :: a -> DrawingR a
raise = pure


-- | Lift a one argument function into a Drawing /functional/.
--
-- This is 'ctxFree' with a specialized type signature. 
--
raise1 :: (r1 -> ans) -> DrawingR (r1 -> ans) 
raise1 = pure

-- | Lift a two argument function into a Drawing /functional/.
--
-- This is 'ctxFree' with a specialized type signature.
--
raise2 :: (r1 -> r2 -> ans) -> DrawingR (r1 -> r2 -> ans) 
raise2 = pure


-- | Extend the arity of a /drawing functional/, the original 
-- function is oblivious to the added argument.
--
-- Typically this combinator is used to take a @Graphic@ to a
-- @LocGraphic@ ingoring the start point (figuratively a @Graphic@ 
-- is not /coordinate free/). 
--
-- > (ctx -> ans) -> (ctx -> r1 -> ans)
--
-- This was called the J-combinator by Joy, Rayward-Smith and
-- Burton (ref. /Compling Functional Languages/ by Antoni 
-- Diller), however it is not the J combinator commonly in the 
-- Literature. 
--
static1 :: DrawingR ans -> DrawingR (r1 -> ans)
static1 df = DrawingR $ \ctx _ -> getDrawingR df ctx


-- | Extend the arity of a /drawing functional/, the original 
-- function is oblivious to the added argument.
--
-- Typically this combinator is used to take a @LocGraphic@ to a
-- @LocThetaGraphic@ ingoring the angle of direction.
--
-- > (ctx -> r1 -> ans) -> (ctx -> r1 -> r2 -> ans)
--
-- This was called the J-Prime combinator by Joy, Rayward-Smith 
-- and Burton (ref. /Compling Functional Languages/ by Antoni 
-- Diller). 
--
static2 :: DrawingR (r1 -> ans) -> DrawingR (r1 -> r2 -> ans)
static2 df = DrawingR $ \ctx a _ -> getDrawingR df ctx a


-- needs new name!
--
-- > (ctx -> ans) -> (ctx -> r1 -> r2 -> ans)
--
dblstatic :: DrawingR ans -> DrawingR (r1 -> r2 -> ans)
dblstatic df = DrawingR $ \ctx _ _ -> getDrawingR df ctx



compose :: DrawingR (b -> c) -> DrawingR (a -> b) -> DrawingR (a -> c)
compose f g = DrawingR $ \ctx a -> getDrawingR f ctx (getDrawingR g ctx a)




bind :: DrawingR a -> (a -> DrawingR ans) -> DrawingR ans
bind df dk = DrawingR $ \ctx -> 
    let z = getDrawingR df ctx in getDrawingR (dk z) ctx


bind1 :: DrawingR (r1 -> a) -> (a -> DrawingR (r1 -> ans)) -> DrawingR (r1 -> ans)
bind1 df dk = DrawingR $ \ctx a -> 
    let z = getDrawingR df ctx a in getDrawingR (dk z) ctx a

bind2 :: DrawingR (r1 -> r2 -> a) -> (a -> DrawingR (r1 -> r2 -> ans)) 
      -> DrawingR (r1 -> r2 -> ans)
bind2 df dk = DrawingR $ \ctx a b -> 
    let z = getDrawingR df ctx a b in getDrawingR (dk z) ctx a b




-- cardinal'  :: (a -> r1 -> ans) -> (r2 -> a) -> r1 -> r2 -> ans

-- | This is a /Cardinal-prime/
--
-- (b -> ctx -> c) -> (a -> b) -> ctx -> a -> c
--
cardinalprime :: (b -> DrawingR c) -> (a -> b) -> DrawingR (a -> c)
cardinalprime f g = promote1 f `compose` (raise g)


-- idstarstar  :: (r1 -> r2 -> ans) -> r1 -> r2 -> ans
--
-- | /id**/
--
-- > (ctx -> a -> b) -> a -> ctx -> b
--
idstarstar :: DrawingR (a -> b) -> a -> DrawingR b
idstarstar df a = DrawingR $ \ctx -> getDrawingR df ctx a




--------------------------------------------------------------------------------
-- Pre-transformers

-- The primary /pre-transformation/ function is @localize@, here 
-- it is without the newtype wrapper or the specific types:
--
-- > fn :: (ctx -> ctx) -> (ctx -> ans) -> ctx -> ans
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

trafo1 :: (r1 -> a) -> DrawingR (a -> b) -> DrawingR (r1 -> b)
trafo1 f mf = DrawingR $ \ctx a -> getDrawingR mf ctx (f a)

trafo2a :: (r1 -> a) -> DrawingR (a -> r2 -> b) -> DrawingR (r1 -> r2 -> b)
trafo2a f mf = DrawingR $ \ctx a b -> getDrawingR mf ctx (f a) b

trafo2b :: (r2 -> a) -> DrawingR (r1 -> a -> b) -> DrawingR (r1 -> r2 -> b)
trafo2b f mf = DrawingR $ \ctx a b -> getDrawingR mf ctx a (f b)


-------------------------------------------------------------------------------
-- Dropping /answers/


extrGraphic :: Image u a -> Graphic u
extrGraphic img = DrawingR $ \ctx -> snd $ getDrawingR img ctx 


extrLocGraphic :: LocImage u a -> LocGraphic u
extrLocGraphic img = DrawingR $ \ctx pt -> snd $ getDrawingR img ctx pt



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






intoLocThetaImage :: LocThetaDrawingR u a 
                  -> LocThetaGraphic u 
                  -> LocThetaImage u a
intoLocThetaImage f g = undefined -- forkA (f theta pt) (g theta pt) 



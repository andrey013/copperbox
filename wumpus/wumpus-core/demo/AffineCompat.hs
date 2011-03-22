{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- | Note - this code was in the module @Wumpus.Core.AffineTrans@
-- up to revision 0.50.0. 
-- 
-- At revision 0.50.0 Wumpus-Core was substantially revised to 
-- support polymorphic units in Wumpus-Drawing and Wumpus-Basic.
-- Perhaps surprisingly, supporting units in the higher-level 
-- code actually meant reducing \"support\" for units in 
-- Wumpus-Core - though in practice unit support in Wumpus-Core 
-- was \"theorectical\" and was not particulary useful.
--
-- A more serious consequence was that the affine transformation
-- code had to be \"devalued\" - it is now considered a base that 
-- a proper treatment of affine transformations will use as 
-- primitives. 
-- 
-- A further consequence of this is that the affine utility 
-- functions previously defined in @Wumpus.Core.AffineTrans@ are 
-- not really useful anymore, a higher-level layer would have to 
-- re-define them accordingly to work with any replacements for 
-- the affine classes. So, they are here present here both for 
-- examples to use and for anyone to take and re-define if they
-- need compatibility for their code.
--
-- The code in this file is public-domain and can be changed
-- as necessary.
--
--
--------------------------------------------------------------------------------


module AffineCompat where


import Wumpus.Core

-------------------------------------------------------------------------------- 
-- Common rotations



-- | Rotate by 30 degrees about the origin. 
--
rotate30 :: AffineRotate t => t -> t
rotate30 = affineRotate (pi/6) 

-- | Rotate by 30 degrees about the supplied point.
--
rotate30About :: AffineRotateAbout t => DPoint2 -> t -> t
rotate30About = affineRotateAbout (pi/6) 

-- | Rotate by 45 degrees about the origin. 
--
rotate45 :: AffineRotate t => t -> t
rotate45 = affineRotate (pi/4) 

-- | Rotate by 45 degrees about the supplied point.
--
rotate45About :: AffineRotateAbout t => DPoint2 -> t -> t
rotate45About = affineRotateAbout (pi/4)

-- | Rotate by 60 degrees about the origin. 
--
rotate60 :: AffineRotate t => t -> t
rotate60 = affineRotate (2*pi/3) 

-- | Rotate by 60 degrees about the supplied point.
--
rotate60About :: AffineRotateAbout t => DPoint2 -> t -> t
rotate60About = affineRotateAbout (2*pi/3)

-- | Rotate by 90 degrees about the origin. 
--
rotate90 :: AffineRotate t => t -> t
rotate90 = affineRotate (pi/2) 

-- | Rotate by 90 degrees about the supplied point.
--
rotate90About :: AffineRotateAbout t => DPoint2 -> t -> t
rotate90About = affineRotateAbout (pi/2)

-- | Rotate by 120 degrees about the origin. 
--
rotate120 :: AffineRotate t => t -> t
rotate120 = affineRotate (4*pi/3) 

-- | Rotate by 120 degrees about the supplied point.
--
rotate120About :: AffineRotateAbout t => DPoint2 -> t -> t
rotate120About = affineRotateAbout (4*pi/3)



--------------------------------------------------------------------------------
-- Common scalings

-- | Scale both x and y dimensions by the same amount.
--
uniformScale :: AffineScale t => Double -> t -> t
uniformScale a = affineScale a a 

-- | Reflect in the X-plane about the origin.
--
reflectX :: AffineScale t => t -> t
reflectX = affineScale (-1) 1

-- | Reflect in the Y-plane about the origin.
--
reflectY :: AffineScale t => t -> t
reflectY = affineScale 1 (-1)

--------------------------------------------------------------------------------
-- Translations

-- | Translate by the x and y components of a vector.
--
translateBy :: AffineTranslate t => DVec2 -> t -> t
translateBy (V2 x y) = affineTranslate x y


--------------------------------------------------------------------------------
-- Translation and scaling

-- | Reflect in the X plane that intersects the supplied point. 
--
reflectXPlane :: (AffineScale t, AffineTranslate t) 
              => DPoint2 -> t -> t
reflectXPlane (P2 x y) = 
    affineTranslate x y . affineScale (-1) 1 . affineTranslate (-x) (-y)

-- | Reflect in the Y plane that intersects the supplied point.
--
reflectYPlane :: (AffineScale t, AffineTranslate t) 
              => DPoint2 -> t -> t
reflectYPlane (P2 x y) = 
    affineTranslate x y . affineScale 1 (-1) . affineTranslate (-x) (-y)

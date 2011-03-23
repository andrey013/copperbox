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
rotate30 :: DRotate t => t -> t
rotate30 = drotate (pi/6) 

-- | Rotate by 30 degrees about the supplied point.
--
rotate30About :: DRotateAbout t => DPoint2 -> t -> t
rotate30About = drotateAbout (pi/6) 

-- | Rotate by 45 degrees about the origin. 
--
rotate45 :: DRotate t => t -> t
rotate45 = drotate (pi/4) 

-- | Rotate by 45 degrees about the supplied point.
--
rotate45About :: DRotateAbout t => DPoint2 -> t -> t
rotate45About = drotateAbout (pi/4)

-- | Rotate by 60 degrees about the origin. 
--
rotate60 :: DRotate t => t -> t
rotate60 = drotate (2*pi/3) 

-- | Rotate by 60 degrees about the supplied point.
--
rotate60About :: DRotateAbout t => DPoint2 -> t -> t
rotate60About = drotateAbout (2*pi/3)

-- | Rotate by 90 degrees about the origin. 
--
rotate90 :: DRotate t => t -> t
rotate90 = drotate (pi/2) 

-- | Rotate by 90 degrees about the supplied point.
--
rotate90About :: DRotateAbout t => DPoint2 -> t -> t
rotate90About = drotateAbout (pi/2)

-- | Rotate by 120 degrees about the origin. 
--
rotate120 :: DRotate t => t -> t
rotate120 = drotate (4*pi/3) 

-- | Rotate by 120 degrees about the supplied point.
--
rotate120About :: DRotateAbout t => DPoint2 -> t -> t
rotate120About = drotateAbout (4*pi/3)



--------------------------------------------------------------------------------
-- Common scalings

-- | Scale both x and y dimensions by the same amount.
--
uniformScale :: DScale t => Double -> t -> t
uniformScale a = dscale a a 

-- | Reflect in the X-plane about the origin.
--
reflectX :: DScale t => t -> t
reflectX = dscale (-1) 1

-- | Reflect in the Y-plane about the origin.
--
reflectY :: DScale t => t -> t
reflectY = dscale 1 (-1)

--------------------------------------------------------------------------------
-- Translations

-- | Translate by the x and y components of a vector.
--
translateBy :: DTranslate t => DVec2 -> t -> t
translateBy (V2 x y) = dtranslate x y


--------------------------------------------------------------------------------
-- Translation and scaling

-- | Reflect in the X plane that intersects the supplied point. 
--
reflectXPlane :: (DScale t, DTranslate t) 
              => DPoint2 -> t -> t
reflectXPlane (P2 x y) = dtranslate x y . dscale (-1) 1 . dtranslate (-x) (-y)

-- | Reflect in the Y plane that intersects the supplied point.
--
reflectYPlane :: (DScale t, DTranslate t) 
              => DPoint2 -> t -> t
reflectYPlane (P2 x y) = dtranslate x y . dscale 1 (-1) . dtranslate (-x) (-y)

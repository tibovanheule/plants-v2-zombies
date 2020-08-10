
-- | Based on The WORK OF http://hackage.haskell.org/package/gloss-algorithms-1.13.0.2/docs/src/Graphics.Gloss.Data.Extent.html#Extent
--   All Credits to the author!
--   Included here so it could be edited (use of Float) and simplify a few modules in Graphical module
--   Represents an integral rectangular area of the 2D plane.
module Extent ( Extent , makeExtent , takeExtent , pointInExtent , centerCoordOfExtent ) where
import Graphics.Gloss.Data.Point

-- | A rectangular area of the 2D plane.
--   We keep the type abstract to ensure that invalid extents cannot be
--   constructed.
data Extent = Extent Float Float Float Float deriving (Eq, Show)

-- | Construct an extent.
--      The north value must be > south, and east > west, else `error`.
makeExtent
        :: Float  -- ^ y max (north)
        -> Float  -- ^ y min (south)
        -> Float  -- ^ x max (east)
        -> Float  -- ^ x min (west)
        -> Extent

makeExtent n s e w | n >= s, e >= w = Extent n s e w
                   | otherwise = error "makeExtent: invalid extent"


-- | Take the NSEW components of an extent.
takeExtent :: Extent -> (Float, Float, Float, Float)
takeExtent (Extent n s e w)  = (n, s, e, w)

-- | Check whether a point lies inside an extent.
pointInExtent :: Extent -> Point -> Bool
pointInExtent (Extent n s e w) (x, y) = x >= w && x <= e     && y >= s && y <= n


-- | Get the coordinate that lies at the center of an extent.
centerCoordOfExtent :: Extent -> (Float, Float)
centerCoordOfExtent (Extent n s e w) = ( w + (e - w)/ 2 , s + (n - s) /2)
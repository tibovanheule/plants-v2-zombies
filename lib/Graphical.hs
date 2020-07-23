-- | Graphical interface of the game
module Graphical (createPossibleGame, graphic) where

import           Graphics.Gloss                     (Picture(..), play,translate, rectangleSolid,loadBMP)
import           Graphics.Gloss.Data.Color          (Color,white)
import           Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Types
import Game
import Data.Tuple
import System.Exit
import GlossBanana
import Reactive.Banana.Combinators (stepper)
import Reactive.Banana.Frameworks
import Reactive.Banana
import Graphics.Gloss.Data.Extent

-- | Constants
schaal, breedte, hoogte :: Float
schaal = 50
breedte = 9
hoogte = 7

extentR, extentA, extentB, extentC :: Extent
extentR = makeExtent   90    60  65 (-65)
extentA = makeExtent   40    10  65 (-65)
extentB = makeExtent (-10) (-40) 65 (-65)
extentC = makeExtent (-60) (-90) 65 (-65)

-- | create a simple rectangle picture.
vakje :: Picture
vakje = rectangleSolid fscale fscale
        where fscale = schaal

-- | creates a windows with a title, size and
window :: Display
window = InWindow "Vanheule Tibo, 001700370" (round $ breedte * schaal, round $ schaal * hoogte) (0,0)

-- | Entry code for the GUI, calls gloss in a reactive-banana wrapper.
-- | Reactive-banana makes event driven interaction with the program possible.
graphic :: World -> IO()
graphic start = playBanana window white 60 reactiveMain
    where reactiveMain :: Event Float -> Event InputEvent -> MomentIO (Behavior Picture)
          reactiveMain floats events = do --let moveE = filterE isValidKeypress inputEvent
                                              --startE = filterE isStart inputEvent
                                          --gameState <- accumB (start) $ unions [ slideBoardHandler <$> moveE
                                            --          , startGameHandler <$ startE ]
                                              --return $ renderApp <$> gameState
                                          return $ pure $ pictures  (map button buttons)

isValidKeypress :: InputEvent -> Bool
--isValidKeypress (ek k Down _ _) = elem (ek,k) [ (SpecialKey KeyUp) ]
isValidKeypress _                     = False




button :: Extent -> Picture
button ex  = color azure bg <> color white fg
  where
    bg = polygon (cornerPoints ex)
    fg = translate x y
       $ uscale 0.1
       $ translate (-150) (-50)  -- vertically centered, random x offset :(
       $ text "test"
    (x, y) = c2p (centerCoordOfExtent ex)

buttons :: [Extent]
buttons = [extentA, extentB, extentC]

coorsToGloss ::  Coordinate -> Picture -> Picture
coorsToGloss c@(x, y) = translate (convert breedte x) (negate $ convert hoogte (fromIntegral y))
  where
    schaalhalf = schaal/2
    convert bofh xofy = (-schaalhalf*bofh) + schaalhalf + xofy * schaal

-- | converteerd coordinaten van gloss terug om in die van de matrix
glossTocoors :: (Float, Float) -> Coordinate
glossTocoors (x, y) = (fromIntegral . round $ convert x, round . convert $ negate y)
  where convert = (/schaal) . (125 +)

uscale :: Float -> Picture -> Picture
uscale v = scale v v

c2p :: Coord -> Point
c2p (x,y) = (fromIntegral x, fromIntegral y)

cornerCoords :: Extent -> [Coord]
cornerCoords ex = [(w,n), (e,n), (e,s), (w,s)]
  where
    (n, s, e, w) = takeExtent ex

cornerPoints :: Extent -> [Point]
cornerPoints = map c2p . cornerCoords
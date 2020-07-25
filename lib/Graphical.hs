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

extentQuit, extentA, extentB, extentC :: Extent
extentQuit = makeExtent   90    60  65 (-65)
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
          reactiveMain floats inputEvents = do  timeprint <- accumE 0 (fmap (+) floats)
                                                reactimate $ print <$> timeprint
                                                reactimate $ print <$> inputEvents
                                                reactimate $ specialProgramKeys <$> inputEvents
                                                let mouseEvents = filterE isClick inputEvents
                                                --startE = filterE isStart inputEvent

                                                gameState <- accumB (start) $ unions [ menuMouse <$> mouseEvents ]
                                                  --          , startGameHandler <$ startE ]
                                                    --return $ renderApp <$> gameState
                                                return $ gui <$> gameState

-- | returns appropriate io action when specific events occur, quit on escape keypress
specialProgramKeys :: InputEvent -> IO ()
specialProgramKeys (EventKey (SpecialKey KeyEsc) Down _ _) = print "exiting, bye" >> exitSuccess
specialProgramKeys (EventKey (MouseButton LeftButton) Down _ p) = validate $ pointInExtent extentQuit p
specialProgramKeys _ = return ()

validate :: Bool -> IO ()
validate True = print "exiting, bye" >> exitSuccess
validate _    = return ()

-- | isClick is a filter for the event stream, it only keeps the mouseclicks (left) and no mouse moves
isClick :: InputEvent -> Bool
isClick (EventKey (MouseButton LeftButton) _ _ _) = True
isClick _ = False

menuMouse :: InputEvent -> World -> World
menuMouse (EventKey (MouseButton LeftButton) Down _ p) g@(World _ _ Menu levels _ curr) = nextLevel $ pointInExtent extentB p
menuMouse _                                            g@(World _ _ _ levels _ curr)    = g
            where nextLevel True = g {currlevel = (next ((curr+1) < length levels))}
                  nextLevel _ = g
                  next True = curr + 1
                  next _ = 0

-- | Decides what will be drawn, a menu or the game itself. and always draw exit button
gui :: World -> Picture
gui g@(World _ Nothing Menu _ _ _) = drawMenu g <> clickable extentQuit "exit"
gui g = drawGame g <> clickable extentQuit "exit"

drawMenu :: World -> Picture
drawMenu (World _ Nothing Menu levels _ currentlevel) = clickable extentA stringLevel <> clickable extentB "Next level"
          where stringLevel = levelToString $ levels !! currentlevel
                levelToString (Level title difficulty _ _ _ phase) = title ++ " diffculty: " ++ (show difficulty) ++ " time: " ++ (show $ getEnd phase)


drawGame :: World -> Picture
drawGame g = text $ show g

clickable :: Extent -> String -> Picture
clickable ex string = color azure bg <> color black fg
  where
    bg = polygon (cornerPoints ex)
    fg = translate x y $ uscale 0.1 $ translate (-150) (-50) $ text string
    (x, y) = c2p (centerCoordOfExtent ex)


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


-- | take the coordinates of the corners of the extent and convert them to points
cornerPoints :: Extent -> [Point]
cornerPoints ex = map c2p $ [(w,n), (e,n), (e,s), (w,s)]
      where (n, s, e, w) = takeExtent ex
-- | Graphical interface of the game
module Graphical (createPossibleGame, graphic) where

import           Graphics.Gloss                     (Picture(..), play,translate, rectangleSolid,loadBMP)
import           Graphics.Gloss.Data.Color          (Color,white)
import           Graphics.Gloss.Interface.Pure.Game 
import Types
import Game
import System.Exit

-- | Constants
schaal, breedte, hoogte :: Float
schaal = 50
breedte = 9
hoogte = 7

-- | create a simple rectangle picture.
vakje :: Picture
vakje = rectangleSolid fscale fscale
        where fscale = schaal

-- | creates a windows with a title, size and
window :: Display
window = InWindow "Vanheule Tibo, 001700370" (round $ breedte * schaal, round $ schaal * hoogte) (0,0)

graphic :: World -> IO ()
graphic start = play window   white
               60            start
               displayBoard  correctHandler
               step
          where correctHandler e g@(World _ l s _ _) | s == Menu = handleMenuInput e g
                                                    | otherwise = handleMenuInput e g

step :: Float -> World -> World
step f g@(World _ Nothing  Menu _ _) = g
step f g@(World t (Just l) _ _ e) = changeWorld t e l
step _ g = g

displayBoard ::  World -> Picture
displayBoard game@(World _ level state _ _) | state == Menu = startScreen
                                           | otherwise = displayGame game

displayGame :: World -> Picture
displayGame = undefined

startScreen :: Picture
startScreen = Pictures [exitButton]

exitButton :: Picture
exitButton = coorsToGloss (8,6) vakje 

handleMenuInput :: Event -> World -> World
handleMenuInput (EventKey (MouseButton LeftButton) Down m loc) g = if (glossTocoors loc ) == (8,6) then (die "test") else g
handleMenuInput _ g = g

initImage :: IO Images
initImage = do
  l <- loadBMP "images/back.bmp"
  c <- loadBMP "images/citizen.bmp"
  return Images {back = l}


coorsToGloss ::  Coordinate -> Picture -> Picture
coorsToGloss c@(x, y) = translate (convert breedte x) (negate $ convert hoogte (fromIntegral y))
  where
    schaalhalf = schaal/2
    convert bofh xofy = (-schaalhalf*bofh) + schaalhalf + xofy * schaal

-- converteerd coordinaten van gloss terug om in die van de matrix
glossTocoors :: (Float, Float) -> Coordinate
glossTocoors (x, y) = (fromIntegral . round $ convert x, round . convert $ negate y)
  where convert = (/schaal) . (125 +) 
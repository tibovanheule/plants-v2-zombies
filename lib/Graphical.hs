module Graphical where

import           Graphics.Gloss                     (Picture(..), play,translate, rectangleSolid,loadBMP)
import           Graphics.Gloss.Data.Color          (Color,white)
import           Graphics.Gloss.Interface.Pure.Game 
import Types
import BlaBla
import System.Exit

-- Constants
schaal, breedte, hoogte :: Float
schaal = 50
breedte = 9
hoogte = 7

vakje :: Picture
vakje = rectangleSolid fscale fscale
        where fscale = schaal

window :: Display
window = InWindow "opdracht 4, Vanheule Tibo, 001700370" (round $ breedte * schaal, round $ schaal * hoogte) (0,0)

graphic :: Game -> IO ()
graphic start = play window   white
               10            start
               displayBoard  correctHandler
               step
          where correctHandler e g@(Game _ l s _ _) | s == Menu = handleMenuInput e g
                                                    | otherwise = handleMenuInput e g

step :: Float -> Game -> Game
step f g@(Game _ Nothing  Menu _ _) = g
step f g@(Game t (Just l) _ _ e) = changeWorld t e l
step _ g = g

displayBoard ::  Game -> Picture
displayBoard game@(Game _ level state _ _) | state == Menu = startScreen
                                           | otherwise = displayGame game

displayGame :: Game -> Picture
displayGame = undefined

startScreen :: Picture
startScreen = Pictures [exitButton]

exitButton :: Picture
exitButton = coorsToGloss (8,6) vakje 

handleMenuInput :: Event -> Game -> Game
handleMenuInput (EventKey (MouseButton LeftButton) Down m loc) g = g                   
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
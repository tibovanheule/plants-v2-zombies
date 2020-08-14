-- | Graphical interface of the game

module Graphical
  ( createPossibleGame
  , graphic
  )
where

import           Graphics.Gloss                 ( Picture(..) , translate , rectangleSolid)
import           Graphics.Gloss.Data.Color      ( Color , white  )
import           Graphics.Gloss.Interface.Pure.Game hiding ( Event )
import           Types
import           Game
import           Data.Tuple
import           System.Exit
import           GlossBanana
import           Reactive.Banana.Combinators
import           Reactive.Banana.Frameworks
import           Reactive.Banana
import           Extent
import           Control.Monad
import           Constants

-- | basic globally used extents, these are for
extentQuit, extentNext, extentStart, extentSunflower, extentWalnut, extentPeashooter :: Extent
extentQuit = makeExtent (-210) (-240) 300 170
extentNext = makeExtent (-110) (-140) (-150) (-280)
extentStart = makeExtent (-150) (-180) (-150) (-280)
extentSunflower = makeExtent (-205) (-255) (-125) (-175)
extentWalnut = makeExtent  (-205) (-255) (-75) (-125)
extentPeashooter = makeExtent (-205) (-255) (-25) (-75)

-- | Makes extents and their grid locations
gridExtent :: [(Extent,Coordinate)]
gridExtent = zip extents gridCoors
 where extents = map (extentLoc . toGloss) gridCoors
       extentLoc (x,y) = makeExtent (y+halfSchaal)  (y-halfSchaal) (x+halfSchaal)  (x-halfSchaal)
       toGloss (x, y) = (convert breedte x,negate $ convert hoogte y)
       convert bofh xofy = (-halfSchaal * bofh) + halfSchaal + xofy * schaal

-- | Entry code for the GUI, calls gloss in a reactive-banana wrapper.
-- | Reactive-banana makes event driven interaction with the program possible.
graphic :: Bool -> Bool -> Images -> World -> IO ()
graphic debugtime debugevents images start = playBanana window white 60 reactiveMain
 where
  window = InWindow "Vanheule Tibo, 001700370"
                    (round $ breedte * schaal, round $ hoogte * schaal + schaal)
                    (0                       , 0)
  reactiveMain floats events = do
        -- Debugging
        when debugtime (accumE 0 (fmap (+) floats) >>= \x -> reactimate $ print <$> x)
        when debugevents (reactimate $ print <$> events)
        -- Don't want all mouse/keypress events, only relevant to the program (filter the event stream)
        let inputEvents = filterE inputFilter events
        gameState <- accumB start $ unions [event <$> inputEvents, tick <$> floats]
        -- On escape => quit
        reactimate $ specialProgramKeys<$> inputEvents
        return $ gui images <$> gameState


-- | returns appropriate io action when specific events occur, quit on escape keypress
specialProgramKeys ::  InputEvent -> IO ()
specialProgramKeys (EventKey (SpecialKey KeyEsc) Down _ _)  = print "exiting, bye" >> exitSuccess
specialProgramKeys (EventKey (MouseButton LeftButton) Down _ p) | pointInExtent extentQuit p = print "exiting, bye" >> exitSuccess
                                                                | otherwise = return ()
specialProgramKeys _ = return ()

-- | inputFilter is a filter for the event stream, it only keeps the relevant mouseclicks (left) and relevant keypresses
inputFilter :: InputEvent -> Bool
inputFilter (EventKey (MouseButton LeftButton) _ _ _) = True
inputFilter (EventKey (Char 's') Down _ _) = True
inputFilter (EventKey (Char 'n') Down _ _) = True
inputFilter _ = False

-- | Change the world, depending on the mouse event
event :: InputEvent -> World -> World
-- if 'S' keypress while being in the menu, start level
event (EventKey (Char 's') Down _ _) g@(World _ _ Menu levels curr) = g {state = Ongoing, chosenLevel = Just (levels !! curr), worldtime = 0 }
-- if 'N' keypress while being in the menu, next level
event (EventKey (Char 'n') Down _ _) g@(World _ _ Menu levels curr) = g { currlevel = (curr + 1) `mod` length levels }
-- mouseclick while being in the menu
event (EventKey (MouseButton LeftButton) Down _ p) g@(World _ _ Menu levels curr)
  -- if menu and on next, calculate possible next level to show
  | pointInExtent extentNext p = g { currlevel = (curr + 1) `mod` length levels }
  -- if menu and on start, set-up the world with the chosen level
  | pointInExtent extentStart p = g {state = Ongoing, chosenLevel = Just (levels !! curr), worldtime = 0 }
  -- otherwise leave the world be
  | otherwise = g
-- mouse click while winning a game
event (EventKey (MouseButton LeftButton) Down _ p) g@(World _ _ Won levels curr)
  -- Go to menu, (extentStart used to reuse the extent)
  | pointInExtent extentStart p = g {state = Menu, chosenLevel = Nothing}
  -- Restart the level again
  | pointInExtent extentNext p = g {state = Ongoing, chosenLevel = Just (levels !! curr), worldtime = 0 }
  -- otherwise leave the world be
  | otherwise = g
event (EventKey (MouseButton LeftButton) Down _ p) g@(World _ _ Lost levels curr)
  -- Go to menu, (extentStart used to reuse the extent)
  | pointInExtent extentStart p = g {state = Menu, chosenLevel = Nothing}
  -- Restart the level again
  | pointInExtent extentNext p = g {state = Ongoing, chosenLevel = Just (levels !! curr), worldtime = 0 }
  -- otherwise leave the world be
  | otherwise = g
-- Mouseclick during the game
event (EventKey (MouseButton LeftButton) Down _ p) g@(World _  (Just l) Ongoing _ _)
  -- check if clicked on board, place plant on that gird location
  | any (flip pointInExtent p . fst) gridExtent =  g { chosenLevel = Just (addplant l (snd $ head $ filterGridExtents p))}
  -- Sunflower is selected
  | pointInExtent extentSunflower p =  g { chosenLevel = Just (l { chosenplant=Just Sunflower } )}
  -- walnut is selected
  | pointInExtent extentWalnut p = g { chosenLevel = Just (l { chosenplant=Just Walnut } )}
  -- peaschooter is selected
  | pointInExtent extentPeashooter p = g { chosenLevel = Just (l { chosenplant=Just Peashooter } )}
  -- leave world be
  | otherwise = g
    where -- filter the extents until one remains, to get clicked grid location
          filterGridExtents p = filter (flip pointInExtent p . fst) gridExtent
event _ g = g



-- | Decides what will be drawn, a menu or the game itself. and always draw exit button
gui :: Images -> World -> Picture
gui im g@(World _ Nothing Menu _ _) = drawMenu g im <> clickable extentQuit "exit"
gui im g                            = drawGame g im <> clickable extentQuit "exit"

-- | Draws the menu
drawMenu :: World -> Images -> Picture
drawMenu (World _ _ _ levels currentlevel) im =
 back im <> levelToPic <> clickable extentNext "Next level (N)"  <> clickable extentStart "Start level (S)"
 where
  levelToPic = levelToPictures $ levels !! currentlevel
  toTime =
  levelToPictures (Level title difficulty _ _ _ phase _ _ _) = translate (-290) (-40) (uscale 0.1 (text title)) <>
   translate (-290) (-55) (uscale 0.1 (text ("diffculty: " ++ show difficulty) )) <>
   translate (-290) (-70)  $ uscale 0.1 (text ("time: " ++ show (toTime $ getEnd phase) ))

back :: Images -> Picture
back im = scale ((breedte * schaal)/577) ((hoogte * schaal + schaal)/385) (backgroundimage im)

-- | Draws the game board and Won/Lost screen
drawGame :: World -> Images -> Picture
drawGame (World time (Just l) Ongoing _ _) im =
  board l im <> drawProgressBar time (getEnd $ phase l)  <> drawZombies im l <> drawStore l im <> drawPlants l im <> drawGraves im l <> drawHome im l
drawGame (World _ _ Won _ _) im =
  back im <> translate (-290) (-90) (uscale 0.2 (text "You won!")) <> clickable extentStart "Menu (M)" <> clickable extentNext "Restart"
drawGame (World _ _ Lost _ _) im =
  back im <> translate (-290) (-90) (uscale 0.1 (text "Try again, you Lost!")) <> clickable extentStart "Menu (M)" <> clickable extentNext "Restart"
drawGame _ _ = blank

board :: Level -> Images -> Picture
board l im = pictures $ map (`coorsToGloss` grassImage im) gridCoors ++ map walls gridCoors
 where grassImage = uscale (70/32) . grassimage
       walls c = pictures $ map (wallPic c) (zip (defaultNeigbours c) ['N','E','S','W'])
       isin s w = (s,w) `elem` wall ( levelmap l) || (w,s) `elem` wall ( levelmap l)
       wallPic c (x,'N') | isin c x = translate 0 halfSchaal $ coorsToGloss c (rectangleSolid schaal 5)
                         | otherwise = blank
       wallPic c (x,'E') | isin c x =  translate halfSchaal 0 $ coorsToGloss c (rectangleSolid 5 schaal)
                         | otherwise =  blank
       wallPic c (x,'S') | isin c x = translate 0 (-halfSchaal) $ coorsToGloss c (rectangleSolid schaal 5)
                         | otherwise =  blank
       wallPic c (x,'W') | isin c x = translate (-halfSchaal) 0 $ coorsToGloss c (rectangleSolid 5 schaal)
                         | otherwise =  blank

-- | Places the grave pictures on the correct grid location
drawGraves :: Images -> Level -> Picture
drawGraves im = pictures . map graveToPic . graves . levelmap
 where graveToPic = flip coorsToGloss (uscale (30/1200) $ graveimage im) . fst

-- | Places the home picture on the correct grid location
drawHome :: Images -> Level-> Picture
drawHome im = pictures . map homeToPic . homes . levelmap
 where homeToPic = flip coorsToGloss (uscale (45/1200) $ homeimage im)

-- | For a given level, draw the zombies
drawZombies :: Images -> Level -> Picture
drawZombies im = pictures . map zombieToPicture . zombies
 where
  scaler = scale (40/153) (50/209)
  zombieToPicture ( Zombie Citizen _ coor _ _ _) = coorsToGloss coor (scaler $ citizenimage im)
  zombieToPicture ( Zombie Farmer _ coor _ _ _) = coorsToGloss coor (scaler $ farmerimage im)
  zombieToPicture ( Zombie Dog _ coor _ _ _) = coorsToGloss coor (scaler $ dogimage im)


-- | For a given level, draw the plants on the correct grid locations
drawPlants :: Level -> Images ->  Picture
drawPlants l im = pictures $ map plantToPicture (plants l) ++ peas (plants l)
 where
  peas = map peaToPicture . concatMap shots
  peaToPicture = flip coorsToGloss (color green (circle 15)) . peapos
  plantToPicture p = coorsToGloss (plantpos p) (plantImage (planttype p) im)
  scaler = scale (50/200) (40/161)
  plantImage Peashooter = scaler . peashooterimage
  plantImage Sunflower  = scaler . sunflowerimage
  plantImage Walnut     = scaler . walnutimage

-- | Draw the energy score and draw the store where plants can be bought.
drawStore :: Level -> Images -> Picture
drawStore (Level _ _ seeds z _ _ energy _ _) im = energytext <> store seeds
 where
  energytext = translate (-200) (-200) $ uscale 0.1 $ text $ "Energy: " ++ show energy
  store = pictures . map seedToStorePicture
  scaler = scale (50/200) (40/161)
  seedToStorePicture Peashooter = translate (-50) (-230) (scaler $ peashooterimage im)
  seedToStorePicture Walnut = translate (-100) (-230) (scaler $ walnutimage im)
  seedToStorePicture Sunflower = translate (-150) (-230) (scaler $ sunflowerimage im)

-- | Draws the progressbar
drawProgressBar :: Time -> Time -> Picture
drawProgressBar t tmax = translate 0 260 ( back  <> positingmin ( rectangleSolid schaalmin 20))
 where back = color blue (rectangleSolid schaalmax 20)
       schaalmax = tmax*200/tmax
       schaalmin = t*200/tmax
       positingmin = translate (-schaalmax/2+schaalmin/2) 0 . color yellow

-- | Makes a clickable, visible area
clickable :: Extent -> String -> Picture
clickable ex string = color azure bg <> color black (fg string)
 where
  bg     = polygon (cornerPoints ex)
  fg     = translate x y . uscale 0.1 . translate (-150) (-70) . text
  (x, y) = centerCoordOfExtent ex

-- | convert grid coordinates to Gloss points
coorsToGloss :: Coordinate -> Picture -> Picture
coorsToGloss (x, y) = translate (convert breedte x) (negate $ convert hoogte y)
 where convert bofh xofy = (-halfSchaal * bofh) + halfSchaal + xofy * schaal

-- | scale a picture given a float, representing x en y scalers
uscale :: Float -> Picture -> Picture
uscale v = scale v v

-- | Give the world a time tick
tick :: Float -> World -> World
tick _ g@(World _ (Just _) _ _ _) = changeWorld g
tick _ g = g
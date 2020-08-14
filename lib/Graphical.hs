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

gridCoors :: [Coordinate]
gridCoors = concatMap (flip zip [0..5] . replicate 9) [0..8]

gridExtent :: [(Extent,Coordinate)]
gridExtent = zip (map (extentLoc . toGloss) gridCoors) gridCoors
 where extentLoc (x,y) = makeExtent (y+halfSchaal)  (y-halfSchaal) (x+halfSchaal)  (x-halfSchaal)
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
  reactiveMain floats inputEvents = do
    -- Debugging
    when debugtime (accumE 0 (fmap (+) floats) >>= \x -> reactimate $ print <$> x)
    when debugevents (reactimate $ print <$> inputEvents)
    -- Don't want all mouse/keypress events, only relevant to the program (filter the event stream)
    let mouseEvents = filterE isClick inputEvents
    gameState <- accumB start $ unions [mouse <$> mouseEvents, tick <$> floats]
    -- On escape => quit
    reactimate $ specialProgramKeys<$> inputEvents
    return $ gui images <$> gameState


-- | returns appropriate io action when specific events occur, quit on escape keypress

specialProgramKeys ::  InputEvent -> IO ()
specialProgramKeys (EventKey (SpecialKey KeyEsc) Down _ _)  = print "exiting, bye" >> exitSuccess
specialProgramKeys (EventKey (MouseButton LeftButton) Down _ p) | pointInExtent extentQuit p = print "exiting, bye" >> exitSuccess
                                                                | otherwise = return ()
specialProgramKeys _ = return ()



-- TODO rename
-- | isClick is a filter for the event stream, it only keeps the relevant mouseclicks (left) and relevant keypresses
isClick :: InputEvent -> Bool
isClick (EventKey (MouseButton LeftButton) _ _ _) = True
isClick (EventKey (Char 's') Down _ _) = True
isClick (EventKey (Char 'n') Down _ _) = True
isClick _ = False

-- TODO rename, update gaurds with won lost state
-- | Change the world, depending on the mouse event
mouse :: InputEvent -> World -> World
mouse (EventKey (Char 's') Down _ _) g@(World _ _ Menu levels curr) = g {state = Ongoing, chosenLevel = Just (levels !! curr), worldtime = 0 }
mouse (EventKey (Char 'n') Down _ _) g@(World _ _ Menu levels curr) = g { currlevel = (curr + 1) `mod` length levels }
mouse (EventKey (MouseButton LeftButton) Down _ p) g@(World _ _ Menu levels curr)
  -- if menu and on next, calculate possible next level to show
  | pointInExtent extentNext p = g { currlevel = (curr + 1) `mod` length levels }
  -- if menu and on start, set-up the world with the chosen level
  | pointInExtent extentStart p = g {state = Ongoing, chosenLevel = Just (levels !! curr), worldtime = 0 }
  -- otherwise leave the world be
  | otherwise = g
mouse (EventKey (MouseButton LeftButton) Down _ p) g@(World _ _ Won levels curr)
  -- otherwise leave the world be
  | pointInExtent extentStart p = g {state = Menu, chosenLevel = Nothing}
  | pointInExtent extentNext p = g {state = Ongoing, chosenLevel = Just (levels !! curr), worldtime = 0 }
  -- otherwise leave the world be
  | otherwise = g
mouse (EventKey (MouseButton LeftButton) Down _ p) g@(World _ _ Lost levels curr)
  -- if menu button click, go
  | pointInExtent extentStart p = g {state = Menu, chosenLevel = Nothing}
  | pointInExtent extentNext p = g {state = Ongoing, chosenLevel = Just (levels !! curr), worldtime = 0 }
  -- otherwise leave the world be
  | otherwise = g
mouse (EventKey (MouseButton LeftButton) Down _ p) g@(World _  (Just l) Ongoing _ _)
  -- check if clicked on board
  | not (null (filterGridExtents p)) =  g { chosenLevel = Just (addplant l (snd $ head $ filterGridExtents p))}
  | pointInExtent extentSunflower p =  g { chosenLevel = Just (l { chosenplant=Just Sunflower } )}
  | pointInExtent extentWalnut p = g { chosenLevel = Just (l { chosenplant=Just Walnut } )}
  | pointInExtent extentPeashooter p = g { chosenLevel = Just (l { chosenplant=Just Peashooter } )}
  | otherwise = g
mouse _ g = g

filterGridExtents :: (Float,Float) -> [(Extent,Coordinate)]
filterGridExtents p = filter (\(ex,_) -> pointInExtent ex p) gridExtent

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
  levelToPictures (Level title difficulty _ _ _ phase _ _ _) = translate (-290) (-40) (uscale 0.1 (text title)) <>
   translate (-290) (-55) (uscale 0.1 (text ("diffculty: " ++ show difficulty) )) <>
   translate (-290) (-70) (uscale 0.1 (text ("time: " ++ show (getEnd phase) )))

back :: Images -> Picture
back im = scale ((breedte * schaal)/577) ((hoogte * schaal + schaal)/385) (backgroundimage im)

-- | Draws the game board and Won/Lost screen
drawGame :: World -> Images -> Picture
drawGame (World time (Just l) Ongoing _ _) im =
  board l im <> drawProgressBar time (getEnd $ phase l)  <> drawZombies l im <> drawStore l im <> drawPlants l im <> drawGraves l im <> drawHome l im
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

drawGraves :: Level -> Images -> Picture
drawGraves l im = pictures $ map graveToPic (graves (levelmap l))
 where graveToPic = flip coorsToGloss (uscale (30/1200) $ graveimage im) . fst

drawHome :: Level -> Images -> Picture
drawHome l im = pictures $ map homeToPic (homes (levelmap l))
 where homeToPic = flip coorsToGloss (uscale (45/1200) $ homeimage im)

-- | For a given level, draw the zombies
drawZombies :: Level -> Images -> Picture
drawZombies l im = pictures $ map zombieToPicture $ zombies l
 where
  scaler = scale (40/153) (50/209)
  zombieToPicture ( Zombie Citizen _ coor _ _) = coorsToGloss coor (scaler $ citizenimage im)
  zombieToPicture ( Zombie Farmer _ coor _ _) = coorsToGloss coor (scaler $ farmerimage im)
  zombieToPicture ( Zombie Dog _ coor _ _) = coorsToGloss coor (scaler $ dogimage im)


-- | For a given level, draw the plants
drawPlants :: Level -> Images -> Picture
drawPlants (Level _ _ _ _ p _ _ _ _) im = pictures $ map plantToPicture p ++ peas
 where
  peas =  map peaToPicture $ concatMap shots p
  plantToPicture ( Plant typ _ coor _ _ _) = coorsToGloss coor (plantImage typ)
  peaToPicture ( Pea coor _ _ _) = coorsToGloss coor $ color green $ circle 15
  scaler = scale (50/200) (40/161)
  plantImage Peashooter = scaler $ peashooterimage im
  plantImage Sunflower  = scaler $ sunflowerimage im
  plantImage Walnut     = scaler $ walnutimage im

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
 where back = color blue $ rectangleSolid schaalmax 20
       schaalmax = tmax*200/tmax
       schaalmin = t*200/tmax
       positingmin = translate (-schaalmax/2+schaalmin/2) 0 . color yellow

-- | Makes a clickable, visible area
clickable :: Extent -> String -> Picture
clickable ex string = color azure bg <> color black fg
 where
  bg     = polygon (cornerPoints ex)
  fg     = translate x y $ uscale 0.1 $ translate (-150) (-50) $ text string
  (x, y) = centerCoordOfExtent ex

coorsToGloss :: Coordinate -> Picture -> Picture
coorsToGloss c@(x, y) = translate (convert breedte x) (negate $ convert hoogte y)
 where
  schaalhalf = schaal / 2
  convert bofh xofy = (-schaalhalf * bofh) + schaalhalf + xofy * schaal

uscale :: Float -> Picture -> Picture
uscale v = scale v v


-- | take the coordinates of the corners of the extent and convert them to points
cornerPoints :: Extent -> [Point]
cornerPoints ex = [(w, n), (e, n), (e, s), (w, s)]
  where (n, s, e, w) = takeExtent ex

-- | Give the world a time tick
tick :: Float -> World -> World
tick _ g@(World _ (Just _) _ _ _) = changeWorld g
tick _ g = g
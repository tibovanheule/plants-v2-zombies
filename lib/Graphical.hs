-- | Graphical interface of the game

module Graphical
  ( createPossibleGame
  , graphic
  )
where

import           Graphics.Gloss                 ( Picture(..)
                                                , play
                                                , translate
                                                , rectangleSolid
                                                , loadBMP
                                                )
import           Graphics.Gloss.Data.Color      ( Color
                                                , white
                                                )
import           Graphics.Gloss.Interface.Pure.Game
                                         hiding ( Event )
import           Types
import           Game
import           Data.Tuple
import           System.Exit
import           GlossBanana
import           Reactive.Banana.Combinators
import           Reactive.Banana.Frameworks
import           Reactive.Banana
import           Graphics.Gloss.Data.Extent
import           Control.Monad
import           System.IO.Unsafe
import           Debug.Trace

-- | Constants
schaal, breedte, hoogte :: Float
schaal = 70
breedte = 9
hoogte = 7

-- | basic globally used extents, these are for
extentQuit, extentNext, extentStart, extentSunflower, extentWalnut, extentPeashooter :: Extent
extentQuit = makeExtent (-210) (-240) 300 170
extentNext = makeExtent (-10) (-40) 65 (-65)
extentStart = makeExtent (-60) (-90) 65 (-65)
extentSunflower = makeExtent (-205) (-255) (-125) (-175)
extentWalnut = makeExtent  (-205) (-255) (-75) (-125)
extentPeashooter = makeExtent (-205) (-255) (-25) (-75)

gridCoors :: [Coordinate]
gridCoors = concatMap (flip zip [0..5] . replicate 9) [0..8]

gridExtent :: [(Extent,Coordinate)]
gridExtent = zip (map (extentLoc . toGloss) gridCoors) gridCoors
 where halfSchaal = schaal / 2
       extentLoc (x,y) = makeExtent (round $ y+halfSchaal)  (round $ y-halfSchaal) (round $ x+halfSchaal)  (round $ x-halfSchaal)
       toGloss (x, y) = ((convert breedte x),(negate $ convert hoogte y))
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
    gameState <- accumB (start) $ unions [mouse <$> mouseEvents, tick <$> floats]
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
mouse (EventKey (Char 's') Down _ _) g@(World _ _ Menu levels curr) = g {state = Ongoing, chosenLevel = Just (levels !! curr), time = 0 }
mouse (EventKey (Char 'n') Down _ _) g@(World _ _ Menu levels curr) = g { currlevel = (curr + 1) `mod` (length levels) }
mouse (EventKey (MouseButton LeftButton) Down _ p) g@(World _ _ Menu levels curr)
  -- if menu and on next, calculate possible next level to show
  | pointInExtent extentNext p = g { currlevel = (curr + 1) `mod` (length levels) }
  -- if menu and on start, set-up the world with the chosen level
  | pointInExtent extentStart p = g {state = Ongoing, chosenLevel = Just (levels !! curr), time = 0 }
  -- otherwise leave the world be
  | otherwise = g
mouse (EventKey (MouseButton LeftButton) Down _ p) g@(World _ _ Won levels curr)
  -- otherwise leave the world be
  | pointInExtent extentStart p = g {state = Menu, chosenLevel = Nothing}
  | pointInExtent extentNext p = g {state = Ongoing, chosenLevel = Just (levels !! curr), time = 0 }
  -- otherwise leave the world be
  | otherwise = g
mouse (EventKey (MouseButton LeftButton) Down _ p) g@(World _ _ Lost levels curr)
  -- if menu button click, go
  | pointInExtent extentStart p = g {state = Menu, chosenLevel = Nothing}
  | pointInExtent extentNext p = g {state = Ongoing, chosenLevel = Just (levels !! curr), time = 0 }
  -- otherwise leave the world be
  | otherwise = g
mouse (EventKey (MouseButton LeftButton) Down _ p) g@(World _  (Just l) Ongoing _ _)
  -- check if clicked on board
  | length (filterGridExtents p) > 0 =  g { chosenLevel = Just (addplant l (snd $ head $ filterGridExtents p))}
  | pointInExtent extentSunflower p =  g { chosenLevel = Just (l { chosenplant=Just Sunflower } )}
  | pointInExtent extentWalnut p = g { chosenLevel = Just (l { chosenplant=Just Walnut } )}
  | pointInExtent extentPeashooter p = g { chosenLevel = Just (l { chosenplant=Just Peashooter } )}
  | otherwise = g
mouse _ g = g

filterGridExtents :: (Float,Float) -> [(Extent,Coordinate)]
filterGridExtents p = filter (\(ex,_) -> pointInExtent ex p) gridExtent

-- | Decides what will be drawn, a menu or the game itself. and always draw exit button
gui :: Images -> World -> Picture
gui im g@(World _ Nothing Menu _ _) = drawMenu g <> clickable extentQuit "exit"
gui im g                            = drawGame g im <> clickable extentQuit "exit"

-- | Draws the menu
drawMenu :: World -> Picture
drawMenu (World _ _ _ levels currentlevel) =
 levelToPic <> clickable extentNext "Next level (N)"  <> clickable extentStart "Start level (S)"
 where
  levelToPic = levelToPictures $ levels !! currentlevel
  levelToPictures (Level title difficulty _ _ _ phase _ _) = translate (-200) 200 (uscale 0.1 (text title)) <>
   translate (-200) 170 (uscale 0.1 (text ("diffculty: " ++ (show difficulty)) )) <>
   translate (-200) 140 (uscale 0.1 (text ("time: " ++ (show $ getEnd phase)) ))

-- | Draws the game board and Won/Lost screen
drawGame :: World -> Images -> Picture
drawGame (World time (Just l) Ongoing _ _) im = grass im <> drawProgressBar time (duration l)  <> drawZombies l im <> drawStore l im <> drawPlants l im
drawGame (World _ _ Won _ _) _ = uscale 0.2 (text $ "Good job! You won") <> clickable extentStart "Menu (M)" <> clickable extentNext "Restart"
drawGame (World _ _ Lost _ _) _ = uscale 0.2 (text $ "Try again, you Lost") <> clickable extentStart "Menu (M)" <> clickable extentNext "Restart"
drawGame _ _ = blank

grass :: Images -> Picture
grass im = pictures $ map (flip coorsToGloss grassImage ) gridCoors
 where grassImage = scale (70/32) (70/32) $ grassimage(im)


duration :: Level -> Time
duration (Level _ _ _ _ _ p _ _) = getEnd p

-- | For a given level, draw the zombies
drawZombies :: Level -> Images -> Picture
drawZombies (Level _ _ _ z _ _ _ _) im = pictures $ map zombieToPicture z
 where
  scaler = scale (40/153) (50/209)
  zombieToPicture ( Zombie Citizen _ coor _ _) = coorsToGloss coor (scaler $ citizenimage(im))
  zombieToPicture ( Zombie Farmer _ coor _ _) = coorsToGloss coor (scaler $ farmerimage(im))
  zombieToPicture ( Zombie Dog _ coor _ _) = coorsToGloss coor (scaler $ dogimage(im))


-- | For a given level, draw the plants
drawPlants :: Level -> Images -> Picture
drawPlants (Level _ _ _ _ p _ _ _) im = pictures $ (map plantToPicture p) ++ peas
 where
  peas =  map peaToPicture $ concatMap getPeas p
  plantToPicture ( Plant typ _ coor _ _ _) = coorsToGloss coor (plantImage typ)
  peaToPicture ( Pea coor _ _ _) = coorsToGloss coor $ color green $ circle 15
  scaler = scale (50/200) (40/161)
  plantImage Peashooter = scaler $ peashooterimage(im)
  plantImage Sunflower  = scaler $ sunflowerimage(im)
  plantImage Walnut     = scaler $ walnutimage(im)

-- | Draw the energy score and draw the store where plants can be bought.
drawStore :: Level -> Images -> Picture
drawStore (Level _ _ seeds z _ _ energy _) im = energytext <> store seeds
 where
  energytext = translate (-200) (-200) $ uscale 0.1 $ text $ "Energy: " ++ show energy
  store = pictures . map (seedToStorePicture)
  scaler = scale (50/200) (40/161)
  seedToStorePicture Peashooter = translate (-50) (-230) (scaler $ peashooterimage(im))
  seedToStorePicture Walnut = translate (-100) (-230) (scaler $ walnutimage(im))
  seedToStorePicture Sunflower = translate (-150) (-230) (scaler $ sunflowerimage(im))

-- | Draws the progressbar
drawProgressBar :: Time -> Time -> Picture
drawProgressBar t tmax = translate 0 (260)  ( color blue (rectangleSolid schaalmax 20)  <> positingmin ( rectangleSolid schaalmin 20)) <> (text (show (t / 60) ))
 where schaalmax = tmax*200/tmax
       schaalmin = t*200/tmax
       positingmin = translate (-schaalmax/2+schaalmin/2) 0 . color yellow

-- | Makes a clickable, visible area
clickable :: Extent -> String -> Picture
clickable ex string = color azure bg <> color black fg
 where
  bg     = polygon (cornerPoints ex)
  fg     = translate x y $ uscale 0.1 $ translate (-150) (-50) $ text string
  (x, y) = c2p (centerCoordOfExtent ex)

coorsToGloss :: Coordinate -> Picture -> Picture
coorsToGloss c@(x, y) = translate (convert breedte x) (negate $ convert hoogte y)
 where
  schaalhalf = schaal / 2
  convert bofh xofy = (-schaalhalf * bofh) + schaalhalf + xofy * schaal

uscale :: Float -> Picture -> Picture
uscale v = scale v v

c2p :: Coord -> Point
c2p (x, y) = (fromIntegral x, fromIntegral y)


-- | take the coordinates of the corners of the extent and convert them to points
cornerPoints :: Extent -> [Point]
cornerPoints ex = map c2p $ [(w, n), (e, n), (e, s), (w, s)]
  where (n, s, e, w) = takeExtent ex

-- | Give the world a time tick
tick :: Float -> World -> World
tick _ g@(World _ (Just _) _ _ _) = changeWorld g
tick _ g = g
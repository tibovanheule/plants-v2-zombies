
-- | An revised version of the source of gloss-banana package due to api changes.
module GlossBanana (playBanana, InputEvent) where

import Graphics.Gloss hiding (display)
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Data.IORef ( newIORef, readIORef, writeIORef)

-- | A useful type synonym for Gloss event values, to avoid confusion between Gloss and ReactiveBanana.
type InputEvent = G.Event

-- | Play the game in a window, updating when the value of the provided
--   Behavior t Picture changes.
playBanana :: Display -- ^ The display method
           -> Color   -- ^ The background colour
           -> Int     -- ^ The refresh rate, in Hertz
           -> (Event Float -> Event InputEvent -> MomentIO (Behavior Picture))
           -- ^ A Moment t action to generate the Picture Behavior, taking
           --   the refresh and input Events with respect to which to build it.
           --   The refresh event generates a Float indicating the time delta
           --   since the last refresh.
           -> IO ()
playBanana display colour frequency mPicture = do
  pictureref <- newIORef blank
  (tickHandler,  tick)  <- newAddHandler
  (eventHandler, event) <- newAddHandler
  compile (makeNetwork tickHandler eventHandler $ writeIORef pictureref) >>= actuate
  playIO display
    colour
    frequency
    () -- initial world
    (\      _ -> readIORef pictureref) -- initial drawing
    (\ ev   _ -> event ev) -- event handling
    (\ time _ -> () <$ tick time) -- step drawing function
  where
    makeNetwork tickHandler eventHandler change = do
      eTick  <- fromAddHandler tickHandler
      eEvent <- fromAddHandler eventHandler
      bRawPicture <- mPicture eTick eEvent
      stepperEtick <- stepper undefined eTick
      stepperEvent <- stepper undefined eEvent
      let bPicture = bRawPicture <* stepperEtick <* stepperEvent
      changes bPicture >>= reactimate' . fmap (fmap change)


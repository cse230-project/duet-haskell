module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Graphics.Vty as V
import Model
import View
import System.Random

-------------------------------------------------------------------------------
main :: IO ()
main = do
  -- speed <- fromMaybe defaultSpeed <$> getSpeed
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      -- threadDelay (getDelay speed) -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  n <- randomIO :: IO Int
  res <- customMain initialVty buildVty (Just chan) app $ Model.init n
  print (gameOver res)

app :: App PlayState Tick String
app =
  App
    { appDraw = view,
      appChooseCursor = const . const Nothing,
      appHandleEvent = control,
      appStartEvent = return,
      appAttrMap = const theMap
    }

module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Model
import System.Environment (getArgs)
import Text.Read (readMaybe)
import View

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
  res <- customMain initialVty buildVty (Just chan) app Model.init
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

-- speeds = [200000, 150000, 100000]
-- getDelay :: Int -> Int
-- getDelay n =  speeds !! (n - 1)

-- getSpeed :: IO (Maybe Int)
-- getSpeed = do
--   args <- getArgs
--   case args of
--     (str : _) -> return (readMaybe str)
--     _ -> return Nothing

-- defaultSpeed :: Int
-- defaultSpeed = 1

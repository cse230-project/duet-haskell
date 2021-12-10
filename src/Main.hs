module Main where

import Brick (App (..), customMain)
import Brick.BChan (newBChan, writeBChan)
import Control (control)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Graphics.Vty as V
import Model (PlayState (psScore), Tick (..), init)
import qualified Model.Score as Score
import System.Random (randomIO)
import View (theMap, view)

-------------------------------------------------------------------------------
main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  n <- randomIO :: IO Int
  res <- customMain initialVty buildVty (Just chan) app $ Model.init n
  putStrLn ("Highest score: " ++ show (Score.maxScore $ psScore res))

app :: App PlayState Tick String
app =
  App
    { appDraw = view,
      appChooseCursor = const . const Nothing,
      appHandleEvent = control,
      appStartEvent = return,
      appAttrMap = const theMap
    }

module View (view, theMap) where

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Data.List (elem, elemIndex)
import qualified Data.Map as M
import Graphics.Vty hiding (dim)
import Model
import qualified Model as Board
import Model.Board
import qualified Model.Board as Board
import qualified Model.Score as Score
import Text.Printf (printf)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s =
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      -- vTile [ mkRow s row | row <- [1..dim] ]
      vLimit 50 $ vBox rows
  where
    rows = [hLimit 100 $ hBox $ cellsInRow r | r <- [1 .. dim]]
    cellsInRow r = [mkCell s r c | c <- [1 .. dim]]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c = center (mkXO xoMb)
  where
    -- xoMb      = psBoard s ! Pos r c
    xoMb
      | r == Board.pRow (bluePos s) && c == Board.pCol (bluePos s) = Just BlueO
      | r == Board.pRow (redPos s) && c == Board.pCol (redPos s) = Just RedO
      | Pos r 1 `elem` psObs s && c < 30 = Just X
      | Pos r 21 `elem` psObs s && c > 20 = Just X
      | otherwise = Nothing

mkXO :: Maybe XO -> Widget n
mkXO Nothing = blockB
mkXO (Just X) = blockX
mkXO (Just RedO) = redO
mkXO (Just BlueO) = blueO

blockB, blockX, blueO, redO :: Widget n
blockB = vBox [str "  "]
blockX = vBox [str "██"]
blueO = withAttr blueAttr $ vBox [str "  "]
redO = withAttr redAttr $ vBox [str "  "]

header :: PlayState -> String
header s =
  if gameOver s
    then printf "♢♢ Gameover! ♢♢ Score = %d ♢♢" (Score.score (psScore s))
    else
      printf "♢♢ Duet Game ♢♢ Speed = %d ♢♢ Score = %d ♢♢" (Score.speed sc) (Score.score sc)
        where 
          sc = psScore s
        -- debug message
        -- "Duet Game GameOver = %s, row = %d, col = %d, blueRow = %d, blueCol = %d"
        -- (show (gameOver s))
        -- (pRow (head p))
        -- (pRow (head p))
        -- (pRow b)
        -- (pRow b)
        -- where
        --   p = psObs s
        --   b = bluePos s

blueAttr, redAttr :: AttrName
blueAttr = attrName "blueAttr"
redAttr = attrName "redAttr"

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (blueAttr, blue `on` blue),
      (redAttr, red `on` red)
    ]

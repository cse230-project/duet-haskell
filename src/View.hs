module View (view, theMap) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Graphics.Vty hiding (dim)
import Model
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
      | Pos r 1 `elem` psObs s && c < 28 = Just X
      | Pos r 23 `elem` psObs s && c > 22 = Just X
      | Pos r 21 `elem` psObs s && c > 20 && c < 30 = Just X
      | Pos (r - 15) (-1) `elem` psObs s && Pos (r + 8) 1 `elem` psObs s && c > 30 = Just X
      | Pos (r - 15) (-1) `elem` psObs s && Pos (r + 8) 23 `elem` psObs s && c < 20 = Just X
      | Pos r (-1) `elem` psObs s && Pos (r + 23) 21 `elem` psObs s && c < 28 = Just X
      | Pos r (-1) `elem` psObs s && Pos (r + 23) (-1) `elem` psObs s && c > 22 = Just X
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
    then 
      printf "♢♢ Gameover! ♢♢ %s Mode ♢♢ Score = %d ♢♢ Highest Score = %d ♢♢ " 
        (calculateLevel (psSpeed s)) (Score.score sc) (Score.maxScore sc)
    else
      printf "♢♢ Duet Game ♢♢ %s Mode ♢♢ Score = %d ♢♢ Highest Score = %d ♢♢ " 
        (calculateLevel (psSpeed s)) (Score.score sc) (Score.maxScore sc)
  where 
    sc = psScore s
    
blueAttr, redAttr :: AttrName
blueAttr = attrName "blueAttr"
redAttr = attrName "redAttr"

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (blueAttr, Graphics.Vty.blue `on` Graphics.Vty.blue),
      (redAttr, Graphics.Vty.red `on` Graphics.Vty.red)
    ]

calculateLevel :: Int -> String
calculateLevel speed
  | speed == 8 = "Easy"
  | speed == 4 = "Medium"
  | otherwise = "Hard"

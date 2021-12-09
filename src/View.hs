module View (view, theMap) where

import Brick
  ( AttrMap,
    AttrName,
    Widget,
    attrMap,
    attrName,
    hBox,
    hLimit,
    on,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
  )
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Graphics.Vty (blue, defAttr, magenta, red)
import Model
  ( PlayState
      ( bluePos,
        gameOver,
        psBlueDye,
        psObs,
        psRedDye,
        psScore,
        psSpeed,
        redPos
      ),
  )
import Model.Board (Pos (Pos), XO (..), dim)
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
      vLimit 50 $ vBox rows
  where
    rows = [hLimit 100 $ hBox $ cellsInRow r | r <- [1 .. dim]]
    cellsInRow r = [mkCell s r c | c <- [1 .. dim]]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c = center (mkXO xoMb)
  where
    xoMb
      | r == Board.pRow (bluePos s) && c == Board.pCol (bluePos s) = Just BlueO
      | r == Board.pRow (redPos s) && c == Board.pCol (redPos s) = Just RedO
      | Pos r c `elem` psBlueDye s && Pos r c `elem` psRedDye s = Just PurpleO
      | Pos r c `elem` psBlueDye s = Just BlueO
      | Pos r c `elem` psRedDye s = Just RedO
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
mkXO (Just PurpleO) = purpleO

blockB, blockX, blueO, redO, purpleO :: Widget n
blockB = vBox [str "  "]
blockX = vBox [str "██"]
blueO = withAttr blueAttr $ vBox [str "  "]
redO = withAttr redAttr $ vBox [str "  "]
purpleO = withAttr purpleAttr $ vBox [str "  "]

header :: PlayState -> String
header s
  | gameOver s =
    printf
      "♢♢ Game Over! ♢♢ %s Mode ♢♢ Score = %d ♢♢ Highest Score = %d ♢♢"
      (calculateLevel (psSpeed s))
      (Score.score sc)
      (Score.maxScore sc)
  | Score.number sc == 100 =
    printf
      "♢♢ Game Clear! ♢♢ Score = %d ♢♢ Highest Score = %d ♢♢ Press ESC to Exit ♢♢"
      (Score.score sc)
      (Score.maxScore sc)
  | otherwise =
    printf
      "♢♢ Duet Game ♢♢ %s Mode ♢♢ Score = %d ♢♢ Highest Score = %d ♢♢"
      (calculateLevel (psSpeed s))
      (Score.score sc)
      (Score.maxScore sc)
  where
    sc = psScore s

blueAttr, redAttr :: AttrName
blueAttr = attrName "blueAttr"
redAttr = attrName "redAttr"

purpleAttr = attrName "purpleAttr"

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (blueAttr, Graphics.Vty.blue `on` Graphics.Vty.blue),
      (redAttr, Graphics.Vty.red `on` Graphics.Vty.red),
      (purpleAttr, Graphics.Vty.magenta `on` Graphics.Vty.magenta)
    ]

calculateLevel :: Int -> String
calculateLevel speed
  | speed == 8 = "Easy"
  | speed == 4 = "Medium"
  | otherwise = "Hard"

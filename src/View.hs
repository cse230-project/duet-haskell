module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board
import Graphics.Vty hiding (dim)
import qualified Data.Map as M 
import qualified Model as Board
import qualified Model.Board as Board
import Data.List (elemIndex, elem)
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
        rows         = [hLimit 100 $ hBox $ cellsInRow r | r <- [1..dim]]
        cellsInRow r = [mkCell s r c | c <- [1..dim]]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c = center (mkXO xoMb)
  where 
    -- xoMb      = psBoard s ! Pos r c
    xoMb 
      | r == Board.pRow (bluePos s) && c == Board.pCol (bluePos s) = Just BlueO
      | r == Board.pRow (redPos s) && c == Board.pCol (redPos s) = Just RedO
      | Pos r 1 `elem` psObs s && c < 35 = Just X
      | Pos r 16 `elem` psObs s && c > 15 = Just X
      | otherwise = Nothing
    

mkXO :: Maybe XO -> Widget n
mkXO Nothing  = blockB
mkXO (Just X) = blockX
mkXO (Just RedO) = redO
mkXO (Just BlueO) = blueO

blockB, blockX, blueO, redO :: Widget n
blockB = vBox [ str "  " ]
blockX = vBox [ str "██" ]
blueO = vBox [ str "🔵" ]
redO = vBox [ str "🔴" ]

header :: PlayState -> String
header s = case (gameOver s) of 
  True  -> printf "Gameover!!!!!!!!!!!!!!!!"
  False -> printf "Duet Game GameOver = %s, row = %d, col = %d, blueRow = %d, blueCol = %d" (show (gameOver s))(pRow (head p))(pRow (head p))(pRow b)(pRow b)
  where 
    p = psObs s
    b = bluePos s

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget
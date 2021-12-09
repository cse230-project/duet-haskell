{-# LANGUAGE RecordWildCards #-}
module Model.Score where

import Model.Board as Board
import Data.Foldable
import Control.Monad 
import Prelude

-- -------------------------------------------------------------------------------
-- -- | Score --------------------------------------------------------------------
-- -------------------------------------------------------------------------------

data Score = Score 
  { 
    level   :: Int,
    score   :: Int
  }
  deriving (Eq, Ord, Show)

init :: Score
init = Score 0 0

add :: Score -> Int -> Score
add sc speed = sc { score = score sc + calculateScore speed }

calculateScore :: Int -> Int
calculateScore speed
  | speed == 8 = 1
  | speed == 4 = 2
  | otherwise = 3

clear :: Score -> Score
clear sc  = sc { score = 0 }

updateScore :: [Pos] -> Score -> Int -> Score
updateScore ps sc speed = foldr f (clear sc) ps
    where 
        f p s = if pRow p > 35 then add s speed else s
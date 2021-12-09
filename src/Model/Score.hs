{-# LANGUAGE RecordWildCards #-}
module Model.Score where

import Model.Board as Board
import Data.Foldable
import Prelude

-- -------------------------------------------------------------------------------
-- -- | Score --------------------------------------------------------------------
-- -------------------------------------------------------------------------------

data Score = Score 
  { 
    score   :: Int,
    maxScore :: Int,
    number :: Int
  }
  deriving (Eq, Ord, Show)

init :: Score
init = Score 0 0 0

add :: Score -> Int -> Score
add sc speed = sc { score = score sc + calculateScore speed, 
                    maxScore = if maxScore sc < (score sc + calculateScore speed)
                                then score sc + calculateScore speed
                                else maxScore sc,
                    number = number sc + 1
                  }

calculateScore :: Int -> Int
calculateScore speed
  | speed == 8 = 1
  | speed == 4 = 2
  | otherwise = 3

clear :: Score -> Score
clear sc  = sc { score = 0, number = 0 }

updateScore :: [Pos] -> Score -> Int -> Score
updateScore ps sc speed = foldr f sc ps
    where 
        f p s = if pRow p == 45 then add s speed else s
{-# LANGUAGE RecordWildCards #-}
module Model.Score where

import Model.Board as Board
import Data.Foldable
import Control.Monad 

-- -------------------------------------------------------------------------------
-- -- | Score --------------------------------------------------------------------
-- -------------------------------------------------------------------------------

data Score = Score 
  { 
    speed   :: Int,
    score   :: Int
  }
  deriving (Eq, Ord, Show)

init :: Int -> Score
init n = Score n 0

add :: Score -> Score
add sc  = sc { score = score sc + speed sc }

clear :: Score -> Score
clear sc  = sc { score = 0 }

updateScore :: [Pos] -> Score -> Score
updateScore ps sc = foldr f (clear sc) ps
    where 
        f p s = if pRow p > dim then add s else s
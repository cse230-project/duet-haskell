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
        f p s = if (pRow p == dim + 1) then (add s) else s
        
-- get :: Score -> Int
-- get sc = sc score

-- currRound :: Score -> Int
-- currRound Score {..} = scX + scO + scD + 1

-- startPlayer :: Score -> XO
-- startPlayer sc 
--   | even (currRound sc) = X
--   | otherwise           = O

-- winner :: Score -> Result () 
-- winner sc@Score {..}
--   | scX > scO + left = Win X
--   | scO > scX + left = Win O
--   | left == 0        = Draw
--   | otherwise        = Cont ()
--   where 
--     left             = 1 + scMax - currRound sc
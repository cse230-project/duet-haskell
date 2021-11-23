{-# LANGUAGE DeriveFunctor #-}
module Model.Board
  ( -- * Types
    Board
  , Obs
  , XO (..)
  , Pos (..)
  , Result (..)

    -- * Board API
  , dim
  , init
  , initBlue
  , initRed
  , initObs

    -- * Moves
  , down
  , clockwise
  , counterClockwise
  , check
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M
import Data.List (elemIndex)
-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Obs = [Pos]
type Board = M.Map Pos XO

data XO
  = X
  | RedO
  | BlueO
  deriving (Eq, Show)

data Pos = Pos
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

dim :: Int
dim = 50

init :: Board
-- init =  M.fromList [(Pos 17 7, BlueO), (Pos 17 13, RedO)]
init = M.empty

initObs :: Obs
-- initObs = [ Pos r c | r <- [1], c <- [1..10] ] 
initObs = [Pos 1 1, Pos (-5) 1, Pos (-15) 16]

initBlue :: Pos
initBlue = Pos 40 15

initRed :: Pos
initRed = Pos 40 35
-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------

data Result a
  = Draw
  | Win XO
  | Retry
  | Cont a
  deriving (Eq, Functor, Show)

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

down :: [Pos] -> [Pos]
down ps = [ p { pRow = min (dim + 1) (pRow p + 1) } | p <- ps ]

check :: [Pos] -> Pos -> Pos -> Bool
check ps blue red = collision1 || collision2 || collision3 || collision4
  where
    collision1 = Pos (pRow blue - 1) 1 `elem` ps && (pCol blue < 35)
    collision2 = Pos (pRow red - 1) 1 `elem` ps && (pCol red < 35)
    collision3 = Pos (pRow blue - 1) 16 `elem` ps && (pCol blue > 15)
    collision4 = Pos (pRow red - 1) 16 `elem` ps && (pCol red > 15)

posList :: [Pos]
posList = [Pos 40 15, Pos 41 16, Pos 42 17, Pos 43 18, Pos 44 19, Pos 45 20, Pos 46 21, Pos 47 22, Pos 48 23, Pos 49 24, Pos 50 25, Pos 49 26, Pos 48 27, Pos 47 28, Pos 46 29, Pos 45 30, Pos 44 31, Pos 43 32, Pos 42 33, Pos 41 34, Pos 40 35, Pos 39 34, Pos 38 33, Pos 37 32, Pos 36 31, Pos 35 30, Pos 34 29, Pos 33 28, Pos 32 27, Pos 31 26, Pos 30 25, Pos 31 24, Pos 32 23, Pos 33 22, Pos 34 21, Pos 35 20, Pos 36 19, Pos 37 18, Pos 38 17, Pos 39 16]

nextPos :: Pos -> Int -> Pos
nextPos p dir = case elemIndex p posList of
    Just i -> posList !! ((i + dir) `mod` length posList)
    Nothing -> p
clockwise :: Pos -> Pos
clockwise p = nextPos p 1

counterClockwise :: Pos -> Pos
counterClockwise p = nextPos p (-1)

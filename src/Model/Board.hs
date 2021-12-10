{-# LANGUAGE DeriveFunctor #-}

module Model.Board
  ( -- * Types
    Board,
    Obs,
    XO (..),
    Pos (..),
    Result (..),

    -- * Board API
    dim,
    Model.Board.init,
    blue,
    red,
    obs,
    blueDye,
    redDye,

    -- * Moves
    up,
    down,
    clockwise,
    counterClockwise,
    addBlueDye,
    addRedDye,
    check,
    obsReset,
    redReset,
    blueReset,
  )
where

import Data.List (elemIndex)
import qualified Data.Map as M
import System.Random (Random (randomRs), mkStdGen)
import Prelude hiding (init)

-------------------------------------------------------------------------------

-- | Board --------------------------------------------------------------------

-------------------------------------------------------------------------------

type Obs = [Pos]

type Board = M.Map Pos XO

data XO
  = X
  | RedO
  | BlueO
  | PurpleO
  deriving (Eq, Show)

data Pos = Pos
  { pRow :: Int, -- 1 <= pRow <= dim
    pCol :: Int -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord, Show)

dim :: Int
dim = 50

init :: Board
init = M.empty

brickCTypes :: [Int]
brickCTypes = [1, 23, 21, -1]

initObs :: Int -> Obs
initObs n = do
  let seed = mkStdGen n
  let randomCList = take 100 $ randomRs (0, 3) seed
  [Pos (r * (-23)) (brickCTypes !! c) | (r, c) <- zip [0 ..] randomCList]

initBlue :: Pos
initBlue = Pos 35 35

initRed :: Pos
initRed = Pos 35 15

obs :: Int -> Obs
obs = initObs

blue :: Pos
blue = initBlue

red :: Pos
red = initRed

blueDye :: [Pos]
blueDye = []

redDye :: [Pos]
redDye = []

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
up :: [Pos] -> Int -> [Pos]
up ps n = [p {pRow = pRow p - n} | p <- ps]

down :: [Pos] -> [Pos]
down ps = [p {pRow = pRow p + 1} | p <- ps]

obsReset :: Int -> [Pos] -> Bool
obsReset n obs = all (`elem` obs) $ initObs n

redReset :: Pos -> Bool
redReset red = red == initRed

blueReset :: Pos -> Bool
blueReset blue = blue == initBlue

addBlueDye :: Pos -> [Pos] -> [Pos]
addBlueDye p ps = p : ps

addRedDye :: Pos -> [Pos] -> [Pos]
addRedDye p ps = p : ps

check :: [Pos] -> Pos -> Bool
check ps vessel =
  collision1 || collision2 || collision3 || collision4 || collision5 || collision6 || collision7 || collision8
  where
    collision1 = Pos (pRow vessel) 1 `elem` ps && (pCol vessel < 28)
    collision2 = Pos (pRow vessel) 23 `elem` ps && (pCol vessel > 22)
    collision3 = Pos (pRow vessel) 21 `elem` ps && (pCol vessel > 20) && (pCol vessel < 30)
    collision4 = Pos (pRow vessel - 15) (-1) `elem` ps && Pos (pRow vessel + 8) 1 `elem` ps && (pCol vessel > 30)
    collision5 = Pos (pRow vessel - 15) (-1) `elem` ps && Pos (pRow vessel + 8) 23 `elem` ps && (pCol vessel < 20)
    collision6 = Pos (pRow vessel) (-1) `elem` ps && Pos (pRow vessel + 23) 21 `elem` ps && (pCol vessel < 28)
    collision7 = Pos (pRow vessel) (-1) `elem` ps && Pos (pRow vessel + 23) (-1) `elem` ps && (pCol vessel > 22)
    collision8 = Pos (pRow vessel) (-1) `elem` ps && notElem (Pos (pRow vessel + 23) 1) ps && notElem (Pos (pRow vessel + 23) 23) ps && (pCol vessel > 20) && (pCol vessel < 30)

posList :: [Pos]
posList = [Pos 35 15, Pos 34 15, Pos 33 15, Pos 32 15, Pos 31 16, Pos 30 16, Pos 29 17, Pos 28 18, Pos 27 19, Pos 26 20, Pos 26 21, Pos 25 22, Pos 25 23, Pos 25 24, Pos 25 25, Pos 25 26, Pos 25 27, Pos 25 28, Pos 26 29, Pos 26 30, Pos 27 31, Pos 28 32, Pos 29 33, Pos 30 34, Pos 31 34, Pos 32 35, Pos 33 35, Pos 34 35, Pos 35 35, Pos 36 35, Pos 37 35, Pos 38 35, Pos 39 34, Pos 40 34, Pos 41 33, Pos 42 32, Pos 43 31, Pos 44 30, Pos 44 29, Pos 45 28, Pos 45 27, Pos 45 26, Pos 45 25, Pos 45 24, Pos 45 23, Pos 45 22, Pos 44 21, Pos 44 20, Pos 43 19, Pos 42 18, Pos 41 17, Pos 40 16, Pos 39 16, Pos 38 15, Pos 37 15, Pos 36 15]

nextPos :: Pos -> Int -> Pos
nextPos p dir = case elemIndex p posList of
  Just i -> posList !! ((i + dir) `mod` length posList)
  Nothing -> p

clockwise :: Pos -> Pos
clockwise p = nextPos p 1

counterClockwise :: Pos -> Pos
counterClockwise p = nextPos p (-1)

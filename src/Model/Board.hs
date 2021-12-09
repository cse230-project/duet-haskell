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

    -- * Moves
    up,
    down,
    clockwise,
    counterClockwise,
    check,
    obsReset,
    redReset,
    blueReset,
  )
where

import Data.List (elemIndex)
import qualified Data.Map as M
import Prelude hiding (init)
import System.Random 
-- import Control.Monad.Random (getRando)

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
  { pRow :: Int, -- 1 <= pRow <= dim
    pCol :: Int -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord, Show)

dim :: Int
dim = 50

init :: Board
-- init =  M.fromList [(Pos 17 7, BlueO), (Pos 17 13, RedO)]
init = M.empty

brickCTypes :: [Int]
brickCTypes = [1, 23, 21, -1]

initObs :: Obs
-- initObs = [Pos 0 1, Pos (-25) 1, Pos (-50) 23, Pos (-75) 21]
initObs = do
  let seed = mkStdGen 0
  let randomCList = take 100 $ randomRs (0, 3) seed
  [Pos (r * (-23)) (brickCTypes !! c) | (r, c) <- zip [0..] randomCList]


initBlue :: Pos
initBlue = Pos 35 15

initRed :: Pos
initRed = Pos 35 35

obs :: Obs
obs = initObs

-- initObs = [Pos 0 20]

blue :: Pos
blue = initBlue
red :: Pos
red = initRed

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
up :: [Pos] -> [Pos]
up ps = [p {pRow = pRow p - n} | p <- ps]
  where
    n = pRow (head ps) `div` 10 + 1

down :: [Pos] -> [Pos]
down ps = [p {pRow = pRow p + 1} | p <- ps]

obsReset :: [Pos] -> Bool
obsReset obs = all (`elem` obs) initObs

redReset :: Pos -> Bool
redReset red = red == initRed

blueReset :: Pos -> Bool
blueReset blue = blue == initBlue

check :: [Pos] -> Pos -> Pos -> Bool
check ps blue red = collision1 || collision2 || collision3 || collision4 
  || collision5 || collision6 || collision7 || collision8 || collision9 || collision10
  || collision11 || collision12 || collision13 || collision14
  where
    collision1 = Pos (pRow blue) 1 `elem` ps && (pCol blue < 28)
    collision2 = Pos (pRow red) 1 `elem` ps && (pCol red < 28)
    collision3 = Pos (pRow blue) 23 `elem` ps && (pCol blue > 22)
    collision4 = Pos (pRow red) 23 `elem` ps && (pCol red > 22)
    collision5 = Pos (pRow blue) 21 `elem` ps && (pCol blue > 20) && (pCol blue < 30)
    collision6 = Pos (pRow red) 21 `elem` ps && (pCol red > 20) && (pCol red < 30)
    collision7 = Pos (pRow blue - 15) (-1) `elem` ps && Pos (pRow blue + 8) 1 `elem` ps && (pCol blue > 30)
    collision8 = Pos (pRow red - 15) (-1) `elem` ps && Pos (pRow red + 8) 1 `elem` ps && (pCol red > 30)
    collision9 = Pos (pRow blue - 15) (-1) `elem` ps && Pos (pRow blue + 8) 23 `elem` ps && (pCol blue < 20)
    collision10 = Pos (pRow red - 15) (-1) `elem` ps && Pos (pRow red + 8) 23 `elem` ps && (pCol red < 20)
    collision11 = Pos (pRow blue) (-1) `elem` ps && Pos (pRow blue + 23) 21 `elem` ps && (pCol blue < 28)
    collision12 = Pos (pRow red) (-1) `elem` ps && Pos (pRow red + 23) 21 `elem` ps && (pCol red < 28)
    collision13 = Pos (pRow blue) (-1) `elem` ps && Pos (pRow blue + 23) (-1) `elem` ps && (pCol blue > 22)
    collision14 = Pos (pRow red) (-1) `elem` ps && Pos (pRow red + 23) (-1) `elem` ps && (pCol red > 22)

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

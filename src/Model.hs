{-# LANGUAGE RecordWildCards #-}

module Model where

import qualified Model.Board as Board
import Prelude hiding ((!!))

import qualified Model.Score  as Score

-------------------------------------------------------------------------------

-- | Ticks mark passing of time: a custom event that we constantly stream

-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------

-- | Top-level App State ------------------------------------------------------

-------------------------------------------------------------------------------

data State
  = Intro
  | Play PlayState
  | Outro

data PlayState = PS
  { psScore  :: Score.Score,     -- ^ current score
    psBoard :: Board.Board,
    -- | current board
    psObs :: Board.Obs,
    -- | whose turn
    psTurn :: Board.XO,
    -- | blue vessel
    bluePos :: Board.Pos,
    -- | red
    redPos :: Board.Pos,
    -- | result
    psResult :: Board.Result (),
    gameOver :: Bool,
    psTick :: Int
  }

init :: Int -> PlayState
init n =
  PS
    { psScore  = Score.init n,
      psBoard = Board.init,
      psObs = Board.obs,
      psTurn = Board.X,
      bluePos = Board.blue,
      redPos = Board.red,
      psResult = Board.Cont (),
      gameOver = False,
      psTick = 0
    }

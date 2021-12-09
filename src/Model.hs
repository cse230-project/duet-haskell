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
  { psScore  :: Score.Score,  -- | current score
    psObs :: Board.Obs,       -- | positions of all obs
    bluePos :: Board.Pos,     -- | blue vessel
    redPos :: Board.Pos,      -- | red vessel
    gameOver :: Bool,         -- | game status
    psTick :: Int,            -- | time
    psSpeed :: Int            -- | speed of moving vessels
  }

init :: PlayState
init = do
  PS
    { psScore  = Score.init,
      psObs = Board.obs,
      bluePos = Board.blue,
      redPos = Board.red,
      gameOver = False,
      psTick = 0,
      psSpeed = 8
    }

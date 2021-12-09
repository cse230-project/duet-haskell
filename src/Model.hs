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
    psBlueDye :: [Board.Pos], -- | blue dye
    psRedDye :: [Board.Pos],  -- | red dye
    gameOver :: Bool,         -- | game status
    psTick :: Int,            -- | time
    psSpeed :: Int,           -- | speed of moving vessels
    psSeed :: Int            -- | seed for random number generator
  }

init :: Int -> PlayState
init n = do
  PS
    { psScore  = Score.init,
      psObs = Board.obs n,
      bluePos = Board.blue,
      redPos = Board.red,
      psBlueDye = Board.blueDye,
      psRedDye = Board.redDye,
      gameOver = False,
      psTick = 0,
      psSpeed = 8,
      psSeed = n
    }

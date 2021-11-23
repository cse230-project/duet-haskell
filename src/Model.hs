{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
-- import qualified Model.Score  as Score

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
  { -- , psScore  :: Score.Score     -- ^ current score
    psBoard  :: Board.Board     -- ^ placeholder
  , psObs     :: Board.Obs      -- ^ current board
  , psTurn   :: Board.XO        -- ^ whose turn 
  , bluePos    :: Board.Pos       -- ^ blue vessel
  , redPos    :: Board.Pos       -- ^ red
  , psResult :: Board.Result () -- ^ result    
  , gameOver :: Bool 
  } 

init :: Int -> PlayState
init n = PS 
  { 
  -- , psScore  = Score.init n
    psBoard  = Board.init
  , psObs  = Board.initObs
  , psTurn   = Board.X
  , bluePos  = Board.initBlue
  , redPos   = Board.initRed
  , psResult = Board.Cont ()
  , gameOver = False
  }


-- nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
-- nextBoard s res = case res' of
--                     Board.Win _ -> Left res' 
--                     Board.Draw  -> Left res'
--                     _           -> Right s' 
--   where 
--     sc'  = Score.add (psScore s) (Board.boardWinner res) 
--     res' = Score.winner sc'
--     s'   = s { psScore = sc'                   -- update the score
--              , psBoard = mempty                -- clear the board
--              , psTurn  = Score.startPlayer sc' -- toggle start player
--              } 


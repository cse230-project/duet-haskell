module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
    ( PlayState(gameOver, psObs, bluePos, redPos), Tick(..) )
import Model.Board

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of
  AppEvent Tick                   -> Brick.continue (step s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move clockwise s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move counterClockwise s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { bluePos = f (bluePos s), redPos = f (redPos s) }

-------------------------------------------------------------------------------
step :: PlayState -> PlayState
-------------------------------------------------------------------------------
step s =  if gameOver s 
            then s 
          else s { psObs = down (psObs s), 
                   gameOver = check (psObs s) (bluePos s) (redPos s) }



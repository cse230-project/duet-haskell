module Control where

import Brick hiding (Result)
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Model
  ( PlayState (psScore, bluePos, gameOver, psObs, redPos, psTick),
    Tick (..),
  )
import Model.Board
import Model.Score

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of
  AppEvent Tick -> Brick.continue (step s)
  T.VtyEvent (V.EvKey V.KLeft _) -> if gameOver s then Brick.continue s else Brick.continue (move counterClockwise (step s))
  T.VtyEvent (V.EvKey V.KRight _) -> if gameOver s then Brick.continue s else Brick.continue (move clockwise (step s))
  T.VtyEvent (V.EvKey V.KEsc _) -> Brick.halt s
  _ -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s {bluePos = f (bluePos s), redPos = f (redPos s)}

-------------------------------------------------------------------------------
step :: PlayState -> PlayState
-------------------------------------------------------------------------------
step s =
  if gameOver s
    then s {
      psObs = if obsReset (psObs s) then psObs s else up (psObs s),
      bluePos = if blueReset (bluePos s) then bluePos s else clockwise (bluePos s),
      redPos = if redReset (redPos s) then redPos s else clockwise (redPos s),
      gameOver = not (obsReset (psObs s) && blueReset (bluePos s) && redReset (redPos s))
      }
    else s {
      psObs = if psTick s == 2 then down (psObs s) else psObs s,
      psScore = updateScore (psObs s) (psScore s),
      gameOver = check (psObs s) (bluePos s) (redPos s),
      psTick = if psTick s == 2 then 0 else psTick s + 1
      }

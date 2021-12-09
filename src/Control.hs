module Control where

import Brick (BrickEvent (AppEvent), EventM, Next, continue, halt)
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Model
  ( PlayState (bluePos, gameOver, psBlueDye, psObs, psRedDye, psScore, psSeed, psSpeed, psTick, redPos),
    Tick (..),
  )
import Model.Board
  ( Pos (pRow),
    addBlueDye,
    addRedDye,
    blueReset,
    check,
    clockwise,
    counterClockwise,
    down,
    obsReset,
    redReset,
    up,
  )
import Model.Score (clear, updateScore)

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of
  AppEvent Tick -> Brick.continue (step s)
  T.VtyEvent (V.EvKey V.KLeft _) -> if gameOver s then Brick.continue (step s) else Brick.continue (move counterClockwise (step s))
  T.VtyEvent (V.EvKey V.KRight _) -> if gameOver s then Brick.continue (step s) else Brick.continue (move clockwise (step s))
  T.VtyEvent (V.EvKey V.KEsc _) -> Brick.halt s
  T.VtyEvent (V.EvKey V.KUp _) -> if psSpeed s == 2 then Brick.continue (step s) else Brick.continue (setSpeed 1 (step s))
  T.VtyEvent (V.EvKey V.KDown _) -> if psSpeed s == 8 then Brick.continue (step s) else Brick.continue (setSpeed 0 (step s))
  _ -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s {bluePos = f (bluePos s), redPos = f (redPos s)}

-------------------------------------------------------------------------------
setSpeed :: Int -> PlayState -> PlayState
-------------------------------------------------------------------------------
setSpeed i s =
  s
    { psSpeed = if i == 1 then psSpeed s `div` 2 else psSpeed s * 2,
      psTick = 0
    }

-------------------------------------------------------------------------------
step :: PlayState -> PlayState
-------------------------------------------------------------------------------
step s =
  if gameOver s
    then
      s
        { psObs = if obsReset (psSeed s) (psObs s) then psObs s else up (psObs s) (pRow (head (psObs s)) `div` 10 + 1),
          psBlueDye = if obsReset (psSeed s) (psObs s) then psBlueDye s else up (psBlueDye s) (pRow (head (psObs s)) `div` 10 + 1),
          psRedDye = if obsReset (psSeed s) (psObs s) then psRedDye s else up (psRedDye s) (pRow (head (psObs s)) `div` 10 + 1),
          bluePos = if blueReset (bluePos s) then bluePos s else clockwise (bluePos s),
          redPos = if redReset (redPos s) then redPos s else clockwise (redPos s),
          gameOver = not (obsReset (psSeed s) (psObs s) && blueReset (bluePos s) && redReset (redPos s)),
          psScore = clear (psScore s)
        }
    else
      s
        { psObs = if psTick s == psSpeed s then down (psObs s) else psObs s,
          psBlueDye =
            if psTick s == psSpeed s
              then
                if check (psObs s) (bluePos s)
                  then down (addBlueDye (bluePos s) (psBlueDye s))
                  else down (psBlueDye s)
              else
                if check (psObs s) (bluePos s)
                  then addBlueDye (bluePos s) (psBlueDye s)
                  else psBlueDye s,
          psRedDye =
            if psTick s == psSpeed s
              then
                if check (psObs s) (redPos s)
                  then down (addRedDye (redPos s) (psRedDye s))
                  else down (psRedDye s)
              else
                if check (psObs s) (redPos s)
                  then addRedDye (redPos s) (psRedDye s)
                  else psRedDye s,
          psScore = if psTick s == psSpeed s then updateScore (psObs s) (psScore s) (psSpeed s) else psScore s,
          gameOver = check (psObs s) (bluePos s) || check (psObs s) (redPos s),
          psTick = if psTick s == psSpeed s then 0 else psTick s + 1
        }

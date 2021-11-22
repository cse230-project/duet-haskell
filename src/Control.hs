module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player ( Player(plStrat), Strategy )
-- import Model.Player 

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
step s = s { psObs = down (psObs s) }

-------------------------------------------------------------------------------
play :: XO -> PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = put (psBoard s) xo <$> getPos xo s 
  | otherwise      = return Retry

getPos :: XO -> PlayState -> IO Pos
getPos xo s = getStrategy xo s (bluePos s) (psBoard s) xo

getStrategy :: XO -> PlayState -> Strategy 
getStrategy X s = plStrat (psX s)
getStrategy BlueO s = plStrat (psO s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s b of
  Right s' -> continue s'
  Left res -> halt (s { psResult = res }) 



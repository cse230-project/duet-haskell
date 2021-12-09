module Main where

import System.Exit
import Test.HUnit
import Prelude
import qualified Model.Board as Board
import qualified Model.Score as Score

-- test Model/Board.hs
testClockwise = TestCase(assertEqual "clockwise" (Board.Pos 34 15) (Board.clockwise (Board.Pos 35 15)))
testCClockwise = TestCase(assertEqual "cClockwise" (Board.Pos 35 15) (Board.clockwise (Board.Pos 36 15)))
testUp = TestCase(assertEqual "up" [Board.Pos 9 10, Board.Pos 19 20] (Board.up [Board.Pos 11 10, Board.Pos 21 20] 2))
testDown = TestCase(assertEqual "down" [Board.Pos 11 10, Board.Pos 21 20] (Board.down [Board.Pos 10 10, Board.Pos 20 20]))
testRReset = TestCase(assertEqual "rreset" True (Board.redReset (Board.Pos 35 35)))
testBReset = TestCase(assertEqual "breset" True (Board.blueReset (Board.Pos 35 15)))
testCheck1 = TestCase(assertEqual "check1" True (Board.check [Board.Pos 30 1](Board.Pos 30 16)))
testCheck2 = TestCase(assertEqual "check2" False (Board.check [Board.Pos 30 23](Board.Pos 30 16)))

-- test Model/Score.hs
testAddScore = TestCase(assertEqual "add" (Score.Score 1 1) (Score.add (Score.Score 0 0) 8))
testMaxScore = TestCase(assertEqual "max" (Score.Score 2 5) (Score.add (Score.Score 0 5) 4))
testClearScore = TestCase(assertEqual "clear" (Score.Score 0 100) (Score.clear (Score.Score 10 100)))
testUpdateScore = TestCase(assertEqual "update" (Score.Score 3 10) (Score.updateScore [Board.Pos 45 1] (Score.Score 0 10) 2))

main :: IO ()
main = do
  putStrLn "\nRunning my tests... "
  counts <- runTestTT ( test [
      testClockwise,
      testCClockwise,
      testUp,
      testDown,
      testRReset,
      testBReset,
      testCheck1,
      testCheck2,
      testAddScore,
      testMaxScore,
      testClearScore,
      testUpdateScore
      ])
  putStrLn "\nDone Testing"
  if errors counts + failures counts == 0
      then exitSuccess
      else exitFailure

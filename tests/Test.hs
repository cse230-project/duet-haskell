module Main where 

import System.Exit
import Test.QuickCheck
import Test.Tasty
import Test.HUnit
import Prelude
import qualified Model.Board as Board

-- test Model/Board.hs
testClockwise = TestCase(assertEqual "clockwise" (Board.Pos 34 15) (Board.clockwise (Board.Pos 35 15)))
testCClockwise = TestCase(assertEqual "cClockwise" (Board.Pos 35 15) (Board.clockwise (Board.Pos 36 15)))
testUp = TestCase(assertEqual "up" [Board.Pos 10 10, Board.Pos 20 20] (Board.up ([Board.Pos 11 10, Board.Pos 21 20])))
testDown = TestCase(assertEqual "down" [Board.Pos 11 10, Board.Pos 21 20] (Board.down ([Board.Pos 10 10, Board.Pos 20 20])))

main :: IO ()
main = do 
  putStrLn "\nRunning my tests... "
  counts <- runTestTT ( test [
      testClockwise,
      testCClockwise,
      testUp,
      testDown
      ])
  putStrLn "\nDone Testing"
  if (errors counts + failures counts == 0)
      then exitSuccess
      else exitFailure
  exitWith ExitSuccess 

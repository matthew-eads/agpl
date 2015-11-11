module Main where
import Test.HUnit
import Parser
--import Agpl_syntax (GameState, board)
import Language.Haskell.TH.Syntax
import Control.Monad
import System.Exit
import Quote
import Language.Haskell.TH.Quote
import Data.Matrix

[agpl_f|ttt.agpl|]



--[agpl_f|chess.agpl|]

-- chessT = chess 2
tttT =  boardInitF Nil
expected = "Game ([Board (AppT ListT (AppT ListT (ConT Int))),Piece (ConT Char),Turn (ConT Int)],Move (AppT (AppT (TupleT 2) (ConT Int)) (ConT Int)),IsValidFun (VarE undefined),PossMovesFun (VarE undefined),OutcomeFun (VarE undefined),InitState (VarE undefined),Player (ConT Char),[])" 


m = matrix 3 3 $ \(i, j) -> Nil
gs = GameState {board = tttT, currentTurn = turn}

--expectedOutcome = 

tttTest1 = TestCase (assertEqual "Nil Matrix not formed" m tttT )
tttTest2 = TestCase (assertBool "isValid (1, 3) fails" 
                     (isValid gs (Move (1, 3))))
tttTest3 = TestCase (assertBool "isValid (0, 4) succeeds" 
                     (not (isValid gs (Move (0, 4)))))

-- chessTest = TestCase (assertEqual "Tic Tac Toe" 4 chessT )

-- tests = TestList [(TestLabel "TTT Test" tttTest), (TestLabel "Chess Test" chessTest)]
tests = TestList [(TestLabel "TTT Test" tttTest1), 
                  (TestLabel "isValid Test" tttTest2),
                  (TestLabel "isValid Test 2" tttTest3)]
main :: IO Counts
main = do {
        cs@(Counts _ _ errs fails) <- runTestTT tests;
        if (errs > 0 || fails > 0)
           then exitFailure
           else exitSuccess
}

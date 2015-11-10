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


tttTest = TestCase (assertEqual "Tic Tac Toe" m tttT )
-- chessTest = TestCase (assertEqual "Tic Tac Toe" 4 chessT )

-- tests = TestList [(TestLabel "TTT Test" tttTest), (TestLabel "Chess Test" chessTest)]
tests = TestList [(TestLabel "TTT Test" tttTest)]
main :: IO Counts
main = do {
        cs@(Counts _ _ errs fails) <- runTestTT tests;
        if (errs > 0 || fails > 0)
           then exitFailure
           else exitSuccess
}

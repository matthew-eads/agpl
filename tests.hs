module Main where
import Test.HUnit
import Parser
import Agpl_syntax
import Language.Haskell.TH.Syntax
import Control.Monad
import System.Exit

ttt = "TTT: Game\n\nGamestate:{Board: {[[Int]]}\nPiece:{Char}\nTurn:{Int}\n}\n"
      ++ "Player:{Char}\nMove:{(Int, Int)}\nisValid:{undefined}\npossMoves:"
      ++ "{undefined}\noutcome:{undefined}\ninitialState:{undefined}\n"

expected = "Game ([Board (AppT ListT (AppT ListT (ConT Int))),Piece (ConT Char),Turn (ConT Int)],Move (AppT (AppT (TupleT 2) (ConT Int)) (ConT Int)),IsValidFun (VarE undefined),PossMovesFun (VarE undefined),OutcomeFun (VarE undefined),InitState (VarE undefined),Player (ConT Char),[])" 

tttTest = TestCase (assertEqual "Tic Tac Toe" expected (parseToS ttt))

tests = TestList [TestLabel "TTT Test" tttTest]

main :: IO Counts
main = do {
        cs@(Counts _ _ errs fails) <- runTestTT tests;
        if (errs > 0 || fails > 0)
           then exitFailure
           else exitSuccess
}

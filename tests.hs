module Main where
import Test.HUnit
import Parser
import Agpl_syntax
import Language.Haskell.TH.Syntax
import Control.Monad
import System.Exit
import Quote
import Language.Haskell.TH.Quote

[agpl|TicTacToe: Game                
                Gamestate: {                   
                   Board: {Matrix}
                   Piece: {X | O | NIL}        
                }                
                Player: {X | O}     
                Move: {(Int, Int)}        
                isValid: { (\(i, j) -> ((i < 3) && (j < 3) &&  
                            (i >= 0) && (j >= 0) && ((board game) ! 
                                                  (i,j)) == nil))}     
                possMoves: {\() -> mfold (\((i, j), piece, acc) -> 
                          if piece == NIL then ((i, j):acc) 
                                          else acc) [] 
                                                      (board game)}  
                outcome: {\(x) -> if (((board game) ! ((fst (n x)), 
                        (snd (n x))) ==  (board game) ! ((fst (s x)),
                            (snd (s x)))) &&    ((board game) ! 
                                          ((fst (n x)), (snd (n x))) ==
                              (board game) ! ((fst x), (snd x)))) 
                              then 1  else 0}               
                initialState:{Board: {matrix 3 3 (\(i,j) -> NIL)} 
                              Turn: {X}}               |]
{-
quoted = [agpl|TicTacToe: Game

Gamestate: {
           Board: {Matrix}
           Piece: {X | O | NIL}
}

Player: {X | O}

--Type Coord: {Coord (Int, Int)}
--Move: {Move (Coord)}

Move: {(Int, Int)}
isValid: { (\(i, j) -> ((i < 3) && (j < 3) &&
                         (i >= 0) && (j >= 0) && ((board game) ! (i,j)) == nil))}


possMoves: {\() -> mfold (\((i, j), piece, acc) -> if piece == NIL then ((i, j):acc)
                                                        else acc) [] (board game)}

{- $fun ne (x, y) = (x+1,y+1)
 fun e  (x, y) = (x+1, y)
 fun se (x, y) = (x+1, y-1)
 fun s  (x, y) = (x, y-1)
 fun sw (x, y) = (x-1, y-1)
 fun w  (x, y) = (x-1, y)
 fun nw (x, y) = (x-1, y+1)
 fun n  (x, y) = (x, y+1)$
-}
outcome: {\(x) -> if (((board game) ! ((fst (n x)), (snd (n x))) ==
                      (board game) ! ((fst (s x)), (snd (s x)))) &&
                     ((board game) ! ((fst (n x)), (snd (n x))) ==
                      (board game) ! ((fst x), (snd x))))   
                  then 1 --Fin(Game.Board[fst x][snd x], 2)
                   --etc
                  else 0
         }

initialState:{Board: {matrix 3 3 (\(i,j) -> NIL)}
              Turn: {X}}
|]
-}

--[agpl| TicTacToe: Game Gamestate:{}|]
res = foo 2
expected = "Game ([Board (AppT ListT (AppT ListT (ConT Int))),Piece (ConT Char),Turn (ConT Int)],Move (AppT (AppT (TupleT 2) (ConT Int)) (ConT Int)),IsValidFun (VarE undefined),PossMovesFun (VarE undefined),OutcomeFun (VarE undefined),InitState (VarE undefined),Player (ConT Char),[])" 




tttTest = TestCase (assertEqual "Tic Tac Toe" 4 res )

tests = TestList [TestLabel "TTT Test" tttTest]

main :: IO Counts
main = do {
        cs@(Counts _ _ errs fails) <- runTestTT tests;
        if (errs > 0 || fails > 0)
           then exitFailure
           else exitSuccess
}

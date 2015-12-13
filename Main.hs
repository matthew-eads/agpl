module Main where
import Debug.Trace (trace)
import System.Environment
import Parser
import Quote
import Data.Matrix as M hiding (trace)
import Data.List as L
import Data.List.Split
import Data.Vector as V hiding (foldl)
[agpl_f|ttt.agpl|]
--[agpl_f|chess.agpl|]
--[agpl_f|connect4.agpl|]

getMove :: GameState -> IO Move
getMove gs = do {
               putStrLn ("Player " L.++ (show (currentTurn gs)) L.++ "'s turn." L.++
                         "Please enter a move:");
               m <- getLine;
               return (fromString m);
             }

won :: Player -> IO ()
won p = do {
          putStrLn ("Player " L.++ (show p) L.++ " has won!");
          return ();
}

tie :: IO ()
tie = do {
        putStrLn "The game has ended in a tie.";
        return ();
}


playGame :: GameState -> IO ()
playGame gs = do {
                m <- getMove gs;
                if isValid gs m then
                    let (result, i) = outcome gs m 
                    in (case result of 
                           (Win p) -> (won p)
                           (Tie) -> tie
                           x -> (trace (show (board x)) (playGame x)))
                else do {
                       putStrLn "Invalid move, try again.";
                       (playGame gs);
                     }
}

main :: IO ()
main = let gs = GameState{board = (boardInitF), currentTurn=turn} in
       do{
         playGame gs;
         return ();

       }

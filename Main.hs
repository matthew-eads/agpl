module Main where
import Debug.Trace (trace)
import System.Environment
import Parser
import Quote
import Data.Matrix hiding (trace)
import Data.List
import Data.List.Split

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True

[agpl_f|ttt.agpl|]
-- [agpl_f|chess.agpl|]

getMove :: GameState -> IO Move
getMove gs = do {
               putStrLn ("Player " ++ (show (currentTurn gs)) ++ "'s turn." ++
                         "Please enter a move:");
               m <- getLine;
               putStrLn ("Parsing: " ++ m);
               return (fromString m);
             }

won :: Player -> IO ()
won p = do {
          putStrLn ("Player " ++ (show p) ++ " has won!");
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

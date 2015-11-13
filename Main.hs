module Main where
import Debug.Trace (trace)
import System.Environment
import Parser
import Quote
import Data.Matrix hiding (trace)
import Data.List
import Data.List.Split
[agpl_f|ttt.agpl|]

getMove :: GameState -> IO Move
getMove gs = do {
               putStrLn ("Player " ++ (show (currentTurn gs)) ++ "'s turn." ++
                         "Please enter a move:");
               m <- getLine;
               let move = map read (splitOn "," m) in
               return (Move ((head move), (head (tail move))));
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
                let (result, i) = outcome gs m in
                case result of 
                  (Win p) -> (won p)
                  (Tie) -> do tie
                  x -> (playGame x)
}

main :: IO ()
main = let gs = GameState{board = (boardInitF Nil), currentTurn=turn} in
       do{
         playGame gs;
         return ();
       }

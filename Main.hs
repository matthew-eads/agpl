module Main where
import System.Environment
import Parser

main :: IO ()
main = do {
         args <- getArgs;
         lines <- readFile (head args);
         res <- parseGame lines;
         print res;
         return ();
       }

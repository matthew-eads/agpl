module Main where
import System.Environment
import Parser

main :: IO ()
main = do {
         args <- getArgs;
         lines <- readFile (head args);
         let res = (parseGame lines) in
         print res;
         return ();
       }

module Main where
import Test.HUnit
import Agpl_tests
main :: IO ()
main = do {
         runTestTT tests;
         return ();
}

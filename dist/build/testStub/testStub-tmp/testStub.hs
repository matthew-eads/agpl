module Main ( main ) where
import Distribution.Simple.Test.LibV09 ( stubMain )
import Agpl_tests ( tests )
main :: IO ()
main = stubMain tests

module Agpl_lib where

import Agpl_syntax
import Data.Matrix

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True

iToC :: Int -> Int -> Int-> (Int, Int)
iToC rows cols i = let c = if (i `mod` cols) == 0 then cols else (i `mod` cols) 
                       r = ceiling ((realToFrac i) / (realToFrac rows))
                   in (r, c)

lfoldi :: (Int -> (Int, Int)) -> Int -> (((Int, Int), a, b) -> b) -> b -> [a] -> b
lfoldi toC i f b [] = b
lfoldi toC i f b (x:xs) = f ((toC i), x, (lfoldi toC (i-1) f b xs)) 

mfold :: (((Int, Int), a, b) -> b) -> b -> Matrix a -> b
mfold f b m = let nr = (nrows m)
                  nc = (ncols m)
                  l  = (toList m)
              in lfoldi (iToC nr nc) (length l) f b l

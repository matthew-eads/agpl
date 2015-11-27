module MFold where
import Data.Matrix as M hiding (trace)
import Data.Vector as V
import Data.List as L
import Debug.Trace
iToC :: Int -> Int -> Int-> (Int, Int)
iToC rows cols i = if cols > rows then
                       let c = if (i `mod` cols) == 0 then cols else (i `mod` cols) 
                           r = ceiling ((realToFrac (i-c)) / (realToFrac cols))
                           r2 = if r == 0 then 1 else r+1
                       in (r2, c)
                   else 
                       let c = if (i `mod` cols) == 0 then cols else (i `mod` cols) 
                           r = ceiling ((realToFrac (i-c)) / (realToFrac cols))
                           r2 = if r == 0 then 1 else r+1
                       in (r2, c)

lfoldi :: (Int -> (Int, Int)) -> Int -> (((Int, Int), a, b) -> b) -> b -> [a] -> b
lfoldi toC i f b [] = b
lfoldi toC i f b (x:xs) = f ((toC i), x, (lfoldi toC (i-1) f b xs)) 

mfold :: (((Int, Int), a, b) -> b) -> b -> Matrix a -> b
mfold f b m = let nr = (nrows m)
                  nc = (ncols m)
                  l  = (M.toList m)
              in lfoldi (iToC nr nc) (L.length l) f b l

getDiags :: Matrix t -> [[t]]
getDiags m = let k = (ncols m) + (nrows m)
                 -- midi = (2 * k) - (ncols m) - 2
                 midi = (2*k) - ((2 * (k - 1)) `div` 2) + 1
                 diags = generate (2*k) (\_ -> empty)
             in V.toList (V.map V.toList (mfold (\((i, j), elem, diagv) -> 
                           let subv = snoc (diagv V.! (i + j - 2)) elem 
                               subv2 = snoc (diagv V.! (i - j + midi)) elem
                           in (diagv // [((i + j - 2), subv), ((i-j+midi), subv2)])) diags m))

m = matrix 3 3 $ \(i, j) -> (i, j)
res = mfold (\((i, j), a, b) -> (i,j):b) [] m

diags = getDiags m

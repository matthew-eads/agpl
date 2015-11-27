-- module Agpl_lib where

-- import Agpl_syntax
-- import Data.Matrix

-- game :: GameState
-- game = undefined
-- playerToPiece :: Player -> Piece
-- playerToPiece = undefined
isFull b = mfold (\((i, j), piece, acc) -> if piece == Nil then False else (True && acc)) True b

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True

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


-- isEmpty :: Piece -> Bool
-- isEmpty Nil = True
-- isEmpty _ = False

inRow :: Int -> Player -> Board -> Bool
inRow n player board = 
    let piece = playerToPiece player :: Piece
        rows = toLists board  :: [[Piece]]
        frow = (\c -> (\x -> if c == n then c else if x == piece then (c + 1) else 0)) :: Int -> Piece -> Int
        frows = (\acc -> (\row -> (foldl frow 0 row) >= n || acc)) :: Bool -> [Piece] -> Bool
    in foldl frows False rows 

inCol :: Int -> Player -> Board -> Bool
inCol n player board = 
    let piece = playerToPiece player :: Piece
        cols = toLists (M.transpose board) :: [[Piece]]
        fcol = (\c -> (\x -> if c == n then c else if x == piece then (c + 1) else 0)) :: Int -> Piece -> Int
        fcols = (\acc -> (\col -> (foldl fcol 0 col) >= n || acc)) :: Bool -> [Piece] -> Bool
    in foldl fcols False cols

getDiags :: Matrix t -> [[t]]
getDiags m = let k = (ncols m) + (nrows m)
                 -- midi = (2 * k) - (ncols m) - 2
                 midi = (2*k) - ((2 * (k - 1)) `div` 2) + 1
                 diags = generate (2*k) (\_ -> empty)
             in V.toList (V.map V.toList (mfold (\((i, j), elem, diagv) -> 
                           let subv = snoc (diagv V.! (i + j - 2)) elem 
                               subv2 = snoc (diagv V.! (i - j + midi)) elem
                           in (diagv // [((i + j - 2), subv), ((i-j+midi), subv2)])) diags m))

inDiag :: Int -> Player -> Board -> Bool
inDiag n player board = 
    let piece = playerToPiece player :: Piece
        diags = getDiags board :: [[Piece]]
        fdiag = (\c -> (\x -> if c == n then c else if x == piece then (c + 1) else 0)) :: Int -> Piece -> Int
        fdiags = (\acc -> (\diag -> (foldl fdiag 0 diag) >= n || acc)) :: Bool -> [Piece] -> Bool
    in (trace ("diags: " L.++ (show diags)) (foldl fdiags False diags))

inRowColOrDiag :: Int -> Player -> Board -> Bool
inRowColOrDiag n player board = inDiag n player board || inCol n player board || inRow n player board

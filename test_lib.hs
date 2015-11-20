isFull b = mfold (\((i, j), piece, acc) -> if piece == Nil then False else (True && acc)) True b
ne (x, y) = (x+1,y+1)
e  (x, y) = (x+1, y)
se (x, y) = (x+1, y-1)
s  (x, y) = (x, y-1)
sw (x, y) = (x-1, y-1)
w  (x, y) = (x-1, y)
nw (x, y) = (x-1, y+1)
n  (x, y) = (x, y+1)

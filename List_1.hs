-- ex 1
palindromo :: String -> Bool
palindromo w = reverse w == w

-- ex 2
verificaTriangulo :: Int -> Int -> Int -> Bool
verificaTriangulo a b c = (a + b > c) && (b + c > a) && (a + c > b)

-- ex 3
sinal :: Int -> Int
sinal x 
 | x < 0 = -1
 | x > 0 = 1
 | otherwise = 0

-- ex 4
menorTres :: Int -> Int -> Int -> Int
menorTres x y z 
 | (x < y) && (x < z) = x
 | (y < x) && (y < z) = y
 | otherwise = z

-- ex 5
potencia :: Int -> Int -> Int
potencia b e
 | e == 0 = 1
 | e == 1 = b
potencia b e = b * potencia b (e-1)
-- ex 1
multLista:: Int -> [Int] -> [Int]
multLista n [] = []
multLista n (x:xs) = n*x : multLista n xs

-- ex 2
elemento:: Int -> [Int] -> Bool
elemento _ [] = False
elemento n (x:xs)
 | x == n = True
 | otherwise = elemento n xs
 
-- ex 3
conta :: Int -> [Int] -> Int
conta _ [] = 0
conta n (x:xs)
 | x == n = 1 + conta n xs
 | otherwise = conta n xs
 
-- ex 4
contaMaiores :: Int -> [Int] -> Int
contaMaiores _ [] = 0
contaMaiores n (x:xs)
 | x > n = 1 + contaMaiores n xs
 | otherwise = contaMaiores n xs
 
-- ex 5 
maiores :: Int -> [Int] -> [Int]
maiores _ [] = []
maiores n (x:xs)
 | x > n = x : maiores n xs
 | otherwise = maiores n xs

-- ex 6
geraLista :: Int -> Int -> [Int]
geraLista m n
 | m == 1 = n : []
 | otherwise = n : geraLista (m-1) n 

-- ex 7
addFim :: Int -> [Int] -> [Int]
addFim n [] = [n]
addFim n (x:xs) = x : addFim n xs

-- extra 
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]
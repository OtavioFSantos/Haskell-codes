data Arvore = Folha Int | Nodo Int Arvore Arvore
 deriving(Eq,Show)

arv1 :: Arvore
arv1 = Nodo 10 (Nodo 14 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6)) (Folha 9)

-- ex 1
multArvore :: Int -> Arvore -> Arvore
multArvore x (Folha n) = Folha (n*x)
multArvore x (Nodo n a1 a2) = Nodo (x*n) (multArvore x a1) (multArvore x a2)

-- ex 2
contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) = (contaFolhas a1) + (contaFolhas a2)

-- ex 3
contaNodos :: Arvore -> Int
contaNodos (Folha n) = 0
contaNodos (Nodo n a1 a2) = 1 + (contaNodos a1) + (contaNodos a2)

-- ex 4
quantasVezes :: Int -> Arvore -> Int
quantasVezes x (Folha n)
 | x == n = 1
 | otherwise = 0
quantasVezes x (Nodo n a1 a2)
 | x == n = 1
 | otherwise = (quantasVezes x a1) + (quantasVezes x a2)
 
-- ex 5 
maxArvore :: Arvore -> Int
maxArvore (Folha n) = n
maxArvore (Nodo n a1 a2) = max n ( max (maxArvore a1) (maxArvore a2))

-- ex 6
refleteArvore :: Arvore -> Arvore
refleteArvore (Folha n) = Folha n
refleteArvore (Nodo n a1 a2) = Nodo n (refleteArvore a2) (refleteArvore a1)

-- ex 7
geraListaArv :: Arvore -> [Int]
geraListaArv (Folha n) = n : []
geraListaArv (Nodo n a1 a2) = n : (geraListaArv a2) ++ (geraListaArv a1)
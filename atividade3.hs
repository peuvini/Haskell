{- 1) Função 'itemN', que recebe um número inteiro positivo n e uma lista e retorna o
n-ésimo elemento da lista.
Ex.:
> itemN 2 ["primeiro", "segundo", "terceiro"]
"segundo" -}
{- itemN :: Int -> [String] -> String
itemN _ [] = []
itemN num (x:xs) | num-1 == 0 = x
                 | otherwise = itemN num xs -}
{- 2) Função recursiva 'parImpar', que recebe uma lista de inteiros e retorna uma tupla
cujo primeiro elemento é uma lista dos números pares da lista de entrada e o
segundo é uma lista dos números ímpares da lista de entrada.
Ex.:
> parImpar [1,2,3,4,5]
([2,4],[1,3,5])3 -}
parImpar :: [Int] -> ([Int],[Int])
parImpar x = (listaPar x, listaImpar x)


listaPar :: [Int] -> [Int]
listaPar [] = []
listaPar (x:xs) | even x == True = x:listaPar xs
                | otherwise = listaPar xs
listaImpar :: [Int] -> [Int]
listaImpar [] = []
listaImpar (x:xs) | odd x == True = x:listaImpar xs
                  | otherwise = listaImpar xs
{- 3) Função recursiva 'estaOrdenado', que recebe uma lista e retorna um Bool que
indica se a lista está ordenada ou não.
Ex.:
> estaOrdenado [1,2,3]
True
-}
estaOrdenado :: [Int] -> Bool
estaOrdenado [] = True
estaOrdenado [x] = True
estaOrdenado (x:xs) | x > head xs  = False 
                    | x == head xs = estaOrdenado xs
                    | otherwise = estaOrdenado xs
{- 4) Função para calcular a média dos elementos de uma lista de números.
Podem-se usar duas funções, uma para obter a quantidade de elementos e outra
para obter a soma dos elementos, e finalmente, calcular a divisão entre a soma e a
quantidade. -}
media :: [Int] -> Int
media [] = 0
media x = (sum x) `div` (length x)
{- 5) Função recursiva que dada uma lista de inteiros e um número n, retorne o total de
elementos de valor superior a n.
Ex.:
> retornaSup 5 [3, 2, 5, 6, 9]
2
 -}
retornaSup :: Int -> [Int] -> Int
retornaSup _ [] = 0
retornaSup num (x:xs) | x > num = 1 + retornaSup num xs
                      | otherwise = retornaSup num xs
{- 6) Função recursiva para buscar um inteiro em uma dada lista de inteiros:
Ex.:
> find 5 [3, 2, 5, 6, 9]
5 
> find 10 [3, 2, 5, 6, 9]
error “Elemento não está na lista”
-}
find :: Int -> [Int] -> Int
find _ [] = error "elemento nao esta na lista"
find num (x:xs) | x == num = x
                | otherwise = find num xs
{- 7) Função para repetir os elementos de uma lista de acordo com o valor do
elemento.
> dupli [1, 2, 3, 5]
[1, 2, 2, 3, 3, 3, 5, 5, 5, 5, 5] -}
dupli :: [Int] -> [Int]
dupli list = concat [replicate i i | i<-list]
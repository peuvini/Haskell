{- 1) Escreva uma função insere que receba um elemento(inteiro) e uma lista de Inteiro
e insere o elemento se somente se ele ainda não pertence à lista. Em seguida faça
os seguintes testes 
insere 2 [1,2,3]
[1, 2, 3]
-}
insere :: Int -> [Int] -> [Int]
insere x listaInt | (x `elem` listaInt) == True = listaInt
                  | otherwise = (:) x listaInt
{- 2) Defina uma função que dada uma lista de inteiros, retorna uma nova lista
contendo os elementos de valor superior a um número n qualquer.
> retornaSup 4 [3,2,5,6]
[5,6] -}
retornaSup :: Int -> [Int] -> [Int]
retornaSup n listaInt = [i | i<-listaInt, maior i n]

---Funcao Auxiliar
maior :: Int -> Int -> Bool
maior x y | x > y = True
          | otherwise = False
{- 3) Defina a função splitHalf que divide uma lista em duas, de tamanho iguais (ou
com diferença de apenas um elemento no caso de uma lista de tamanho ímpar). 
> splitHalf [1,3,5,8,15]
([1,3],[5,8,15] )-}
splitHalf :: [Int] -> ([Int],[Int])
splitHalf lista = splitAt ((length lista) `div` 2) lista
{- 4) Defina uma função que recebe duas listas de inteiros e retorna a interseção delas -}
intersecao :: [Int] -> [Int] -> [Int]
intersecao l1 l2 = [i | i<-l2, i `elem` l1]
{- 5) Defina uma função para dada uma string, contar o número de ocorrências de
vogais -}
numeroVogais :: String -> Int
numeroVogais letra = length [i | i<-letra, elem i vogais  ]
                   where
                    vogais = ['a','e','i','o','u','A','E','I','O','U']
{- 6) Defina uma função para verificar se uma cadeia de caracteres é uma palavra (ou
seja, é formada apenas de letras). -}
apenasLetras :: String -> Bool
apenasLetras str | length [ i | i <- str, elem i ['a'..'z'] ] == length str = True
                 |otherwise = False
{- 7) Defina a função strip que recebe duas listas, retira da segunda todos os
elementos que ocorrem na primeira.
> strip [1,5] [1,2,5,5,3,5,1]
[2,3] -}
strip :: [Int] -> [Int] -> [Int]
strip l1 l2 = [i | i<-l2, not (elem i l1)]
{- 8) O produto escalar de dois vetores v e w de tamanho n é dado pela soma dos
produtos dos elementos correspondentes: 
Exemplos: O produto escalar entre os vetores v = (1,2,5) e w = (2,−7,12)
é: v⋅w = 1.2 + 2.(−7) + 5.12 = 48
Usando compreensão de lista, defina uma função que retorna o produto escalar de
dois vetores representados por listas de inteiros.
> produtoEscalar :: [Int] -> [Int] -> Int-}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar v1 v2 = sum numerosMultiplicados
                      where
                        vetores = zip v1 v2 --- [(),()]
                        numerosMultiplicados = [multiplicaTupla i | i<- vetores]

multiplicaTupla :: (Int,Int) -> Int
multiplicaTupla t1 = fst t1*snd t1
{- 9) Defina uma função para somar uma lista de pares
> sumPairs [(1, 2), (3, 5)]
[3, 8] -}
sumPairs :: [(Int,Int)] -> [Int]
sumPairs l1 = [a+b | (a,b) <- l1]

{- 1º Elabore uma função que recebe um número e uma lista de números e retorna os
números que são maiores do que o valor informado.
Ex.:
> maioresQue 10 [4, 6, 30, 3, 15, 3, 10, 7]
[30, 15]
 -}
maioresQue :: Int -> [Int] -> [Int]
maioresQue _ [] = []
maioresQue int (x:xs) | x > int = [x] ++ maioresQue int xs
                      | otherwise = maioresQue int xs

{- 2º Elabore uma função que recebe um inteiro N e uma lista de inteiros e retorna o
número de ocorrências N na lista.
> contaOcorrencias 2 [1, 3, 4]
0
> contaOcorrencias 4 [1, 3, 4]
1
 -}
contaOcorrencias :: Int -> [Int] -> Int
contaOcorrencias _ [] = 0
contaOcorrencias int (x:xs) | int == x = 1 + contaOcorrencias int xs
                            | otherwise = contaOcorrencias int xs

{- 3º Elabore uma função recursiva que receba um inteiro N e uma lista de inteiros e
retorna a posição da última ocorrência de N na lista.
Ex.:
> posicaoN 2 [0, 5, 2, 9, 2]
4
> posicaoN 2 [0, 5, 2, 9]
2 -}
posicaoN :: Int -> [Int] -> Int
posicaoN _ [] = -1
posicaoN int [x] | int == x = 0
                 | otherwise = -1
posicaoN int (x:xs) = 1+ posicaoN int xs


{- 4º Elabore uma função recursiva que receba uma lista de inteiros e identifique se
algum elemento da lista é negativo.
Ex.:
> possuiNegativo [2,3,4]
False
> possuiNegativo [2,-1,0]
True -}
possuiNegativo :: [Int] -> Bool
possuiNegativo [] = False
possuiNegativo (x:xs) | x < 0 = True
                      | otherwise = possuiNegativo xs
{- 5º Elabore uma função recursiva que dada uma lista de string, retorna as strings que
tem, pelo menos, 5 vogais. -}
stringVogais :: [String] -> [String]
stringVogais [] = []
stringVogais (x:xs) | quantidadeVogal x >= 5 = x:stringVogais xs
                    | otherwise = stringVogais xs


quantidadeVogal :: String -> Int
quantidadeVogal [] = 0
quantidadeVogal (x:xs) | elem x vogais == True = 1 + quantidadeVogal xs
                       | otherwise = quantidadeVogal xs
                        where 
                            vogais = ['A','E','I','O','U','a','e','i','o','u'] 
{- 6º Elabore uma função recursiva que verifica se todos os elementos de uma lista de
inteiros são iguais. -}
verificaIguais :: [Int] -> Bool
verificaIguais [] = True
verificaIguais [x] = True
verificaIguais (x:n:xs) | x == n = verificaIguais (n:xs)
                        | otherwise = False
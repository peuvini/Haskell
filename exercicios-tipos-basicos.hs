import Data.Char
{- 
Escreva a declaração de uma função que:​

1 -Tem como entrada dois inteiros e devolve um booleano​

2 - Tem como entrada um inteiro e um char e devolve um float​

3 - Tem como entrada dois char e devolve um booleano​

Escreva as declarações e definições de funções para :​

4 - Dados dois inteiros , retornar o triplo da divisão inteira destes números​

5 -Dados dois caracteres, retornar o menor deles de acordo com a ordem alfabética​

6 - Dados dois inteiros, retornar True se forem iguais em valor absoluto; caso contrário, retornar False​ -}

primeiraQuestao :: Int -> Int -> Bool
primeiraQuestao x y = x == y

segundaQuestao  :: Int -> Char -> Bool
segundaQuestao x y = (x >= 65  && x <= 90) && (y >= 'a' && y <= 'z')

terceiraQuestao :: Char -> Char -> Bool
terceiraQuestao x y = (x == y)

quartaQuestao :: Int -> Int -> Int
quartaQuestao x y = (x `div` y) * 3

quintaQuestao :: Char -> Char -> Char
quintaQuestao x y | x > y = x
                  | otherwise = y

sextaQuestao :: Int -> Int -> Bool
sextaQuestao x y = abs(x) == abs(y)

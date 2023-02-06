
import Data.Char
{- Elabore uma função ultimaVogal tal que dada uma string xs, determina a posição
da última ocorrência de uma vogal em xs. Caso não haja nenhuma ocorrência de vogal,
a função deve retornar -1. Por exemplo,
ultimaVogal “Aracaju” devolverá 7 e
ultimaVogal “CBF” devolverá -1.
"pedro" -}
ultimaVogal :: String -> Int
ultimaVogal str | [i | i <- str, elem i vogais] == [] = (-1)
                | otherwise = head [pegaNum i | i<-reverseIndexa, elem (snd i) vogais ]
                 where

                    reverseIndexa = reverse indexa
                    indexa = zip [1..(length str)] str
                    vogais = ['a','e','i','o','u','A','E','I','O','U']
pegaNum :: (Int,Char) -> Int
pegaNum (num,letra) = num
pegaLetra :: (Int,Char) -> Char
pegaLetra (_,chr) = chr
{- Elabore uma função paresPrecedem tal que dada uma lista de inteiros de contendo
pelo menos um elemento par e outro ímpar, devolve True se todos os elementos pares
precedem todos os elementos ímpares na lista. Caso contrário, devolve False. (Dica:
pense em termos de posições dos elementos da lista). Por exemplo,
paresPrecedem [1,100,30,9,150,7] devolverá False e
paresPrecedem [2,4,1,7] devolverá True. -}
paresPrecedem :: [Int] -> Bool
paresPrecedem lista | lista == (pegaPares ++ pegaImpares) = True
                    |otherwise = False
                     where
                        pegaImpares = [i | i<-lista,ehImpar i]
                        pegaPares = [i | i <- lista, ehPar i]


---Funcao auxiliar
ehPar :: Int -> Bool
ehPar x   | even x == True = True
          |otherwise = False
ehImpar :: Int -> Bool
ehImpar x | odd x == True = True
          |otherwise = False
{-Primeiros Conhecimentos sobre Haskell-}


--Como funções são declaradas--
areaQuadrado :: Int -> Int
areaQuadrado x = x*2 
{- 
Na função acima é calculada a área do quadrado e seus elementos são os seguintes:
areaQuadrado -> é o nome da função
O primeiro Int -> É o tipo do argumento de entrada
O segundo Int -> É o tipo do resultado
x -> o lado do quadrado	
-}

--Tipos primitivo--
{-
 Bool -> Analisa se a função é verdadeira ou falsa
 Int -> São os literais, ou seja, todos os números até 2147483647
 Integer -> É usado quando necessita de números extremamente grandes maiores que 2147483647
 Char -> É o tipo onde são caracteres
 Float e Double -> É usado para representar os números reais
-} 
funcaoBool :: Int -> Bool -- função onde se entra um inteiro e devolve um valor booleano
funcaoInt :: Int -> Int --função se entra um inteiro e devolve um inteiro
funcaoInteger :: Int -> Integer --função onde se entra um inteiro e devolve um integer
funcaoChar:: Char -> Bool --função onde se entra um caractere e devolve um valor booleano
funcaoFloat :: Float -> Bool --função onde se entra float e devolve um valor booleano

--Obs.: Nomes de tipos SEMPRE são escritos com letra maiúscula e nomes de funções SEMPRE são escritos com letra minúscula


{- Funções de tipos primitivos em haskell -}

--Int e Integer
--A função div devolve o consciente da divisão de x por y
divisao :: Int -> Int -> Int
divisao x y = x `div` y
--Já a função mod devolve o resto da divisao entre c por p
funcaoMod :: Int -> Int -> Int 
funcaoMod c p = c `mod` p
{-
  Algumas outras funções usadas pelos inteiros são:
  abs-> retorna o valor absoluto de um inteiro
  negate-> retorna a variavel com sinal trocado
-}

--Char
{-
  Para usar as funções char precisa incluir no inicio do código o comando import data.Char
  As funções que mais se usa em char são:
  ord-> traz o valor inteiro do caractere informado
  chr-> traz o caractere do número informado
-}

--Float e Double
{-
  São inúmeros as funções usadas com os tipos float e double, citarei abaixo alguns
  abs-> valor absoluto do numero
  acos,asin,atan->traz o inverso do cosseno,seno e tangente
  ceiling,floor-> converte uma fração em um inteiro aproximando para o inteiro mais proximo
  fromInteger-> converte Integer em Float
  fromIntegral->converte Inteiro em Float
  sqrt->traz a raiz quadrada do numero
-}


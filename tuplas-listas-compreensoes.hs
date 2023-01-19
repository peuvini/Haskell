import Data.Char
--Escreva um função que dados dois números inteiros, retorna o maior e o menor  valores​
--O retorno será na forma de uma tupla (menor, maior), pois uma função não pode retornar mais de um valor em separado.​
maiorNum :: Int -> Int -> (Int,Int)
maiorNum num1 num2 | num1 >= num2 = (num2,num1)
                   |otherwise = (num1,num2)
--Dados dois inteiros elabore uma função para retornar o máximo e o número de vezes que ele ocorre​
maxOcorre :: Int -> Int -> (Int,Int)
maxOcorre x y | x == y = (x,2)
             | otherwise = (max x y, 1)
--Usando maxOcorre defina uma função similar só que agora para três números.​
maxOcorre3 :: Int -> Int -> Int -> (Int,Int)
maxOcorre3 num1 num2 num3 |num1 == num2 && num2 == num3 = (num1,3)
                          |num1 == num2 && num2 > num3 = (num2,2)
                          |num1 == num3 && num3 > num2 = (num3,2)
                          |num2 == num3 && num3 > num1 = (num3,2)
                          |otherwise = (max num3 (max num1 num2),1)
--Dada uma tupla com três elementos inteiros, elabore uma função para devolver uma tupla com os elementos em ordem.​
elemOrdem :: (Int,Int,Int) -> (Int,Int,Int)
elemOrdem (x,y,z) |x == y && y == z = (x,y,z)
                  |x <= y && y<=z = (x,y,z)
                  |x <= z && z<= y =(x,z,y)
                  |y <= x && x<=z = (y,x,z)
                  |y <= z && z<=y = (y,z,x)
                  |z <= x && x<=y = (z,x,y)
                  |z <= y && y<=x = (z,y,x)
--Escreva a lista dos pares entre 1 e 11.​
listaPar = [2,4..10]
--Escreva uma função para dada uma lista de inteiros, seleciona os ímpares desta lista e devolve uma lista em que os elementos ímpares aparecem triplicados.​
ehImpar :: Int -> Bool
ehImpar x |mod x 2 == 0 = False
          |otherwise = True
triplicaImpar :: [Int] -> [Int]
triplicaImpar xs = [3*i | i<-xs,ehImpar i]
--Escreva uma função que dada uma String, devolve outra String em que as letras minúsculas foram transformadas em maiúsculas e os demais caracteres permanecem iguais. 
--Você pode usar as função paraMaiusculo, já vista, na sua solução.​
ehMinusculo :: Char -> Bool
ehMinusculo ch = ('a'<= ch) && ch <= 'z'
paraMaiusculo :: Char -> Char
paraMaiusculo letra | ehMinusculo letra = chr(ord letra-ord 'a' + ord 'A')
                    |otherwise = letra
{- transMaiusculo :: String -> [Char]
transMaiusculo frase = [i | i<-frase,paraMaiusculo i] -}
--Dada uma lista de reais, elabore uma função para retornar os elementos x tal que 0≤x≤100
intervalo :: Int -> Bool
intervalo x | x >= 0 && x<=100 = True
            |otherwise = False 
entre0e100 :: [Int] -> [Int]
entre0e100 x = [ i | i<-x,intervalo i]
--Dada uma String elabore uma função para retornar a mesma String sem os caracteres que são dígitos. Ex: “loja 2” deve retornar “loja ”
--97 - 122  65-90
stringSemDig :: String -> String
stringSemDig x = [ch | ch <- x, isAlpha ch]

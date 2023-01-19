--Faca uma função que receba um ângulo a e retorne uma tupla contendo o seno da metade desse ângulo utilizando a identidade:
--sin(x/2) = +- sqrt(1-cos(x)/2)
senoMetade :: Float -> (Float,Float)
senoMetade x = (y,-y)
             where
                y = sqrt((1-cos(x))/2)
--Verifica se o ano e bissexto em uma lista de anos e devolve os bissextos
ehBissexto :: Int -> Bool
ehBissexto x |mod x 4 == 0 && (mod x 100 /= 0 || mod x 400 == 0)  = True
             |otherwise = False
anosBissexto :: [Int] -> [Int]
anosBissexto xs = [i | i<-xs, ehBissexto i]
--Crie um concatenador de strings que concatena duas strings separadas por espaço
concatenaString :: String -> String -> String
concatenaString x y = x++" "++y
--Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo.
--"se a soma das medidas de dois deles é sempre maior que a medida do terceiro, então, eles podem formar um triângulo"
ehTriangulo :: Int -> Int -> Int -> Bool
ehTriangulo x y z |((x+y)>z && (x+z)>y && (z+y)>z) == True = True
                  |otherwise = False
--Crie uma função tipoTriangulo que determina o tipo do triângulo formado pelos três lados x, y, z
tipoTriangulo::  Int -> Int -> Int -> String
tipoTriangulo x y z | (ehTriangulo x y z == True) && x == y && y == z = "Triangulo Equilatero"
                    | (ehTriangulo x y z == True) && (x == y && y /= z || x == z && z /= y || y == z && x /= y) = "Triangulo isosceles"
                    | (ehTriangulo x y z == True) && x /= y && y /= z && z /= x = "Triangulo escaleno"
                    |otherwise = "Nao eh triangulo"

         
-- Faca uma funcao mult3 que retorne True caso a entrada seja multiplo de 3 e False caso contrario
mult3 :: Int -> Bool
mult3 x | mod x 3 == 0 = True
        | otherwise = False
--Faca uma  funcao mult5que retorne True caso a entrada seja multiplo de 5 e False caso contrario
mult5 :: Int -> Bool
mult5 x | mod x 5 == 0 = True
        | otherwise = False
--Faca uma  funcao mult35 que retorne True caso a entrada seja multiplo de 3 e de 5 e False caso contrario
mult35 :: Int -> Bool
mult35 x | mod x 3 == 0 && mod x 5 == 0 = True
         | otherwise = False
--Faca um programa que retorne True caso a entrada seja menor que -1 ou (maior que 1 E multiplo de 2), e False caso contrario
q4 :: Int -> Bool
q4 x | x < -1 || mod (x) 2 == 0 && x > 1 = True
     | otherwise = False 
--Dados tres numeros,determine se todos sao diferentes
tresDif :: Int -> Int -> Int -> Bool
tresDif x y z | x/=y && y/=z && x/=z = True
              | otherwise = False
-- Dados dois numeros, determine o menor deles
menorNum :: Int -> Int -> Int
menorNum x y | x > y = y
             | y >= x = x
--Dados tres numeros, determine quantos estao acima da media dos tres
acimaMedia :: Float -> Float -> Float -> Int
acimaMedia x y z 
                 | (x > media && y> media) || (x> media && z> media) || (y> media && z> media) = 2 
                 | x > media || y > media || z > media = 1
                 |otherwise = 0
                 where
                    media = (x+y+z)/3
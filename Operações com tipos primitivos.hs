{-Operações com tipos primitivos-}

--Bool
exemploBool :: Bool -> Bool -> Bool
exemploBool x y = (x || y) && not (x && y)
{- essa função é importante pois ela recebe a maioria das operações com bool
 a função ela recebe dois argumentos bool e tem um resultado true or false
 só será verdadeira se:
 (x || y)[x ou y] &&[e] not[não] (x && y)[x e y]
 caso contrário será falsa
-}
exemplo2Bool :: Int -> Int -> Int -> Bool
exemplo2Bool x y z 
    |(x == y) && (z /= x ) = true
    |otherwise = false
{- essa função ela usa as outras duas operações de bool
   exemplo2Bool recebe três argumentos inteiros e um resultado bool
   ela diz que se x == y(x igual a y) e z /= x(z diferente de x) o valor é verdadeiro
   caso contrário o valor da função é falso 
-}

-- Int
areaQuadrado :: Int -> Int
areaQuadrado x = x*x

areaTriangulo :: Int -> Int -> Int
areaTriangulo x y = (x*y)/2
{-
 As operações que os números inteiros aceitam são:
 +(soma) , *(multiplicação) , ^(potência) , -(subtração) , <(menor que) , <=(menor ou igual a) , >(maior que) , >=(maior ou igual a) , ==(igual) , /=(diferente),etc
 na função areaQuadrado foi usada apenas a operação de multiplicação onde 2 multiplica x
 já na função areaTriangulo foi usada a multiplicação para fazer a operação x*y e o resultado foi dividido por 2  
-}

-- Char 
maiorChar :: Char -> Char -> Bool
maiorChar x y
  |x >= y = true
  |otherwise = false
{-
 O tipo char ele aceita as seguintes operações:
 <(menor que) , >(maior que) , ==(igual) , /=(diferente) , <=(menor ou igual a) , >= (maior ou igual a),etc
 A função maiorChar ela recebe dois argumentos char e tem resultado bool, a função dele diz que se o primeiro valor char for maior ou igual aparece true caso contrário false
-}

-- Float e Double
areaCirculo :: Float -> Float
areaCirculo x = (x*x)*pi
{-
 Os Tipos Float e Double aceitam as seguintes operações:
 +(soma), -(subtração),*(multiplicação), /(divisão),^(potência),**(potência onde pode escolher o número),<(menor que), <=(menor ou igual que), >(maior que), >=(maior ou igual que),etc
 a função areaCirculo recebe um argumento float e devolve um argumento float, a função diz que um valor qualquer x multiplicado por x o resultado multiplicado por pi dá a area do circulo
-} 



--Crie uma funcao chamada nAnd, que dadas duas variaveis booleanas,retorna true em todos os casos a excecao do caso em que as duas variaveis sejam verdadeiras
nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd _ _ = True
--Elabore uma funcao que determine se um inteiro e multiplo de 3 e/ou de 5 ou de nenhum deles. A saida deve ser a mensagem "multiplo de 3 e de 5". "multiplo de 3", "multiplo de 5"
--ou "nao eh multiplo de 3 nem de 5"
mult35 :: Int -> String
mult35 x | mult3 == 0 && mult5 == 0 = "multiplo de 3 e de 5"
         | mult3 == 0 && mult5 /= 0 = "multiplo de 3"
         | mult3 /= 0 && mult5 == 0 = "multiplo de 5"
         | otherwise = "nao eh multiplo de 3 nem de 5"
         where
            mult3 = mod x 3
            mult5 = mod x 5
--Elabore uma função que dados dois inteiros representando dois lados perpendiculares de uma forma geométrica retorne se esta forma é um quadrado ou retângulo.
ehQuadOuRet :: Int -> Int -> String
ehQuadOuRet x y | areaQuad == areaRet = "Quadrado"
                | otherwise = "Retangulo"
                where
                    areaQuad = x^2
                    areaRet  = x*y
--Elabore uma função que dados dois lados perpendiculares do retângulo calcula sua área.
areaRet :: Int -> Int -> Int
areaRet x y = x*y
--Usando a função acima, calcule a área de um cuboide de lados a, b e c
areaCuboide :: Int -> Int -> Int -> Int
areaCuboide x y z = 2 * (areaRet x y) + 2 * (areaRet x z) + 2 * (areaRet y z)
--Usando expressões let e where refaça a questão anterior sem usar a função retângulo externamente à sua função.​
areaCuboide2 :: Int -> Int -> Int -> Int
areaCuboide2 x y z = 2 * ab + 2 * ac + 2 * bc
                   where
                    ab = x*y
                    ac = x*z
                    bc = y*z
--Usando casamento de padrão escreva uma função que dada sua cor preferida retorna seu significado, ou dá uma mensagem de consolação se não souber o significado. 
--Você pode usar: branco-paz, amarelo-alegria, verde-esperança , azul - tranquilidade, vermelho – paixão e qualquer outra - desculpa.​
significaCor :: String -> String
significaCor "branco" = "paz"
significaCor "amarelo" = "alegria"
significaCor "verde" = "esperança"
significaCor "azul" = "tranquilidade"
significaCor "vermelho" = "paixão"
significaCor _ = "desculpa"
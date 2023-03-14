import Data.Char
{- Usando a função map, elabore:
Função para converter uma lista de caracteres numa lista dos códigos numéricos destes caracteres.
map :: (a -> b) -> [a] -> [b]​
map f [ ] = [ ]​
map f (z:zs) = f z : map f zs​ 
ou
map :: (a -> b) -> [a] -> [b]​
map f xs = [f x | x<-xs]
-}
listaCodCaracteres :: [Char] -> [Int]
listaCodCaracteres lista = [ord x | x<-lista]

{- Função para converter uma lista de inteiros em uma lista dos quadrados destes inteiros. -}
listaQuadrados :: [Int] -> [Int]
listaQuadrados [] = []
listaQuadrados (x:xs) = x : listaQuadrados xs

{- Função para dado uma lista de pares cartesianos retornar uma lista dos segundos elementos destes pares. -}
segundoPar :: [(Int,Int)] -> [Int]
segundoPar list = map snd list

{- Elabore uma função para verificar se dado uma string ela não contém dígitos.-}
semDigit :: String -> Bool
semDigit list | [i | i <- list, i `elem` algarismos] == [] = True
              | otherwise = False
                where
                    algarismos = ['0','1','2','3','4','5','6','7','8','9']

{- Elabore uma função para verificar se dado uma string ela contém algum caracter que não seja letra. -}
apenasLetras :: String -> Bool
apenasLetras list | length [i | i<-list,i `elem` alfabeto] == length list = True
                  | otherwise = False
                     where
                        alfabeto = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']


{- Usando a função filter, elabore:
Função para selecionar os elementos positivos de uma lista de inteiros. 
filter :: (a -> Bool) -> [a] -> [a]​
filter f zs = [z | z <- zs, f z]-}
elementosPositivos :: (Int->Bool) -> [Int] -> [Int]
elementosPositivos f list = [i | i<-list,i>0]

{- Função para dada uma propriedade p e uma lista, remove da lista de entrada o último elemento que não satisfaz p. -}
fFilter :: (Int->Bool) -> [Int] -> [Int]
fFilter f list = init[z | z <- list, f z]

{- Função para selecionar os elementos que ocupem posições pares na lista de entrada. -}
posicoesPares :: [Int] -> [Int]
posicoesPares list = [ snd z | z<- listaPosicao, even (fst z) ]
                       where
                        listaPosicao = zip [1,2 .. length(list)] list

{- Função para dada uma lista gerar um par onde o primeiro elemento do par contém a lista de elementos 
das posições pares e o segundo elemento do par a lista dos elementos das posições ímpares. -}
posicoesImpares :: [Int] -> [Int]
posicoesImpares list = [ snd z | z<- listaPosicao, odd (fst z) ]
                       where
                        listaPosicao = zip [1,2 .. length(list)] list

parCartesiano :: [Int] -> [(Int,Int)]
parCartesiano l1 = zip pares impares
                    where
                     impares = posicoesImpares l1
                     pares = posicoesPares l1
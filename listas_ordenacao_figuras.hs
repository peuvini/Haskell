{- Dados duas listas sem elementos repetidos, faça uma função para checar se as listas são disjuntas. -}
listasDisjuntas :: [Int] -> [Int] -> Bool
listasDisjuntas [] l2 = True
listasDisjuntas l1 [] = True
listasDisjuntas x (y:ys) | y `elem` x = False
                         | otherwise = listasDisjuntas x ys


{-Dadas duas listas sem elementos repetidos, 
faça uma função para gerar uma lista com os elementos comuns das duas listas sem usar a função elem.​-}
listaComum :: [Int] -> [Int] -> [Int]
listaComum [] l2 = []
listaComum l1 [] = []
listaComum x (y:ys) | lista /= [] = [y] ++ listaComum x ys
                    | otherwise = listaComum x ys
                        where
                            lista = [i |i <-x, i==y]
{-Dada uma lista de inteiros em ordem arbitrária, com a possibilidade de ter elementos repetidos, 
gere uma lista ordenada crescente, sem elementos repetidos.​-}
listaOrd :: [Int] -> [Int]
listaOrd [] = []
listaOrd (x:xs) = listaOrd us ++ [x] ++ listaOrd vs
                  where
                    us = [i | i<-xs,i<=x]
                    vs = [i | i<-xs,i>x]
retiraIguais :: [Int] -> [Int]
retiraIguais [] = []
retiraIguais [x] = [x]
retiraIguais (x:xs) | x == head xs = retiraIguais xs
                    | otherwise = [x] ++ retiraIguais xs

listaOrdCres :: [Int] -> [Int]
listaOrdCres [] = []
listaOrdCres (x:xs) = retiraIguais(listaOrd (x:xs))


{- Dada uma lista de pares cartesianos do tipo abaixo, ordene os pares pelo nome da pessoa, em ordem lexicográfica (a ordem dos dicionários) 
e em caso de mesmo nome, ordene pela idade da pessoa, em ordem decrescente.​

Assim, para a lista [(“Maria”, 20), (“Jonas”, 30), (“Maria”, 50)] a saída seria ​

[(“Jonas”, 30), (“Maria”, 50), (“Maria”, 20)]​
 -}
type Nome = String
type Idade = Int
type Pessoa = (Nome, Idade)

ordemDicionario :: [Nome] -> [Nome]
ordemDicionario [] = []
ordemDicionario (x:xs) | head xs < x = [head xs] ++ [x] ++ ordemDicionario (tail xs) 


----Funcao auxiliar

retiraNome :: Pessoa -> Nome
retiraNome (nome,_) = nome
retiraIdade :: Pessoa -> Idade
retiraIdade (_,idade) = idade
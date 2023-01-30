{- O que acontece se você usar take -2 [1,2,3] ? -}
{- Dada uma lista de inteiros, elabore uma função que calcula o produto de todos os elementos pares da lista​ -}
calculaProduto :: [Int] -> Int
calculaProduto list = product list
{- Dado o símbolo ‘.‘ , construa uma string com 30 ‘.’ -}
multiplica30 :: Char -> String
multiplica30 x = replicate 30 x
{-Dadas duas palavras delvolva True se forem palindromos;caso contrario,devolva False.
Duas palavras sao palindromos se a ordem de leitura, da esquerda para direito ou da direita para a esquerda, eh indiferente.
Ex: arara.-}
ehPalindromo:: String -> String -> Bool
ehPalindromo str1 str2 | reverse str1 == str2 = True
                       | otherwise = False

{- Dadas duas listas uma do tipo Nome e outra do tipo Idade, já vistos, construa uma lista com elementos do tipo (Nome, Idade) -}
type Pessoa = [(Nome,Idade)]
type Nome = String
type Idade = Int

pessoa :: [Nome] -> [Int] -> Pessoa
pessoa lnome lint = zip lnome lint

{- Dada uma lista de (Estados, Capitais), construa um par com duas listas, onde o primeiro elemento é a lista de estados e o segundo elemento a de capitais​ -}
parDeListas :: [(String,String)] -> ([String],[String])
parDeListas x = unzip x


{- Dada uma lista de reais, elabore uma função para calcular a soma dos valores da lista maiores ou iguais a 5.0 -}
maioresIgualQueCinco :: [Float] -> [Float]
maioresIgualQueCinco listNum = [i| i<-listNum, i>= 5.0]

{- Faça uma função que dada uma string e um inteiro representando um tamanho máximo de impressão para uma informação, devolve a mesma string justificada à direita, 
deixando espaços em branco na frente da string, se for o caso. Ex: se você fornecer “papagaio” e 15 de tamanho, a função deve retornar “       papagaio”,  
incluindo assim 7 espaços em branco à esquerda de papagaio que tem 8 caracteres, 
para completar os 15 caracteres de tamanho reservados. -}
espacoNaFrente :: String -> Int -> String
espacoNaFrente str int = espacos ++ str
                       where
                        espacos = replicate int ' '

{- Dada uma lista de reais, elabore uma função para calcular a média dos valores da lista -}
mediaLista :: [Float] -> Float
mediaLista lNum = soma/fromIntegral(tamanho)
                where
                    soma = sum lNum
                    tamanho = length lNum
{- Dada uma lista de notas de alunos, elabore uma função para determinar a lista das notas acima da média das notas da lista. -}
acimaMedia :: [Float] -> [Float]
acimaMedia notas = [i |i<-notas , i> media]
                 where
                    media = (sum notas) / fromIntegral(length notas)

{- Escreva um função que dada uma string, devolva a mesma string com o caracter \n ao final. Ex: se a entrada for “gato” retornará “gato\n”. -}
caracterNoFinal :: String -> String
caracterNoFinal str = str ++ "\n"


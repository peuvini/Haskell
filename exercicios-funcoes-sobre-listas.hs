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
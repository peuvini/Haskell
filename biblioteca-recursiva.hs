import Data.List
type Pessoa = String
type Livro = String
type Emprestimos = [(Pessoa, Livro)]
biblioteca :: Emprestimos
biblioteca = [("Marcos Sa","A"), ("Mateus Oliveira", "A"),("Sofia Reis","C"), ("Paulo Souza", "D"), ("Cleberton", "B"), ("Marcos Sa","40")]
{- Dada uma pessoa, encontre os livros que ela emprestou -}
emprestimosPessoa :: Pessoa -> Emprestimos -> [Livro]
emprestimosPessoa pessoa [] = []
emprestimosPessoa pessoa (x:xs) | pessoa == fst x = [snd x] ++ emprestimosPessoa pessoa xs 
                                |otherwise = emprestimosPessoa pessoa xs

--Funcao auxiliar
pegaPessoa :: Emprestimos -> Pessoa
pegaPessoa [(pessoa,_)] = pessoa

pegaLivro :: Emprestimos -> Pessoa
pegaLivro [(_,livro)] = livro

{- Dado um livro, encontre quem emprestou este livro, assumindo que o livro pode ter mais de um exemplar -}
quemEmprestou :: Livro -> Emprestimos -> [Pessoa]
quemEmprestou livro [] = []
quemEmprestou livro (x:xs) | livro == snd x = [fst x] ++ quemEmprestou livro xs
                           | otherwise = quemEmprestou livro xs
{- Dada uma pessoa, desejamos saber a quantidade de livros que ela tomou emprestado -}
quantidadeEmprestimos :: Pessoa -> Emprestimos -> Int
quantidadeEmprestimos pessoa [] = 0
quantidadeEmprestimos pessoa (x:xs) | pessoa == fst x = 1 + quantidadeEmprestimos pessoa xs                     
                                    | otherwise = quantidadeEmprestimos pessoa xs
{- Dado um par (Pessoa, Livro), queremos removê-lo da lista de emprestados, sinalizando a sua devolução. -} 
removeLista :: (Pessoa,Livro) -> Emprestimos -> Emprestimos
removeLista pessoaLivro [] = []
removeLista pessoaLivro (x:xs) | (fst pessoaLivro == fst x) && (snd pessoaLivro == snd x) = removeLista pessoaLivro xs
                               | otherwise = [x] ++ removeLista pessoaLivro xs
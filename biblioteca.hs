module SistemaBiblioteca where
import Data.List
type Pessoa = String
type Livro = String
type Emprestimos = [(Pessoa, Livro)]
biblioteca :: Emprestimos
biblioteca = [("Marcos Sa","A"), ("Mateus Oliveira", "A"),("Sofia Reis","C"), ("Paulo Souza", "D"), ("Cleberton", "B"), ("Marcos Sa","40")]
--Dada uma pessoa, encontre os livros que ela emprestou;
emprestimosPessoa :: Pessoa -> Emprestimos -> Livro
emprestimosPessoa name library = catchBook person
                                    where
                                        person = [i | i <- library, catchPerson [i] == name ]

---Funcao auxiliar
catchPerson :: Emprestimos -> Pessoa
catchPerson [(person,_)] = person
catchBook :: Emprestimos -> Livro
catchBook [(_,book)] = book

---Dado um livro, encontre quem emprestou este livro, assumindo que o livro pode ter mais de um exemplar;
quemEmprestou :: Livro -> Emprestimos -> [Pessoa]
quemEmprestou book library = names
                            where
                                bookFinal = [i | i<-library, catchBook [i] == book]
                                names = [fst i | i<-bookFinal]
---Dado um livro, desejamos saber se o mesmo se encontra emprestado ou não;
emprestadoOuNao :: Livro -> Emprestimos -> String
emprestadoOuNao book library | listaEmprestados /= [] = "Emprestado"
                             |otherwise = "Em estoque" 
                              where
                                listaEmprestados = [i | i<-library, catchBook [i] == book]
---Dada uma pessoa, desejamos saber a quantidade de livros que ela tomou emprestado;
quantidadeEmprestimos :: Pessoa -> Emprestimos -> Int
quantidadeEmprestimos person library = length pessoa
                                     where
                                        pessoa = [i | i<-library, catchPerson [i] == person]
--- Dado um par (Pessoa, Livro), queremos adicioná-lo  à lista de emprestados, sinalizando o empréstimo realizado;
adicionaLista :: (Pessoa,Livro) -> Emprestimos -> Emprestimos
adicionaLista pessoaLivro emprestimos = (:) pessoaLivro emprestimos   
---Dado um par (Pessoa, Livro), queremos removê-lo  da lista de emprestados, sinalizando a sua devolução.                      
removeLista :: (Pessoa,Livro) -> Emprestimos -> Emprestimos
removeLista (pessoa,livro) emprestimos | ((pessoa,livro) `elem` emprestimos) = (pessoa,livro) `delete` emprestimos
                                       | otherwise = emprestimos

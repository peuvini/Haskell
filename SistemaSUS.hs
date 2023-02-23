type CadastroSUS = [Cidadao]
type CPF = Integer
type Nome = String
type Genero = Char
type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia, Mes, Ano)
type DataNasc = Data
type Endereco = String
type Municipio = String
type Estado = String
type Telefone = String
type Email = String
type Cidadao = (CPF, Nome, Genero, DataNasc, Endereco, Municipio,Estado, Telefone, Email)

meuCadastro :: CadastroSUS
meuCadastro = [(1, "Paulo Souza", 'M', (11,10,1996),"Rua A, 202", "Muribeca", "SE", "999997000", "psouza@gmail.com"),
               (2, "Ana Reis",'F', (5,4,1970), "Rua B, 304","Aracaju", "SE", "999826004", "areis@gmail.com")] 




-------------------------------------------------------------------------------------
{- (a) Cadastramento de um cidadão no sistema. -}
adicionaSUS :: Cidadao -> CadastroSUS -> CadastroSUS -- adiciona cidadao a um cadastro
adicionaSUS pessoa cadastro | checaCPF (capturaCPF pessoa) cadastro == False = (:) pessoa cadastro
                            | otherwise = error "Usuario ja cadastrado"
checaCPF :: CPF -> CadastroSUS -> Bool --- checa se um cpf faz parte de um cadastro
checaCPF myCPF myCadastro | [i | i<-myCadastro,capturaCPF i == myCPF] == [] = False 
                          | otherwise = True

---Funcoes auxiliares
capturaCPF :: Cidadao -> CPF --- captura o cpf de um cidadao
capturaCPF (cpf,_,_,_,_,_,_,_,_) = cpf
-------------------------------------------------------------------------------------
{- (b) O cidadão pode querer modificar algum desses dados, por exemplo, o número de
telefone ou endereço. Para isto, precisamos de funções de atualização dos dados no
cadastro, passando os novos dados. Para simplificar o sistema, vamos supor apenas
as funções de atualização do endereço e do telefone, já que as demais atualizações
seguiriam o mesmo princípio. No processo de atualização, o cadastro SUS
informado será copiado para um novo cadastro SUS. Neste novo cadastro, os
registros de outros cidadãos permanecerão inalterados e somente os dados do
cidadão que está sendo atualizado sofrerão modificações. -}
-------------------------------------------------------------------------------------
atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS myCPF cadastro end | checaCPF myCPF cadastro == False = error "Usuario nao esta cadastrado"
                                  | otherwise = [trocaEnd i end | i<-cadastro,capturaCPF i == myCPF] ++ [i | i<-cadastro,capturaCPF i /= myCPF]
----Funcoes Auxiliares
trocaEnd :: Cidadao -> Endereco -> Cidadao -- Troca o endereco antigo por um novo
trocaEnd (cpf,nome,genero,dataNasc,end,municipio,estado,telefone,email) novoEnd = (cpf,nome,genero,dataNasc,novoEnd,municipio,estado,telefone,email)
-------------------------------------------------------------------------------------

atualizaTelSUS :: CPF -> CadastroSUS -> Telefone -> CadastroSUS -- atualiza Telefone de um CPF no cadastro
atualizaTelSUS myCPF cadastro tel | checaCPF myCPF cadastro == False = error "Usuario nao esta cadastrado"
                                  | otherwise = [trocaTel i tel | i<-cadastro,capturaCPF i == myCPF] ++ [i | i<-cadastro,capturaCPF i /= myCPF]
---Funcoes Auxiliares
trocaTel :: Cidadao -> Endereco -> Cidadao -- Troca o endereco antigo por um novo
trocaTel (cpf,nome,genero,dataNasc,end,municipio,estado,telefone,email) novoTel = (cpf,nome,genero,dataNasc,end,municipio,estado,novoTel,email)

{- (c) Quando um cidadão falece, a família tem que notificar o fato em um posto de saúde,
para que ele seja retirado do cadastro corrente do SUS. Como há uma verificação do
atestado de óbito, isto só pode ser feito no posto. O sistema precisará da função
abaixo. Se o CPF existir no cadastro corrente do SUS, o registro do cidadão deve
ser completamente excluído, gerando um novo cadastro sem os dados deste cidadão.
Se o CPF não existir, uma mensagem de erro, usando error, sinalizando que o
cidadão não pertence ao cadastro deve ser exibida. -}
removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS myCPF cadastro | checaCPF myCPF cadastro == False = error "Usuario nao pertence a esse cadastrado"
                         | otherwise = [i | i<-cadastro,capturaCPF i /= myCPF]
-------------------------------------------------------------------------------------
type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)
type Quantidade = Int
{- (d) Um gestor de saúde pode querer pesquisar algumas informações deste cadastro,
como por exemplo, quantidade de cidadãos por município, por estado, ou ainda por
município e por faixa de idade, ou por estado e por faixa de idade, para ter uma ideia
de como planejar as faixas de vacinação. Assim, o sistema deve prever algumas
funções de consulta: -}
cidadaosPorMunicipio :: CadastroSUS -> Municipio -> Quantidade  -- dado um cadastro e um municipio descobre a quantidade de pessoas desse municipio no banco
cidadaosPorMunicipio cadastro myMunicipio | checaMunicipio myMunicipio cadastro == True = length [ i | i<-cadastro, capturaMunicipio i == myMunicipio]
                                          | otherwise = 0

-------------------------------------------------------------------------------------
---- Funcoes auxiliares
checaMunicipio :: Municipio -> CadastroSUS -> Bool --- checa se um municipio faz parte de um cadastro
checaMunicipio myMunicipio myCadastro | [i | i<-myCadastro,capturaMunicipio i == myMunicipio] == [] = False 
                                      | otherwise = True
capturaMunicipio :: Cidadao -> Municipio
capturaMunicipio (_,_,_,_,_,municipio,_,_,_) = municipio



cidadaosPorEstado :: CadastroSUS -> Estado -> Quantidade -- dado um cadastro e um estado descobre a quantidade de pessoas desse estado no banco
cidadaosPorEstado cadastro myEstado       | checaEstado myEstado cadastro == True = length [ i | i<-cadastro, capturaEstado i == myEstado]
                                          | otherwise = 0
-------------------------------------------------------------------------------------
---- Funcoes auxiliares
checaEstado :: Estado -> CadastroSUS -> Bool --- checa se um estado faz parte de um cadastro
checaEstado myEstado myCadastro       | [i | i<-myCadastro,capturaEstado i == myEstado] == [] = False 
                                      | otherwise = True
capturaEstado :: Cidadao -> Estado --- dado um cidadao captura o estado dele
capturaEstado (_,_,_,_,_,_,estado,_,_) = estado
-------------------------------------------------------------------------------------


cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio-> FaixaIdade -> Quantidade
cidadaosPorMunicipioIdade cadastro myMunicipio faixaIdade = length [i | i<-cadastro, capturaMunicipio i == myMunicipio, naFaixaIdade(idade (capturaDataNasc i)) faixaIdade]
---Funcoes Auxiliares


capturaDataNasc :: Cidadao -> DataNasc -- dado um cidadao captura sua data de nascimento
capturaDataNasc (_,_,_,dataNasc,_,_,_,_,_) = dataNasc

---calculo da idade levando em conta apenas o ano vigente 
idade :: DataNasc -> Int
idade (_,_,ano) = 2023 - ano 

naFaixaIdade :: Int -> FaixaIdade -> Bool
naFaixaIdade idade faixa | idade >= fst faixa && idade <= snd faixa = True
                         | otherwise = False
cidadaosPorEstadoIdade :: CadastroSUS -> Estado -> FaixaIdade -> Quantidade
cidadaosPorEstadoIdade cadastro myEstado faixaIdade = length [i | i<-cadastro, capturaEstado i == myEstado,naFaixaIdade(idade (capturaDataNasc i)) faixaIdade]

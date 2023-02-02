type CodProd = Int
type NomeProd = String
type PrecoProd = Int
type Produto = (CodProd, NomeProd, PrecoProd)
type Menu = [Produto]
cardapio :: Menu
cardapio = [(150, "Hamburguer", 1000), (15, "Agua", 400), (2, "Coca-cola", 600), (40, "Batata-frita", 850), (52, "Tartelete", 1550)] 
{- 3.1 Escreva as funções a seguir para manipular o cardápio do restaurante armazenado no
sistema.
(a) Adiciona um produto no cardápio. Se o código do produto já existir no
cardápio deve retornar uma mensagem de erro sinalizando que existe um produto
já cadastrado para aquele código. -}
adicionaProdMenu :: Menu -> Produto -> Menu
adicionaProdMenu menu produto |checaCodProd (firstCodProd produto) menu = error "Produto ja existe"
                              | otherwise = (:) produto menu


--Funcoes auxiliares
firstCodProd :: Produto -> CodProd
firstCodProd (x,_,_) = x

checaCodProd :: CodProd -> Menu -> Bool
checaCodProd cod menu = or [cod == codProd| (codProd,_,_)<-menu]
{- (b) Remove um produto no cardápio, informando seu código. Se o código do
produto não existir no cardápio deve retornar uma mensagem de erro sinalizando
que não existe um produto no cardápio para aquele código. -}
removeProdMenu :: Menu -> CodProd -> Menu
removeProdMenu menu cod | not(checaCodProd cod menu) = error "Nao existe"
                        | otherwise = [produto | produto <-menu, cod /= (firstCodProd produto)]

{- (c) Coleta um produto no cardápio, informando seu código. Para simplificar,
considere que esta operação só tem o caso de sucesso, ou seja, o item consultado
sempre vai existir no cardápio. -}
coletaProdMenu :: Menu -> CodProd -> Produto
coletaProdMenu menu cod = head listaMenu
                        where
                            listaMenu = [i | i<- menu, firstCodMenu [i] == cod]
                         
                       
----Funcao auxiliar
firstCodMenu :: Menu -> CodProd
firstCodMenu [(cod,_,_)] = cod 


type CodCliente = Int
type NomeCliente = String
type CategCliente = Char
type ConsumoAnual = Int
type Cliente = (CodCliente, NomeCliente, CategCliente,ConsumoAnual)
type Clientes = [Cliente]
fregueses :: Clientes
fregueses = [(4, "Marcos Sa", 'A', 38000), (3, "Mateus Oliveira", 'A', 30000),(2, "Sofia Reis", 'B', 50000), (1, "Paulo Souza", 'C', 100000)]
type Quant= Int
type SolCliente = (CodProd, NomeProd, Quant)
type PedidoCliente = [SolCliente]

{- 3.2 Escreva funções para gerir clientes.
(a) Adiciona um cliente na lista de clientes do restaurante. O cliente será adicionado
sempre no início da lista. O código do novo cliente é gerado adicionando-se 1 ao
código do cliente mais antigo, que está no início da lista vigente. Caso a lista
vigente esteja vazia, o primeiro cliente a ser adicionado terá código 1. O novo
cliente possuirá categoria A e consumo anual 0 ao se cadastrar. Observe que a
lista assim construída será ordenada, de forma decrescente, pelo código do cliente. -}
adicionaCliente :: Clientes-> NomeCliente -> Clientes
adicionaCliente clientes nomeCliente | clientes == [] = (:) (1,nomeCliente,'A',0) clientes
                                     | otherwise = adicionaCliente ++ [(1,nomeCliente,'A',0)]
                                      where
                                        adicionaCliente = [(cod + 1,nome, categ,consumo) |(cod,nome, categ,consumo)<- clientes]
{- (b) Consulta os dados do cliente, informando seu código. Para simplificar,
considere que esta operação só tem o caso de sucesso, ou seja, o código
consultado sempre vai existir na lista de clientes. -}
coletaCliente :: Clientes -> CodCliente -> Cliente
coletaCliente clientes cod = head listaClientes
                          where
                            listaClientes = [i | i <- clientes, capturaCodClientes [i] == cod ]

---Funcao auxiliar
capturaCodClientes :: Clientes -> CodCliente
capturaCodClientes [(cod,_,_,_)] = cod

{- (c) Atualiza o consumo anual do cliente a cada compra, informando o código do
cliente e o valor da compra corrente, que será acrescido ao valor vigente do
consumo anual. Considere que o código do cliente está correto e existe na lista de
clientes. -}
type Compra = Int

atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumo clientes cod compra = listaClientes
                                    where
                                        cliente = head listaClientes
                                        listaClientes = [(codigo,nome,categoria,consumo + compra) | (codigo,nome,categoria,consumo) <- clientes, 
                                                         capturaCodClientes [(codigo,nome,categoria,consumo)] == cod]
---Funcao auxiliar
capturaConsumo :: Cliente -> ConsumoAnual
capturaConsumo (_,_,_,consumo) = consumo

{- (d) Atualiza a lista dos clientes a cada ano. Esta função atualiza as categorias de
todos os clientes da lista de acordo com o consumo anual acumulado no ano e
zera o consumo anual para o próximo ano. -}

{- Elabore uma função para receber uma nota de aluno e retornar o conceito (A, B,
C, D ou E) em que esta nota se enquadra. As notas serão fornecidas apenas com
uma casa decimal. Os conceitos seguem a seguinte tabela:
Nota Conceito
9 < nota <= 10,0 A
8< nota <= 9,0 B
7 < nota <= 8,0 C
6 < nota <= 7,0 D
nota <= 6,0 E -}
categoriaNota :: Float -> Char
categoriaNota nota | 9 < nota && nota <= 10  =  'A'
                   | 8 < nota && nota <= 9 = 'B'
                   | 7 < nota && nota <= 8 = 'C'
                   | 6 < nota && nota <= 7 = 'D'
                   | otherwise = 'E'
{- 2.Elabore uma função para receber as dimensões de um retângulo (base e altura) e
retornar a área do retângulo -}
areaRet :: Float -> Float -> Float
areaRet b h = b*h   
{- 3. Elabore uma função para receber o salário mensal atual de um funcionário e o
percentual de reajuste e retornar o valor do novo salário -}
novoSalario :: Float -> Float -> Float
novoSalario sal reajuste = sal * (reajuste/100)
{- 4. Elabore uma função para receber três notas de um aluno e retornar a média final
deste aluno. Considerar que a média é ponderada e que o peso das notas é 2, 3 e 5.
Fórmula para o cálculo da média final é:
𝑚𝑒𝑑𝑖𝑎𝐹𝑖𝑛𝑎𝑙 = (𝑛1 * 2 + 𝑛2 * 3 + 𝑛3 * 5) ÷ 10 -}
mediaFinal :: Float -> Float -> Float -> Float 
mediaFinal n1 n2 n3 =  media/10
                    where 
                        media = ((n1*2) + (n2*3) + (n3*5))
{- 5. As maçãs custam R$ 1,30 cada se forem compradas menos de uma dúzia, e R$
1,00 se forem compradas pelo menos 12. Escreva uma função para receber o
número de maçãs compradas e retornar o custo total da compra. -}
custoCompra :: Int -> Float
custoCompra x |x < 12 = fromIntegral x * (130/100)
              |x >= 12 = fromIntegral x
{- 6. Elabore uma função para receber dois valores e retornar uma das três strings a seguir:
        ‘Números iguais’, caso os números sejam iguais
        ‘Primeiro é maior’, caso o primeiro seja maior que o segundo;
        ‘Segundo maior’, caso o segundo seja maior que o primeiro. -}    
ehIgual :: Int -> Int -> String
ehIgual x y | x == y = "Numeros iguais "
            | x > y  = "Primeiro eh maior"
            |otherwise = "Segundo maior"
{- 7. Faça uma função para receber um número e retornar o dia correspondente da
semana. (1-Domingo, 2- Segunda, etc.), se digitar outro valor deve retornar valor
“inválido”. -}
diaSemana :: Int -> String
diaSemana x |x == 1 = "Domingo"
            |x == 2 = "Segunda"
            |x == 3 = "Terca"
            |x == 4 = "Quarta"
            |x == 5 = "Quinta"
            |x == 6 = "Sexta"
            |x == 7 = "Sabado"
            |otherwise = "invalido"
{- 8. Faça uma função para receber a temperatura em graus Fahrenheit e retornar a
temperatura em graus Celsius.
C = ((F-32) / 1,8). -}
grausCelsius :: Float -> Float
grausCelsius x = (x-32)/1.8
{- 9. Tendo como dado de entrada a altura (h) e o sexo (‘M’ ou ‘F’) de uma pessoa,
construa uma função que retorna seu peso ideal, utilizando as seguintes fórmulas:
1. Para homens: (72.7*h) - 58
2. Para mulheres: (62.1*h) - 44.7 -}
pesoIdeal :: Float -> Char -> Float
pesoIdeal h sexo |(sexo == 'M' || sexo == 'm') = formulaHomem
                 |(sexo == 'F' || sexo == 'f') = formulaMulher
                 where
                    formulaHomem = (72.7*h)-58
                    formulaMulher = (62.1*h) - 44.7
{- 10. Faça uma função que receba 3 números reais e verifique se o primeiro é maior
que a soma dos outros dois -}
maiorQueSoma :: Int -> Int -> Int -> Bool
maiorQueSoma x y z | x > y+z = True
                   |otherwise = False
                
                

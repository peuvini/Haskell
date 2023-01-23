{- Elabore uma função temperatura que dado um mês do ano devolve a temperatura
esperada do mês de acordo com a tabela a seguir. Por exemplo, temperatura “Jan”
retornará “Quente”. Caso o usuário informe um mês inexistente, deve devolver “Mes
invalido”. -}
temperatura :: String -> String
temperatura x |x == "Nov" || x== "Dez" || x=="Jan" || x== "Fev" || x== "Mar" = "Quente"
              |x == "Abr" || x== "Mai" = "Chuvoso"
              |x == "Jun" || x== "Jul" || x=="Ago" = "Frio"
              |x == "Set" || x== "Out" = "Ameno"
              |otherwise = "Mes invalido"
{- Elabore uma função diferentesMinMax que dados quatro inteiros devolve quantos
elementos são distintos dos elementos mínimo e máximo. Sua função deve
obrigatoriamente usar as funções max e min como parte da solução. Por exemplo,
diferentesMinMax 2 7 1 7 retornará 1, pois somente o elemento 2 é distinto do
mínimo que é 1 e do máximo que é 7. Já diferentesMinMax 2 7 2 7 devolverá 0 pois
todos os elementos ou são iguais ao mínimo ou ao máximo. -}
diferentesMinMax :: Int -> Int -> Int -> Int -> Int
diferentesMinMax a b c d | (a == b && b == c && c == d) || (a == maximo && b == maximo && c == minimo && d == minimo) || (a == maximo && c == maximo && b == minimo && d == minimo) || (a == maximo && d == maximo && b == minimo && c == minimo) || (b == maximo && c == maximo && a == minimo && d == minimo) || (b == maximo && d == maximo && a == minimo && c == minimo) || (c == maximo && d == maximo && a == minimo && b == minimo)    = 0
                         | ((maximo /= minimo) && ((a/= maximo && a/=minimo && b/=maximo && b/=minimo) || (a/= maximo && a/=minimo && c/=maximo && c/=minimo) || (a/= maximo && a/=minimo && d/=maximo && d/=minimo) || (b/= maximo && b/=minimo && c/=maximo && c/=minimo) || (b/= maximo && b/=minimo && d/=maximo && d/=minimo) || (c/= maximo && c/=minimo && d/=maximo && d/=minimo) )) = 2                    
                         | otherwise = 1
                         where
                            maximo = max a (max b (max c d))
                            minimo = min a (min b (min c d))
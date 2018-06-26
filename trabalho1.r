## Alunos: Giovani Bossoni, Paulo Roberto Bauer

## trabalho funções R
## 1) ## exercicio problematico ainda

## 2)
## essa funcao resolve o excercio basta escolher os valor p e q
exer2a  <- function(p,q)
{
  Z = 1.65
  E = 0.02
  n <- ((Z*Z) * p*q)/((E)*(E))
  return(n)
  
}
## letra a
## utilize os valores p = 0.5 e q 0.5
func(0.5,0.5)
## letra b 
## utilize os valores p = 0.2 e q 0.8
func(0.2,0.8)

func(0.5,0.5)
## P(x <= 70)
## pnorm(q=70,mean=75,sd=5,lower.tail=T)


## calculando propabildiade de Z
##pnorm(Z< , mean=0, sd = 1)

## dnorm gera a probalidade em um x (apenas naquele ponto)
## pnorm retorna a integral ate a probabilidade ate o Z
## exemplo pnorm(0) -> 0.5
## lower.tail -> true = -inf ate p
## lower.tail -> false = inf ate p

## qnorm com a probabilidade acha o Z

## pode ser meio q usado com a tabela Z

## t.test
## fazer um desenho que representa o Z ali
## exercicio 3
exer3 <- function(qtdDados,media,variancia,confianca){
  # gerando os dados da normal
  dados <- rnorm(qtdDados,mean = media,sd = sqrt(variancia))
  Z <- qnorm(0.5 + (confianca/2))
  ## sqrt na variancia para encontrar o desvio padrao
  erro  <-  Z *(sqrt(variancia)/sqrt(qtdDados))
  maior <- media + erro
  menor <- media - erro
  return (c(menor,maior))
}
exer3(100,media = 100,variancia = 100,confianca = 0.95)



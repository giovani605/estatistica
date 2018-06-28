## Alunos: Giovani Bossoni, Paulo Roberto Bauer

## trabalho funções R
## 1) ## exercicio problematico ainda
exer1 <- function(favoraveis,total,confianca){
  Z <- qnorm(0.5 + (confianca/2))
  ## probabilidade de favoravel
  p <- favoraveis/total
  denominador = p*(1-p)
  
  erro  <-  Z *(sqrt(denominador/total))
  maior <- p + erro
  menor <- p - erro
  return (c(menor,maior)) 
}
exer1(180,380,0.90)


## 2)
## essa funcao resolve o excercio basta escolher os valor p e q
exer2  <- function(p,q)
{
  Z = 1.65
  E = 0.02
  n <- ((Z*Z) * p*q)/((E)*(E))
  return(n)
  
}
## letra a
## utilize os valores p = 0.5 e q 0.5
exer2(0.5,0.5)
## letra b 
## utilize os valores p = 0.2 e q 0.8
exer2(0.2,0.8)

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

## exercicio 4
exer4 <- function(erro,desvio,confianca){
  Z <- qnorm(0.5 + (confianca/2))
  n <- ((Z*desvio)/erro)^2
  return(n)
  
}
exer4(50,400,0.95)

## exercicio 5
exer5 <- function(vetor1,vetor2,n,significancia){
  ## vetor1 < vetor2
  vetordif = vetor1 - vetor2;
  vetordifQuadrada = (vetor1 - vetor2)^2
  sumDif <- sum(vetordif)
  sumDifQuadrada <- sum(vetordifQuadrada)
  mediaDiff <- sumDif / n
  Sd <- sqrt( ( sumDifQuadrada - ((sumDif^2)/n) ) / ( n-1) )
  uD = 0;
  Tobs = (mediaDiff - uD) / (Sd/sqrt(n) )
  t.test(x =vetor1,y =vetor2,conf.level = significancia,alternative = "less" )
}
exer5(vetorY,vetorX,5,0.1)

## exer 6
vetorAntes <- c(635,704,662,560,603,745,698,575,633,669)
vetorDepois <- c(640,712,681,558,610,740,707,585,635,682)
exer6 <- function(vetor1,vetor2,n,significancia){
  ## vetor1 < vetor2
  t.test(x =vetor1,y =vetor2,conf.level = significancia)
}
exer6(vetorAntes,vetorDepois,10,0.01)



## exercicio 7
X <- c(2,2,2,4,4,4,6,6,6,8,8,8,10,10,10)
Y <- c(2.1,1.8,1.9,4.5,4.2,4,6.2,6,6.5,8.2,7.8,7.7,9.6,10,10.1)
mod <- lm(Y ~ X)
coef(mod)

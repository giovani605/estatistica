## Alunos: Giovani Bossoni, Paulo

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

## t.test

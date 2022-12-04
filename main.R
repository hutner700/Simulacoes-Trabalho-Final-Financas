library(ggplot2)
library(ggpubr)
library(FinancialMath)
library(tidyverse)
install.packages("tidyverse")
library(rootSolve)

# Entradas
cap0 <- 80000
alunos0 <- 551770.00
aluguelSalas0 <- 173390.00
entradas0 <- alunos0 + aluguelSalas0 + cap0
#Saídas
custoOp0	 <- 85505.43
pessoal0	 <- 240763.68
despeOp0	 <- 28398.92
imposto0     <- 81217.92
invInicial0 <- 1072214.00
totalSaidas0	<- custoOp0 + pessoal0 + despeOp0 + imposto0 + invInicial0
fluxo0 <- entradas0 - totalSaidas0


# Simulacoes:
final <- matrix(ncol = 5)
for (j in 1:5000){ alunos <- c(alunos0)
  aluguel <- c(aluguelSalas0)
  custoOp <- c(custoOp0)
  pessoal <- c(pessoal0)
  despeOp <- c(despeOp0)
  imposto <- c(imposto0)
  for (i in 1:3) {
    # Entrada
    siAlunos <- rnorm(1, 0.05348, 0.022434)
    siAluguel <- rnorm(1, 0.066395086, 0.025874353)
    alunos <- c(alunos, tail(alunos, 1) * (1 + siAlunos))
    aluguel <- c(aluguel, tail(aluguel, 1) * (1 + siAluguel))
    # Saida
    siCustoOp <- rnorm(1, 0.0361, 0.07106617)
    siPessoal <- rnorm(1, 0.0361, 0.07106617)
    siDespeOp <- rnorm(1, 0.0361, 0.07106617)
    siImposto <- rnorm(1, 0.05348, 0.022434)
    custoOp <- c(custoOp, tail(custoOp, 1) * (1 + siCustoOp))
    pessoal <- c(pessoal, tail(pessoal, 1) * (1 + siPessoal))
    despeOp <- c(despeOp, tail(despeOp, 1) * (1 + siDespeOp))
    imposto <- c(imposto, tail(imposto, 1) * (1 + siImposto))
  }
  fluxoCaixa <- c(fluxo0,alunos + aluguel - (custoOp + pessoal + despeOp + imposto))
  final <- rbind(final, fluxoCaixa)
}
final <- final[-1,]
vpls <- c()
taxa <- 0.10
VPL <- function(vector,i){
  valor <- 0
  vezes <- 0

  for (x in vector){
    valor <- valor + (x/(1+i)^vezes)
    vezes <- vezes + 1
  }
  return(valor)
}

for (i in 1:5000){
  vpls <- c(vpls, VPL(c(final[i,1] - invInicial0,final[i,2:dim(final)[2]]),i=taxa))
}

TIR <- function(vector,min,max,step){
  testes <- seq(min,max,step)
  menordif <- Inf
  for (teste in testes){
    valor <- VPL(vector = vector, i = teste)
    print(paste("Valor:", valor))
    print(paste("Menor Diferenca:", menordif))
    if (abs(valor) < menordif){
      menordif <- abs(valor)
    } else if(valor == menordif) {
      NA
    }
    else {
      return(testes[match(teste,testes)-1])
    }
  }
}

TIR(final[1,],min=-1,max=1,0.0001)
VPL(vector = vector, i = teste)


RAlunos <- rnorm(1000, 0.05348, 0.022434)
RAluguel <- rnorm(1000, 0.066395086, 0.025874353)
CCustoOp <- rnorm(1000, 0.0361, 0.07106617)
CPessoal <- rnorm(1000, 0.0361, 0.07106617)
DDespeOp <- rnorm(1000, 0.0361, 0.07106617)
Imposto <- rnorm(1000, 0.05348, 0.022434)

summary(cbind(RAlunos, RAluguel, CCustoOp, CPessoal, DDespeOp, Imposto ))

RAlunos  media = 0.05348, desvio = 0.022434
RAluguel  media = 0.066395086, desvio = 0.025874353
CCustoOp  media = 0.0361, desvio = 0.07106617
CPessoal  media = 0.0361, desvio = 0.07106617
DDespeOp  media = 0.0361, desvio = 0.07106617
Imposto  media = 0.05348, desvio = 0.022434

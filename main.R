library(ggplot2)
library(ggpubr)
library(FinancialMath)
library(tidyverse)
install.packages("tidyverse")
library(rootSolve)

VPL <- function(vector,i){
  valor <- 0
  vezes <- 0

  for (x in vector){
    valor <- valor + (x/(1+i)^vezes)
    vezes <- vezes + 1
  }
  return(valor)
}

TIR <- function(vector,min,max,step){
  testes <- seq(min,max,step)
  menordif <- Inf
  for (teste in testes){
    valor <- VPL(vector = vector, i = teste)
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


final <- data.frame()
final <- rbind(final,c(0,0,0,0,0,0,0,0))
colnames(final) <- c("simulacao","alunos","aluguel","custo","pessoal",
                     "despesas","imposto","fluxoCaixa","fluxoAcumulado","ano")
# Simulacoes:
set.seed(123)
for (j in 1:5000){
  alunos <- c(alunos0)
  aluguel <- c(aluguelSalas0)
  custoOp <- c(custoOp0)
  pessoal <- c(pessoal0)
  despeOp <- c(despeOp0)
  imposto <- c(imposto0)
  fluxo <- c(fluxo0)
  fluxoAcumulado <- c(fluxo0)
  for (i in 1:3) {
    # Entrada
    siAlunos <- rnorm(1, 0.05, 0.03)
    siAluguel <- rnorm(1, 0.06, 0.02)
    alunos <- c(alunos, tail(alunos, 1) * (1 + siAlunos))
    aluguel <- c(aluguel, tail(aluguel, 1) * (1 + siAluguel))
    # Saida
    siCustoOp <- rnorm(1, 0.03, 0.01)
    siPessoal <- rnorm(1, 0.03, 0.01)
    siDespeOp <- rnorm(1, 0.03, 0.01)
    siImposto <- (tail(alunos, 1) + tail(aluguel, 1)) * 0.04
    custoOp <- c(custoOp, tail(custoOp, 1) * (1 + siCustoOp))
    pessoal <- c(pessoal, tail(pessoal, 1) * (1 + siPessoal))
    despeOp <- c(despeOp, tail(despeOp, 1) * (1 + siDespeOp))
    imposto <- c(imposto, siImposto)
    fluxo <- c(fluxo,(
      tail(alunos, 1) +
      tail(aluguel, 1)
      - (tail(custoOp,1)+
         tail(pessoal,1)+
         tail(pessoal,1)+
         tail(pessoal,1)+
         tail(imposto,1))
      ))
    fluxoAcumulado <- c(fluxoAcumulado,sum(fluxo))
  }
  temp <- data.frame(
    cbind(
      c(rep(j,length(aluguel))),
      alunos,
      aluguel,
      custoOp,
      pessoal,
      despeOp,
      imposto,
      fluxo,
      fluxoAcumulado,
      seq_along(aluguel)-1
    ))
  colnames(temp) <- c("simulacao","alunos","aluguel","custo","pessoal",
                     "despesas","imposto","fluxoCaixa","fluxoAcumulado","ano")
  final <- rbind(final,temp)
}
final <- final[-1,]
tail(final)

vpls <- c()
taxa <- 0.10
for (i in 1:5000){
  vpls <- c(vpls, VPL(c(final[i,1] - invInicial0,final[i,2:dim(final)[2]]),i=taxa))
}

hist(vpls)

sum(vpls>0) / length(vpls)

TIR(final[1,],min=-1,max=1,0.0001)

rbind(matrix(c(1,2,3,4,5),ncol = 5),matrix(c(1,2,3,4,5),ncol = 5))

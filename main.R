library(ggplot2)
library(ggpubr)
library(FinancialMath)
library(tidyverse)
library(rootSolve)
library(stargazer)

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
colunas <-  c("simulacao","alunos","aluguel","custo","pessoal",
                     "despesas","imposto","fluxoCaixa","fluxoAcumulado","ano")
final <- rbind(final,c(rep(0,length(colunas))))
colnames(final) <- colunas
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
  for (i in 1:4) {
    # Entrada
    siAlunos <- rnorm(1, 0.08, 0.03)
    siAluguel <- rnorm(1, 0.00, 0.03)
    alunos <- c(alunos, tail(alunos, 1) * (1 + siAlunos))
    aluguel <- c(aluguel, tail(aluguel, 1) * (1 + siAluguel))
    # Saida
    siCustoOp <- rnorm(1, 0.10, 0.01)
    siPessoal <- rnorm(1, 0.10, 0.01)
    siDespeOp <- rnorm(1, 0.10, 0.01)
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
         tail(despeOp,1)+
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
  colnames(temp) <- colunas
  final <- rbind(final,temp)
}
final <- final[-1,]
write.table(final,file="Simulacoes.csv",sep=';',dec=',', row.names = FALSE)

resultado_simulacoes <- matrix(ncol=4)
colnames(resultado_simulacoes) <- c("tir","VPL","payBack","roi")
options(scipen = 100)
for (i in unique(final[,'simulacao'])){
  amostra <- final[final[,'simulacao'] == i,]
  tir <- TIR(amostra[,'fluxoCaixa'],0,2,0.01)
  VPLa <- VPL(amostra[,'fluxoCaixa'],0.15)
  payBack <- amostra[amostra[,'fluxoAcumulado'] > 0,"ano"][1]
  roi <- (tail(amostra[,'fluxoAcumulado'],1) + invInicial0)/ invInicial0
  resultado_simulacoes <- rbind(resultado_simulacoes,c(tir,VPLa,payBack,roi))
}
resultado_simulacoes <- resultado_simulacoes[-1,]

mean(resultado_simulacoes[,'VPL'])
sd(resultado_simulacoes[,'VPL'])
hist(resultado_simulacoes[,'VPL'],breaks = 20)
hist(na.omit(resultado_simulacoes[,'roi']))
hist(resultado_simulacoes[,'tir'],breaks = 20)
hist(resultado_simulacoes[,'payBack'],breaks = 20)

summary(resultado_simulacoes)
nomecolunas <- c("Media","Desvio Padrao")
siAlunos <- c(0.08, 0.03)
siAluguel <- c(0.00, 0.03)
siCustoOp <- c(0.10, 0.01)
siPessoal <- c(0.10, 0.01)
siDespeOp <- c(0.10, 0.01)
nomelinhas <- c("Alunos", "Aluguel", "CustoOp", "Pessoal", "DespeOp")
status <- matrix(rbind(siAlunos, siAluguel, siCustoOp, siPessoal, siDespeOp),ncol = 2)
rownames(status) <- nomelinhas
colnames(status) <- nomecolunas
stargazer(status,type="text")

mean(resultado_simulacoes[,'roi'])
sd(resultado_simulacoes[,'roi'])
sum(resultado_simulacoes[,'payBack'] == 2) / sum(resultado_simulacoes[,'payBack'] > 0)
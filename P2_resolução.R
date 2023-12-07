# PROVA 2 DE PROGRAMAÇÃO APLICADA A ESTATÍSTICA
library(tidyverse)

# Questão 1 ----------------------------------



Var_DMA_AS2 <- function(dados){
  
  var_amostral <- function(dados){
    soma = 0
    for(i in 1:length(dados)){ 
      diferenca = (dados[i] - mean(dados))^2
      soma = soma+diferenca
    }
    resultado = (1/(length(dados)- 1))*soma
    return(resultado)
  }
  
  DMA <- function(dados){
    soma = 0
    for(i in 1:length(dados)){ 
      diferenca = abs(dados[i] - mean(dados))
      soma = soma+diferenca
    }
    resultado = (1/(length(dados)- 1))*soma
    return(resultado)
  }
  
  AS2 <- function(dados){
    resultado = 3*((mean(dados)-median(dados))/sqrt(var_amostral(dados)))
  }
  
  R1 = var_amostral(dados)
  R2 = DMA(dados)
  R3 = AS2(dados)
  return(list(R1, R2, R3))
}

# Alternativa A - Falsa

dados_teste<- airquality

dados_teste <- na.omit(airquality)

var_amostral(dados_teste$Ozone)

Var_DMA_AS2(dados_teste$Ozone)

# Alternativa B - Verdadeira

filtrados <- airquality%>%
  filter(Month ==8)
Var_DMA_AS2(filtrados$Wind)

# Alternativa C  - Verdadeira
vars <- list(colnames(dados_teste))

lapply(dados_teste$vars, Var_DMA_AS2)

comparacoes <- list()

for(i in vars[[1]]){
  print(dados_teste[[i]])
  var <- i
  resultado <- Var_DMA_AS2(dados_teste[[i]])
  #print(resultado)
  comparacoes <- append(comparacoes,var)
  comparacoes <- append(comparacoes,resultado)
  print(comparacoes)
}


# Resolvendo em outro formato
comparacoes <- data.frame("variavel" = colnames(dados_teste))

for(i in vars[[1]]){
  resultado <- Var_DMA_AS2(dados_teste[[i]])
  for( j in 1:nrow(comparacoes)){
    if(i == comparacoes$variavel[j]){
    print(j)
    comparacoes$var_amostral[j] <- resultado[[1]]
    comparacoes$DMA[j] <- resultado[[2]]
    comparacoes$comp[j] <- ifelse(comparacoes$var_amostral[j] >
                                    comparacoes$DMA[j], "maior") 
  }}
}

# Alternativa D - Falsa

comparacoes[1,3]

# Alternativa E - Falso

filtrados <- dados_teste%>%
  filter(Month == 8)
Var_DMA_AS2(filtrados$Wind)
# Questão 2---------------------------------------------------------------------

varreMatriz <- function(matriz){
  primo <- function(n) {
    divisores <- seq(2, n - 1)
    resultado <- "primo"  
    for (divisor in divisores) {
      if (n %% divisor == 0) {
        resultado <- "não primo"   
        break  
      }
    }
    return(resultado)
  }
  quadradoP <- function(n){
    decimal <- round(sqrt(n)) - sqrt(n)
    if(decimal !=0){
      resultado = "não é quadrado perfeito"
    }else{
      resultado = "é quadrado perfeito"
    }
    return(resultado)
  }
  nprimos = 0
  for(i in 1:nrow(matriz)){
    for(j in 1:ncol(matriz)){
      if(primo(matriz[i,j])== "primo" | matriz[i,j] == 2 ){
        nprimos = nprimos+1
        matriz[i,j] <-  matriz[i,j]*8 } 
      else if(matriz[i,j]<0){
          matriz[i,j] <- matriz[i,j]^1/3 }
        else if(quadradoP(abs(matriz[i,j])) == "é quadrado perfeito"){
          alteracao <-  matriz[i,j]-19
          if(alteracao <=0){
            alteracao <- alteracao^7 }
          else{
            matriz[i,j] <- matriz[i,j]
          }
      }
    }
  }
  RESUTADOFINAL = list()
  RESUTADOFINAL <- append(RESUTADOFINAL, matriz)
  RESUTADOFINAL <- append(RESUTADOFINAL, nprimos)
  return(matriz)
}

# Alternativa A - Falsa
A <- matrix(c(3,-3,6,-8,1,2,-10,-7,8,-2,-9,-4,10,-5,-1,7), ncol = 4, nrow = 4,
            byrow = T)
A
varreMatriz(A)
A[1,2]^(1/3)

# Alternativa B - Falsa
B <- matrix(c(-1,8,13,-16,0,16,18,-6,-18,-15,-2,-11,11,-12,-13,3,-4,5,-19,17),
            ncol = 4, nrow = 5, byrow = T)


sum(diag(varreMatriz(B)))

# Alternativa C - Falsa

C <- matrix(c(-22,14,21,19,23,-20,-17,17,-7,-11,-23,28,24,-14,-5,8), byrow =T,
            nrow = 4, ncol = 4)

varreMatriz(C)

# Alternativa D - Verdadeira
max(abs(varreMatriz(C)))

# Alternativa E - 
sum(varreMatriz(B)[,2])


# Questão 3 --------------------------------------------------------------------
preparacao <- function(dados){
  separados <- dados%>%
    str_split(.,"", simplify = T)
  separados <- as.data.frame(separados)
  dados <- cbind(dados, separados)
  
  nomes <- c("data", "n1", "n2","carac1", "n3", "n4",
             "carac2", "n5", "n6", "n7", "n8")
  colnames(dados) <- nomes
  
  dados$n1 <- as.numeric(dados$n1)
  dados$n2 <- as.numeric(dados$n2)
  dados$n3 <- as.numeric(dados$n3)
  dados$n4 <- as.numeric(dados$n4)
  dados$n5 <- as.numeric(dados$n5)
  dados$n6 <- as.numeric(dados$n6)
  dados$n7 <- as.numeric(dados$n7)
  dados$n8 <- as.numeric(dados$n8)
  
  dados <- dados%>%
    select(-c(carac1,carac2))
  
  dados <- dados%>%
    mutate(soma = rowSums(across(where(is.numeric))))
  dados$soma <- as.character(dados$soma)
  
  separados2 <- dados$soma%>%
    str_split(.,"", simplify = T)
  separados2 <- as.data.frame(separados2)
  dados <- cbind(dados, separados2)
  
  nomes <- c("data", "n1", "n2", "n3", "n4", "n5", "n6",
             "n7", "n8","soma", "n9", "n10")
  colnames(dados) <- nomes
  
  dados$n9 <- as.numeric(dados$n9)
  dados$n10 <- as.numeric(dados$n10)
  
  dados <- dados%>%
    mutate(soma_final = rowSums(across(c("n9", "n10"))))
  
  return(dados)
}

#TESTE
dados <- data.frame("Data" = "12/12/1965")
preparacao(dados)


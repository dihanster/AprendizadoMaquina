###############################################################################################################
#Aprendizado de Maquina e Reconhecimento de Padroes
#Nome: Willian Dihanster Gomes de Oliveira RA: 112269
#
#Exercício: Algoritmo Vizinhos mais Próximos
#
#Considere o sistema computacional para indicar a que clientes uma promoção deve ser
#mandada, de maneira a ter uma melhor aderência (gerar mais compras). Usando a
#base de dados histórica no arquivo Conj_dados_exercício.xls, faça o seguinte exercício:
###############################################################################################################

#Leitura da Planilhas
dados = read.csv("C:/Users/willi/Desktop/Conj_dados_exercício.csv")
dados

#a) Retire o atributo ID, que não contribui para a predição.
dados = dados[2:6]
dados

#b) Converta os atributos qualitativos em quantitativos
levels(dados[,3]) = c(0,1)  #0 = Grande, 1 = Pequena
levels(dados[,4]) = c(0,1)  #0 = Nao, 1 = Sim
levels(dados[,5]) = c(0,1)  #0 = Nao, 1 = Sim
dados

#c) Normalize os atributos quantitativos (os que forem cadeias de bits não
#precisam ser normalizados, só os que não estiverem entre 0 e 1).
max1 = max(dados[1:1])
min1 = min(dados[1:1])
max2 = max(dados[2:2])
min2 = min(dados[2:2])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
  }

dados[1:2] <- as.data.frame(lapply(dados[1:2], normalize))
dados

#d) Usando esse conjunto de dados e a medida de distância Euclideana, classifique
#os seguintes potenciais novos clientes, usando um vizinho mais próximo:

knn <- function(dataset, query, k = 1){
  query 
  idClass <- ncol(dataset)
  
  Eucl_dist <- apply(dataset,1,function(row){ 
    sqrt(sum((query-as.numeric(row[1:idClass-1]))^2))
  })
  ids <- sort.list(Eucl_dist,dec=F)[1:k]
  labels <- dataset[ids,idClass]
  
  ret <- list()
  ret$nearest <- ids
  
  if(!is.numeric(dataset[,idClass])){
    # classification problem
    U <- unique(labels)
    R <- rep(0,length(U))
    for (i in 1:length(U)){
      R[i] <- sum(U[i] == labels)
    }
    idx <- which.max(R)
    
    ret$voted <- U
    ret$Nvotes <- R
    ret$pred <- U[idx]
  }
  else{
    ret$pred <- mean(labels)
  }
  
  return(ret)
}

#d.1) Maria tem 55 anos, um rendimento de 9500 reais e uma familia pequena.
#Alem disso, ja comprou outros produtos da empresa anteriormente
Maria = c(9500, 55, 2, 1)

Maria[1] = (Maria[1] - min1) / (max1 - min1)
Maria[2] = (Maria[2] - min2) / (max2 - min2)

knn(dados, Maria, k = 1)

#d.2) Joao é um jovem de 23 anos com rendimento de 900 reais e familia
#pequena. Ele ja comprou produtos da empresa
Joao = c(900, 23, 4, 1)

Joao[1] = (Joao[1] - min1) / (max1 - min1)
Joao[2] = (Joao[2] - min2) / (max2 - min2)

knn(dados, Joao, k = 1)

#e) Classifique os clientes anteriores usando três vizinhos mais próximos.
#Usando k = 3 para os clientes anteriores
knn(dados, Maria, k = 3)

knn(dados, Joao, k = 3)

###############################################################################################################
#Aprendizado de Maquina e Reconhecimento de Padroes
#Nome: Willian Dihanster Gomes de Oliveira RA: 112269
#
#Exercício: SVM - Support Vector Machines
#
#Considere o sistema computacional para indicar a que clientes uma promoção deve ser
#mandada, de maneira a ter uma melhor aderência (gerar mais compras). Usando a
#base de dados histórica no arquivo Conj_dados_exercício.xls (mesma do k-vizinhos
#mais próximos), faça o seguinte:

#Biliotecas
library("e1071")

#Leitura do Conjunto de Dados
dados = read.csv("C:/Users/willi/Desktop/Conj_dados_exercício.csv")
dados

#a) Remova o atributo de identificação
dados = dados[2:6]
dados

#b) Converta os atributos qualitativos em quantitativos
dados$Tamanho_família <- as.numeric(dados$Tamanho_família)-1
dados$Comprou_antes <- as.numeric(dados$Comprou_antes)-1
dados$Comprou_anunciado <- as.numeric(dados$Comprou_anunciado)-1
dados

#c) Treine uma SVM, com parâmetros default (se o código usado não tiver a normalização, 
#ela deve ser feita antes)
max1 = max(dados[1:1])
min1 = min(dados[1:1])
max2 = max(dados[2:2])
min2 = min(dados[2:2])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

dados[1:2] <- as.data.frame(lapply(dados[1:2], normalize))
dados

#Treinamento
model <- svm(Comprou_anunciado ~ ., data = dados)
print(model)
summary(model)

#d) Em seguida, usando a SVM obtida, classifique os seguintes potenciais novos
#clientes com essa SVM:
  #d.1) Maria tem 55 anos, um rendimento de 9500 reais e uma família pequena.
#Além disso, já comprou outros produtos da empresa anteriormente.
Maria = data.frame(9500, 55, 1, 1)
colnames(Maria) <- list("Rendimento", "Idade", "Tamanho_família", "Comprou_antes")
Maria[1] = (Maria[1] - min1) / (max1 - min1)
Maria[2] = (Maria[2] - min2) / (max2 - min2)
Maria

pred = predict(model, Maria)
pred
  #d.2) João é um jovem de 23 anos com rendimento de 900 reais e família
#pequena. Ele já comprou produtos da empresa.
Joao = data.frame(900, 23, 1, 1)
colnames(Joao) <- list("Rendimento", "Idade", "Tamanho_família", "Comprou_antes")
Joao[1] = (Joao[1] - min1) / (max1 - min1)
Joao[2] = (Joao[2] - min2) / (max2 - min2)
Joao

pred <- predict(model, Joao)
pred

#e) Altere o kernel para linear e refaça o exercício.
model2 <- svm(Comprou_anunciado ~ ., data = dados, kernel = "linear", cost = 1)
print(model2)
summary(model2)
#SVM Linear para Maria
pred = predict(model2, Maria)
pred
#SVM Linear para Joao
pred = predict(model2, Joao)
pred

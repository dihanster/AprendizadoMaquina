###############################################################################################################
#Aprendizado de Maquina e Reconhecimento de Padroes
#Nome: Willian Dihanster Gomes de Oliveira RA: 112269
#
#Exerc�cio: SVM - Support Vector Machines
#
#Considere o sistema computacional para indicar a que clientes uma promo��o deve ser
#mandada, de maneira a ter uma melhor ader�ncia (gerar mais compras). Usando a
#base de dados hist�rica no arquivo Conj_dados_exerc�cio.xls (mesma do k-vizinhos
#mais pr�ximos), fa�a o seguinte:

#Biliotecas
library("e1071")

#Leitura do Conjunto de Dados
dados = read.csv("C:/Users/willi/Desktop/Conj_dados_exerc�cio.csv")
dados

#a) Remova o atributo de identifica��o
dados = dados[2:6]
dados

#b) Converta os atributos qualitativos em quantitativos
dados$Tamanho_fam�lia <- as.numeric(dados$Tamanho_fam�lia)-1
dados$Comprou_antes <- as.numeric(dados$Comprou_antes)-1
dados$Comprou_anunciado <- as.numeric(dados$Comprou_anunciado)-1
dados

#c) Treine uma SVM, com par�metros default (se o c�digo usado n�o tiver a normaliza��o, 
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
  #d.1) Maria tem 55 anos, um rendimento de 9500 reais e uma fam�lia pequena.
#Al�m disso, j� comprou outros produtos da empresa anteriormente.
Maria = data.frame(9500, 55, 1, 1)
colnames(Maria) <- list("Rendimento", "Idade", "Tamanho_fam�lia", "Comprou_antes")
Maria[1] = (Maria[1] - min1) / (max1 - min1)
Maria[2] = (Maria[2] - min2) / (max2 - min2)
Maria

pred = predict(model, Maria)
pred
  #d.2) Jo�o � um jovem de 23 anos com rendimento de 900 reais e fam�lia
#pequena. Ele j� comprou produtos da empresa.
Joao = data.frame(900, 23, 1, 1)
colnames(Joao) <- list("Rendimento", "Idade", "Tamanho_fam�lia", "Comprou_antes")
Joao[1] = (Joao[1] - min1) / (max1 - min1)
Joao[2] = (Joao[2] - min2) / (max2 - min2)
Joao

pred <- predict(model, Joao)
pred

#e) Altere o kernel para linear e refa�a o exerc�cio.
model2 <- svm(Comprou_anunciado ~ ., data = dados, kernel = "linear", cost = 1)
print(model2)
summary(model2)
#SVM Linear para Maria
pred = predict(model2, Maria)
pred
#SVM Linear para Joao
pred = predict(model2, Joao)
pred

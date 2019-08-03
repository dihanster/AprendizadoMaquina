###############################################################################################################
#Aprendizado de Maquina e Reconhecimento de Padroes
#Nome: Willian Dihanster Gomes de Oliveira RA: 112269
#
#Exerc�cio: Redes Neurais
#
#Considere o sistema computacional para indicar a que clientes uma promo��o deve ser
#mandada, de maneira a ter uma melhor ader�ncia (gerar mais compras). Usando a
#base de dados hist�rica no arquivo Conj_dados_exerc�cio.xls (mesma do k-vizinhos
#mais pr�ximos), fa�a o seguinte:
###############################################################################################################

#Leitura da Planilhas
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

#c) Normalize os atributos num�ricos (bin�rios n�o precisam ser normalizados)
max1 = max(dados[1:1])
min1 = min(dados[1:1])
max2 = max(dados[2:2])
min2 = min(dados[2:2])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

dados[1:2] <- as.data.frame(lapply(dados[1:2], normalize))
dados

#d) Treine uma rede Perceptron com ?? = 0,3
library("neuralnet")
atrib <- Comprou_anunciado~ Rendimento + Idade + Tamanho_fam�lia + Comprou_antes 
redeNeural <- neuralnet(atrib, dados, hidden = 2, threshold = 0.3)
plot(redeNeural)

#e) Em seguida, usando os pesos obtidos, classifique os seguintes potenciais novos
#clientes com essa rede neural:
  #e.1) Maria tem 55 anos, um rendimento de 9500 reais e uma fam�lia pequena.
#Al�m disso, j� comprou outros produtos da empresa anteriormente.
Maria = data.frame(9500, 55, 2, 1)
Maria[1] = (Maria[1] - min1) / (max1 - min1)
Maria[2] = (Maria[2] - min2) / (max2 - min2)
Maria

predicao <- compute(redeNeural, Maria)
predicao

  #e.2) Jo�o � um jovem de 23 anos com rendimento de 900 reais e fam�lia
#pequena. Ele j� comprou produtos da empresa.
Joao = data.frame(900, 23, 4, 1)
Joao[1] = (Joao[1] - min1) / (max1 - min1)
Joao[2] = (Joao[2] - min2) / (max2 - min2)
Joao

predicao <- compute(redeNeural, Joao)
predicao

#f) Refa�a o exerc�cio com uma MLP, variando o n�mero de neur�nios na camada
#intermedi�ria com os valores 2 e 5.
library(RSNNS)
dadosTreino <- subset(dados, select = -Comprou_anunciado)
rede2 <- mlp(dadosTreino, dados$Comprou_anunciado, size=2, learnFuncParams=c(0.1), 
               maxit=100)
rede5 <- mlp(dadosTreino, dados$Comprou_anunciado, size=5, learnFuncParams=c(0.1), 
               maxit=100)
summary(rede2)
summary(rede5)

#f.1) Maria tem 55 anos, um rendimento de 9500 reais e uma fam�lia pequena.
#Al�m disso, j� comprou outros produtos da empresa anteriormente.
predRede2 <- predict(rede2, Maria)
predRede2

predRede5 <- predict(rede5, Maria)
predRede5

#f.2) Jo�o � um jovem de 23 anos com rendimento de 900 reais e fam�lia
#pequena. Ele j� comprou produtos da empresa.
predRede2 <- predict(rede2, Joao)
predRede2

predRede5 <- predict(rede5, Joao)
predRede5

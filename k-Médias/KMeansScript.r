####################################################################################################
#Aprendizado de Maquina e Reconhecimento de Padroes                                             ####
#Nome: Willian Dihanster Gomes de Oliveira RA: 112269                                           ####
#                                                                                               ####
#Exercício: k-Means                                                                             ####
#                                                                                               ####
#mandada, de maneira a ter uma melhor aderência (gerar mais compras). Usando a                  ####
#base de dados histórica no arquivo Conj_dados_exercício.xls (mesma do k-vizinhos               ####
#mais próximos), faça o seguinte:                                                               ####
####################################################################################################

#Leitura do Conjunto de Dados
dados = read.csv("C:/Users/willi/Desktop/Conj_dados_exercício.csv")
dadosBackup = read.csv("C:/Users/willi/Desktop/Conj_dados_exercício.csv")
dados

#a) Codifique os atributos qualitativos como quantitativos. Lembre-se de não usar o
dados = dados[2:5]
dados$Tamanho_família <- as.numeric(dados$Tamanho_família)-1
dados$Comprou_antes <- as.numeric(dados$Comprou_antes)-1
dados$Comprou_anunciado <- as.numeric(dados$Comprou_anunciado)-1
dados

#b) Normalize os atributos numéricos
max1 = max(dados[1:1])
min1 = min(dados[1:1])
max2 = max(dados[2:2])
min2 = min(dados[2:2])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

dados[1:2] <- as.data.frame(lapply(dados[1:2], normalize))
dados
#c) Use o algoritmo k-médias para agrupar os dados, selecionando aleatoriamente
#dois pontos do conjunto como centros.
dadosCluster <- kmeans(dados, centers = 2, nstart = 2) 
dadosCluster

plot(dados, col = dadosCluster$cluster)
points(dadosCluster$centers[,3:4], col = "blue", pch = 8, cex = 2)
table(dadosCluster$cluster, dadosBackup$Comprou_anunciado)

#d) Em seguida avalie se os grupos gerados possuem alguma interpretação, fazendo
#uso da classificação conhecida usada nas outras atividades.
#Sim, baseado nos resultados obtidos anteriormente e a base original, os resultados obtidos fazem
#sentido. Pois o algoritmo K-Means agrupou a maioria das instâncias da classe 1 como sendo do 
#grupo 2 e os da classe 0 como sendo do grupo 1, formando dois grupos, que são análogos às 
#classses (embora tenha sido dada como parametro 2 centros).
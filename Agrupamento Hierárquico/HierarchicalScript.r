##########################################################################################
#Aprendizado de Maquina e Reconhecimento de Padroes - Profa Dra. Ana Carolina Lorena  ####
#Nome: Willian Dihanster Gomes de Oliveira RA: 112269                                 ####
#                                                                                     ####
#Exercício: Agrupamento Hierarquico                                                   ####
##########################################################################################

#Considere o sistema computacional para indicar a que clientes uma promoção deve ser
#mandada, de maneira a ter uma melhor aderência (gerar mais compras). Usando a
#base de dados histórica no arquivo Conj_dados_exercício.xls, faça o seguinte:


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

# c) Use o algoritmo de agrupamento hierárquico para agrupar os dados, variando o
# método de ligação entre single, average e complete.
res <- hclust(dist(dados),method="single")
#res <- hclust(dist(dados),method="average") 
#res <- hclust(dist(dados),method="complete") 
plot(res, hang = -1)

# d) Fazendo um corte com dois grupos, avalie se os grupos gerados pelos diferentes
# métodos possuem alguma interpretação, fazendo uso da classificação
# conhecida usada nas outras atividades.
clusterCut <- cutree(res, 2)
rect.hclust(res, k=2, border="red")
table(dadosBackup$Comprou_anunciado,clusterCut)

#O grupo 1 ficou com os elementos da classe de pessoas que nao comprararam anunciado e grupo 2
#compraram (a maioria)
#Para todas varicoes do metodo de ligacao os agrupamentos gerados foram iguais em acertos. 
#Acertando 10 exemplos.
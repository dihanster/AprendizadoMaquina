##########################################################################################
#Aprendizado de Maquina e Reconhecimento de Padroes - Profa Dra. Ana Carolina Lorena  ####
#Nome: Willian Dihanster Gomes de Oliveira RA: 112269                                 ####
#                                                                                     ####
#Exerc�cio: Agrupamento Hierarquico                                                   ####
##########################################################################################

#Considere o sistema computacional para indicar a que clientes uma promo��o deve ser
#mandada, de maneira a ter uma melhor ader�ncia (gerar mais compras). Usando a
#base de dados hist�rica no arquivo Conj_dados_exerc�cio.xls, fa�a o seguinte:


#Leitura do Conjunto de Dados
dados = read.csv("C:/Users/willi/Desktop/Conj_dados_exerc�cio.csv")
dadosBackup = read.csv("C:/Users/willi/Desktop/Conj_dados_exerc�cio.csv")
dados

#a) Codifique os atributos qualitativos como quantitativos. Lembre-se de n�o usar o
dados = dados[2:5]
dados$Tamanho_fam�lia <- as.numeric(dados$Tamanho_fam�lia)-1
dados$Comprou_antes <- as.numeric(dados$Comprou_antes)-1
dados$Comprou_anunciado <- as.numeric(dados$Comprou_anunciado)-1
dados

#b) Normalize os atributos num�ricos
max1 = max(dados[1:1])
min1 = min(dados[1:1])
max2 = max(dados[2:2])
min2 = min(dados[2:2])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

dados[1:2] <- as.data.frame(lapply(dados[1:2], normalize))
dados

# c) Use o algoritmo de agrupamento hier�rquico para agrupar os dados, variando o
# m�todo de liga��o entre single, average e complete.
res <- hclust(dist(dados),method="single")
#res <- hclust(dist(dados),method="average") 
#res <- hclust(dist(dados),method="complete") 
plot(res, hang = -1)

# d) Fazendo um corte com dois grupos, avalie se os grupos gerados pelos diferentes
# m�todos possuem alguma interpreta��o, fazendo uso da classifica��o
# conhecida usada nas outras atividades.
clusterCut <- cutree(res, 2)
rect.hclust(res, k=2, border="red")
table(dadosBackup$Comprou_anunciado,clusterCut)

#O grupo 1 ficou com os elementos da classe de pessoas que nao comprararam anunciado e grupo 2
#compraram (a maioria)
#Para todas varicoes do metodo de ligacao os agrupamentos gerados foram iguais em acertos. 
#Acertando 10 exemplos.
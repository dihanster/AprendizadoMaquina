###############################################################################################################
#Aprendizado de Maquina e Reconhecimento de Padroes                                             ##                  ##
#Nome: Willian Dihanster Gomes de Oliveira RA: 112269                                           ###
#                                                                                               ###
#Exercício: Naive Bayes                                                                         ###
####################################################################################################
#Considere o sistema computacional para indicar a que clientes uma promoção deve ser
#mandada, de maneira a ter uma melhor aderência (gerar mais compras). Usando a
#base de dados histórica no arquivo Conj_dados_exercício.xls (mesma do k-vizinhos
#mais próximos), faça o seguinte:

#Leitura do Conjunto de Dados
dados = read.csv("C:/Users/willi/Desktop/Conj_dados_exercício.csv")
#Retirar atributo ID
dados = dados[2:6]
dados

#a)Discretize os atributos rendimento e idade da seguinte maneira:
# Idade: menor que 30, entre 30 e 40, maior que 40
# Rendimento: baixo (<= 2000), médio (entre 2000 e 7000), alto (>= 7000)
i = 1
while(i <= 14){ #Discretizando idade
  if(dados[i, 2] < 30){
    dados[i, 2] = "menor que 30"
  }
  else if(dados[i, 2] >= 30 && dados[i, 2] <= 40){
    dados[i, 2] = "entre 30 e 40"
  }
  else {
    dados[i, 2] = "maior que 40"
  }
  i = i+1
}

i = 1
while(i <= 14){ #Discretizando rendimento
  if(dados[i, 1] <= 2000){
    dados[i, 1] = "baixo"
  }
  else if(dados[i, 1] > 2000 && dados[i, 1] < 7000){
    dados[i, 1] = "medio"
  }
  else {
    dados[i, 1] = "alto"
  }
  i = i+1
}

#b) Monte um modelo Naive Bayes para classificar os clientes, a partir dessa base de
#experiência (lembre-se de desconsiderar o atributo ID).
library(e1071)
model <- naiveBayes(Comprou_anunciado ~ ., data = dados)
model

#c) Em seguida, classifique os seguintes potenciais novos clientes para receber a
#propaganda:
# c.1) Maria tem 55 anos, um alto rendimento e uma família pequena. Além
#disso, já comprou outros produtos da empresa anteriormente.
Maria <- c("alto", "maior que 40", "Pequena", "Sim")
predicaoMaria = predict(model, Maria)
predicaoMaria

# c.2) João é um adolescente com rendimento baixo e família pequena. Ele já
#comprou produtos da empresa
Joao <- c("baixo", "menor que 30", "Pequena", "Sim")
predicaoJoao = predict(model, Joao)
predicaoJoao

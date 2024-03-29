###############################################################################################################
#Aprendizado de Maquina e Reconhecimento de Padroes
#Nome: Willian Dihanster Gomes de Oliveira RA: 112269
#
#Exerc�cio: �rvore de Decis�o
#
#Considere o mesmo conjunto de dados do exerc�cio de k-vizinhos mais pr�ximos (uma empresa quer fazer um 
#sistema computacional que indique a que clientes uma promo��o deve ser mandada, com uma base de dados 
#hist�rica de vendas anteriores do produto que ser� anunciado). 
###############################################################################################################

#Leitura da Planilha
dados = read.csv("C:/Users/willi/Desktop/Conj_dados_exerc�cio.csv")
dados

#a) Discretize os atributos rendimento e idade da seguinte maneira:
# Idade: menor que 30, entre 30 e 40, maior que 40
# Rendimento: baixo (<= 2000), m�dio (entre 2000 e 7000), alto (>= 7000)
dados = dados[2:6] #Tirando o atributo ID
dados

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

#b) Monte a �rvore de decis�o para classificar os clientes, a partir dessa base de
#experi�ncia (lembre-se de desconsiderar o atributo ID). Plote a �rvore e verifique que
#atributos foram efetivamente usados.
library(rpart)
# Classification and regression trees (CART)
fit <- rpart(Comprou_anunciado ~ ., data = dados, method = "class", 
             control = rpart.control(minsplit = 1, minbucket=1, cp = 0))
par(xpd = TRUE)
plot(fit, compress = TRUE, uniform = TRUE, main = "Arvore de Decisao para os Dados")
text(fit, use.n = TRUE, all = TRUE, cex = .8)

#Melhor visualizacao da CART
library(rpart.plot)
prp(fit, faclen = 0, cex = 0.8, extra = 1)
? prp

# C5.0
library(C50)
library(printr)
model <- C5.0(Comprou_anunciado ~., data = dados, trials = 1)
summary(model)
plot(model)
dados

#c) Em seguida, classifique os seguintes potenciais novos clientes para receber a propaganda e explique 
#o racioc�nio empregado pela �rvore nessas classifica��es:
#  c.1) Maria tem 55 anos, um alto rendimento e uma fam�lia pequena. Al�m disso, j� comprou outros 
#produtos da empresa anteriormente.
Maria <- c("alto", "maior que 40", "Pequena", "Sim")
prediction <- predict(fit, Maria, type = "class")
# Maria => Sim
#Como Maria tem um rendimento alto, tem alta probabilidade de comprar de novo e por isso eh da classe "SIM".

#  c.2) Jo�o � um adolescente com rendimento baixo e fam�lia pequena. Ele j� comprou produtos da empresa.
Joao <- c("baixo", "menor que 30", "Pequena", "Sim")
prediction <- predict(fit, Joao, type = "class")
# Joao => Sim
# Joao tem rendimento baixo, mas como tem familia pequena e ja comprou antes, tem um certa chance de
# ser da classe "SIM" 

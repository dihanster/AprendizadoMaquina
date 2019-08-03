############################################################################
# Sampling
#Teste com replace
iris5 <- iris[sample(nrow(iris),0.1*nrow(iris), replace = TRUE),]
# Lets see the histogram of the sampled data
myhist(iris5)
# some statistics
summary(iris5)

# Exercise1: modify sampling rate to 0.5 and 0.7 and see what happens to the histograms and summary for iris2
iris3 <- iris[sample(nrow(iris),0.5*nrow(iris)),]
myhist(iris3)
summary(iris3)

iris4 <- iris[sample(nrow(iris),0.7*nrow(iris)),]
myhist(iris4)
summary(iris4)

# Exercise2: now sample with replacement
iris3 <- iris[sample(nrow(iris),0.5*nrow(iris), replace = TRUE),]
myhist(iris3)
summary(iris3)

iris4 <- iris[sample(nrow(iris),0.7*nrow(iris), replace = TRUE),]
myhist(iris4)
summary(iris4)


########################################################################################################
# Unbalanced data
# Exercice: how does knn behaves with the pre-processed datasets?
#knn1         versicolor virginica
#versicolor         48         3
#virginica           2        47

#knn2         versicolor virginica
#versicolor          7         2
#virginica           3        48

#Poucos erros, totalizando 5 erros em cada teste.


# 1nn with irisPart
knn1 <- knn.cv(irisPart[,1:2], irisPart$Species, k = 1)
table(knn1, irisPart$Species)
#knn1         versicolor virginica
#versicolor         47         4
#virginica           3        46

# 1nn with iris3
knn2 <- knn.cv(irisUmb[,1:2], irisUmb$Species, k = 1)
table(knn2, irisUmb$Species)
#knn2         versicolor virginica
#versicolor          7         1
#virginica           3        49

######### Balanceados
knnBalanced1 <- knn.cv(balanced1[,1:2], balanced1$Species, k = 1)
table(knnBalanced1, balanced1$Species)
#knnBalanced1    1 0
#             1  8 2
#             0  2 8

knnBalanced2 <- knn.cv(balanced2[,1:2], balanced2$Species, k = 1)
table(knnBalanced2, balanced2$Species)
#knnBalanced2       1  0
#               1  39  0
#               0   1 30

##############################################################################
# Missing values
# Exercise: what is the difference if the average is taken only for elements from the same class as the element? Which R command can do that?

# RESPOSTA:
# Ao usar somente a media de cada especie mantem os padroes. O comando em R para isso eh o aggregate.
aggregate(iris[,1:4], list(iris$Species), mean)


##################################################################################################
# Noise filtering
# Exercice1: change the number of neighbors in ENN and see what happenslibrary(NoiseFiltersR)
out <- ENN(Species~., data = irisPart, k = 5)
summary(out)

ggplot(data=irisPart, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color=Species, shape=Species)) +
  xlab("Petal Length") + 
  ylab("Petal Width") +
  ggtitle("Petal Length vs Width") +
  geom_point(data=irisPart[out$remIdx, ], aes(x = Petal.Length, y = Petal.Width), colour="black", size=5)

library(NoiseFiltersR)
out <- ENN(Species~., data = irisPart, k = 7)
summary(out)

ggplot(data=irisPart, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color=Species, shape=Species)) +
  xlab("Petal Length") + 
  ylab("Petal Width") +
  ggtitle("Petal Length vs Width") +
  geom_point(data=irisPart[out$remIdx, ], aes(x = Petal.Length, y = Petal.Width), colour="black", size=5)

# Exercice2: how does knn behave with the pre-processed dataset?
# Com um valor de k = 3, o knn achou mais ruido em relação a k = 5 e k = 7. Além disso, com k = 5 e k = 7 não
# houve nenhuma mudança na deteccao de ruidos!

#################################################################################################
# Data discretization
# Data Trabsformation
# Exercice: choose another number of beans and compare the results
x <- iris[,4]
hist(x, breaks=20, main="Equal Interval length") # breaking into 20 beans

# Exercice: choose another number of beans and compare the results
hist(x, breaks=10, main="Equal Interval length") # breaking into 10 beans

hist(x, breaks=40, main="Equal Interval length") # breaking into 40 beans

hist(x, breaks=100, main="Equal Interval length") # breaking into 100 beans

#Em geral, a distribuição se manteve a mesma. Com a excecao de com mais beans, houve mais intervalos vazios.

# Exercice: plot histograms of the normalized and scaled datasets (using the class information, that must be reappended to the data frame)
iris2 = iris[+ncol(iris)]

# normalized
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
iris.normalized <- as.data.frame(lapply(iris2, doNorm))
View(iris.normalized)
summary(iris.normalized)

# standardize 
iris.scaled <- scale(iris2)
View(iris.scaled)
summary(iris.scaled)


######################################################################################################
# Dimensionality reduction
# Exercice: how does knn behave with the pre-processed dataset?
knnReduzido <- knn.cv(pca[,1:2], pca$Species, k = 1)
table(knnReduzido, pca$Species)

# knnReduzido         setosa versicolor virginica
# setosa         50          0         0
# versicolor      0         38        14
# virginica       0         12        36

# Acertou todas as flores da classe Setosa. Mas manteve uma boa taxa de acertos para Versicolor (38/50) 
# e Virginica (36/14)

# feature selection

# Exercice: change previous line to subset <- backward.search(names(iris)[-5], evaluator)  and see what happens
subset <- backward.search(names(iris)[-5], evaluator)
h <- as.simple.formula(subset, "Species")
print(h)

#Acuracia com forward foi maior!
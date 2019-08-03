# Distance based predictive techniques

##################################################################################
# Distance and similarity measures
library(proxy) # proximity measures

# available measures
summary(pr_DB)

# Taking a sample of iris (for easing distance matrix visualization)
set.seed(1)
iris_sample <- iris[sample(nrow(iris),5),]
iris_sample
# Euclidean distance
dist(as.matrix(iris_sample[,1:4]),method="Minkowski",p=2)
dist(as.matrix(iris_sample[,1:4]),method="Euclidean")

Euclidean <- function(xi,xj){
  sqrt(sum((xi-xj)^2))
}
Euclidean(iris_sample[1,1:4],iris_sample[2,1:4])

# Exercice: generate a distance matrix for iris_sample using the Euclidean function

#####################################################################################################
# influence of scale
# modifying first attribute to be in a higher scale
iris_sample[,1] <- iris_sample[,1] * 1000
iris_sample

dist(as.matrix(iris_sample[,1:4]),method="Euclidean")

iris_sample[,1] <- iris_sample[,1] / 1000

# normalizing data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

iris_norm <- as.data.frame(lapply(iris_sample[1:4], normalize))
iris_norm
summary(iris_norm)

dist(as.matrix(iris_norm),method="Euclidean")

#####################################################################################################
# influence of noise
noise<-rnorm(5)  
iris_sample[,3] <- iris_sample[,3]+noise
iris_sample

iris_norm <- as.data.frame(lapply(iris_sample[1:4], normalize))
iris_norm
iris_sample[,3] <- iris_sample[,3]-noise

dist(as.matrix(iris_norm[,1:4]),method="Euclidean")

# Feito em Aula para ver a diferenca 
#iris_norm2 <- as.data..frame(lapply(iris_sample[1:4], normalize))
#dist(as.matrix(iris_norm2[,1:4]),method="Euclidean")

# Exercice: repeat previous lines with distances Manhattan and supremum
dist(as.matrix(iris_norm[,1:4]),method="Manhattan")
dist(as.matrix(iris_norm[,1:4]),method="supremum")

#####################################################################################################
# correlation

cor(iris_sample[,1:4])

#scale effect
iris_sample[,1] <- iris_sample[,1] * 1000
cor(iris_sample[,1:4])
iris_sample[,1] <- iris_sample[,1] / 1000

# influence of noise
noise<-rnorm(5)  
iris_sample[,3] <- iris_sample[,3]+noise
cor(iris_sample[,1:4])
iris_sample[,3] <- iris_sample[,3]-noise

#########################################################################################################
# showing that correlation captures form
library(ggplot2)
library(stats)

x1 <- c(1,5,2,7)
x2 <- c(14,19,15,22)

dat <- data.frame(c=0:3, x1, x2)
ggplot(dat) + geom_path(aes(x=c,y=x1),col=I("red")) + geom_path(aes(x=c,y=x2),col=I("blue"))

dat_norm <- as.data.frame(lapply(dat[2:3], normalize))
dat_norm

# comparing euclidean distance and correlation of the two vectors
Euclidean(dat_norm[,1],dat_norm[,2])
cor(x1,x2)
cor(dat_norm[,1],dat_norm[,2])

#########################################################################################################
# matching functions
x <- matrix(sample(c(FALSE, TRUE), 8, rep = TRUE), ncol = 2)
x
dist(x, method = "Jaccard")
dist(x, method = "simple matching")
dist(x, method = "Manhattan")

########################################################################################################
# Nearest neighbor
# From https://www.youtube.com/watch?v=XeW0e-S4kHo
# With some adaptations

knn <- function(dataset,query,k=1){
  
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

# testing classification
knn(iris_sample,c(5.1,3.4,1.5,0.2),k=1)

# Exercice: modify number of nearest neighbors and see what happens
knn(iris_sample,c(5.1,3.4,1.5,0.2),k=3)
knn(iris_sample,c(5.1,3.4,1.5,0.2),k=5)
#Houve uma maior duvida com as outras classes, mas ainda acertou a classe do exemplo!

# testing regression (tentando  ver a petal.widht)
iris_reg <- iris_sample[-ncol(iris_sample)] # removing last collumn
knn(iris_reg,c(5.1,3.4,1.5),k=1)

# Exercice: modify number of nearest neighbors and see what happens
knn(iris_reg,c(5.1,3.4,1.5),k=3)
knn(iris_reg,c(5.1,3.4,1.5),k=5)
#Com k > 1, já houve uma grande diferença do valor predito pela regressao

# some libraries that also have implementations: class, RWeka, caret, DMwR

#######################################################################
# DWNN using Gaussian weighting for regression
# From: https://bitbucket.org/rodrigo_mello/ml4u/src/ab2300b432ac9105df1352d5890543cf136bef8f/dwnn/dwnn.r?at=master&fileviewer=file-view-default

dwnn <- function(dataset, query, sigma=0.5) {
  
  classId = ncol(dataset)
  
  w = apply(dataset,1,function(row){
    eucl <- sqrt(sum((query-row[1:(classId-1)])^2))
    exp(-eucl^2 / (2*sigma^2))
  })
  Y = dataset[,classId]
  pred = sum(w*Y)/ sum(w)
  
  ret <- list()
  ret$weigths <- w
  ret$pred <- pred
  
  return (ret)
}

# testing in modified iris for regression
dwnn(iris_reg,c(5.1,3.4,1.5),sigma = 0.5)

# Exercice: change sigma value and observe
dwnn(iris_reg,c(5.1,3.4,1.5),sigma = 0.2)
dwnn(iris_reg,c(5.1,3.4,1.5),sigma = 0.3)
dwnn(iris_reg,c(5.1,3.4,1.5),sigma = 0.7)
# A predicao foi praticamente a mesma para todos os valores de sigma. 
# Com a variacao maior apenas nos pesos de contribuicao

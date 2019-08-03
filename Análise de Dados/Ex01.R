# Exercicios da aula do dia 09/03 - Análise de Dados
# Willian Dihanster Gomes de Oliveira RA: 112269

# Exercise: the boxplots of the other input features
boxplot(iris$Sepal.Length ~ iris$Species,data = iris, xlab = "Iris Species", ylab = "Sepal Length", col = 2:4)
boxplot(iris$Sepal.Width ~ iris$Species,data = iris, xlab = "Iris Species", ylab = "Sepal Width", col = 2:4)
boxplot(iris$Petal.Length ~ iris$Species,data = iris, xlab = "Iris Species", ylab = "Petal Length", col = 2:4)
boxplot(iris$Petal.Width ~ iris$Species,data = iris, xlab = "Iris Species", ylab = "Petal Width", col = 2:4)

# Histograms
library(ggplot2)
library(gridExtra)
library(grid)
show(HisSl)
# Exercise: the histograms of the other input features (HistSw,  HistPl, HistPw)
HisSl <- ggplot(data=iris, aes(x=iris$Sepal.Length))+geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +  xlab("Sepal Length (cm)") +   ylab("Frequency") +      theme(legend.position="none")+  ggtitle("Histogram of Sepal Length")+      geom_vline(data=iris, aes(xintercept = mean(iris$Sepal.Length)),linetype="dashed",color="grey")
HistSw <- ggplot(data=iris, aes(x=iris$Sepal.Width))+geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +  xlab("Sepal Width (cm)") +   ylab("Frequency") +      theme(legend.position="none")+  ggtitle("Histogram of Sepal Width")+      geom_vline(data=iris, aes(xintercept = mean(iris$Sepal.Width)),linetype="dashed",color="grey")
HistPl <- ggplot(data=iris, aes(x=iris$Petal.Length))+geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +  xlab("Petal Length (cm)") +   ylab("Frequency") +      theme(legend.position="none")+  ggtitle("Histogram of Petal Length")+      geom_vline(data=iris, aes(xintercept = mean(iris$Petal.Length)),linetype="dashed",color="grey")
HistPw <- ggplot(data=iris, aes(x=iris$Petal.Width))+geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +  xlab("Petal Width (cm)") +   ylab("Frequency") +      theme(legend.position="none")+  ggtitle("Histogram of Petal Width")+      geom_vline(data=iris, aes(xintercept = mean(iris$Petal.Width)),linetype="dashed",color="grey")

# Joining the histograms
# Plot all visualizations
grid.arrange(HisSl + ggtitle(""), HistSw + ggtitle(""), HistPl + ggtitle(""), HistPw  + ggtitle(""),             nrow = 2, top = textGrob("Iris Frequency Histogram", gp=gpar(fontsize=15)))
##############################################################################

# bagplot
library(aplpack)
bagplot(iris$Sepal.Length,iris$Sepal.Width)
# Exercice: do other combinations
bagplot(iris$Sepal.Length,iris$Petal.Length)
bagplot(iris$Sepal.Length,iris$Petal.Width)
bagplot(iris$Sepal.Width,iris$Petal.Length)
bagplot(iris$Sepal.Width,iris$Petal.Width)
bagplot(iris$Petal.Length,iris$Petal.Width)




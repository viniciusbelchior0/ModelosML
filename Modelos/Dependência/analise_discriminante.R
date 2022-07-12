dados <- read.csv2("C:\\Users\\NOTEBOOK CASA\\Desktop\\Modelos\\Dependência\\Discriminante Linear\\exemplo_discriminante_Excel.csv")

library(MASS)

rownames(dados) <- dados$Empresa
dados$Empresa <- NULL
str(dados)
dados$Grupo <- as.factor(dados$Grupo)

cores <- rainbow(length(levels(dados[,"Grupo"])))
pairs(dados[,-5], pch = 21, bg = cores[dados$Grupo], lower.panel = NULL)

#1. Analise Discriminante Linear
linda <- lda(Grupo ~., data = dados)
linda

# Matriz de confusao
lindapred <- predict(linda, dados)$class
tab2 <- xtabs(~ lindapred + Grupo, data = dados)
tab2

coef(linda) # funcoes discriminantes
linda$svd # razao dos desvios padrao entre e intragrupo para cada FD
linda$means # centroides dos grupos
(FDb <- linda$means %*% coef(linda)) #escores dos centroides

#grafico de pontos de FD1
FD <- as.matrix(dados[,-5]) %*% coef(linda)
stripchart(FD[,1] ~Grupo, pch = 20, xlab = "Funcao Discriminante 1",
           ylab = "Grupo", col = cores, method = "stack", data = dados)
points(FDb[,1], (1:length(linda$lev)) + 0.05, pch = 13, col = cores, cex = 1.5)

# Histograma de FD1
ldahist(FD[,1], dados$Grupo)

 #2. Analise discriminante Quadratica
quada <- qda(Grupo ~., data = dados)
quada


# A unica forma de avalicao do modelo e pela matriz de confusao e suas
# metricas derivadas 
# (?)Avaliar a interpretabilidade


# Links:
#http://leg.ufpr.br/~walmes/ensino/ML/tutorials/09-linear-discriminant-analysis.html


library(haven)
GrupoSupermercadista <- read_dta("C:/Users/NOTEBOOK CASA/Desktop/Modelos/z_Manual de Análise de Dados - Fávero/BancosdeDados/Cap. 18/GrupoSupermercadista.dta")

library(tidyverse)
library(CCA)
library(CCP)

glimpse(GrupoSupermercadista)
summary(GrupoSupermercadista)

GrupoSupermercadista$faturamento <- GrupoSupermercadista$faturamento/1000

X <- GrupoSupermercadista[,c(2:3)]
Y <- GrupoSupermercadista[,c(4:5)]

# Analise de correlacao canonica
cca <- matcor(X,Y)
cca$XYcor

img.matcor(cca, type = 2)


ccyx <- cc(X,Y)
names(ccyx)
ccyx$cor #correlcao canonica

# Testes multivariados para significancia
## Funcoes canonicas

p.asym(ccyx$cor, dim(GrupoSupermercadista)[1], dim(X)[2], dim(Y)[2], tstat = "Wilks")# Lambda de Wilks
p.asym(ccyx$cor, dim(GrupoSupermercadista)[1], dim(X)[2], dim(Y)[2], tstat = "Pillai")# Traço de Pillai
p.asym(ccyx$cor, dim(GrupoSupermercadista)[1], dim(X)[2], dim(Y)[2], tstat = "Hotelling")# Traço de Hotelling
p.asym(ccyx$cor, dim(GrupoSupermercadista)[1], dim(X)[2], dim(Y)[2], tstat = "Roy")# Traço de gcr de Roy


#Proporcao da variancia total explicada
loadings <- comput(X,Y,ccyx)

(colSums((loadings$corr.X.xscores)^2))/(dim(X)[2])*100 # Variaveis canonicas U
(colSums((loadings$corr.Y.yscores)^2))/(dim(Y)[2])*100 # Variaveis canonicas V

ccyx$cor^2 #R2 canonico

# Indice de Redundancia
(colSums((loadings$corr.X.xscores)^2))/(dim(X)[2])*(ccyx$cor^2)
(colSums((loadings$corr.Y.yscores)^2))/(dim(Y)[2])*(ccyx$cor^2)

#Estatistica latente
diag(sqrt(diag(cov(Y)))) %*% ccyx$ycoef
diag(sqrt(diag(cov(X)))) %*% ccyx$xcoef


#Cargas canonicas
ccyx$scores$corr.X.xscores
ccyx$scores$corr.Y.yscores

#Cargas canonicas cruzadas
ccyx$scores$corr.X.yscores
ccyx$scores$corr.Y.xscores


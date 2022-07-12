library(readxl)
library(tidyverse)
library(kableExtra)
options(scipen = 999)
X2_GSP_RAW <- read_excel("C:\\Users\\NOTEBOOK CASA\\Desktop\\Modelos\\Interdependência\\PCA\\2_GSP_RAW.xlsx")

names(X2_GSP_RAW)
str(X2_GSP_RAW)

#1. covariancia, correlacao e autovalores
#a. matriz de cov
covar <- X2_GSP_RAW %>% select(2:14) %>% cov()

#b. matriz de cor
correl <- X2_GSP_RAW %>% select(2:14) %>% cor()

correl %>% corrplot::corrplot(method = "color", type = "lower",
                              addCoef.col = "black")

#i. autovalores correl
eigR <- eigen(correl)
autovalores <- eigR$values
variancia <- autovalores/sum(autovalores)

#ii.
plot(eigR$values, type = "b")

#2. Principal Components
pca <- princomp(X2_GSP_RAW[,2:14],cor = TRUE)
summary(pca)
pca$loadings #autovetores
pca$scores #scores - impossivel interpretacao - ignorar


##### PELO FACTOMINER ####
#factominer e bem mais pratico
library(FactoMineR)

pca_fminer <- PCA(X2_GSP_RAW[,2:14], scale.unit = TRUE)
summary.PCA(pca_fminer)

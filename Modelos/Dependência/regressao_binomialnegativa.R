library(tidyverse)
library(haven)
library(AER)
library(MASS)
Acidentes <- read_dta("C:/Users/NOTEBOOK CASA/Desktop/Modelos/z_Manual de Análise de Dados - Fávero/BancosdeDados/Cap. 14/Acidentes.dta")

glimpse(Acidentes)

Acidentes$leiseca <- as.factor(Acidentes$leiseca)

# Regressao Binomial Negativa
regbinneg <- glm.nb(acidentes ~ idade + leiseca + pop, data = Acidentes)
summary(regbinneg)

par(mfrow = c(2,2))
plot(regbinneg)


intervalos <- confint(regbinneg)
estimat <- cbind(regbinneg$coefficients, intervalos)
colnames(estimat)[1] <- "Estimativa Pontual"

estimat
library(tidyverse)
library(haven)
library(AER)
library(MASS)
Acidentes <- read_dta("C:/Users/NOTEBOOK CASA/Desktop/Modelos/z_Manual de Análise de Dados - Fávero/BancosdeDados/Cap. 14/Acidentes.dta")

glimpse(Acidentes)

Acidentes$leiseca <- as.factor(Acidentes$leiseca)


# Regressão Poisson
regpoisson <- glm(acidentes ~ ., family = "poisson", data = Acidentes)
summary(regpoisson)
dispersiontest(regpoisson)
anova(regpoisson, test = "Chisq")

par(mfrow =c(2,2))
plot(regpoisson)

plot(Acidentes$pop, Acidentes$acidentes)
points(regpoisson$fitted.values, col = "red")

# Utilizando offset para controlar pelo numero de acidentes
regpoissonoff <- glm(acidentes ~ idade + leiseca, offset = pop, family = "poisson", data = Acidentes)
summary(regpoissonoff)
dispersiontest(regpoissonoff)
anova(regpoissonoff, test = "Chisq")

plot(Acidentes$acidentes, pch = "o", col = "blue", xlab = "AA", ylab = "Acidentes")
points(regpoissonoff$fitted.values, pch = "p", col = "red")
legend(6,30,c("obs", "pred"), pch = c("o","p"), col = c("blue", "red"))

# Quasi-Poisson to allow the scale of parameter to change from 1
quasipoisson <- glm(acidentes ~ idade + leiseca + offset(log(pop)), family = quasipoisson(link=log),
                    data = Acidentes)

quasipoisson %>% summary.glm()


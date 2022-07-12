# Exercicio Manual de Analise de Dados (Favero) - Cap 10 - Ex.3
library(haven)
library(tidyverse)
library(psych)
library(GPArotation)

PercepcaoDrogaria <- read_dta("C:/Users/NOTEBOOK CASA/Desktop/Mainstream/Estatística/Modelos e Testes/Manual de Análise de Dados/BancosdeDados/Cap. 10/PercepcaoDrogaria.dta")
glimpse(PercepcaoDrogaria)

#a. matriz correl
cor(PercepcaoDrogaria) %>% corrplot::corrplot(method = "color", type = "lower",
                                              addCoef.col = "black")
## Resposta: sim, pois existem varios valores fortemente correlacionados

#b. Teste de Bartlett
bartlett.test(PercepcaoDrogaria)
## Resposta: sim, e apropriada

#c. autovalores
av <- eigen(cor(PercepcaoDrogaria))
plot(av$values, type = "b")
percent <- av$values/sum(av$values)
percent #pelo criterio de kaiser -> 2 ou 3
varexpl <- sum(percent[1:3])
varexpl

KMO(PercepcaoDrogaria)

# d. analise fatorial
fatorial <- fa(PercepcaoDrogaria, nfactors = 3, fm = "pa", rotate = "varimax")

fatorial
fatorial$communality #comunalidades
fatorial$weights #coeficiente dos scores fatoriais
fatorial$scores #scores fatoriais
fatorial$loadings #correlacao das variaveis com os fatores

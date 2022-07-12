library(readxl)
X4_L04 <- read_excel("C:\\Users\\NOTEBOOK CASA\\Desktop\\Modelos\\Interdependência\\Análise Fatorial\\4_L04.xlsx")

registros <- X4_L04[c(2,8,11,25,39,57,62),]
X4_L04 <- X4_L04[-c(2,8,11,25,39,57,62),]

#1 e 2. Screeplot, criterio kaiser
av <- eigen(cor(X4_L04[,-1]))
plot(av$values, type = "b")
percent <- av$values/sum(av$values)
percent #pelo criterio de kaiser -> 6 componentes
varexpl <- sum(percent[1:6])
varexpl

#3. Analise Fatorial
library(psych)
library(GPArotation)

#teste de adequacao - kmo e bartlett
KMO(X4_L04[,-1])
bartlett.test(X4_L04[,-1])

#Analise fatorial e analise de resultados
fatorial <- fa(X4_L04[,-1], nfactors = 6, fm = "pa", rotate = "varimax")

fatorial
fatorial$communality #comunalidades
fatorial$weights #coeficiente dos scores fatoriais
fatorial$scores #scores fatoriais

# Aplicando nos registros removidos
predict(fatorial, registros[,-1])

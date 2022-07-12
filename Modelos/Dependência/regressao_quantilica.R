df <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Modelos\\Dependência\\Regressão\\Regressão Quantílica\\Train-Set.csv")

library(tidyverse)
library(quantreg)

glimpse(df)
summary(df)

df <- df %>% drop_na()

reg.quant <- rq(OutletSales ~ Weight + FatContent + ProductVisibility + OutletSize,
                tau = c(0.10,0.50,0.90), method = "br", data = df)

plot(reg.quant)

# erros padrao
se.regquant <- summary(reg.quant, se = "iid")
se.regquant

summary(reg.quant, se = "boot", R=10)

# Anova fucntion fo r quantile regression fits
erq10 <- rq(OutletSales ~ Weight + FatContent + ProductVisibility + OutletSize, tau = 0.1, method = "br", data = df)
erq50 <- rq(OutletSales ~ Weight + FatContent + ProductVisibility + OutletSize, tau = 0.5, method = "br", data = df)
erq90 <- rq(OutletSales ~ Weight + FatContent + ProductVisibility + OutletSize, tau = 0.9, method = "br", data = df)

anova.rq(erq10, erq50, erq90, test = "Wald", joint = FALSE)

#Testando conjuntamente
anova.rq(erq10, erq50, erq90, test = "Wald", joint = TRUE)

# Grafico de coeficiente quantilicos
qe.regquant <- summary(reg.quant, se = "iid")
plot(qe.regquant)

# Interpretacao do grafico: linha vermelhar -> valor do coeficiente ao se usar
# minimos quadrados ordinarios. Pontilhado (vermelho) intervalo de confianca
# A linha preta é o valor do coeficiente utilizando regressao quantilica,
# a coloracao cinza o intervalo de confianca e os pontinhos  o valor de cada quantil


library(haven)
library(tidyverse)
library(lmtest)
library(car)
library(tseries)

#1. Dados em Painel longitudinal - Lineares ####
#base em painel - balanceada - longa
Criminalidade <- read_dta("C:/Users/NOTEBOOK CASA/Desktop/Mainstream/Estatística/Modelos e Testes/Manual de Análise de Dados/BancosdeDados/Cap. 15/Criminalidade.dta")
str(Criminalidade)

Criminalidade$id <- as.factor(Criminalidade$id)

library(plm) #biblioteca para os modelos

criminalidade.p <- pdata.frame(Criminalidade, index = c("id","t"))#declarando que e painel

ggplot(Criminalidade) +
  aes(x = t, y = homicídios, colour = id) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  theme_gray()


#1. Regressao com painel (pooled)
reg <- plm(homicídios ~ polícia + leiseca, data = criminalidade.p, model = "pooling")
summary(reg)


#2. Painel com efeito fixo
reg_fixos <- plm(homicídios ~ polícia +leiseca , data = criminalidade.p, model = "within")
summary(reg_fixos)
summary(fixef(reg_fixos))

#3. Painel com efeitos aleatórios
reg_aleatorios <- plm(homicídios ~ polícia + leiseca, data = criminalidade.p, model = "random")
summary(reg_aleatorios)
#theta mensura qual parte do efeito vem do indivíduo e qual parte é idiossincrática


#4. Testes 
plmtest(reg, effect = "individual") # LM Breusch-Pagan
pFtest(reg_fixos, reg) # F de Chow
phtest(reg_aleatorios, reg_fixos) # Hausman


#5. Avaliacao dos modelos
residuos <- reg_aleatorios$residuals #normalidade dos residuos
hist(residuos, col = "orange", main = "Distribuição dos Resíduos")
shapiro.test(residuos)$p.value

pcdtest(reg_aleatorios, test = "cd") #dependencia transversal

bptest(reg_aleatorios) #homocedasticidade dos residuos

pbgtest(reg_aleatorios) #independencia dos residuos/correlacao serial

pwtest(reg) #teste para efeitos individuais ou de tempo
pwtest(reg, effect = "time")

vif(reg_aleatorios) #multicolinearidade das variaveis independentes

adf.test(criminalidade.p$homicídios, k = 2) #testando raizes unitarias

# Links para consulta ####
#https://rstudio-pubs-static.s3.amazonaws.com/388380_f8f15524e71242b1a66900a4fd15ba11.html
#https://smolski.github.io/livroavancado/regressao-com-dados-em-painel.html






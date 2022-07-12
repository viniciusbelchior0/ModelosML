tanzania_train <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Analytics\\Exercícios\\testes_ml_colab\\tanzania_train.csv", encoding = "UTF-8")

str(tanzania_train)
summary(tanzania_train)

library(tidyverse)
library(ggplot2) 
library(MASS)
library(car)
library(asbio)

####1. TRATANDO os DADOS ####
str(tanzania_train)
summary(tanzania_train)


# Resolvendo problemas com NAs
tanzania_train <- mutate_all(tanzania_train,list(~na_if(.,"")))
tanzania_train <- tanzania_train %>% mutate(travel_with = replace_na(travel_with,"Unknown"),
                                            most_impressing = replace_na(most_impressing, "Unknown"))

#1 - TRATAMENTO DOS DADOS QUANTITATIVOS ##
##1. *Total cost* muito grande, dividir por 1000
tanzania_train$total_cost <- tanzania_train$total_cost/1000 #dvidir por 1000 -> valores mais interpretaveis

##2. total female e male muito mal distribuidos
quantile(tanzania_train$total_female, seq(0,1,0.01),na.rm = TRUE)
quantile(tanzania_train$total_male, seq(0,1,0.01),na.rm = TRUE)
#criacao de novas categoria: valores maiores que 5 OU remover esse outlier

#criar nova categoria que e a soma das duas
tanzania_train$total_people <- tanzania_train$total_female + tanzania_train$total_male
tanzania_train <- tanzania_train %>% drop_na(total_people)

# tambem usar percentuais em relacao aos generos
# essa variavel tbm tem relacao com ~travel_with
tanzania_train %>% filter(total_female >= 5)


##3. Mainland e Zanzibar
summary(tanzania_train[,18:19])
quantile(tanzania_train$night_mainland, seq(0,1,0.05),na.rm = TRUE)
quantile(tanzania_train$night_zanzibar, seq(0,1,0.05),na.rm = TRUE)

#variavel pode estar errada; estao erradas, oa as pessoas nem ficaram um dia??
tanzania_train %>% filter(night_mainland ==0 & night_zanzibar ==0)
# Ha varios dados com esse problema

tanzania_train$total_night <- tanzania_train$night_mainland + tanzania_train$night_zanzibar

####2 REGRESSAO ####
names(tanzania_train)

aaa <- tanzania_train %>% dplyr::select(-c(1,2,24,25))

reg1 <- lm(total_cost ~., data = aaa)
summary(reg1)

drop1(reg1, test = "F") #teste de hipotese marginal

m1 <- step(reg1, direction = "both") #AIC
m2 <- step(reg1, direction = "both", k = log(nrow(aaa))) #BIC
# remvoer variaveis ate <none>

summary(m1)
summary(m2)


# Medidas de Ajuste
modl <- list(m0 = reg1, m1 = m1, m2 = m2)

asbio::lm.select(modl) %>%
  mutate_if(is.numeric, round, digits = 2)

measures <- function(x) {
  L <- list(npar = length(coef(x)),
            dfres = df.residual(x),
            nobs = length(fitted(x)),
            RMSE = summary(x)$sigma,
            R2 = summary(x)$r.squared,
            R2adj = summary(x)$adj.r.squared,
            PRESS = press(x),
            logLik = logLik(x),
            AIC = AIC(x),
            BIC = BIC(x))
  unlist(L)
}

t(sapply(modl, measures)) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 3)

# Menor PRESS >> melhor modelo preditivo; LogLik quanto maior melhor;
# BIC e AIC quanto menor melhor

summary(m2)


#### DIAGNOSTICO DE MODELO ####
par(mfrow = c(2,2))
plot(reg1)
layout(1)

# Explicacoes: 
# Residulas vs Fitted (falta de ajutes); Scale-location (relacao média-variância);
# Normal QQplot : normalidade dos erros;; Leverage: alavancagem

#Residuals vs Fitted: linha deve ser reta \ lack of fit
# Scale-Location: espera-se que a linha vermelha seja reta
# Normal Q-Q: espera-se que os erros sigam a distribuicao normal (andar junto com a linha )
# Residual vs Leverage: impacto de outliers na direita do grafico

# Deve-se olhar de maneira sequencial (a1,a2,b1,b2)
# Graficos sao mais aconselhaveis do que testes de hipotese (shapiro-wilk, bartlett)
# Usar os dois conjuntamente

hatvalues(reg1) #valores de alavancagem
im <- influence.measures(reg1) #medidas de influencia
summary(im)

# Além dos gráficos básicos
library(car)

qqnorm(rstudent(reg1))
qqline(rstudent(reg1))
qqPlot(reg1) # com envelope

residualPlot(reg1)
residualPlots(reg1) #mais de uma preditora

spreadLevelPlot(reg1)

avPlots(reg1) #residuos parciais

# crPlots
#dfbetasPlots

influenceIndexPlot(reg1)
influencePlot(reg1)

vif(reg1) # VIF acime de 10 e perigoso


# MEDIDAS CORRETIVAS ####
MASS::boxcox(reg1) #transformacao box cox
abline(v = 0.135, col = "red")
## nao ha garantia de normalidade

# as predicoes na escala original podem ser obtidas aplicando a transformacao inversa
# as transformacoes exigem valores >0, para corrigir isso somar todos os valores com uma constante
# e realizar a transformacao

#pelo teste deve-se fazer a transformacao logaritmica

regnova <- lm(log(total_cost) ~., data = aaa)
summary(regnova)

par(mfrow = c(2,2))
plot(regnova)
layout(1)

qqPlot(regnova)

vif(regnova)

# regressao ridge
reg1_ridge <- lm.ridge(formula(reg1), data = reg1$model,
                       lambda = seq(0,0.5,0.001))

plot(reg1_ridge)

MASS::select(reg1_ridge)

reg1_ridge_final <- lm.ridge(formula(reg1), data = reg1$model,
                             lambda = 0.5)

coef(reg1_ridge_final) %>% round(digits = 5) %>% cbind()






# Relacao entre variaveis #####
aaa %>% dplyr::select_if(is.numeric) %>% cor() %>% corrplot::corrplot(method = "color",
                                                                      type = "lower",
                                                                      addCoef.col = "black")

# Relacao entre variaveis binarias
teste <- dummy_cols(dumis, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
teste %>% dist(method = "binary")

lapply(teste[,-1], function(x){
  return(prop.table(table(x, teste$`tour_arrangement_Package Tour`),
                    margin = 1))
})
## Conclusao: se a pessoa tem pacote, quase sempre ela vai ter os beneficios inclusos
# caso ela nao possua, isso pode variar dependendo do beneficio

# colunas para variaveis cat (1,2,5,6,7,8,18,19,20)
chisq.test(aaa$age_group, aaa$travel_with)
chisq.test(aaa$age_group, aaa$purpose)
chisq.test(aaa$travel_with, aaa$purpose)


aaa$personas <- aaa$total_female + aaa$total_male

ggplot(data = aaa, mapping = aes(x = personas, y = total_cost, colour = tour_arrangement)) +
  facet_wrap(facets = ~purpose) +
  geom_point() +
  scale_fill_discrete() +
  stat_summary(geom = "line", fun = "mean", col = "orange") +
  xlim(0L, 10L)

quantile(aaa$personas, seq(0,1,0.01))




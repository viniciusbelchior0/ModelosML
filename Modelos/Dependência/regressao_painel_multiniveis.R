library(haven)
library(tidyverse)
library(nlme)
library(lmtest)
library(lme4) #pacote alternativo para multiniveis lineares
library(glmmTMB) # pacote para modelos nao lineares (logisticos, poisson)

DesempenhoAlunoEscola <- read_dta("C:/Users/NOTEBOOK CASA/Desktop/Mainstream/Estatística/Modelos e Testes/Manual de Análise de Dados/BancosdeDados/Cap. 16/DesempenhoAlunoEscola.dta")

names(DesempenhoAlunoEscola)
glimpse(DesempenhoAlunoEscola)

DesempenhoAlunoEscola[,c(1,2,6)] <- lapply(DesempenhoAlunoEscola[,c(1,2,6)], function(x){as.factor(x)})
# as.factor transformam em niveis


# EDA
# Desempenho x Hora
DesempenhoAlunoEscola %>% ggplot(aes(x = horas, y = desempenho)) +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  geom_point() +
  theme_gray()

# Desempenho x Hora por Escola
DesempenhoAlunoEscola %>% ggplot(aes(x = horas, y = desempenho, color = escola)) +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  geom_point() +
  guides(color = F) +
  theme_gray()

# Estimação do Modelo ####
modelo <- lme(fixed = desempenho ~ horas + texp + horas:texp,
              random = ~ horas | escola,
              data = DesempenhoAlunoEscola,
              method = "REML")

summary(modelo)

#stderr_nlme(modelo) #erros padrao de tau00 e sigma

# Vizualicao dos interceptos e das inclinacoes aleatorias por escola:
u_final <- data.frame(modelo[["coefficients"]][["random"]][["escola"]]) %>%
  rename(u00 =1,
         u10 =2)

u_final$escola <- c(1:46)
u_final$escola <- as.factor(u_final$escola)

# Interceptos aleatórios por Escola
u_final %>% ggplot(aes(x = fct_rev(escola), y = u00)) +
  geom_bar(stat = "identity", fill = "deepskyblue", alpha = 0.8) +
  coord_flip() +
  labs(x = "Escola",
       y = "Interceptos Aleatórios por Escola") +
  theme_gray()

# Inclinações aleatórias por Escola
u_final %>% ggplot(aes(x = fct_rev(escola), y = u10)) +
  geom_bar(stat = "identity", fill = "dodgerblue4", alpha = 0.8) +
  coord_flip() +
  labs(x = "Escola",
       y = "Inclinações Aleatórias por Escola") +
  theme_gray()


# Comparacao como Modelo OLS ####
reg <- lm(formula = desempenho ~ horas + texp, data = DesempenhoAlunoEscola)
summary(reg)

# comparando os LL (LogLikelihood)
data.frame(reg = logLik(reg),
           modelo = logLik(modelo)) %>%
  reshape::melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white") +
  labs(tile = "Comparação do LL", y = "logLik", x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:", values = c("darkorchid","deepskyblue1")) +
  theme(legend.title = element_blank(), panel.background = element_rect("white"),
        legend.position = "none", axis.line = element_line())

#(?) log Likelihood: funcao de verossimilhanca para comparar modelos, o que apresenta
# maior loglik é o que apresenta menores distorcoes entre os valores reais e estimados


#LR test
lrtest(reg, modelo)


# Comparando a aderencia dos fitted values dos modelos elaborados
DesempenhoAlunoEscola["modelo_fitted"] <- predict(modelo, DesempenhoAlunoEscola)
DesempenhoAlunoEscola["reg_fitted"] <- reg$fitted.values

DesempenhoAlunoEscola %>%
  ggplot() +
  geom_smooth(aes(x = desempenho, y = reg_fitted, color = "Estimação OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_smooth(aes(x = desempenho, y = modelo_fitted, color = "Estimação Multinivel"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", color = "gray44", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", values = c("deepskyblue1", "darkorchid")) +
  labs(x = "Desempenho", y = "Fitted Values") +
  theme_gray()


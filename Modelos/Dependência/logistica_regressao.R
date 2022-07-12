library(tidyverse)
library(readxl)
library(lubridate)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
options(scipen = 999, digits = 2)

acidentes01 <- read.csv2("C:/Users/NOTEBOOK CASA/Desktop/Analytics/Exercícios/6. PRF/acidentes/acidentes01.csv", encoding = "UTF-8")
acidentes02 <- read.csv2("C:/Users/NOTEBOOK CASA/Desktop/Analytics/Exercícios/6. PRF/acidentes/acidentes02.csv", encoding = "UTF-8")
acidentes03 <- read.csv2("C:/Users/NOTEBOOK CASA/Desktop/Analytics/Exercícios/6. PRF/acidentes/acidentes03.csv", encoding = "UTF-8")
acidentes04 <- read.csv2("C:/Users/NOTEBOOK CASA/Desktop/Analytics/Exercícios/6. PRF/acidentes/acidentes04.csv", encoding = "UTF-8")

acidentes01[,c(1,26:30)] <- lapply(acidentes01[,c(1,26:30)], function(x){as.character(x)})
acidentes02[,c(1,6,26:30)] <- lapply(acidentes02[,c(1,6,26:30)], function(x){as.character(x)})
acidentes03[,c(1,6,26:30)] <- lapply(acidentes03[,c(1,6,26:30)], function(x){as.character(x)})
acidentes04[,c(1,6,26:27)] <- lapply(acidentes04[,c(1,6,26:27)], function(x){as.character(x)})

acidentes01$km <- as.numeric(acidentes01$km)

acidentes <- bind_rows(acidentes01, acidentes02, acidentes03, acidentes04)

head(acidentes) #nao remover NAs pois ainda ha muitas informacoes uteis / base nao tem dados para certa data
str(acidentes)

acidentes$km <- round(acidentes$km, digits = 0)
acidentes$data_inversa <- as.Date(acidentes$data_inversa, "%d/%m/%Y")
acidentes <- acidentes %>% mutate(ano = year(data_inversa), mes = month(data_inversa), hora = as.POSIXct(acidentes$horario, format = "%H:%M:%S"))
acidentes$horario <- substring(acidentes$horario,1,2)
acidentes <- acidentes %>% filter(classificacao_acidente != "")

head(acidentes)

tipo_acidentes <- read_excel("C:/Users/NOTEBOOK CASA/Desktop/Analytics/Exercícios/6. PRF/acidentes_rotulos_corrigidos.xlsx", col_types = c("text", "text"), sheet = 1)
causa_acidentes <- read_excel("C:/Users/NOTEBOOK CASA/Desktop/Analytics/Exercícios/6. PRF/acidentes_rotulos_corrigidos.xlsx", col_types = c("text", "text"), sheet = 2)

acidentes <- left_join(acidentes, causa_acidentes, by = "causa_acidente")
acidentes <- left_join(acidentes, tipo_acidentes, by = "tipo_acidente")

acidentes$dia_semana <- str_replace_all(acidentes$dia_semana, c("Sexta" = "sexta-feira", "Sábado" = "sábado",
                                                                "Domingo" = "domingo", "Segunda" = "segunda-feira", "Quinta" = "quinta-feira", "Quarta" = "quarta-feira",
                                                                "Terça" = "terça-feira"))

acidentes$dia_semana <- str_replace_all(acidentes$dia_semana, c("segunda-feira" = "1.segunda-feira", "terça-feira" = "2.terça-feira",
                                                                "quarta-feira" = "3.quarta-feira", "quinta-feira" = "4.quinta-feira", "sexta-feira" = "5.sexta-feira", "sábado" = "6.sábado",
                                                                "domingo" = "7.domingo"))

BRS_MUN <- acidentes %>% group_by(br, municipio,uf) %>% summarise(Acidentes = n(), Pessoas = sum(pessoas), Veiculos = sum(veiculos),
                                                                  Ilesos = sum(ilesos), Feridos = sum(feridos), Graves = sum(feridos_graves), Mortos = sum(mortos), ignorados = sum(ignorados),
                                                                  tSemVitimas = sum(classificacao_acidente == "Sem Vítimas"),tFeridas = sum(classificacao_acidente == "Com Vítimas Feridas"),
                                                                  tFatais = sum(classificacao_acidente == "Com Vítimas Fatais"),tIgnorado = sum(classificacao_acidente == "Ignorado")) %>% mutate(PropFeridosGrave = Graves/Feridos) %>% 
  ungroup() %>% mutate(Score = tSemVitimas + tIgnorado + tFeridas * 5 + tFatais * 10) %>% mutate(ScoreAdj = Score/Acidentes, BrMun = paste(br, municipio, sep = "_" )) %>%
  arrange(desc(Acidentes))

# Ajustando os dados para treinamento
acidentes <- acidentes %>% mutate(Desfecho_acidente = ifelse(mortos + feridos > 0,"1","0"),
                                  BrMun = paste(br, municipio, sep = "_")) #prestar atencao na proporcao, ja que possivelmente
# possa ter sido alterada devido aos filtros e remocao de NAs

Scores <- BRS_MUN %>% dplyr::select(19, 18)
acidentes <- left_join(acidentes, Scores, by = "BrMun")

acidentes_modelling <- acidentes %>% dplyr::select(1,3,4,12,13,14,15,16,17,36,38) %>% mutate(Final_Semana = ifelse(dia_semana == "6.sábado",1,ifelse(dia_semana == "7.domingo",1,0)))
acidentes_modelling <- acidentes_modelling %>% dplyr::select(-c(2,3))
acidentes_modelling$Desfecho_acidente <- as.factor(acidentes_modelling$Desfecho_acidente)

#reduzindo a amostragem por ela ser muito alta e custar muito processamento
samples <- acidentes_modelling$Desfecho_acidente %>% createDataPartition(p = 0.02, list = FALSE)
model_df <- acidentes_modelling[samples,]

train_samples <- model_df$Desfecho_acidente %>% createDataPartition(p = 0.75,list = FALSE)
train_data <- model_df[train_samples,] %>% dplyr::select(2:7,9:10,8)
test_data <- model_df[-train_samples,] %>% dplyr::select(2:7,9:10,8)



reg1 <- glm(Desfecho_acidente ~ ., family = binomial(link = "logit"),data = train_data)
summary(reg1)

library(mfx)
logitor(Desfecho_acidente ~., data = train_data) #razao de chances

exp(cbind(OR=coef(reg1),confint(reg1))) #intervalo de confianca

library(modEvA) #Pseudo R2
RsqGLM(reg1)

library(ResourceSelection)
hoslem.test(train_data$Desfecho_acidente, fitted(reg1),g=10)

library(pROC)
roc1 <- plot.roc(train_data$Desfecho_acidente, fitted(reg1))
plot(roc1,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)

# Prevendo valores
pred1 <- predict(reg1, newdata = test_data, type = "response")

# AUC-ROC com dados de predicao 1
pROC_obj <- roc(test_data$Desfecho_acidente, pred1, smoothed = TRUE,
                ci = TRUE, ci.alpha = 0.9, stratified = FALSE,
                plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE,
                grid = TRUE, print.auc = TRUE, show.thres = TRUE)

sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type = "shape", col = "lightblue")
plot(sens.ci, type = "bars")

pred1adj <- ifelse(pred1 > 0.454, 1, 0)
confusionMatrix(table(pred1adj, test_data$Desfecho_acidente), positive = "1")

# AUC-ROC com dados de predicao 2
library(precrec)

precrec_obj <- evalmod(scores = pred1, labels = test_data$Desfecho_acidente)
autoplot(precrec_obj)

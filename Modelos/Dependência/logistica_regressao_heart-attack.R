library(tidyverse)
library(caret)
library(mfx)
library(pROC)
library(ResourceSelection)
library(modEvA)
library(stargazer)
library(faraway)

#link: https://smolski.github.io/livroavancado/reglog.html#regressao-logistica-multipla

heart <- read.csv("C:\\Users\\NOTEBOOK CASA\\Downloads\\Heart Attack\\heart.csv",
                  encoding = "UTF-8",
                  sep = ",")

str(heart)

heart[,c(2,6,9,14)] <- lapply(heart[,c(2,6,9,14)], FUN = function(x){as.factor(x)})
heart[,c(3,7,11,13)] <- lapply(heart[,c(3,7,11,13)], FUN = function(x){as.character(x)})


prop.table(table(heart$output))

# glm - base
n <- nrow(heart)
n_train <- round(0.80 * n)
set.seed(589)
train_indices <- sample(1:n, n_train)
data_train <- heart[train_indices,]
data_test <- heart[-train_indices,]


logistica <- glm(output ~., data = data_train,
                 family = binomial(link = "logit"))

summary(logistica)
stargazer(logistica, title = "Resultados", type = "text")
logitor(output ~., data = data_train) #Estimando a razao de chances
exp(cbind(OR = coef(logistica), confint(logistica))) #Intervalo de confianca

# Previsao
previsao <- as.factor(ifelse(predict(logistica, newdata = data_test[,-14],
                                            type = "response")
                                    >0.5,"1","0"))

confusionMatrix(data_test$output, previsao, positive = "1")

roc1 <- plot.roc(data_train$output, fitted(logistica)) #ROC curve
plot(roc1,print.auc=TRUE, auc.polygon=TRUE, 
     grud=c(0.1,0.2),grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)

hoslem.test(data_train$output, fitted(logistica), g=10) #Hosmer-Lemeshow
RsqGLM(logistica)

step(logistica, direction = "both") #Stepwise
vif(logistica)

# Caret Training####
set.seed(123)
train_control <- trainControl(method = "repeatedcv", number = 10)

model <- train(output ~., data = heart,
               trControl = train_control,
               method = "glm", family = binomial())

print(model)

library(tidyverse)
library(survival)
library(haven)
library(survminer)

TelcoChurn <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Modelos\\Dependência\\Análise de Sobrevivência\\TelcoChurn\\WA_Fn-UseC_-Telco-Customer-Churn.csv")
attach(TelcoChurn)

glimpse(TelcoChurn)
summary(TelcoChurn)

TelcoChurn$Churn <- ifelse(TelcoChurn$Churn=="Yes",1,0)

# Analise de sobrevida
kmsurvival <- survfit(Surv(TelcoChurn$tenure, TelcoChurn$Churn)~1)
summary(kmsurvival)

ggsurvplot(survfit(Surv(TelcoChurn$tenure, TelcoChurn$Churn)~1,data=TelcoChurn), 
           palette = "blue",ggtheme = theme_minimal(),
           linetype = 1, surv.median.line = "hv",
           risk.table = TRUE, cumevents = TRUE,
           cumcensor = TRUE, tables.height = 0.1)

plot(kmsurvival, xlab = "Time", ylab = "Survival Probability")

# Kaplan-Meier nao parametrica por grupo
kmsurvivalgroup <- survfit(Surv(TelcoChurn$tenure, TelcoChurn$Churn)~TelcoChurn$gender)
summary(kmsurvivalgroup)
plot(kmsurvivalgroup, xlab = "Time", ylab = "Survival Probability")

# Nelson-Aalen non parametric analysis
nasurvival <- survfit(coxph(Surv(TelcoChurn$tenure, TelcoChurn$Churn)~1), type = "aalen")
summary(nasurvival)
plot(nasurvival, xlab = "Time", ylab = "Survival Probability")

# Cox proportional hazard model - coefficientes and hazard rates
# Regressao de cox 
coxphh <- coxph(Surv(tenure, Churn) ~  gender + SeniorCitizen + Partner + 
                  Dependents + PhoneService + MultipleLines + InternetService +
                  OnlineSecurity + OnlineBackup + DeviceProtection +
                  TechSupport + StreamingTV + StreamingMovies +
                  Contract + PaperlessBilling + PaymentMethod +
                  MonthlyCharges + TotalCharges, method = "breslow")

summary(coxphh)

# Weibull for estimating smooth survival curves
wb <- survreg(Surv(tenure, Churn) ~1, TelcoChurn)
coef(wb)


Teste <- TelcoChurn[5698:5742,]

predicoes <- predict(coxphh, newdata = Teste)
predicoes

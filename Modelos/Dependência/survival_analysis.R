library(tidyverse)
library(survival)
library(haven)
library(survminer)
TempoFormaturaCox <- read_dta("C:/Users/NOTEBOOK CASA/Desktop/Modelos/z_Manual de Análise de Dados - Fávero/BancosdeDados/Cap. 17/TempoFormaturaCox.dta")

glimpse(TempoFormaturaCox)
summary(TempoFormaturaCox)

# Kaplan-Meier non parametric analysis
kmsurvival <- survfit(Surv(TempoFormaturaCox$tempomonitor, TempoFormaturaCox$status)~1)
summary(kmsurvival)
ggsurvplot(survfit(Surv(TempoFormaturaCox$tempomonitor, TempoFormaturaCox$status)~1,data=TempoFormaturaCox), 
           palette = "blue",ggtheme = theme_minimal(),
           linetype = 1, surv.median.line = "hv",
           risk.table = TRUE, cumevents = TRUE,
           cumcensor = TRUE, tables.height = 0.1)

plot(kmsurvival, xlab = "Time", ylab = "Survival Probability")

# Kaplan-Meier non parametric analysis by group
kmsurvivalgroup <- survfit(Surv(TempoFormaturaCox$tempomonitor, TempoFormaturaCox$status)~TempoFormaturaCox$bolsa)
summary(kmsurvivalgroup)
plot(kmsurvivalgroup, xlab = "Time", ylab = "Survival Probability")

# Nelson-Aalen non parametric analysis
nasurvival <- survfit(coxph(Surv(TempoFormaturaCox$tempomonitor, TempoFormaturaCox$status)~1), type = "aalen")
summary(nasurvival)
plot(nasurvival, xlab = "Time", ylab = "Survival Probability")


# Cox proportional hazard model - coefficientes and hazard rates
# Regressao de cox 
coxphh <- coxph(Surv(TempoFormaturaCox$tempomonitor, TempoFormaturaCox$status) ~ TempoFormaturaCox$idade + TempoFormaturaCox$bolsa, method = "breslow")
summary(coxphh)

plot(kmsurvival, main = "Sobrevida", ylab = "Estimativas de S(T)", xlab = "Tempo")
#lines(survfit(coxphh, type = "breslow", newdata = ))


# Weibull for estimating smooth survival curves
wb <- survreg(Surv(tempomonitor, status) ~1, TempoFormaturaCox)
coef(wb)

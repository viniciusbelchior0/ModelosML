library(nbastatR)
library(tidyverse)
library(ggpubr)
library(summarytools)
library(rpart)
library(rstatix)

#cuidado pois o plyr pode overload a funcao summarise e ela nao vai funcionar properly
#1. Total of points by team and Total (sum of the teams) ####

teams <- game_logs(seasons = 2021, result_types = c("team")) %>%
  filter(slugTeam == "TOR" | slugTeam == "BKN") %>% 
  select("yearSeason", "slugSeason","typeSeason","dateGame",
         "nameTeam","locationGame","slugTeam","slugOpponent", "outcomeGame", "isWin",
         "fgmTeam","fgaTeam", "pctFGTeam", "fg3mTeam", "fg3aTeam",
         "pctFG3Team", "pctFTTeam", "fg2mTeam", "fg2aTeam", "pctFG2Team",
         "ftmTeam","ftaTeam", "orebTeam", "drebTeam", "trebTeam",
         "astTeam", "stlTeam", "blkTeam", "tovTeam",  "pfTeam", "ptsTeam",
         "plusminusTeam") %>% mutate(ptsOpp = ifelse(isWin == TRUE, ptsTeam - plusminusTeam,ptsTeam + abs(plusminusTeam)),
                                     ptsTot = ptsTeam + ptsOpp)

teams <- teams[1:122,]

#descriptive statistics - choose the one with less variance
teams %>% dplyr::group_by(nameTeam,locationGame) %>% dplyr::summarize(avg_pts = mean(ptsTeam),
                                                                      cv_pts = sd(ptsTeam)/mean(ptsTeam)*100,
                                                                      med_plms = median(plusminusTeam),
                                                                      sd_plms = sd(plusminusTeam),
                                                                      avgtot = mean(ptsTot),
                                                                      cvtot = sd(ptsTot)/mean(ptsTot)*100)

des <- descr(teams[,c("ptsTeam","plusminusTeam","ptsOpp","ptsTot")])
des <- des[c(1,2,5,10),]
options(scipen=999)
options(digits = 2) #plusminus nao e boa metrica pois tem muita variacao


#Anova para diferenca de pontos (equipe, sofridos e totais) entre times e 
#localidade dos jogos
teams_anova <- teams %>% select(nameTeam, locationGame, ptsTeam,plusminusTeam,ptsTot)
teams_anova$nameTeam <- as.factor(teams_anova$nameTeam)
teams_anova$locationGame <- as.factor(teams_anova$locationGame)

#1.1 - teste de normalidade
ggqqplot(teams_anova$ptsTot)
hist(teams_anova$ptsTot)
teams_anova %>% group_by(nameTeam, locationGame) %>%
  shapiro_test(ptsTot)
# HO = values are normaly distributed; H1 = values arent normaly distributed
# if p-value > 0.05 we can assume normality
# discussao: normalidade deve ser avaliada por grupo ou por todo (grupo melhor)

#1.2 - existencia de outliers
ggplot(teams_anova) +
  aes(x = nameTeam, y = ptsTot, fill = nameTeam) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  theme_gray() +
  facet_wrap(vars(locationGame))

out <- teams_anova %>% group_by(nameTeam, locationGame) %>%
  identify_outliers(ptsTot)
# tratando os outliers
teams_anova <- teams_anova %>% filter(ptsTot > 167 & ptsTot < 286)

#1.3 teste de homogeneidade das variancias
library(car)
leveneTest(ptsTot ~ nameTeam*locationGame, data = teams_anova, center = mean)
# H0 = as variancias sao homogeneas (homocedasticidade); H1 = as variancias nao
# sao homogeneas (heterocedasticidade)
# if p-value > 0.05 we can assume equal variance

# a verificacao dos residuos pode ser feita apos a construcao do modelo
anova_teams <- aov(ptsTot ~ nameTeam*locationGame, teams_anova)

shapiro.test(anova_teams$residuals) #normalidade

boxplot(anova_teams$residuals) #outliers
teams_anova$residuals <- anova_teams$residuals
teams_anova %>% group_by(nameTeam, locationGame) %>%
  identify_outliers(ptsTot)

leveneTest(residuals ~ nameTeam*locationGame, teams_anova, center = mean)#homocedasticidade


#ANALISANDO O MODELO - ANOVA
options(contrasts = c("contr.sum","contr.poly"))
summary(anova_teams)
Anova(anova_teams, type = "III") #results of anova
#location game e o unico fator significante, porem pouco
#nao ha interacao entre o time e o local que ele joga

#grafico de interacao
ggplot(teams_anova, aes(x = locationGame, y = ptsTot, group = nameTeam, color = nameTeam)) +
  geom_line(stat = "summary", fun.data = "mean_se", size = 0.6) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2)

#estimated marginal mean
library(emmeans)
teams_anova %>% group_by(nameTeam) %>%
  emmeans_test(ptsTot ~ locationGame, p.adjust.method = "bonferroni")
# outra opcao: correcao de sidak

teams_anova %>% group_by(locationGame) %>%
  emmeans_test(ptsTot ~ nameTeam, p.adjust.method = "bonferroni")

#Post Hoc
library(DescTools)
PostHocTest(anova_teams, method = "hsd")

#analise descritiva
teams_anova %>%
  group_by(nameTeam, locationGame) %>%
  get_summary_stats(ptsTot, type = "mean_sd")
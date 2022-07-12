# Séries Temporais ####

library(tidyverse)
library(tsibble) #manipulacao de series temporais
library(fable) #funcoes de previsao
library(feasts) #graficos e esttisticas de series temporais
library(tsibbledata) #series temporais tidy
library(fpp3) #todos os itens acima  e mais


# Arquivos da NBA

nba <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Analytics\\Bases\\Esportes\\NBA\\nba_boxscore_players.csv")
names(nba)

player<- nba %>% filter(namePlayer == "LeBron James") %>% dplyr::select(23,24,2,5,6,14,16,19,22,34,29,30,31,32,35,36,37,38,39,46,49,50,51,55,58)
summary(player)

player$pctFT <- ifelse(is.na(player$pctFT), 0, player$pctFT)
player$dateGame <- as.Date(player$dateGame)

player <- player %>% group_by(namePlayer,month = lubridate::floor_date(dateGame, "month")) %>%
  summarise(AvgPts = mean(pts, na.rm = TRUE),
            AvbMin = mean(minutes, na.rm = TRUE),
            Games = n())

player$month <- yearmonth(player$month)

player_ts <- player %>% as_tsibble(index = month, key = namePlayer) %>%
  mutate(MM_Pts = zoo::rollmean(AvgPts, k=3, fill = NA, align = "right"),
         MM_Min = zoo::rollmean(AvbMin, k=3, fill = NA, align = "right")) %>%
  fill_gaps(.full = TRUE)
  

# Plotando com ggplot
player_ts %>%
  autoplot(MM_Pts) + labs(x = "DateGame", y = "Pts", title = "Roll Mean of Player Pts by Month")

# Componentes de uma ST ##

#Tendencia: quando ha um aumento ou diminuicao ao longo do tempo
# Sazonalidade: quando uma serie  e influenciada pelo tempo
# Ciclo: quando a serie apresenta padroes (nao ocorre de forma equidistante/espacada no tempo)

## o momento dos picos na sazonalidade ele e mais previsivel do que do ciclo


player_ts %>% ACF(AvgPts, lag_max = 10) %>%
  autoplot()#funcao de autocorrelacao

# tendencia: decaimento da fac e bem lento
# sazonalidade: pico nos lags

# ruido branco: dados nao estao correlacinados ao tempo

# se os lags estao dentro das linhas do autoplot (intervaloes de confianca)
# --> a serie nao possui padrao nenhum ; processo aleatorio


# Transformacoes ###
# boxcox

player_ts %>% features(AvgPts, features = guerrero)# melhor lambda
player_ts %>% autoplot(box_cox(AvgPts, 0.662)) 


# Decomposicao ###

player_ts %>% model(STL(AvgPts)) %>%
  components()

player_ts %>% model(STL(AvgPts)) %>%
  components() %>% autoplot()


# Ajuste sazonal
# aditiva: Yt-St=Tt+Rt
# Multiplicativa: Yt/St= Tt*Rt

# PRevisao ###
# Benchmarks - modelos feitos para serem batidos (fracos)
#  *Mean() - media historica como valor de previsao
#  *Naive() - ultimo valor
#  *Snaive() -  ultimo valor para cada periodo
#  * RW - ultimo valor com crescimento medio (tendencia)

player_fit <- player_ts %>%
  model(Snaive = SNAIVE(AvgPts),
        Naive = NAIVE(AvgPts),
        Drift = RW(AvgPts ~ drift()),
        Mean = MEAN(AvgPts))

player_fit #mable - listas dos forecasts

# Para fazer uma previsao
player_forecast <- player_fit %>%
  forecast(h =15)

player_forecast

player_forecast %>% autoplot(player_ts, level = NULL)

augment(player_fit)

augment(player_fit) %>%
  filter(.model == "Snaive") %>%
  ACF(.resid) %>%
  autoplot()

# Teste de Ljung-box
# H0: os rresiduos sao independentes e identicamente distribuidos
# nao quero rejeitar H0

augment(player_fit) %>%
  features(.resid, ljung_box)

# Medidas de acuracia

modfit <- player_ts %>%
  filter(month <= yearmonth('2020-08-01')) %>% 
  model(Snaive = SNAIVE(AvgPts))

modfit %>% forecast(h = 20) %>%
  autoplot(player_ts, level = NULL)

# MAE, MSE, MAPE, RMSE
# MAPE e independente da escala

accuracy(player_forecast, player_ts)


# Exponential Smoothing ###

# Error Trend Season
# Erro: aditivo ou multiplicativo
# Tendencia: nenhuma, aditiva, ou multiplicativa ou amortecida
# sazonalidade: nenhuma, aditiva, ou multiplicativa

fit <- player_ts %>% model(ets = ETS(AvgPts))
fit
report(fit)
components(fit)
components(fit) %>% autoplot()

# Bootstrap

sim <- fit %>%
  generate(h = 14, times =10,  )##

# Arima

fit_ARIMA <- player_ts %>%
  model(arima = ARIMA(AvgPts))

fit_ARIMA
report(fit_ARIMA)

fit_ARIMA %>%
  forecast(h = 20) %>%
  autoplot(player_ts)


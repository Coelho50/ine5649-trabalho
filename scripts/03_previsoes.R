# scripts/03_previsoes.R
# ------------------------------------------------------------
# PREVISÕES DE CONSUMO – MODELOS ARIMA E ARIMAX
# ------------------------------------------------------------

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

# 1) Carregar base analítica (com classes separadas) ----------
dados <- readRDS("results/base_sp_analitica.rds")

# Garantir ordenação por data
dados <- dados |> arrange(ano_mes)

# Criar ts para cada classe
ts_res   <- ts(dados$consumo_residencial, start = c(2015,1), frequency = 12)
ts_ind   <- ts(dados$consumo_industrial,  start = c(2015,1), frequency = 12)
ts_com   <- ts(dados$consumo_comercial,   start = c(2015,1), frequency = 12)

# Total SP (somando classes)
dados <- dados |> 
  mutate(consumo_total = consumo_residencial + consumo_industrial + consumo_comercial)

ts_total <- ts(dados$consumo_total, start = c(2015,1), frequency = 12)


# ------------------------------------------------------------
# 2) MODELOS ARIMA (simples, cada classe)
# ------------------------------------------------------------

fit_res <- auto.arima(ts_res)
fit_ind <- auto.arima(ts_ind)
fit_com <- auto.arima(ts_com)
fit_tot <- auto.arima(ts_total)

# Previsões para 12 meses
prev_res <- forecast(fit_res, h = 12)
prev_ind <- forecast(fit_ind, h = 12)
prev_com <- forecast(fit_com, h = 12)
prev_tot <- forecast(fit_tot, h = 12)

# Plotar
autoplot(prev_res) + ggtitle("Previsão – Residencial (ARIMA)")
autoplot(prev_ind) + ggtitle("Previsão – Industrial (ARIMA)")
autoplot(prev_com) + ggtitle("Previsão – Comercial (ARIMA)")
autoplot(prev_tot) + ggtitle("Previsão – Total SP (ARIMA)")


# ------------------------------------------------------------
# 3) MODELO ARIMAX – Só Residencial (melhor série para prever)
#     Regressoras: temperatura média + carga SECO
# ------------------------------------------------------------

# Criar matriz X (regressoras)
xreg <- cbind(
  temp = dados$temp_media_mes,
  carga = dados$carga_mwmed
)

# Ajustar ARIMAX
fit_arimax <- auto.arima(ts_res, xreg = xreg)

# Para prever 12 meses você PRECISA de xreg futuro:
# Aqui, criarei futuros "neutros": usando média histórica
# (até você inserir previsão real de INMET + ONS)

future_xreg <- matrix(
  c(rep(mean(dados$temp_media_mes, na.rm = TRUE), 12),
    rep(mean(dados$carga_mwmed, na.rm = TRUE), 12)),
  ncol = 2
)

colnames(future_xreg) <- c("temp", "carga")

prev_arimax <- forecast(fit_arimax, xreg = future_xreg, h = 12)

# Plot
autoplot(prev_arimax) +
  ggtitle("Previsão Residencial – ARIMAX (Temp + Carga)") 


# ------------------------------------------------------------
# 4) Salvar modelos e previsões
# ------------------------------------------------------------

saveRDS(fit_res,    "results/model_arima_residencial.rds")
saveRDS(fit_ind,    "results/model_arima_industrial.rds")
saveRDS(fit_com,    "results/model_arima_comercial.rds")
saveRDS(fit_tot,    "results/model_arima_total.rds")

saveRDS(prev_res,   "results/prev_residencial_arima.rds")
saveRDS(prev_ind,   "results/prev_industrial_arima.rds")
saveRDS(prev_com,   "results/prev_comercial_arima.rds")
saveRDS(prev_tot,   "results/prev_total_arima.rds")

saveRDS(fit_arimax, "results/model_arimax_residencial.rds")
saveRDS(prev_arimax, "results/prev_residencial_arimax.rds")

cat(">> Previsões salvas em results/*.rds\n")


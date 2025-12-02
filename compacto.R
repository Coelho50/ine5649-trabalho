library(tidyverse)
library(forecast)
library(lubridate)
library(ggplot2)
library(gridExtra)

dados <- readRDS("results/base_sp_analitica.rds") %>% 
  filter(ano_mes >= "2015-01-01")

ts_res <- ts(dados$consumo_residencial, start = c(2015, 1), frequency = 12)
ts_ind <- ts(dados$consumo_industrial, start = c(2015, 1), frequency = 12)

# Divisão Treino vs Teste
tam <- length(ts_res)
treino_res <- subset(ts_res, end = tam - 12); teste_res <- subset(ts_res, start = tam - 11)
treino_ind <- subset(ts_ind, end = tam - 12); teste_ind <- subset(ts_ind, start = tam - 11)


#SELEÇÃO DE VARIÁVEIS

# --- SETOR RESIDENCIAL ---
df_res <- dados %>% filter(!is.na(consumo_residencial), !is.na(temp_media_mes))
full_res <- lm(consumo_residencial ~ carga_mwmed + temp_media_mes + nom_bandeira_acionada, data=df_res)
step_res <- step(full_res, direction="backward", trace=0)

print(">>> Variáveis Selecionadas para RESIDENCIAL:")
print(summary(step_res)$coefficients)

# --- SETOR INDUSTRIAL ---
df_ind <- dados %>% filter(!is.na(consumo_industrial), !is.na(temp_media_mes))
full_ind <- lm(consumo_industrial ~ carga_mwmed + temp_media_mes + nom_bandeira_acionada, data=df_ind)
step_ind <- step(full_ind, direction="backward", trace=0)

print(">>> Variáveis Selecionadas para INDUSTRIAL:")
print(summary(step_ind)$coefficients)


# MÉTODO DE SELEÇÃO DE MODELO
get_mape <- function(model, test_data) {
  mean(abs((test_data - forecast(model, h=12)$mean)/test_data)) * 100
}

# Treino Candidatos
# Residencial
m_res_sarima <- auto.arima(treino_res, seasonal=TRUE)
m_res_arima  <- auto.arima(treino_res, seasonal=FALSE)
m_res_hw     <- hw(treino_res, seasonal="multiplicative")

# Industrial
m_ind_sarima <- auto.arima(treino_ind, seasonal=TRUE)
m_ind_arima  <- auto.arima(treino_ind, seasonal=FALSE)
m_ind_hw     <- hw(treino_ind, seasonal="multiplicative")

# Tabela de Resultados
tabela_erros <- data.frame(
  Modelo = c("SARIMA (Sazonal)", "ARIMA (Sem Sazonal)", "Holt-Winters"),
  Erro_Residencial = c(get_mape(m_res_sarima, teste_res),
                       get_mape(m_res_arima, teste_res),
                       get_mape(m_res_hw, teste_res)),
  Erro_Industrial  = c(get_mape(m_ind_sarima, teste_ind),
                       get_mape(m_ind_arima, teste_ind),
                       get_mape(m_ind_hw, teste_ind))
)

print(">>> Tabela de Erros (MAPE):")
print(tabela_erros)


#  VISUALIZAÇÃO DOS ERROS (BARRAS)

df_mape <- data.frame(
  Setor = c(rep("Residencial", 3), rep("Industrial", 3)),
  Modelo = c("SARIMA", "Sem Sazonal", "Holt-Winters", 
             "SARIMA", "Sem Sazonal", "Holt-Winters"),
  MAPE = c(tabela_erros$Erro_Residencial, tabela_erros$Erro_Industrial)
)

g_erro <- ggplot(df_mape, aes(x = reorder(Modelo, MAPE), y = MAPE, fill = Setor)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(MAPE, 2), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, fontface = "bold") +
  facet_wrap(~Setor, scales = "free_x") +
  scale_fill_manual(values = c("firebrick", "steelblue")) +
  labs(title = "Erros nos Modelos (Validação)",
       y = "Erro Médio (%)", x = "") +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 12, face="bold"))

print(g_erro)


# GRÁFICO DE VALIDAÇÃO (TESTE vs REAL)
mod_camp_res <- auto.arima(treino_res, seasonal = TRUE)
prev_teste_res <- forecast(mod_camp_res, h = 12, level = 95)
prev_teste_ind <- hw(treino_ind, seasonal = "multiplicative", h = 12, level = 95)

datas_teste <- seq(as.Date("2015-01-01"), by = "month", length.out = length(ts_res)) %>% 
  tail(12) 

montar_df <- function(prev_obj, real_data, nome_setor) {
  data.frame(
    Data = datas_teste,
    Setor = nome_setor,
    Real = as.numeric(real_data),
    Previsto = as.numeric(prev_obj$mean),
    Lower = as.numeric(prev_obj$lower), 
    Upper = as.numeric(prev_obj$upper)
  )
}

df_validacao <- bind_rows(
  montar_df(prev_teste_res, teste_res, "1. Residencial (SARIMA)"),
  montar_df(prev_teste_ind, teste_ind, "2. Industrial (Holt-Winters)")
)

g_validacao <- ggplot(df_validacao, aes(x = Data)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Setor), alpha = 0.2) +
  geom_line(aes(y = Previsto, color = "Modelo"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Real, color = "Real"), size = 1.2) +
  facet_wrap(~Setor, scales = "free_y") +
  scale_color_manual(values = c("Modelo" = "red", "Real" = "black")) +
  scale_fill_manual(values = c("steelblue", "firebrick")) + 
  labs(title = "Teste de Aderência: Real vs Modelo (IC 95%)",
       y = "Consumo (MWh)", x = "", color = "Legenda", fill = "Setor") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        strip.text = element_text(face="bold", size=12))

print(g_validacao)



# RESÍDUOS

# Diagnóstico Residencial (SARIMA)
checkresiduals(mod_camp_res)

# Diagnóstico Industrial (Holt-Winters)
mod_hw_check <- hw(treino_ind, seasonal = "multiplicative")
checkresiduals(mod_hw_check)


# 7. PREVISÃO DO FUTURO

# RESIDENCIAL (Vencedor: SARIMA) - Treina na base completa
modelo_final_res <- auto.arima(ts_res, seasonal = TRUE)
prev_futuro_res  <- forecast(modelo_final_res, h = 12)

# INDUSTRIAL (Vencedor: HOLT-WINTERS) - Treina na base completa
prev_futuro_ind <- hw(ts_ind, seasonal = "multiplicative", h = 12)

# Plot Residencial
p1 <- autoplot(prev_futuro_res) +
  autolayer(prev_futuro_res$mean, series="Previsão", size=1.2) +
  labs(title = "Previsão RESIDENCIAL (12 Meses)", 
       subtitle = "Modelo Escolhido: SARIMA", 
       y="MWh", x="") +
  coord_cartesian(xlim = c(2023, 2025)) +
  theme_minimal() + 
  theme(legend.position="none", plot.title = element_text(color="steelblue", face="bold"))

# Plot Industrial
p2 <- autoplot(prev_futuro_ind) +
  autolayer(prev_futuro_ind$mean, series="Previsão", size=1.2) +
  labs(title = "Previsão INDUSTRIAL (12 Meses)", 
       subtitle = "Modelo Escolhido: Holt-Winters", 
       y="MWh", x="") +
  coord_cartesian(xlim = c(2023, 2025)) +
  theme_minimal() + 
  theme(legend.position="none", plot.title = element_text(color="firebrick", face="bold"))

# Exibir juntos
grid.arrange(p1, p2, ncol = 1)

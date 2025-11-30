# scripts/02_analises_iniciais.R
# -------------------------------------------------------------------
# ANÁLISES INICIAIS DO PROJETO
# Este script produz gráficos exploratórios e modelos econométricos
# sobre consumo de energia elétrica no Estado de São Paulo.
# Base: dados de clima, consumo, carga do ONS e bandeiras tarifárias.
# -------------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# ===================================================================
# 1) Carregar bases já limpas (geradas no script 01_input_dados.R)
# ===================================================================

# Base agregada mensal completa (2004–2023)
base_sp <- readRDS("results/base_sp.rds")

# Sub-bases para análises específicas
base_sp_tudo      <- readRDS("results/base_sp_tudo.rds")       # 2004–2023
base_sp_clima     <- readRDS("results/base_sp_clima.rds")      # 2010–2023
base_sp_completo  <- readRDS("results/base_sp_completo.rds")   # 2015–2023
base_sp_analitica <- readRDS("results/base_sp_analitica.rds")  # inclui classes

# Conferir estrutura dos dados
str(base_sp)
summary(base_sp)
range(base_sp$ano_mes)

# ===================================================================
# 2) Série temporal — Consumo total SP (2004–2023)
# ===================================================================
# Objetivo: visualizar a evolução histórica do consumo agregado.

ggplot(base_sp_tudo, aes(x = ano_mes, y = consumo_mwh)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Consumo mensal de energia elétrica - SP (2004–2023)",
    x = "Ano-mês",
    y = "Consumo (MWh)"
  ) +
  theme_minimal()

# ===================================================================
# 3) Relação Consumo x Temperatura (Clima afeta consumo?)
# ===================================================================
# Pergunta analisada: "Meses mais quentes aumentam o consumo de energia?”
# Base filtrada a partir de 2010, quando começam dados de clima.
#Pontos são os meses, mais para direita = +Temp; mais para alto = +Consumo
#conclusao: Tem tendencia mas tem ruído

ggplot(base_sp_clima, aes(x = temp_media_mes, y = consumo_mwh)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Consumo vs Temperatura média - SP (2010–2023)",
    x = "Temperatura média do mês (°C)",
    y = "Consumo (MWh)"
  ) +
  theme_minimal()

# ===================================================================
# 4) Consumo x Carga SE/CO — indicador da atividade econômica
# ===================================================================
# Interpretação:
#   - A carga do ONS representa “demanda do sistema”.
#   - Alta correlação sugere que economia/atividade movimenta consumo.
#Esse gráfico prova que São Paulo é o "motor" do subsistema. Forte correlação entre eles

ggplot(base_sp_tudo, aes(x = carga_mwmed, y = consumo_mwh)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Consumo SP vs Carga Sudeste/Centro-Oeste (2004–2023)",
    x = "Carga média mensal SE/CO (MW médios)",
    y = "Consumo SP (MWh)"
  ) +
  theme_minimal()

# ===================================================================
# 5) Consumo colorido por bandeira (impacto das tarifas)
# ===================================================================
# Aqui investigamos se a adoção de bandeiras (verde → vermelha)
# está associada a mudanças perceptíveis de consumo.
# -------------------------------------------------------------------
# 1. CORREÇÃO DO NOME (Tirar o "?")
# -------------------------------------------------------------------
base_sp_completo <- base_sp_completo |>
  mutate(nom_bandeira_acionada = if_else(
    nom_bandeira_acionada == "Escassez H?drica", 
    "Escassez Hídrica",  # Nome ajustado
    nom_bandeira_acionada 
  ))

# -------------------------------------------------------------------
# 2. DEFINIÇÃO DAS CORES (Com os nomes exatos que vimos)
# -------------------------------------------------------------------
cores_bandeiras <- c(
  "Verde"            = "forestgreen",
  "Amarela"          = "gold", 
  "Vermelha P1"      = "orange",    
  "Vermelha P2"      = "firebrick", 
  "Escassez Hídrica" = "black"      # A pior de todas
)

# -------------------------------------------------------------------
# 3. PLOTAGEM
# -------------------------------------------------------------------
ggplot(base_sp_completo, aes(x = ano_mes, y = consumo_mwh)) +
  # Linha de fundo cinza
  geom_line(color = "grey60", alpha = 0.6, size = 0.8) +
  
  # Pontos coloridos
  geom_point(aes(color = nom_bandeira_acionada), size = 2.5) +
  
  # Aplicação das cores manuais
  scale_color_manual(values = cores_bandeiras) +
  
  # Legendas e Títulos
  labs(
    title = "Evolução do Consumo SP e as Bandeiras Tarifárias",
    subtitle = "Linha cinza: Trajetória do consumo | Cores: Tarifa vigente",
    x = "Ano",
    y = "Consumo (MWh)",
    color = "Bandeira Vigente"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ===================================================================
# 6) Distribuição do consumo por bandeira — Boxplot
# ===================================================================
# Ajuda a responder:
#   “As pessoas consomem menos quando a bandeira é vermelha?”

# Ordenar as bandeiras da mais barata para a mais cara
base_sp_completo <- base_sp_completo |>
  mutate(nom_bandeira_acionada = factor(nom_bandeira_acionada, levels = c(
    "Verde", 
    "Amarela", 
    "Vermelha P1", "Vermelha Patamar 1", 
    "Vermelha P2", "Vermelha Patamar 2",
    "Escassez Hídrica"
  )))

ggplot(base_sp_completo,
       aes(x = nom_bandeira_acionada, y = consumo_mwh)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(
    title = "Distribuição do consumo por bandeira tarifária – SP",
    x = "Bandeira",
    y = "Consumo mensal (MWh)"
  ) +
  theme_minimal()

#Observa-se um comportamento inelástic,  mesmo com a tarifa punitiva (Escassez Hídrica), a demanda se mantém alta, 
#pois essas tarifas coincidem com períodos de altas temperaturas. -> conforto térmico

# ===================================================================
# 7) Consumo per capita — eficiência energética
# ===================================================================
# Pergunta: “O consumo por cliente está aumentando, caindo ou estável?” -> caindo
# A crise econômica de 2015-2016 fez as famílias economizarem e a indústria desacelerar.
#Isso prova que o consumo per capita é um termômetro da riqueza da população.
#Quem sabe substituindo por geladeiras mais economicas usando mais LED tambem

base_sp_tudo <- base_sp_tudo |>
  mutate(consumo_per_capita = consumo_mwh / numero_consumidores)

ggplot(base_sp_tudo, aes(x = ano_mes, y = consumo_per_capita)) +
  geom_line(color = "purple") +
  labs(
    title = "Consumo médio por consumidor – SP (2004–2023)",
    x = "Ano-mês",
    y = "MWh por consumidor"
  ) +
  theme_minimal()

# ===================================================================
# 8) Sazonalidade — padrão mensal (clima + hábitos)
# ===================================================================
# Exemplo:
#   - verão → ar-condicionado
#   - inverno → banho quente

base_sp_tudo <- base_sp_tudo |>
  mutate(mes = month(ano_mes, label = TRUE, abbr = TRUE))

ggplot(base_sp_tudo, aes(x = mes, y = consumo_mwh)) +
  geom_boxplot(fill = "khaki", alpha = 0.8) +
  labs(
    title = "Sazonalidade do consumo – SP (2004–2023)",
    x = "Mês",
    y = "Consumo (MWh)"
  ) +
  theme_minimal()

#Isso prova que São Paulo tem um perfil de consumo dominado pela Refrigeração (Verão) e não pelo Aquecimento.
#Marco e abril ainda ta calor e todos voltaram a trabalhar/estudar -> mais consumo?
#Tamanho das caixas mostram a volatillidade e previsibilidade 



#####################################################################################
# ===================================================================
# 9) MODELO 2.0 — regressão com log-transform (melhor especificação)
# ===================================================================
# Especificação:
#   log(consumo) = log(carga) + temperatura + bandeira
#
# Interpretação:
#   log-log → elasticidade
#   temperatura → efeito climático
#   bandeira → impacto no bolso (preço)

base_modelo <- base_sp_completo |>
  filter(
    !is.na(temp_media_mes),
    !is.na(carga_mwmed),
    !is.na(nom_bandeira_acionada)
  ) |>
  mutate(
    bandeira    = factor(nom_bandeira_acionada),
    log_consumo = log(consumo_mwh),
    log_carga   = log(carga_mwmed)
  )

modelo2 <- lm(log_consumo ~ log_carga + temp_media_mes + bandeira,
              data = base_modelo)

summary(modelo2)

saveRDS(modelo2, "results/modelo_lm_bandeira.rds")


# ===================================================================
# 11) Consumo por classe — visualização comparativa
# ===================================================================
# Objetivo:
#   Mostrar como cada classe (Residencial, Industrial, Comercial)
#   responde de forma diferente ao clima/economia.

cols_consumo <- grep("^consumo_", names(base_sp_analitica), value = TRUE)

base_classes_long <- base_sp_analitica |>
  select(ano_mes, all_of(cols_consumo)) |>
  tidyr::pivot_longer(
    cols = starts_with("consumo_"),
    names_to = "classe",
    values_to = "consumo_mwh"
  ) |>
  mutate(
    classe = stringr::str_remove(classe, "^consumo_"),
    classe = stringr::str_to_title(classe)
  )

ggplot(base_classes_long,
       aes(x = ano_mes, y = consumo_mwh, color = classe)) +
  geom_line() +
  labs(
    title = "Consumo mensal por classe – SP (2015–2023)",
    x = "Ano-mês",
    y = "Consumo (MWh)",
    color = "Classe"
  ) +
  theme_minimal()

# ===================================================================
# 12) Correlação: temperatura x classes
# ===================================================================
# Mostra quem reage ao clima — esperado:
#   residencial → muito
#   industrial → pouco ou nada

cor_residencial <- cor(
  base_sp_analitica$temp_media_mes,
  base_sp_analitica$consumo_residencial,
  use = "complete.obs"
)

cor_industrial <- cor(
  base_sp_analitica$temp_media_mes,
  base_sp_analitica$consumo_industrial,
  use = "complete.obs"
)

cor_comercial <- cor(
  base_sp_analitica$temp_media_mes,
  base_sp_analitica$consumo_comercial,
  use = "complete.obs"
)

print(cor_comercial)
print(cor_residencial)
print(cor_industrial)

#Enquanto os setores de comércio e residência apresentam correlação positiva com a temperatura (> 0.5), 
#o setor industrial apresenta correlação inversa, reforçando a heterogeneidade da demanda

# ===================================================================
# 13) Modelos individuais por classe
# ===================================================================
# Este é o modelo mais importante para análise de bandeiras.

base_resid <- base_sp_analitica |>
  mutate(
    bandeira    = factor(nom_bandeira_acionada),
    log_cons_res = log(consumo_residencial),
    log_carga    = log(carga_mwmed)
  )

modelo_residencial <- lm(
  log_cons_res ~ log_carga + temp_media_mes + bandeira,
  data = base_resid
)

summary(modelo_residencial)
saveRDS(modelo_residencial, "results/modelo_residencial.rds")

# Preparando a base Industrial ###################################################
base_ind <- base_sp_analitica |>
  filter(
    !is.na(consumo_industrial),
    !is.na(carga_mwmed), 
    !is.na(temp_media_mes)
  ) |>
  mutate(
    bandeira     = factor(nom_bandeira_acionada),
    log_cons_ind = log(consumo_industrial),
    log_carga    = log(carga_mwmed)
  )

# Rodando o modelo
modelo_industrial <- lm(
  log_cons_ind ~ log_carga + temp_media_mes + bandeira, 
  data = base_ind
)

# O Raio-X completo
summary(modelo_industrial)
saveRDS(modelo_industrial, "results/modelo_industrial_detalhado.rds")

# Preparando a base Comercial ###################################################
base_com <- base_sp_analitica |>
  filter(
    !is.na(consumo_comercial),
    !is.na(carga_mwmed), 
    !is.na(temp_media_mes)
  ) |>
  mutate(
    bandeira     = factor(nom_bandeira_acionada),
    log_cons_com = log(consumo_comercial),
    log_carga    = log(carga_mwmed)
  )

# Rodando o modelo
modelo_comercial <- lm(
  log_cons_com ~ log_carga + temp_media_mes + bandeira, 
  data = base_com
)

# O Raio-X completo
summary(modelo_comercial)
saveRDS(modelo_comercial, "results/modelo_comercial_detalhado.rds")
# ===================================================================
# 14) Comparação direta dos 3 modelos (Residencial / Industrial / Comercial)
# ===================================================================
# Objetivo:
#   Mostrar, lado a lado, que o efeito global do clima e das bandeiras
#   varia  entre as classes.

library(broom)
library(stargazer)

dados <- base_sp_analitica |>
  mutate(bandeira = factor(nom_bandeira_acionada))

mod_res <- lm(log(consumo_residencial) ~ log(carga_mwmed) + temp_media_mes + bandeira, data = dados)
mod_ind <- lm(log(consumo_industrial)  ~ log(carga_mwmed) + temp_media_mes + bandeira, data = dados)
mod_com <- lm(log(consumo_comercial)   ~ log(carga_mwmed) + temp_media_mes + bandeira, data = dados)

stargazer(
  mod_res, mod_ind, mod_com,
  type = "text",
  column.labels = c("Residencial", "Industrial", "Comercial"),
  digits = 3,
  title = "Comparação dos modelos por classe"
)

# ===================================================================
# 15) Visual — impacto da temperatura em cada classe
# ===================================================================
# Gráfico-chave do projeto:
#   mostra que o Residencial é muito sensível ao clima,
#   enquanto Industrial não reage.

coefs <- bind_rows(
  tidy(mod_res) |> mutate(modelo = "Residencial"),
  tidy(mod_ind) |> mutate(modelo = "Industrial"),
  tidy(mod_com) |> mutate(modelo = "Comercial")
) |>
  filter(term == "temp_media_mes")

ggplot(coefs, aes(x = modelo, y = estimate, fill = modelo)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = estimate - std.error, ymax = estimate + std.error),
    width = 0.2
  ) +
  labs(
    title = "Impacto da Temperatura no Consumo por Classe",
    subtitle = "Coeficiente estimado da variável temp_media_mes",
    y = "Efeito (escala log)",
    x = "Classe"
  ) +
  theme_minimal()

#Comercial e o unico setor que realmente gasta mais porque esquentou (ex: shoppings)
#Quando a temperatura sobe, a indústria gasta menos. -> CALENDARIO -> Jan/Fev -> ferias coletivas
#A barra azul mostra que o consumo residencial segue a manada. As casas já estão representadas dentro da Carga Geral do Sistema. 
#O efeito 'extra' da temperatura acabou sendo diluído na analise do residencial


# scripts/01_input_dados.R
# -----------------------------------

library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(janitor)
library(stringr)
library(tidyr)

# -------------------------------------------------
# 1) CLIMA SP DIÁRIO → MENSAL  (sp_dados.csv - INMET API)
# -------------------------------------------------

temperatura_raw <- readr::read_csv2(
  "data/sp_dados.csv",
  skip = 9,
  locale = locale(decimal_mark = ","),
  na = c("", "NA", "null"),
  show_col_types = FALSE
) |>
  janitor::clean_names() |>
  select(-matches("^x\\d+$"))

temperatura_sp_diario <- temperatura_raw |>
  mutate(
    data = as.Date(data_medicao)
  ) |>
  transmute(
    data,
    chuva_total_dia = precipitacao_total_diario_mm,
    temp_max_dia    = temperatura_maxima_diaria_c,
    temp_med_dia    = temperatura_media_compensada_diaria_c,
    temp_min_dia    = temperatura_minima_diaria_c
  )

temperatura_sp_mensal <- temperatura_sp_diario |>
  mutate(
    ano_mes = lubridate::floor_date(data, "month")
  ) |>
  group_by(ano_mes) |>
  summarise(
    temp_max_mes    = max(temp_max_dia, na.rm = TRUE),
    temp_min_mes    = min(temp_min_dia, na.rm = TRUE),
    temp_media_mes  = mean(temp_med_dia, na.rm = TRUE),
    chuva_total_mes = sum(chuva_total_dia, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------
# 2) CARGA MENSAL ONS  (carga_mensal.xlsx)
# -------------------------------------------------

carga <- read_excel("data/carga_mensal.xlsx") |>
  clean_names() |>
  rename(
    subsistema = matches("nom_"),
    data_ref   = matches("din_"),
    carga_mw   = matches("val_")
  ) |>
  mutate(
    data_ref = as.Date(data_ref),
    ano_mes  = floor_date(data_ref, "month"),
    subsistema_clean = stringr::str_to_upper(subsistema)
  )

print(unique(carga$subsistema_clean))

carga_seco_tmp <- carga |>
  filter(
    stringr::str_detect(subsistema_clean, "SUDESTE") |
      stringr::str_detect(subsistema_clean, "SE/CO") |
      stringr::str_detect(subsistema_clean, "SECO")
  )

if (nrow(carga_seco_tmp) == 0) {
  warning("Nenhum subsistema 'Sudeste/CO' encontrado. Usando TODA a carga da planilha.")
  carga_seco_tmp <- carga
}

carga_seco <- carga_seco_tmp |>
  group_by(ano_mes) |>
  summarise(
    carga_mwmed = mean(carga_mw, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------
# 3) CONSUMO MENSAL DE ENERGIA POR UF (MME)  -> SP
#     arquivo: consumo_energia_eletrica_uf.gz
#     (total + por classe)
# -------------------------------------------------

consumo_raw <- read_csv(
  "data/consumo_energia_eletrica_uf.gz",
  show_col_types = FALSE
) |>
  clean_names() |>
  rename(
    ano = ano,
    mes = mes,
    uf  = sigla_uf
  ) |>
  filter(uf == "SP") |>
  mutate(
    ano = as.integer(ano),
    mes = as.integer(mes),
    ano_mes = make_date(ano, mes, 1)
  )

# 3a) TOTAL SP (o que você já usava antes)
consumo_uf <- consumo_raw |>
  group_by(ano_mes) |>
  summarise(
    consumo_mwh         = sum(consumo, na.rm = TRUE),
    numero_consumidores = sum(numero_consumidores, na.rm = TRUE),
    .groups = "drop"
  )

# 3b) DETALHE POR CLASSE (long: ano_mes x tipo_consumo)
consumo_sp_classes <- consumo_raw |>
  group_by(ano_mes, tipo_consumo) |>
  summarise(
    consumo_mwh         = sum(consumo, na.rm = TRUE),
    numero_consumidores = sum(numero_consumidores, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(consumo_sp_classes, "results/consumo_sp_classes.rds")

# 3c) VERSÃO WIDE PARA ANÁLISE (uma coluna por classe)
consumo_classes_wide <- consumo_sp_classes |>
  mutate(
    tipo_consumo = tipo_consumo |>
      stringr::str_squish() |>
      tolower() |>
      stringr::str_replace_all("[^a-z0-9]+", "_"),      # troca espaço/pontuação por "_"
    tipo_consumo = paste0("consumo_", tipo_consumo)     # ex: consumo_residencial
  ) |>
  select(ano_mes, tipo_consumo, consumo_mwh) |>
  tidyr::pivot_wider(
    names_from  = tipo_consumo,
    values_from = consumo_mwh
  )

# -------------------------------------------------
# 4) BANDEIRAS TARIFÁRIAS  (bandeira_energia.csv)
# -------------------------------------------------

bandeiras <- read_delim(
  "data/bandeiras_energia.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  trim_ws = TRUE,
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(
    dat_competencia = as.Date(dat_competencia),
    ano_mes = floor_date(dat_competencia, "month")
  ) |>
  select(
    ano_mes,
    nom_bandeira_acionada,
    vlr_adicional_bandeira
  )

# -------------------------------------------------
# 4.1) BASE ANALÍTICA POR CLASSE (para modelos separados)
# -------------------------------------------------

base_sp_analitica <- consumo_classes_wide |>
  left_join(temperatura_sp_mensal, by = "ano_mes") |>
  left_join(bandeiras,              by = "ano_mes") |>
  left_join(carga_seco,             by = "ano_mes") |>
  filter(ano_mes >= as.Date("2015-01-01"))

saveRDS(base_sp_analitica, "results/base_sp_analitica.rds")

# -------------------------------------------------
# 5) CRIAR BASE UNIFICADA "GERAL" (consumo total + carga + clima + bandeira)
# -------------------------------------------------

base_sp <- consumo_uf |>
  left_join(carga_seco,            by = "ano_mes") |>
  left_join(temperatura_sp_mensal, by = "ano_mes") |>
  left_join(bandeiras,             by = "ano_mes") |>
  arrange(ano_mes)

# 5A) Sub-bases por período útil
base_sp_tudo <- base_sp

base_sp_clima <- base_sp |>
  filter(ano_mes >= as.Date("2010-01-01"))

base_sp_completo <- base_sp |>
  filter(ano_mes >= as.Date("2015-01-01"))

# -------------------------------------------------
# 6) SALVAR OBJETOS LIMPOS (em results/)
# -------------------------------------------------

if (!dir.exists("results")) dir.create("results")

saveRDS(temperatura_sp_mensal, "results/temperatura_sp_mensal.rds")
saveRDS(carga_seco,            "results/carga_seco.rds")
saveRDS(consumo_uf,            "results/consumo_sp_mensal.rds")
saveRDS(bandeiras,             "results/bandeiras.rds")
saveRDS(base_sp,               "results/base_sp.rds")

saveRDS(base_sp_tudo,          "results/base_sp_tudo.rds")
saveRDS(base_sp_clima,         "results/base_sp_clima.rds")
saveRDS(base_sp_completo,      "results/base_sp_completo.rds")

# novos para análise por classe:
saveRDS(consumo_sp_classes,    "results/consumo_sp_classes.rds")
saveRDS(consumo_classes_wide,  "results/consumo_classes_wide.rds")
saveRDS(base_sp_analitica,     "results/base_sp_analitica.rds")

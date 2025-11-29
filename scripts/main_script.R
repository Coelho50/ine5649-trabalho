
#Consumo em MWh
consumo_eletrico <- read.csv("br_mme_consumo_energia_eletrica_uf.csv.gz")

metereologia <- read.csv("br_inmet_bdmep_estacao.csv.gz")

#ler filtro_dados_climaticos.R
dados_clima_sp <- read.csv("microdados_inmet_sp_mirante.csv")

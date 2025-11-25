
#O script abaixo faz uma query em uma base de dados climaticos
#em :https://basedosdados.org/dataset/782c5607-9f69-4e12-b0d5-aa0f1a7a94e2?table=28d16282-d100-4ea8-9dde-36c05c8f1ca2
#NAO rodar caso os dados ja estejam filtrados da forma necessaria

if (!require("basedosdados")) install.packages("basedosdados")
library(basedosdados)
library(dplyr) #se precisar

set_billing_id("consumo-eletrico-479321") 

library(basedosdados)

query <- "
SELECT 
  data,
  -- pega a temperatura mais alta registrada entre todas as horas do dia
  MAX(temperatura_max) as temp_max_dia,
  
  -- pega a temperatura mais baixa registrada entre todas as horas do dia
  MIN(temperatura_min) as temp_min_dia,
  
  -- Pega a umidade mais alta do dia
  MAX(umidade_rel_max) as umidade_max_dia,
  
  -- pega a umidade mais baixa do dia
  MIN(umidade_rel_min) as umidade_min_dia,
  
  -- calcula a velocidade media do vento naquele dia
  AVG(vento_velocidade) as vento_medio_dia,
  
  -- soma toda a chuva que precipitou no dia
  SUM(precipitacao_total) as chuva_total_dia

FROM `basedosdados.br_inmet_bdmep.microdados`
WHERE id_estacao = 'A701'
AND data >= '2004-01-01'
AND data <= '2023-12-31'
GROUP BY data
ORDER BY data
"

write.csv(dados_clima_sp, "microdados_inmet_sp_mirante.csv", row.names = FALSE)


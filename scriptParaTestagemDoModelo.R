install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)

path_modelo <- "insira o caminho para o arquivo rds. É possível buscar o caminho apertando tab"

path_csv <- "insira o caminho para o arquivo rds csv"

dados_test <- read_csv(path_csv)
modelo <- read_rds(path_modelo)

## É adicionado uma coluna com as predições do modelo ao csv
housing_com_previsao <- dados_test %>% 
  mutate(
    price_pred = predict(modelo, new_data = .)$.pred
  )
write_csv(housing_com_previsao, "housing_com_previsao.csv")

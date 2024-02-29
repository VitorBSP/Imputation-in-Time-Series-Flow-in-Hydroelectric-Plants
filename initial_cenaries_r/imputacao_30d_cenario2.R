library(tidyverse)
library(imputeTS)
library(Metrics)
df = read_csv('data/vazoes_CA_20_23.csv')
df$kalman_struct = imputeTS::na_kalman(df$Vazao2_CA_30d, model = 'StructTS')
df$kalman_arima = imputeTS::na_kalman(df$Vazao2_CA_30d, model ='auto.arima')

calcula_metricas <- function(valores_verdadeiros, valores_previstos){
  mae_resultado <- mae(valores_verdadeiros, valores_previstos)
  cat("Mean Absolute Error (MAE):", mae_resultado, "\n")
  
  # RMSE (Root Mean Squared Error)
  rmse_resultado <- rmse(valores_verdadeiros, valores_previstos)
  cat("Root Mean Squared Error (RMSE):", rmse_resultado, "\n")
  
  # Correlação de Pearson
  correlacao_resultado <- cor(valores_verdadeiros, valores_previstos, method = "pearson")
  cat("Pearson Correlation:", correlacao_resultado, "\n")
  
  # Calcular o Rank Product
  mape_resultado <- mape(valores_verdadeiros, valores_previstos)
  
  # Exibir o resultado
  cat("Mean Absolute Percentual Error (MAPE):" , mape_resultado*100, "\n")
}

nulos <- which(is.na(df$Vazao2_CA_30d), arr.ind = TRUE)

colunas = colnames(df)

for (col in colunas[12:length(colunas)]){
  cat('------------------', '\n')
  cat('Método: ', col, '\n')
  calcula_metricas(df$Vazao_CA[nulos], as.vector(df[,col][nulos, ]) %>% unlist())
}

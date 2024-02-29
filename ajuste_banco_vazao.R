# MC <- readxl::read_excel("Monte_Claro_20_23.xlsx")
# CA <- readxl::read_excel("Castro_Alves_20_23.xlsx")
# JU <- readxl::read_excel("14_de_Julho_20_23.xlsx")
# MC[,8400]
# CA[,8633]
# JU[,8400]
# 
# MC1 <- tidyr::pivot_longer(MC, 
#                     cols = `1 de mar de 00`:`28 de fev de 23`, 
#                     names_to = "Data",
#                     values_to = "Vazao_MC")
# 
# CA1 <- tidyr::pivot_longer(CA, 
#                            cols = `1 de mar de 00`:`19 de out de 23`, 
#                            names_to = "Data",
#                            values_to = "Vazao_CA")
# 
# JU1 <- tidyr::pivot_longer(JU, 
#                            cols = `1 de mar de 00`:`28 de fev de 23`, 
#                            names_to = "Data",
#                            values_to = "Vazao_Jul")
# 
# Vazoes<-data.frame(Data=CA1$Data,Vazao_MC=c(MC1$Vazao_MC,rep(NA,233)),
#                    Vazao_CA=c(CA1$Vazao_CA),Vazao_Jul=c(JU1$Vazao_Jul,rep(NA,233)))
# 
# write.csv(Vazoes,file = "vazoes_20_23.csv")
# 
# 
# plot.ts(Vazoes$Vazao_CA);summary(Vazoes$Vazao_CA)
# 
# 
# # ---------------------------------------------------------------------------- #
# 
# vazoes <- readxl::read_excel("vazoes_20_23.xlsx")
# vazoes$Data<-as.Date(vazoes$Data)
# k=3
# for (i in 1:1000) {
# {
# vazoes1 <- vazoes |> 
#   dplyr::mutate(
#     Vazao1_CA_1d=dplyr::case_when(Data %in% c(sample(vazoes$Data,420)) ~ NA, TRUE ~ Vazao_CA),
#     #Vazao2_CA_1d=dplyr::case_when(Data %in% c(sample(vazoes$Data,420)) ~ NA, TRUE ~ Vazao_CA),
#     Vazao1_CA_7d=dplyr::case_when(Data %in% c(as.Date(as.vector(replicate(60,expr = seq(from=sample(vazoes$Data,1),length.out = 7,by="day"))))) ~ NA, TRUE ~ Vazao_CA),
#     #Vazao2_CA_7d=dplyr::case_when(Data %in% c(as.Date(as.vector(replicate(60,expr = seq(from=sample(vazoes$Data,1),length.out = 7,by="day"))))) ~ NA, TRUE ~ Vazao_CA),
#     Vazao1_CA_15d=dplyr::case_when(Data %in% c(as.Date(as.vector(replicate(28,expr = seq(from=sample(vazoes$Data,1),length.out = 15,by="day"))))) ~ NA, TRUE ~ Vazao_CA),
#     #Vazao2_CA_15d=dplyr::case_when(Data %in% c(as.Date(as.vector(replicate(28,expr = seq(from=sample(vazoes$Data,1),length.out = 15,by="day"))))) ~ NA, TRUE ~ Vazao_CA),
#     Vazao1_CA_30d=dplyr::case_when(Data %in% c(as.Date(as.vector(replicate(14,expr = seq(from=sample(vazoes$Data,1),length.out = 30,by="day"))))) ~ NA, TRUE ~ Vazao_CA),
#     #Vazao2_CA_30d=dplyr::case_when(Data %in% c(as.Date(as.vector(replicate(14,expr = seq(from=sample(vazoes$Data,1),length.out = 30,by="day"))))) ~ NA, TRUE ~ Vazao_CA),
#   ) |> 
#   dplyr::select(-c(Vazao_Jul,Vazao_MC))
# 
# a<-fastrep::describe(vazoes1[,-1])
# }
# 
# # plot.ts(vazoes1$Vazao1_CA_30d)
# 
# if(identical(a$na_count, c(0, 420, 420, 420, 420))){print(k);write.csv(vazoes1,file = paste0("vazoes",k,"_CA_20_23.csv"));k=k+1}
# }
# # length(c(rep(seq(from=sample(vazoes$Data,1),length.out = 30,by="day"),3)))
# # seq(from=sample(vazoes$Data,1),length.out = 15,by="day")
# 


calcula_metricas <- function(valores_verdadeiros, valores_previstos){
  
  # Correlação de Pearson
  correlacao_resultado <- cor(valores_verdadeiros, valores_previstos, method = "pearson")
  cat("Pearson Correlation:", correlacao_resultado, "\n")
  
  mae_resultado <- Metrics::mae(valores_verdadeiros, valores_previstos)
  cat("Mean Absolute Error (MAE):", mae_resultado, "\n")
  
  # RMSE (Root Mean Squared Error)
  rmse_resultado <- Metrics::rmse(valores_verdadeiros, valores_previstos)
  cat("Root Mean Squared Error (RMSE):", rmse_resultado, "\n")
  
  # Calcular o Rank Product
  mape_resultado <- Metrics::mape(valores_verdadeiros, valores_previstos)
  
  # Exibir o resultado
  cat("Mean Absolute Percentual Error (MAPE):" , mape_resultado*100, "\n")
}
# ============================================================================ #
library(ggplot2)
setwd("~/Estudos/vazao/joão")
vazoes_IL <- readr::read_csv("Int_Linear_30d_1.csv")
# fastrep::describe(vazoes_KS[,-1])

d1<-ts(vazoes_IL$Vazao_CA)

forecast::autoplot(d1)+theme_minimal()

forecast::ggAcf(d1,lag.max = 100,type = c("correlation"))+labs(y = "FAC Amostral",title="")+
  theme_minimal()
forecast::ggAcf(d1,lag.max = 100,type = c("partial"))+labs(y = "FACP Amostral",title="")+
  theme_minimal()

# plot(density(d1[-test$all.stats$Obs.Num,]))
# a<-boxplot(d1[-test$all.stats$Obs.Num,])
# library(EnvStats)
# test <- rosnerTest(d1, k=300) #k= n?meros de possiveis outliers
# test$all.stats$Obs.Num

# Analise de tendencia deterministica: --------------------------------------- #
randtests::cox.stuart.test(d1,c("two.sided"))#H0: não existe tendencia
trend::cs.test(d1) #H0: não existe tendencia
randtests::runs.test(d1) #H0: não existe tendencia
trend::ww.test(d1) #H0: não existe tendencia
trend::mk.test(d1,continuity = TRUE) #H0: a série é i.i.d. / não existe tendencia
Kendall::MannKendall(d1) #H0: não existe tendencia

# Todos com tendência deterministica, p-valor menor que alpha

# Teste para raiz unitaria: -------------------------------------------------- #
tseries::adf.test(d1,alternative = c("stationary")) #H0: raiz unitária
tseries::pp.test(d1,alternative = c("stationary")) #H0: raiz unitária
tseries::kpss.test(d1, null = c("Level")) #H0: Nivel estac
tseries::kpss.test(d1, null = c("Trend")) #H0: Tend estac

# Sazonalidade --------------------------------------------------------------- #

seastests::kw((d1), freq=365, diff=F, residuals=F, autoarima = T) #H0: Não Sazonal
seastests::fried((d1), freq=365, diff=F, residuals=F, autoarima = T) #H0: Não Sazonal

#p/ o adf e pp estac. e p/ kpss Tem raiz unitaria e tem tendencia deterministica 

# diff.TX<-diff(d1, differences = 1)
# forecast::autoplot(diff.TX)+theme_minimal() # não precisa ir no relatorio
# 
# forecast::ggAcf(diff.TX,lag.max = 100,type = c("correlation"))+labs(y = "FAC Amostral",title="")+
#   theme_minimal()
# forecast::ggAcf(diff.TX,lag.max = 100,type = c("partial"))+labs(y = "FACP Amostral",title="")+
#   theme_minimal()

# Calcule o periodograma
# a<-(TSA::periodogram(d1))
# 1/a$freq[a$spec==max(a$spec)]
# Mesmo dando 360, acredito que o mais ideal é uma frequência de 365 dias (1 Ano)

dts<-ts(as.vector(d1),start = c(2000,60), frequency = 365)
plot(decompose(dts))
prev<-30
#Separar a serie em treino e teste:
tr<- ts(d1[-((length(d1)-c(prev-1)):length(d1))],start = c(2000,60), frequency = 365) 
te<- ts(d1[(length(d1)-c(prev-1)):length(d1)],start = c(2022,330), frequency = 365)

# test <- EnvStats::rosnerTest(d1, k=12) #k= n?meros de possiveis outliers
# vezoes_mes<- as.data.frame(vezoes_mes) |> 
#   dplyr::mutate(n=1:length(d1)) |> 
#   dplyr::mutate(voc_outliers=dplyr::case_when( n %in% c(test$all.stats$Obs.Num) ~ 1,
#                                                TRUE ~ 0))
# vezoes_mes |> dplyr::filter(voc_outliers == 1)

saz <- decompose(dts)
xreg <- as.vector(saz$seasonal)[1:(length(dts)-c(prev))]
c_hat <- saz$seasonal[((length(dts)-c(prev-1)):c(length(dts)))]

# mod<-forecast::auto.arima(tr,xreg = xreg)
# ARIMA(2,0,2)
mod<-arima(x = tr,order = c(3,0,0),xreg = xreg)

#Análise de Resíduos: -------------------------------------------------------- #
# Obter os coeficientes do modelo ARIMA
lmtest::coeftest(mod)
#Modelo Ajstado
summary(mod)
#Verificação das Suposições do Modelo
autoplot(mod)+ggtitle("Dados")+ xlab("Time") + ylab("Valores $")+theme_minimal()
# tsdiag(mod)
#Intervalo de Confiança dos parâmetros
confint(mod,level=0.95)
#Análise de Resíduos
res.mod<-mod$residuals
forecast::ggtsdisplay(res.mod)
Box.test(res.mod,lag = 15, type="Ljung") 
#h0: são não correlacionados
forecast::checkresiduals(mod,lag=15,df=15,test="LB")
nortest::ad.test(res.mod)
tseries::adf.test(res.mod, alternative ="stationary")
ggpubr::ggqqplot(res.mod)

# Predição e Previsão do modelo de treino ------------------------------------ #

predict.mod<-predict(mod,n.ahead=prev,newxreg = c_hat)
fastrep::tbl(predict.mod)

predicao = tr-mod$residuals
t3<-seq(as.Date("2000/03/01"), by = "day", length.out = length(dts)) 
df3<-data.frame(t3,Y=dts,pred=c(as.vector(predicao),as.vector(predict.mod$pred)))
g1<-ggplot(df3)+
  geom_line(aes(x=t3,y=Y))+
  geom_line(aes(x=t3,y=pred),colour = "red")+
  geom_vline(xintercept = t3[length(dts)-prev],linetype="dashed",colour = "black",size=0.5)+
  labs(x="Tempo (Dias)",
       y="Vazão (m³/s)")+
  theme_classic()
g1

g1 + scale_x_continuous(limits = c(t3[length(dts)-prev-10],as.Date("2023/10/19")))+scale_y_continuous(limits = c(0,3000))

# ============================================================================ #

mod2<-arfima::arfima(tr,order = c(3,0,0),xreg = xreg)

summary(mod2)
res.mod2<-residuals(mod2)$Mode1
forecast::ggtsdisplay(res.mod)
Box.test(res.mod2,lag = 15, type="Ljung") 
#h0: são não correlacionados
nortest::ad.test(res.mod2)
tseries::adf.test(res.mod2, alternative ="stationary")
ggpubr::ggqqplot(res.mod2)

# OBS: NÃO SEI PQ, MAS O ARFIMA NÃO SAI REFERENCIA PARA PREVISÃO, TEM QUE COPIAR O VETOR
predict.mod2<-predict(mod2,n.ahead=prev,newxreg = as.matrix(c_hat))
predict.mod2<-predict.mod2[[1]][["Forecast"]]
# 10 DIAS:
# predict.mod2<-c(638.589, 637.672, 674.414, 642.278, 491.471, 438.650, 507.922, 543.698, 490.128, 454.602)
# 15 DIAS:
# predict.mod2<-c(591.716, 512.438, 544.354, 555.258, 406.109, 329.874, 343.027, 430.524, 455.132, 340.809, 308.979, 392.350, 439.632, 396.076, 368.384)
# 30 DIAS: 
# predict.mod2<-c(654.997, 423.071, 314.234, 299.666, 317.693, 439.562, 451.772, 342.080, 278.889, 256.531, 248.841,
#                 315.617, 361.784, 265.065, 262.458, 257.121, 326.548, 425.153, 456.446, 322.587, 260.505, 283.612,
#                 377.476, 406.775, 296.428, 267.677, 353.340, 402.596, 360.918, 334.851)
# fastrep::tbl(predict.mod)

predicao2 = tr-res.mod2
t3<-seq(as.Date("2000/03/01"), by = "day", length.out = length(dts)) 
df3<-data.frame(t3,Y=dts,pred=c(as.vector(predicao2),as.vector(predict.mod2)))
g2<-ggplot(df3)+
  geom_line(aes(x=t3,y=Y))+
  geom_line(aes(x=t3,y=pred),colour = "red")+
  geom_vline(xintercept = t3[length(dts)-prev],linetype="dashed",colour = "black",size=0.5)+
  labs(x="Tempo (Dias)",
       y="Vazão (m³/s)")+
  theme_classic()
g2

g2 + scale_x_continuous(limits = c(t3[length(dts)-prev-10],as.Date("2023/10/19")))+scale_y_continuous(limits = c(0,3000))

# ============================================================================ #
{
especificacao_garch <- rugarch::ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0,2)),
  mean.model = list(armaOrder = c(3,0)))


# Ajustar o modelo GARCH
ajuste_garch <- rugarch::ugarchfit(spec = especificacao_garch, data = tr)

# Exibir resumo do modelo ajustado
print(ajuste_garch)

# Fazer previsões
previsoes_garch <- rugarch::ugarchforecast(ajuste_garch, n.ahead = prev)
previsoes_garch@forecast$seriesFor


predicao3 = tr-ajuste_garch@fit$residuals
t3<-seq(as.Date("2000/03/01"), by = "day", length.out = length(dts)) 
df3<-data.frame(t3,Y=dts,pred=c(as.vector(predicao3),as.vector(previsoes_garch@forecast$seriesFor)))
g3<-ggplot(df3)+
  geom_line(aes(x=t3,y=Y))+
  geom_line(aes(x=t3,y=pred),colour = "red")+
  geom_vline(xintercept = t3[length(dts)-prev],linetype="dashed",colour = "black",size=0.5)+
  labs(x="Tempo (Dias)",
       y="Vazão (m³/s)")+
  theme_classic()
g3

g3 + scale_x_continuous(limits = c(t3[length(dts)-prev-10],as.Date("2023/10/19")))+scale_y_continuous(limits = c(0,3000))
}

# # ---------------------------------------------------------------------------- #
# ordens <- c(0:3)
# ajustes <- list()
# i=1
# # Ajustar modelos GARCH para diferentes ordens, alterar apenas armaOrder
# for (ordem1 in ordens) {
#   for (ordem2 in ordens) {
#   modelo <- paste("sGARCH(", ordem1, ",", ordem2, ")", sep = "")
#   especificacao_garch <- rugarch::ugarchspec(
#     variance.model = list(model = "sGARCH", submodel  = NULL),
#     mean.model = list(armaOrder = c(3,0)))
#   ajuste_garch <- rugarch::ugarchfit(spec = especificacao_garch, data = tr)
#   ajustes[[modelo]]<-ajuste_garch
#   cat("
# ",names(ajustes)[i],": AIC = ")
#   cat(try(rugarch::infocriteria(object = ajustes[[i]])[i]*length(tr),T))
#   i=i+1
# }
# }

# Ar = 3 : sGARCH(0,2) = 103296.9 Ajuste OK
# Ar = 2 : sGARCH(0,2) = 103306.1 Ajuste OK
# Ar = 1 : sGARCH(3,0) = 104233.6 Ajsute OK

# ---------------------------------------------------------------------------- #

df4<-data.frame(t3,Y=dts,pred1=c(as.vector(predicao),as.vector(predict.mod$pred)),
                pred2=c(as.vector(predicao2),as.vector(predict.mod2)),
                pred3=c(as.vector(predicao3),as.vector(previsoes_garch@forecast$seriesFor)))
g4<-ggplot(df4)+
  geom_line(aes(x=t3,y=Y),size=0.7)+
  geom_line(aes(x=t3,y=pred1),linetype = "dashed",color="red2",size=0.7)+
  geom_line(aes(x=t3,y=pred2),linetype = "longdash",color="blue2",size=0.7)+
  geom_line(aes(x=t3,y=pred3),linetype = "dotted",color="green4",size=0.7)+
  geom_vline(xintercept = t3[length(dts)-prev],linetype="dashed",colour = "black",size=0.5)+
  labs(x="Tempo (Dias)",
       y="Vazão (m³/s)")+
  theme_classic()
g4

g4 + scale_x_continuous(limits = c(t3[length(dts)-prev-10],as.Date("2023/10/19")))+scale_y_continuous(limits = c(0,3000))

# ============================================================================ #
# ============================================================================ #
# ============================================================================ #
# ===================== Método Interpolação Linear =========================== #
# ============================================================================ #
# ============================================================================ #
# ============================================================================ #

d1<-ts(vazoes_IL$InterpolacaoLinear)

# Média:
# vazoes_IL$Vazao1_CA_30d[is.na(vazoes_IL$Vazao1_CA_30d)]<-mean(vazoes_IL$Vazao1_CA_30d,na.rm = T)
# fastrep::rep_na(vazoes_IL)
# d1<- ts(vazoes_IL$Vazao1_CA_30d)

dts<-ts(as.vector(d1),start = c(2000,60), frequency = 365)
#Separar a serie em treino e teste:
tr<- ts(d1[-((length(d1)-c(prev-1)):length(d1))],start = c(2000,60), frequency = 365) 
te<- ts(d1[(length(d1)-c(prev-1)):length(d1)],start = c(2022,330), frequency = 365)

saz <- decompose(dts)
xreg <- as.vector(saz$seasonal)[1:(length(dts)-c(prev))]
c_hat <- saz$seasonal[((length(dts)-c(prev-1)):c(length(dts)))]

mod<-arima(x = tr,order = c(3,0,0),xreg = xreg)

#Análise de Resíduos: -------------------------------------------------------- #
# Obter os coeficientes do modelo ARIMA
lmtest::coeftest(mod)
#Modelo Ajstado
summary(mod)
#Verificação das Suposições do Modelo
autoplot(mod)+ggtitle("Dados")+ xlab("Time") + ylab("Valores $")+theme_minimal()
# tsdiag(mod)
#Intervalo de Confiança dos parâmetros
confint(mod,level=0.95)
#Análise de Resíduos
res.mod<-mod$residuals
forecast::ggtsdisplay(res.mod)
Box.test(res.mod,lag = 15, type="Ljung") 
#h0: são não correlacionados
forecast::checkresiduals(mod,lag=15,df=15,test="LB")
nortest::ad.test(res.mod)
tseries::adf.test(res.mod, alternative ="stationary")
ggpubr::ggqqplot(res.mod)

# Predição e Previsão do modelo de treino ------------------------------------ #

predict.mod4<-predict(mod,n.ahead=prev,newxreg = c_hat)
fastrep::tbl(predict.mod4)

predicao4 = tr-mod$residuals
t3<-seq(as.Date("2000/03/01"), by = "day", length.out = length(dts)) 
df3<-data.frame(t3,Y=dts,pred=c(as.vector(predicao),as.vector(predict.mod$pred)),pred4=c(as.vector(predicao4),as.vector(predict.mod4$pred)))
g1<-ggplot(df3)+
  geom_line(aes(x=t3,y=Y))+
  geom_line(aes(x=t3,y=pred),colour = "blue",linetype="longdash",size=0.8)+
  geom_line(aes(x=t3,y=pred4),colour = "red",linetype="dashed",size=0.8)+
  geom_vline(xintercept = t3[length(dts)-prev],linetype="dashed",colour = "black",size=0.5)+
  labs(x="Tempo (Dias)",
       y="Vazão (m³/s)")+
  theme_classic()
g1

g1 + scale_x_continuous(limits = c(t3[length(dts)-prev-10],as.Date("2023/10/19"))) + scale_y_continuous(limits = c(0,3000))

{
cat("Ajuste modelo:
");calcula_metricas(valores_verdadeiros = predicao,valores_previstos = predicao4)
cat("
previsão:
");calcula_metricas(valores_verdadeiros = predict.mod$pred,valores_previstos = predict.mod4$pred)
}

# Média ----------------------------------------- #
# Ajuste modelo:
#   Pearson Correlation: 0.9840185 
# Mean Absolute Error (MAE): 7.354438 
# Root Mean Squared Error (RMSE): 41.9143 
# Mean Absolute Percentual Error (MAPE): 7.130024 
# 
# previsão:
#   Pearson Correlation: 0.9999891 
# Mean Absolute Error (MAE): 0.2513778 
# Root Mean Squared Error (RMSE): 0.4150363 
# Mean Absolute Percentual Error (MAPE): 0.084865 

# Interpolação Linear --------------------------- # (Está melhor, pouco mas está)
# Ajuste modelo:
#   Pearson Correlation: 0.9841127 
# Mean Absolute Error (MAE): 5.585302 
# Root Mean Squared Error (RMSE): 41.86874 
# Mean Absolute Percentual Error (MAPE): 3.892783 
# 
# previsão:
#   Pearson Correlation: 0.9999882 
# Mean Absolute Error (MAE): 0.2461774 
# Root Mean Squared Error (RMSE): 0.4434488 
# Mean Absolute Percentual Error (MAPE): 0.07636832 

# ============================================================================ #

mod2<-arfima::arfima(tr,order = c(3,0,0),xreg = xreg)

summary(mod2)
res.mod2<-residuals(mod2)$Mode1
forecast::ggtsdisplay(res.mod)
Box.test(res.mod2,lag = 15, type="Ljung") 
#h0: são não correlacionados
nortest::ad.test(res.mod2)
tseries::adf.test(res.mod2, alternative ="stationary")
ggpubr::ggqqplot(res.mod2)

predict.mod5<-predict(mod2,n.ahead=prev,newxreg = as.matrix(c_hat))
# predict.mod5<-c(643.931, 644.830, 679.174, 644.837, 492.955, 439.801, 509.067, 544.764, 490.938, 455.286)
#30 dias: (Interpolação linear)
predict.mod5 <- c(654.287, 421.383, 313.415, 299.908, 318.272, 440.234, 452.524, 342.880, 279.740, 257.432, 249.794,
                  316.662, 362.906, 266.176, 263.609, 258.309, 327.813, 426.510, 457.852, 323.944, 261.854, 285.000,
                  378.943, 408.282, 297.892, 269.144, 354.876, 404.178, 362.493, 336.425)
# Pela Média:
# predict.mod5 <- c(654.203, 420.258, 311.893, 298.327, 316.632, 438.493, 450.749, 341.137, 278.029, 255.745, 248.132,
#                   315.005, 361.260, 264.588, 262.049, 256.777, 326.284, 424.975, 456.331, 322.489, 260.441, 283.602,
#                   377.536, 406.886, 296.550, 267.830, 353.554, 402.858, 361.203, 335.160)
# fastrep::tbl(predict.mod2)
 
predicao5 = tr-res.mod2
t3<-seq(as.Date("2000/03/01"), by = "day", length.out = length(dts)) 
df3<-data.frame(t3,Y=dts,pred=c(as.vector(predicao2),as.vector(predict.mod2)),
                pred5=c(as.vector(predicao5),as.vector(predict.mod5)))
g2<-ggplot(df3)+
  geom_line(aes(x=t3,y=Y))+
  geom_line(aes(x=t3,y=pred),colour = "blue",linetype="longdash",size=0.8)+
  geom_line(aes(x=t3,y=pred5),colour = "red",linetype="dashed",size=0.8)+
  geom_vline(xintercept = t3[length(dts)-prev],linetype="dashed",colour = "black",size=0.5)+
  labs(x="Tempo (Dias)",
       y="Vazão (m³/s)")+
  theme_classic()
g2

g2 + scale_x_continuous(limits = c(t3[length(dts)-prev-10],as.Date("2023/10/19"))) + scale_y_continuous(limits = c(0,3000))

{
cat("Ajuste modelo:
");calcula_metricas(valores_verdadeiros = predicao2,valores_previstos = predicao5)
cat("
previsão:
");calcula_metricas(valores_verdadeiros = predict.mod2,valores_previstos = predict.mod5)
}

# Média ----------------------------------------- #
# Ajuste modelo:
#   Pearson Correlation: 0.9838626 
# Mean Absolute Error (MAE): 7.715073 
# Root Mean Squared Error (RMSE): 42.16025 
# Mean Absolute Percentual Error (MAPE): 7.731713 
# 
# previsão:
#   Pearson Correlation: 0.9999643 
# Mean Absolute Error (MAE): 0.6116333 
# Root Mean Squared Error (RMSE): 0.8853808 
# Mean Absolute Percentual Error (MAPE): 0.1803329 

# Interpolação Linear --------------------------- # aqui perdeu na previsão '-'

# Ajuste modelo:
#   Pearson Correlation: 0.9839523 
# Mean Absolute Error (MAE): 5.999215 
# Root Mean Squared Error (RMSE): 42.12487 
# Mean Absolute Percentual Error (MAPE): 3.952006 
# 
# previsão:
#   Pearson Correlation: 0.9999645 
# Mean Absolute Error (MAE): 1.1626 
# Root Mean Squared Error (RMSE): 1.216356 
# Mean Absolute Percentual Error (MAPE): 0.3520721

# ============================================================================ #

{
  especificacao_garch <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(0,2)),
    mean.model = list(armaOrder = c(3, 0)))
  
  
  # Ajustar o modelo GARCH
  ajuste_garch <- rugarch::ugarchfit(spec = especificacao_garch, data = tr)
  
  # Exibir resumo do modelo ajustado
  print(ajuste_garch)
  
  # Fazer previsões
  previsoes_garch2 <- rugarch::ugarchforecast(ajuste_garch, n.ahead = prev)
  previsoes_garch2@forecast$seriesFor
  
  
  predicao6 = tr-ajuste_garch@fit$residuals
  t3<-seq(as.Date("2000/03/01"), by = "day", length.out = length(dts)) 
  df3<-data.frame(t3,Y=dts,pred=c(as.vector(predicao3),as.vector(previsoes_garch@forecast$seriesFor)),
                  pred6=c(as.vector(predicao6),as.vector(previsoes_garch2@forecast$seriesFor)))
  g2<-ggplot(df3)+
    geom_line(aes(x=t3,y=Y))+
    geom_line(aes(x=t3,y=pred),colour = "blue",linetype="longdash",size=0.8)+
    geom_line(aes(x=t3,y=pred6),colour = "red",linetype="dashed",size=0.8)+
    geom_vline(xintercept = t3[length(dts)-prev],linetype="dashed",colour = "black",size=0.5)+
    labs(x="Tempo (Dias)",
         y="Vazão (m³/s)")+
    theme_classic()
  g2
  
  g2 + scale_x_continuous(limits = c(t3[length(dts)-prev-10],as.Date("2023/10/19"))) + scale_y_continuous(limits = c(0,3000))
  
}


{
  cat("Ajuste modelo:
");calcula_metricas(valores_verdadeiros = predicao3,valores_previstos = predicao6)
  cat("
previsão:
");calcula_metricas(valores_verdadeiros = previsoes_garch@forecast$seriesFor,valores_previstos = previsoes_garch2@forecast$seriesFor)
}

# Média ----------------------------------------- #
# Ajuste modelo:
#   Pearson Correlation: 0.9834965 
# Mean Absolute Error (MAE): 5.435704 
# Root Mean Squared Error (RMSE): 41.87686 
# Mean Absolute Percentual Error (MAPE): 3.956851 
# 
# previsão:
#   Pearson Correlation: 0.9999828 
# Mean Absolute Error (MAE): 0.6970646 
# Root Mean Squared Error (RMSE): 1.212172 
# Mean Absolute Percentual Error (MAPE): 0.2399711 

# Interpolação Linear --------------------------- # aqui perdeu na previsão '-'

# Ajuste modelo:
#   Pearson Correlation: 0.9836188 
# Mean Absolute Error (MAE): 4.671908 
# Root Mean Squared Error (RMSE): 41.82715 
# Mean Absolute Percentual Error (MAPE): 2.266073 
# 
# previsão:
#   Pearson Correlation: 0.9999943 
# Mean Absolute Error (MAE): 3.759335 
# Root Mean Squared Error (RMSE): 3.771193 
# Mean Absolute Percentual Error (MAPE): 1.844785 



# # ---------------------------------------------------------------------------- #
# 
# df4<-data.frame(t3,Y=dts,pred1=c(as.vector(predicao),as.vector(predict.mod$pred)),
#                 pred2=c(as.vector(predicao2),as.vector(predict.mod2)),
#                 pred3=c(as.vector(predicao3),as.vector(previsoes_garch@forecast$seriesFor)))
# g4<-ggplot(df4)+
#   geom_line(aes(x=t3,y=Y),size=0.7)+
#   geom_line(aes(x=t3,y=pred1),linetype = "dashed",color="red2",size=0.7)+
#   geom_line(aes(x=t3,y=pred2),linetype = "longdash",color="blue2",size=0.7)+
#   geom_line(aes(x=t3,y=pred3),linetype = "dotted",color="green4",size=0.7)+
#   geom_vline(xintercept = as.Date("2023/10/09"),linetype="dashed",colour = "black",size=0.5)+
#   labs(x="Tempo (Dias)",
#        y="Vazão (m³/s)")+
#   theme_classic()
# g4
# 
# g4 + scale_x_continuous(limits = c(as.Date("2023/10/01"),as.Date("2023/10/19")))+scale_y_continuous(limits = c(0,3500))

# # ============================================================================ #

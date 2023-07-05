pacman::p_load('dplyr','lubridate','Mcomp','forecast','tseries','ggplot2','ggpubr')

data(M3)
id=1686 # id da série
h<-M3[[id]]$h # horizonte de previsão
x<-M3[[id]]$x # dados de treinamento
dados_teste<-M3[[id]]$xx # dados de teste

#gráfico da serie
M3[[id]] %>% plot()

# Decomposicao da serie
mstl(x) %>% autoplot()+
  labs(title="Decomposição MSTL")+
  scale_x_continuous()+
  theme_bw()

#diferenciação 
d = ndiffs(x) #d=1
D = diff(x) %>% nsdiffs() #D=0

#Verificando gráficos da série com 1 diferenciação
x %>% diff() %>% kpss.test(.)
wt <- x %>% diff()
par(mfrow=c(3,1),mar=c(4, 3, 3, 1) + 0.1)
plot(wt,main="Série wt")
acf(wt, lag = 5*12)
pacf(wt, lag = 5*12)


#Escolhendo o melhor modelo
melhor_AICc = Inf
for (P in 0:1) { 
  for (Q in 0:1) {
    for (p in 0:3) { 
      for (q in 0:3) {
        fit = Arima(M3[[id]]$x, order=c(p,1,q), seasonal=c(P,0,Q))
        if (fit$aicc < melhor_AICc) { 
          melhor_AICc = fit$aicc
          cat("p =",p,", q =",q,", P =",P,", Q =",Q,", AICc =", fit$aicc, "\n")
        }
      }
    }
  }
}

arima_model = Arima(M3[[id]]$x,order=c(1,1,1),seasonal=c(1,0,1))
arima_model

arima_box_cox_model = Arima(x,order=c(1,1,1),seasonal=c(1,0,1), lambda = 'auto')
arima_box_cox_model

#verificando as previsões
arima_model %>% forecast(h=12, level=c(80, 95)) %>% plot()
arima_box_cox_model %>% forecast(h=12, level=c(80, 95)) %>% plot()



#calculando os resíduos
residuo_arima = arima_model$residuals
residuo_arima_box_cox = arima_box_cox_model$residuals

#estacionariedade
kpss.test(residuo_arima)
kpss.test(residuo_arima_box_cox)

#independencia
Box.test(residuo_arima, lag = 15, type ="Ljung-Box")
Box.test(residuo_arima_box_cox, lag = 15, type ="Ljung-Box")

#normalidade
shapiro.test(residuo_arima)
shapiro.test(residuo_arima_box_cox)


############ Modelos ETS

ses = ets(x, model = "ANN", damped=FALSE)
ses$aic

holt = ets(x, model = "AAN", damped=FALSE)
holt$aic

holt_damped = ets(x, model = "AAN", damped=TRUE)
holt_damped$aic

holt_aditivo = ets(x, model = "AAA", damped=FALSE)
holt_aditivo$aic

holt_winter_aditivo = ets(x, model = "MAM", damped=FALSE)
holt_winter_aditivo$aic

E <- holt_winter_aditivo$residuals %>% window(start=1985)

par(mfrow=c(2,2)); plot(E);acf(E);pacf(E);qqnorm(E);qqline(E)

#estacionariedade
kpss.test(E)

#independencia
Box.test(E, lag = 15, type ="Ljung-Box")

#normalidade
shapiro.test(E)

f = holt_winter_aditivo %>% forecast(h=12)
f

f %>% plot()

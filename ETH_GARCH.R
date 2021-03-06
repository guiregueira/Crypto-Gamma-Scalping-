# DATATHON - GARCH MODEL

library(tseries)
library(NTS)
library(moments)
library(PerformanceAnalytics)
library(TSA)
library(vrtest)
library(car)
library(fNonlinear)
library(FinTS)
library(rugarch)
library(mFilter)
library(dynlm)
library(readxl)
library(sandwich)
library(dygraphs)
library(forecast)
library(urca)
library(vars)
library(tseries)
library(quantmod)
library(lubridate)
library(stats)

# Importa��o a base de dados

ETHR <- read_excel("C://Users//Carlos//Downloads//ETH-USD(2).xlsx")


# Manipula��o da base de dados e transforma��o para time series

ETHclose <- as.data.frame(ETHR$`Adj Close`) 
ETHR$Date<- ymd(ETHR$Date)
row.names(ETHclose) <- as.Date(ETHR$Date, format = "%y/%m/%d")

colnames(ETHclose) <- ("AC")
View(ETHclose)
ETHclose$AC <- as.numeric(ETHclose$AC)
ETHclose <- as.timeSeries(ETHclose)


# Defini��o da vari�vel retorno

ETHreturns = diff(ETHclose$AC)/lag(ETHclose$AC)
head(ETHreturns, n=528)

# Manipula��o da nova vari�vel e transforma��o para time series

ETHreturns <- ETHreturns[-528]

ETHreturns <- as.data.frame(ETHreturns)

datas <- as.Date(ETHR$Date, format = "%y/%m/%d")
datas2 <- as.array(datas)
datas2 <- datas2[-1]

row.names(ETHreturns) <- as.Date(datas2)
colnames(ETHreturns) <- ("return")

ETHreturns$return <- as.numeric(ETHreturns$return)

View(ETHreturns)

ETHreturns <- as.timeSeries(ETHreturns)


# Plot das time series

plot.ts(ETHreturns)
plot.ts(ETHclose)


# C�lculo da volatilidade dos retornos:

sd(ETHreturns$return)

sqrt(365)*sd(ETHreturns$return)

chart.RollingPerformance(R = ETHreturns, width = 30, FUN = "sd.annualized", scale = 527, main = "ETH Volatility" )

# Importa��o da volatilidade impl�cita

ETH_IV <- read_excel("C://Users//Carlos//Downloads//ethvol(2).xlsx")

# Manipula��o da vari�vel e transforma��o para time series

datas3 <- as.array(ETH_IV$Date)

ETH_IV$Date <- NULL
ETH_IV$DTE <- NULL

row.names(ETH_IV) <- as.Date(datas3)

view(ETH_IV)
ETH_IV <- as.timeSeries(ETH_IV)


# Plot da nova vari�vel

plot.ts(ETH_IV)


# Modelos GARCH (1,1)

# Estima��o e previs�o do sigma 23 dias � frente, como explicado nos slides. (Sigma � a medida escolhida para volatilidade)

# SGARCH

modspec <- ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model = 'norm')

modspec

modfit <- ugarchfit(data = ETHreturns, spec = modspec, out.sample = 20)

modfit

plot(modfit, which = "all")

forc = ugarchforecast(fitORspec = modfit, n.ahead = 23)

plot(fitted(forc))

plot(sigma(forc))

fpm(forc)

# IGARCH
modspec2 <- ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = "iGARCH", garchOrder = c(1,1)), distribution.model = 'norm')

modspec2

modfit2 <- ugarchfit(data = ETHreturns, spec = modspec2, out.sample = 20)

modfit2

plot(modfit2, which = "all")

forc2 = ugarchforecast(fitORspec = modfit2, n.ahead = 23)

plot(fitted(forc2))

plot(sigma(forc2))

fpm(forc2)

# UGARCH
modspec3 <- ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = "eGARCH", garchOrder = c(1,1)), distribution.model = 'norm')

modspec3

modfit3 <- ugarchfit(data = ETHreturns, spec = modspec3, out.sample = 20)

modfit3

plot(modfit3, which = "all")

forc3 = ugarchforecast(fitORspec = modfit3, n.ahead = 23)

plot(fitted(forc3))

plot(sigma(forc3))

fpm(forc3)



# Testes 

adf.test(ETHclose)

pacf(ETHclose, lag.max = 50)

acf(ETHclose, lag.max = 50)

auto.arima(ETHclose)

auto.arima(ETHreturns)

adf.test(ETHclose)



# Rascunhos �teis para a realiza��o do trabalho

ETHclose_diff <- diff(ETHclose)
plot.ts(ETHclose_diff)

ETHclose_diff2 <- as.list(ETHclose_diff$AC)
ETHclose_diff2 <- ETHclose_diff2[-1]

ETHclose_diff$AC <- ETHclose_diff2

View(ETHclose_diff)

pacf(ETHclose_diff, lag.max = 50, na.omit(ETHclose_diff))

acf(ETHclose_diff, lag.max = 50, na.omit(ETHclose_diff))


ETHclose_logsqrt <- as.data.frame(sqrt(log(ETHclose$AC)))

row.names(ETHclose_logsqrt) <- as.Date(ETHR$Date, format = "%y/%m/%d")
colnames(ETHclose_logsqrt) <- ("AC")

plot.ts(ETHclose_logsqrt)

###

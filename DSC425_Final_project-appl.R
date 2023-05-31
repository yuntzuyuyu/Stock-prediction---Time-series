
library(ggplot2)
library(ggfortify)
library(forecast)
library(lmtest)
library(aTSA)
library(fUnitRoots)
library(tseries)
library(ggfortify)
library(fBasics)

library(readr)



#readdata
str(AAPL)
AAPL<- read_csv("Downloads/HistoricalPrices.csv")


# transpose of dataframe
transpose <- t(AAPL)

# converting the result to dataframe
transpose <- as.data.frame(transpose)

# calculating reverse of dataframe
rev_data_frame <- rev(transpose)

# transpose of reverse dataframe
rev_data_frame <- t(rev_data_frame)

# converting the result to dataframe
rev_data_frame <- as.data.frame(rev_data_frame)


AAPLts <- ts(rev_data_frame$Close,start = 2007.1, frequency = 251)
AAPLts1 <- ts(rev_data_frame$Volume,start = 2007.1, frequency = 251)

plot(decompose(AAPLtslog))
autoplot(AAPLts1)
rev_data_frame$Close<- as.numeric(rev_data_frame$Close)
rev_data_frame$Volume<- as.numeric(rev_data_frame$Volume)

v0 <- ts(log(rev_data_frame$Close),start = 2007.1, frequency = 251)
v1 <- ts(log(rev_data_frame$Volume),start = 2007.1, frequency = 251)
s = as.zoo(ts.intersect(v0, v1lag=stats::lag(v1, -822)))
autoplot(s)    # At the beginning of the plot, we can now see the correlation
# When soi is high, rec is low and vice-versa

0.4*length(v1) - 1645 + 1  # The last 6 elements of soi are not used in the training 
# set, so we can use them to forecast "rec" out beyond 
v1Test = subset(v1, start = length(v1) - 411 + 1)

# Notice that a zoo is a little more convenient because we can use $!!
fit2 = Arima(s$v0, xreg=s$v1, order=c(0, 0, 0))
fit2    # Same slope and intercept

# Now, let's run the forecast.  
plot(forecast::forecast(fit2, xreg=v1Test), xlim=c(2007, 2023))







v0 <- ts(log(rev_data_frame$Close),start = 2007.1, frequency = 251)
v1 <- ts(log(rev_data_frame$Volume),start = 2007.1, frequency = 251)
v = ts.intersect(v0, v1)
v0 = v[, "v0"]
v1 = v[, "v1"]
v1_validation = subset(v1, start=3289) 
v1_validation 

fit3 = Arima(subset(v1, start=412), xreg=subset(v0, end=3702), order=c(0, 0, 0))
plot(forecast::forecast(fit3, xreg=subset(v0, start=3703)))
lines(4114:4526, v1_validation, col="red")
length(v1)
length(v1_validation)
4113+413
#####################################################################

#AAPL <- read_csv("Desktop/AAPL.csv")
AAPL$Date=as.Date(AAPL$Date,"%y/%m/%d")
head(AAPL)
AAPLts <- ts(AAPL$Close,start = 2007.1, frequency = 251)
#data normality
qplot(AAPLts,geom="histogram",main=" Stock price")
qqnorm(AAPLts)
qqline(AAPLts)

#We check the adfTest, we see there is a unit root that we cannot reject.

autoplot(AAPLts)

adfTest(AAPLts,type="ct")
adfTest(AAPLts,type="nc")
adfTest(AAPLts,type="c")

kpss.test(AAPLts)
#log

AAPLtslog <- ts(log(AAPL$`Adj Close`),start = 2007.1, frequency = 251)
#check adf test if there is any unit root
adfTest(AAPLtslog,type="ct")
adfTest(AAPLtslog,type="nc")
adfTest(AAPLtslog,type="c")
kpss.test(AAPLtslog)


autoplot(AAPLtslog )
acf(AAPLtslog )
pacf(AAPLtslog)
eacf(AAPLtslog)

#try differencing the series to de-trend
AAPLtslogdiff<- diff(AAPLtslog)
autoplot(AAPLtslogdiff)
t.test(diff(AAPLtslog))
adfTest(AAPLtslogdiff,type="ct")
adfTest(AAPLtslogdiff,type="nc")
adfTest(AAPLtslogdiff,type="c")
kpss.test(AAPLtslogdiff)
# Difference the model
autoplot(AAPLtslogdiff )
acf(diff(AAPLtslog))
pacf(diff(AAPLtslog))
eacf(AAPLtslogdiff)

# fit model according to the results above.
#1. seperate model as training dataset and testing dataset.


#Ttry (0,1,4)
fitAAPL1<-Arima(AAPLtslog, order = c(0,1,4),include.drift = TRUE)
coeftest(fitAAPL1)
fitAAPL2<-Arima(AAPLtslog, order = c(0,1,4),include.drift = TRUE,fixed = c(0,0,0,NA,NA))
coeftest(fitAAPL2)
summary(fitAAPL2)


#Check the residual
acf(fitAAPL2$residuals)
Box.test(fitAAPL2$residuals,lag=10,type = "Ljung")

#Ttry (0,1,7)
fitAAPL3<-Arima(AAPLtslog, order = c(0,1,7),include.drift = TRUE)
coeftest(fitAAPL3)
fitAAPL4<-Arima(AAPLtslog, order = c(0,1,7),include.drift = TRUE,fixed = c(NA,NA,0,NA,0,NA,NA,NA))
coeftest(fitAAPL4)
summary(fitAAPL4)
#Check the residual(0,1,7)
Acf(fitAAPL4$residuals,lag=100)
Box.test(fitAAPL4$residuals,lag=10,type = "Ljung")


#Ttry (2,1,4)
fitAAPL5<-Arima(AAPLtslog, order = c(2,1,4),include.drift = TRUE)
coeftest(fitAAPL5)
fitAAPL6<-Arima(AAPLtslog, order = c(2,1,4),include.drift = TRUE,fixed = c(0,NA,0,NA,0,0,NA))
coeftest(fitAAPL6)
summary(fitAAPL6)
#Check the residual(2,1,4)
# we can reject the null hypothesis here which means there could be correlation in the time series
acf(fitAAPL6$residuals)
Box.test(fitAAPL6$residuals,lag=10,type = "Ljung")


#Check the autocorrelation
fitAAPL7<-auto.arima(AAPLtslog)
coeftest(fitAAPL7)
summary(fitAAPL7)
#Check the residual
acf(fitAAPL7$residuals)
Box.test(fitAAPL7$residuals,lag=10,type = "Ljung")






#see how model performs using back test
# hold out the last 20% for prediction
length(AAPLtslog)*0.8
AAPLTrain = subset(AAPLtslog, end=3296)
AAPLTest = subset(AAPLtslog, start=3297)

#try (0,1,4)
fitAAPL2<-Arima(AAPLTrain, order = c(0,1,4),include.drift = TRUE,fixed = c(0,NA,0,NA,NA))
coeftest(fitAAPL2)

forcastm1 = forecast::forecast(fitAAPL2)
plot(forcastm1)
lines(AAPLTest, col="red")
RMSFE1 = sqrt(mean((forcastm1$mean - AAPLTest)^2))
RMSFE1

MAFE1 = mean(abs(forcastm1$mean - AAPLTest))
MAFE1

MAPE1 = mean(abs((forcastm1$mean - AAPLTest) / AAPLTest))
MAPE1   # About 3% off on average


#try (0,1,7)

fitAAPL4<-Arima(AAPLTrain, order = c(0,1,7),include.drift = TRUE,fixed = c(NA,NA,0,NA,0,NA,NA,NA))
summary(fitAAPL4)
forcastm2 = forecast::forecast(fitAAPL4)
plot(forcastm2)
lines(AAPLTest, col="red")
RMSFE2 = sqrt(mean((forcastm2$mean - AAPLTest)^2))
RMSFE2

MAFE2 = mean(abs(forcastm2$mean - AAPLTest))
MAFE2

MAPE2 = mean(abs((forcastm2$mean - AAPLTest) / AAPLTest))
MAPE2   # About 3% off on average

#try (0,1,2)

fitAAPL7<-auto.arima(AAPLTrain)
forcastm3 = forecast::forecast(fitAAPL7)
plot(forcastm3)
lines(AAPLTest, col="red")
RMSFE3 = sqrt(mean((forcastm3$mean - AAPLTest)^2))
RMSFE3

MAFE3 = mean(abs(forcastm3$mean - AAPLTest))
MAFE3

MAPE3 = mean(abs((forcastm3$mean - AAPLTest) / AAPLTest))
MAPE3   #

#Garch Analysis

# check the box.text of the return
r<- diff(AAPLts)
Box.test(r,type="Ljung")
# we ar going to check residual square
autoplot(fitAAPL4$residuals^2)
Acf(fitAAPL4$residuals^2,lag=50)


res = fitAAPL4$residuals
gFit = garch(res, order =c(1,1))
coeftest(gFit)
gresid= na.omit(gFit$residuals)
autoplot(gresid)
Acf(gresid,lag=100)
Box.test(gresid^2,type = "Ljung")


###############################################################
#Include the garch model and arma model together, t
install.packages("fGarch")
library(fGarch)
AAPLdiff<- diff(AAPLTrain)

gFit1 = garchFit( ~ arma(0, 7) + garch(1, 1), data = AAPLdiff, trace = F)
gFit1

# in garchFit, the "residuals" are the result of the Arima
# and the input to the garch.  To get the residuals after 
# the garch, we need to divide by the computed "volatility"
# i.e. the standard deviation
autoplot(ts(gFit1@residuals / gFit1@sigma.t))

# Compare normality measures
skewness(gFit1@residuals / gFit1@sigma.t)
kurtosis(gFit1@residuals / gFit1@sigma.t)
normalTest(gFit1@residuals / gFit1@sigma.t)

# Now, the computed standard deviations are in sigma.t
# the computed variances are in "h.t"
autoplot(ts(gFit1@residuals)) + 
  autolayer(ts(1.96 * gFit1@sigma.t), color="red") + 
  autolayer(ts(-1.96 * gFit1@sigma.t), color="red")

ugarch.spec = ugarchspec(variance.model=list(garchOrder=c(1, 1)),
               mean.model=list(armaOrder=c(0, 7)))

gFit2 <- ugarchfit(ugarch.spec, diff(AAPLTrain))


gFit2@fit
gFit2res <- residuals(gFit2, standardize=T)
autoplot(gFit2res^2)
acf(gFit2res^2)

Box.test(gFit2res^2, lag = 15, type = 'Ljung')

gFit2.rolltest <- ugarchroll(ugarch.spec, data = diff(AAPLTrain), n.start = 2000,
                             refit.window = 'moving',
                             refit.every = 500)

report(gFit2.rolltest, type = 'fpm')


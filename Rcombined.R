
#----------------------------------------------------------------------------------------------------------------------Libraries
library(AER)
library(bootUR)
library(bruceR)
library(conflicted)
library(corrplot)
library(devtools)
library(dplyr)
library(fable)
library(feasts)
library(FinTS)
library(forecast)
library(fpp3)
library(ggfortify)
library(graphics)
library(keras)
library(lmtest)
library(lubridate)
library(magrittr)
library(moments)
library(MTS)
library(multivar)
library(nonlinearTseries)
library(parallel)
library(PerformanceAnalytics)
library(pdR)
library(purrr)
library(readr)
library(readstata13)
library(readxl)
library(RTransferEntropy)
library(reticulate)
library(rugarch)
library(rmgarch)
library(quantmod)
library(stargazer)
library(summarytools)
library(tensorflow)
library(tibble)
library(tidyr)
library(tsDyn)
library(tseries)
library(tsibble)
library(tidyverse)
library(tsoutliers)
library(vars)
library(xts)
library(urca)

conflicts_prefer(PerformanceAnalytics::skewness)
conflicts_prefer(graphics::legend)
conflicts_prefer(PerformanceAnalytics::kurtosis)
conflicts_prefer(PerformanceAnalytics::legend)
conflicts_prefer(dplyr::select)
conflicts_prefer(vars::VAR)
conflicts_prefer(pdR::model) 
conflicts_prefer(dplyr::lag)

#----------------------------------------------------------------------------------------------------------------------Get data

tickers <- c("BTC-USD", "ETH-USD","USDT-USD")
start_date=as.Date("2018-01-01")
end_date=as.Date("2023-01-01")
# Download the data for each ticker
getSymbols(tickers,from=start_date,to=end_date)

BTC<- `BTC-USD`
ETH<- `ETH-USD`
USDT<-`USDT-USD`
#-----------------------------------------------------------------------------------------
(names(BTC) [1] <- "BTC_Open")
(names(ETH) [1] <- "ETH_Open")
(names(USDT)[1] <- "USDT_Open")

(names(BTC)[2] <- "BTC_High")
(names(ETH)[2] <- "ETH_High")
(names(USDT)[2] <- "USDT_High")

(names(BTC)[3] <- "BTC_Low")
(names(ETH)[3] <- "ETH_Low")
(names(USDT)[3] <- "USDT_Low")

(names(BTC)[4] <- "BTC_Close")
(names(ETH)[4] <- "ETH_Close")
(names(USDT)[4]<- "USDT_Close")

(names(BTC)[5] <- "BTC_Volume")
(names(ETH)[5] <- "ETH_Volume")
(names(USDT)[5] <- "USDT_Volume")

(names(BTC)[6] <- "BTC_Adjusted")
(names(ETH)[6] <- "ETH_Adjusted")
(names(USDT)[6] <- "USDT_Adjusted")

head(BTC, 3)
head(ETH, 3)
head(USDT,3)

#---------------------------------------------------------------------------------------
BTCdf <- as.data.frame(BTC)
ETHdf <- as.data.frame(ETH)
USDTdf <- as.data.frame(USDT)
#-----------------------------------------------------------------------------------------
BTCdf$Date <- rownames(BTCdf )    # Convert row names to column
rownames(BTCdf ) <- NULL           # Reset row names
ETHdf$Date <- rownames(ETHdf )    # Convert row names to column
rownames(ETHdf ) <- NULL           # Reset row names
USDTdf$Date <- rownames(USDTdf )  # Convert row names to column
rownames(USDTdf ) <- NULL          # Reset row names

BTCdf <- BTCdf[, c(7, 1, 2, 3, 4,5,6)]
ETHdf <- ETHdf[, c(7, 1, 2, 3, 4,5,6)]
USDTdf <- USDTdf[, c(7, 1, 2, 3, 4,5,6)]
#-----------------------------------------------------------------------------------------

BTCdf$Date=gsub("-","",format(ymd(BTCdf$Date), "%d-%m-%Y") )
head(BTCdf$Date, 3) 

ETHdf$Date=gsub("-","",format(ymd(ETHdf$Date), "%d-%m-%Y") )
head(ETHdf$Date, 3)

USDTdf$Date=gsub("-","",format(ymd(USDTdf$Date), "%d-%m-%Y") )
head(USDTdf$Date, 3)

#-----------------------------------------------------------------------------------------
check_missing_insample_values(BTCdf)
check_missing_insample_values(ETHdf)
check_missing_insample_values(USDTdf)

#-----------------------------------------------------------------------------------------
sink("Summary.txt")
summary(BTCdf)
summary(ETHdf)
summary(USDTdf)
sink()
#-----------------------------------------------------------------------------------------
write_csv(BTCdf,    "C:/Users/OD/Documents/ALCALA/TFM/My/MyCODE/dataBitcoin.csv")
write_csv(ETHdf,    "C:/Users/OD/Documents/ALCALA/TFM/My/MyCODE/dataEthereum.csv")
write_csv(USDTdf,   "C:/Users/OD/Documents/ALCALA/TFM/My/MyCODE/dataTether.csv")
#-----------------------------------------------------------------------------------------
saveRDS(BTCdf,  file="BTC.rds")
saveRDS(ETHdf,  file="ETH.rds")
saveRDS(USDTdf, file="USDT.rds")
#----------------------------------------------------------------------------------------
BTC_AC  <-BTC[, "BTC_Adjusted"]
ETH_AC  <-ETH[, "ETH_Adjusted"]
USDT_AC <-USDT[, "USDT_Adjusted"]
#--------------------------------------------------------------------------------------
#merge the data
AC3<-merge(BTC_AC,ETH_AC,USDT_AC)

# ----------------------------------------------Normalize Prices ------------------------------------------------ 
# check if ts  have different starting and end points
sample_check <- find_nonmissing_subsample(AC3)
sample_check$range #check the start and end points
sample_check$all_equal
#--------------------------------------------------------------------------------------
#daily log returns for merged data
LogRtrns3<-Return.calculate(AC3, method = "log")
#rename the columns names as returns instead of close
colnames(LogRtrns3) = c("BTC","ETH","USDT")
head(LogRtrns3)
#Remove NA value
LogRtrns3<-LogRtrns3[-1]
head(LogRtrns3)

#-----------------------------------------------------------------------------------------
SimpleRtrns3<-Return.calculate(AC3, method = "discrete")
#rename the columns names as returns instead of close
colnames(SimpleRtrns3) = c("BTC","ETH","USDT")
SimpleRtrns3<-SimpleRtrns3[-1]
head(SimpleRtrns3)
#-----------------------------------------------------------------------------------------
AdjClosed3 = cbind(BTCdf$BTC_Adjusted,ETHdf$ETH_Adjusted,USDTdf$USDT_Adjusted)
colnames(AdjClosed3) = c("BTC","ETH","USDT")
#-----------------------------------------------------------------------------------------
#time series plot of close prices of 3 stocks in one figure
options(repr.plot.width=3, repr.plot.height=2)
plot.zoo(LogRtrns3, plot.type = "single", main = "Log Returns", 
         col =  c("red","blue","green"),
         lty = c("solid","dashed", "dotted"), 
         lwd = 2, ylab = "Log Returns",  xlab='1Jan2018- 1Jan2023', )
abline(h = 0)
legend(x = "bottomleft", legend = colnames(LogRtrns3),
       lty = c("solid", "dashed", "dotted"), 
       lwd = 2,
       col =  c("red","blue","green")
)
#--------------------------------------------------------------------------------------
# Calculate the correlation matrix
sink("CorrAdjClPrices3.xls")

corAC3= round(cor(AdjClosed3),3)
corAC3
corLogRtrns3 <- round(cor(LogRtrns3),3)
corLogRtrns3
sink()
#-----------------------------------------------------------------------------------------
sink("DescripriveStatistics3.xls")

DescriStat3 = rbind( apply(LogRtrns3, 2, mean),
                     apply(LogRtrns3, 2, var), 
                     apply(LogRtrns3, 2, sd),
                     apply(LogRtrns3, 2, skewness), 
                     apply(LogRtrns3, 2, kurtosis))
rownames(DescriStat3) = c("Mean", "Variance", "Std Dev", "Skewness", "Excess Kurtosis")
round(DescriStat3 , digits = 3)
sink()
#-----------------------------------------------------------------------------------------
sink("ADFReturns3.txt")
#------------------------#

adf.test(SimpleRtrns3$BTC)
kpss.test(SimpleRtrns3$BTC,  null="Trend")
#Ljung Bob test for aunto correlation
Box.test(SimpleRtrns3$BTC, lag = 20, type = c("Ljung-Box"), fitdf = 0)
summary(SimpleRtrns3$BTC)

adf.test(SimpleRtrns3$ETH)
kpss.test(SimpleRtrns3$ETH,  null="Trend")
#Ljung Bob test for aunto correlation
Box.test(SimpleRtrns3$ETH, lag = 20, type = c("Ljung-Box"), fitdf = 0)
summary(SimpleRtrns3$ETH)

adf.test(SimpleRtrns3$USDT)
kpss.test(SimpleRtrns3$USDT,  null="Trend")
#Ljung Bob test for aunto correlation
Box.test(SimpleRtrns3$USDT, lag = 20, type = c("Ljung-Box"), fitdf = 0)
summary(SimpleRtrns3$USDT)

#------------------------------------------------
adf.test(LogRtrns3$BTC)
kpss.test(LogRtrns3$BTC,  null="Trend")
#Ljung Bob test for auto correlation
Box.test(LogRtrns3$BTC, lag = 20, type = c("Ljung-Box"), fitdf = 0)
summary(LogRtrns3$BTC)

adf.test(LogRtrns3$ETH)
kpss.test(LogRtrns3$ETH,  null="Trend")
#Ljung Bob test for auto correlation
Box.test(LogRtrns3$ETH, lag = 20, type = c("Ljung-Box"), fitdf = 0)
summary(LogRtrns3$ETH)


adf.test(LogRtrns3$USDT)
kpss.test(LogRtrns3$USDT,  null="Trend")
#Ljung Bob test for auto correlation
Box.test(LogRtrns3$USDT, lag = 20, type = c("Ljung-Box"), fitdf = 0)
summary(LogRtrns3$USDT)
#-----------------------------------------------------------ACF PACF GRAPH
par(mar = c(5, 5, 5, 5) + 0.1)
par(mfrow = c(3,2))
acf(LogRtrns3$BTC,  main = 'ACF  log returns BTC')
pacf(LogRtrns3$BTC, main = 'PACF log returns BTC')

acf(LogRtrns3$ETH,  main = 'ACF  log returns ETH')
pacf(LogRtrns3$ETH, main = 'PACF log returns ETH')

acf(LogRtrns3$USDT,  main = 'ACF  log returns USDT')
pacf(LogRtrns3$USDT, main = 'PACF log returns USDT')
#------------------------#
sink()
#----------------------------------------------------------------------------------------------------------------------Orders of Integration
# ---------------------------------------------- NonLinearity ------------------------------------------------ 
sink("nonlinearityTest3.xls")

nonlinearityTest(BTCdf$BTC_Adjusted,  verbose = FALSE)
nonlinearityTest(ETHdf$ETH_Adjusted,  verbose = FALSE)
nonlinearityTest(USDTdf$USDT_Adjusted, verbose = FALSE)

sink()
# ---------------------------------------------- Stationarity ------------------------------------------------ #
sink("Stationarity3.txt")

adf.test(BTCdf$BTC_Adjusted)
PP.test(BTCdf$BTC_Adjusted)
kpss.test(BTCdf$BTC_Adjusted) # KPSS unit root test 
kpss.test(BTCdf$BTC_Adjusted,null="Trend")
URndBTC=unitroot_ndiffs(BTCdf$BTC_Adjusted,alpha = 0.05,unitroot_fn = ~unitroot_kpss(.)["kpss_pvalue"],differences = 0:5)# testing for number of unit roots 
URnsdBTC=unitroot_nsdiffs(BTCdf$BTC_Adjusted,alpha = 0.05,unitroot_fn = ~feat_stl(., .period)[2] < 0.64,differences = 0:5) # testing for seasonal differences required
BTC.SDiff2 =ndiffs(BTCdf$BTC_Adjusted, alpha=0.05, test=c("kpss","adf", "pp"), max.d=5)


adf.test(ETHdf$ETH_Adjusted)
PP.test(ETHdf$ETH_Adjusted)
kpss.test(ETHdf$ETH_Adjusted) # KPSS unit root test 
kpss.test(ETHdf$ETH_Adjusted,null="Trend")
URndETH=unitroot_ndiffs(ETHdf$ETH_Adjusted,alpha = 0.05,unitroot_fn = ~unitroot_kpss(.)["kpss_pvalue"],differences = 0:5)# testing for number of unit roots
URnsdETH=unitroot_nsdiffs(ETHdf$ETH_Adjusted,alpha = 0.05,unitroot_fn = ~feat_stl(., .period)[2] < 0.64,differences = 0:5) # testing for seasonal differences required
ETH.SDiff2 =ndiffs(ETHdf$ETH_Adjusted, alpha=0.05, test=c("kpss","adf", "pp"), max.d=5)

adf.test(USDTdf$USDT_Adjusted)
PP.test(USDTdf$USDT_Adjusted)
kpss.test(USDTdf$USDT_Adjusted) # KPSS unit root test 
kpss.test(USDTdf$USDT_Adjusted,null="Trend")
URndUSDT=unitroot_ndiffs(USDTdf$USDT_Adjusted,alpha = 0.05,unitroot_fn = ~unitroot_kpss(.)["kpss_pvalue"],differences = 0:5)# testing for number of unit roots
URnsdUSDT=unitroot_nsdiffs(USDTdf$USDT_Adjusted,alpha = 0.05,unitroot_fn = ~feat_stl(., .period)[2] < 0.64,differences = 0:5) # testing for seasonal differences required
USDT.SDiff2 =ndiffs(USDTdf$USDT_Adjusted, alpha=0.05, test=c("kpss","adf", "pp"), max.d=5)
sink()

#-----------------------------------------------------------ACF PACF GRAPH
par(mar = c(1, 1, 1, 1) + 0.1)
par(mfrow = c(3,2))
acf(BTCdf$BTC_Adjusted,  main = 'ACF AdjustedClose BTC')
pacf(BTCdf$BTC_Adjusted, main = 'PACF AdjustedClose BTC')

acf(ETHdf$ETH_Adjusted,  main = 'ACF AdjustedClose ETH')
pacf(ETHdf$ETH_Adjusted, main = 'PACF AdjustedClose ETH')

acf(USDTdf$USDT_Adjusted,  main = 'ACF  AdjustedClose USDT')
pacf(USDTdf$USDT_Adjusted, main = 'PACF AdjustedClose USDT')

# ---------------------------------------------- Decomposition ------------------------------------------------ #
sink("Decompose3.txt")
Decompose3=decompose(AC3);Decompose3
StlBTC_AdjC =stl(BTCdf$BTC_Adjusted,s.window = "periodic");StlBTC_AdjC
StlETH_AdjC =stl(ETHdf$ETH_Adjusted,s.window = "periodic");StlETH_AdjC
StlUSDT_AdjC=stl(USDTdf$USDT_Adjusted,s.window = "periodic");StlUSDT_AdjC
sink()
# ---------------------------------------------- Differencing ------------------------------------------------ #
#2.Make stationary
sink("ADFdiff3.txt")

BTCAdjdiff= diff(BTCdf$BTC_Adjusted)
adf.test(BTCAdjdiff)
kpss.test(BTCAdjdiff, null="Trend")
par(oma=c(1, 1, 1, 1) )
par(mar = c(3, 1, 3, 1))
par(mfrow=c(3,1))
plot(BTCAdjdiff,type="l")
acf(BTCAdjdiff,  main = 'ACF Adjusted Close Bitcoin')
pacf(BTCAdjdiff, main = 'PACF Adjusted Close Bitcoin')
auto.arima(BTCdf$BTC_Adjusted); auto.arima(BTCAdjdiff)

ETHAdjdiff= diff(ETHdf$ETH_Adjusted)
adf.test(ETHAdjdiff)
kpss.test(ETHAdjdiff, null="Trend")
par(oma=c(1, 1, 1, 1) )
par(mar = c(3, 1, 3, 1))
par(mfrow=c(3,1))
plot(ETHAdjdiff,type="l")
acf(ETHAdjdiff,  main = 'ACF Adjusted Close Ethereum')
pacf(ETHAdjdiff, main = 'PACF Adjusted Close Ethereum')
auto.arima(ETHdf$ETH_Adjusted); auto.arima(ETHAdjdiff)

USDTAdjdiff= diff(USDTdf$USDT_Adjusted)
adf.test(USDTAdjdiff)
kpss.test(USDTAdjdiff, null="Trend")
par(oma=c(1, 1, 1, 1) )
par(mar = c(3, 1, 3, 1))
par(mfrow=c(3,1))
plot(USDTAdjdiff,type="l")
acf(USDTAdjdiff,  main = 'ACF Adjusted Close Tether')
pacf(USDTAdjdiff, main = 'PACF Adjusted Close Tether')
auto.arima(USDTdf$USDT_Adjusted); auto.arima(USDTAdjdiff)

sink()

#-----------------------------------------------------------ACF PACF GRAPH
par(mar = c(2, 1, 2, 1))
par(mfrow = c(3,2))
acf(BTCAdjdiff,  main = 'ACF 1st diff AdjustedClose BTC')
pacf(BTCAdjdiff, main = 'PACF 1st diff AdjustedClose BTC')

acf(ETHAdjdiff,  main = 'ACF 1st diff AdjustedClose ETH')
pacf(ETHAdjdiff, main = 'PACF 1st diff AdjustedClose ETH')

acf(USDTAdjdiff,  main = 'ACF 1st diff AdjustedClose USDT')
pacf(USDTAdjdiff, main = 'PACF1st diff AdjustedClose USDT')
#----------------------------------------------------------
par(mar = c(2, 1, 2, 1))
par(mfrow = c(3,1))
plot(BTCAdjdiff,  main = '1st diff AdjustedClose BTC')
plot(ETHAdjdiff,  main = '1st diff AdjustedClose ETH')
plot(USDTAdjdiff, main = '1st diff AdjustedClose USDT')
#---------------------------------------------------------------------------------------------------------------------Cointegration
sink("CointegrationAC3.txt")

lagselect <- VARselect(AC3, lag.max = 30, type = "none")
lagselect$selection
summary(lagselect,lagselect$selection)


jotest7=ca.jo(AC3,  type="eigen", K=7, ecdet="none", spec="longrun");summary(jotest7)
jotest6=ca.jo(AC3,  type="eigen", K=6, ecdet="none", spec="longrun");summary(jotest6)
jotest3=ca.jo(AC3,  type="eigen", K=3, ecdet="none", spec="longrun");summary(jotest3)


Portfolio2 = 1.000*BTC_AC  + 4.131068e+00*ETH_AC  + 2.457119e+07*USDT_AC
adf.test(Portfolio2 )
plot(Portfolio2, type="l")

Portfolio3 = 1.000*BTC_AC  +  3.562798e+00*ETH_AC  + 2.254886e+07*USDT_AC
adf.test(Portfolio3 )
plot(Portfolio3, type="l")

Portfolio1 = 1.000*BTC_AC  + 1.261644e+01*ETH_AC  + 3.861888e+07*USDT_AC
adf.test(Portfolio1)
par(mar = c(6, 6, 6, 6) + 0.1)
#windows.options(width = 2, height = 2, reset = FALSE)
plot(Portfolio1, type="l")
options(repr.plot.width=3, repr.plot.height=3)

sink()
#---------------------------------------------------------------------------------------------------------------------Causality
sink("GrangerLogRerturns3.txt")

VARselect( c(LogRtrns3$BTC, LogRtrns3$ETH), lag.max = 30, type = "none")
grangertest(LogRtrns3$BTC ~ LogRtrns3$ETH, order = 3)
grangertest(LogRtrns3$ETH ~ LogRtrns3$BTC, order = 3)

VARselect( c(LogRtrns3$BTC,  LogRtrns3$USDT), lag.max = 30, type = "none")
grangertest(LogRtrns3$BTC  ~ LogRtrns3$USDT, order = 2)
grangertest(LogRtrns3$USDT ~ LogRtrns3$BTC, order = 2)

VARselect( c(LogRtrns3$ETH,  LogRtrns3$USDT), lag.max = 30, type = "none")
grangertest(LogRtrns3$ETH  ~ LogRtrns3$USDT,  order = 2)
grangertest(LogRtrns3$USDT ~ LogRtrns3$ETH, order = 2)
sink()
#------------------------------------------------------------------------------------------------ VAR Log Returns      
sink("VARLogReturns3.txt")
#---------------------------------
lagselect <- VARselect(LogRtrns3, lag.max = 30, type = "none")
lagselect$selection
lagselect$criteria
summary(lagselect,lagselect$selection,lagselect$criteria)
#---------------------------------
VARmodelLR=VAR(LogRtrns3, p = 6, type = "const", season=NULL, exog=NULL, ic = c("AIC", "HQ", "SC", "FPE"))
summary(VARmodelLR)
coefficients(VARmodelLR)
summary(VARmodelLR)$corres
print(VARmodelLR, digits = max(3, getOption("digits") - 3))
#---------------------------------lags.pt = 6, 
SerialCorrResiduals=serial.test(VARmodelLR,lags.pt = 9,  type = "PT.adjusted") #serial correlation in the residuals  
print(SerialCorrResiduals )
#---------------------------------
#H0: that the residuals are homoscedastic
Hetrscdsty =arch.test(VARmodelLR,  multivariate.only = TRUE) # heteroscedasticity test 
Hetrscdsty

Nrmlt <- normality.test(VARmodelLR, multivariate.only = TRUE)
Nrmlt

stabilityTest <- stability(VARmodelLR)
stabilityTest$stability$BTC

par(mar = c(1, 1, 1, 1))
dev.new(width=6, height=3)
par(col = "black", col.axis = "mediumpurple", mfrow = c(3,1))
plot( stabilityTest$stability$BTC, main="BTC  log returns")
plot(stabilityTest$stability$ETH,  main="ETH  log returns")
plot(stabilityTest$stability$USDT, main="USDT log returns")


StabilityTest =sctest(VARmodelLR) # p-value of stability test
StabilityTest
sink()

#----------------------------------------------------------------------------#forecast for 30 days 
N=30 
forecastVARLR=predict(VARmodelLR,n.ahead=N, ci=0.95)
#-------------- 
par(mar = c(4, 6, 3, 1) + 0.1)
par(mfrow = c(3,1))
dev.new(width=6, height=6)
plot(forecastVARLR, xlab="days:02Jan2018-31Jan2023", ylab="USD")
#----------------------------------------------------------------------------#impulse response function
sink("ImpulseResponse3ACdiff.txt")
#---------------------------------
irf_varBTClr  <- irf(VARmodelLR, n.ahead = 30, ortho = F, impulse = "BTC", response = c("ETH","USDT"))
irf_varETHlr  <- irf(VARmodelLR, n.ahead = 30, ortho = F, impulse = "ETH" , response = c("BTC","USDT"))
irf_varUSDTlr <- irf(VARmodelLR, n.ahead = 30, ortho = F, impulse = "USDT", response = c("ETH","BTC"))

#par(mfrow = c(3,2))
par(mar = c(1, 1, 1, 1))
dev.new(width=6, height=3)
plot(irf_varBTClr)
plot(irf_varETHlr)
plot(irf_varUSDTlr)
#--------------------------forecast error variance decomposition of a VAR(p) for n.ahead steps.

#The variance decomposition indicates the amount of information each variable contributes to the other variables in the autoregression. 
#It determines how much of the forecast error variance of each of the variables can be explained by exogenous shocks to the other variables
par(mar = c(1, 1, 1, 1))
fevd_Mvar <- fevd(VARmodelLR, n.ahead = 30)
par(col = "mediumpurple", col.axis = "mediumpurple", mfrow = c(3,1), mar = c(2,2,2,2))
plot(fevd_Mvar)

sink()
#----------------------------------------------------------------------------
sink("CausalityVARmodelLR3.txt") 
causality(VARmodelLR, cause = "BTC")
causality(VARmodelLR, cause = "ETH")
causality(VARmodelLR, cause = "USDT")

sink()
#------------------------------------------------------------------------------------------------ 
sink("VAR3ACdiff.txt")

AC3diff=cbind(BTCAdjdiff,ETHAdjdiff,USDTAdjdiff) 
#---------------------------------
lagselect <- VARselect(AC3diff, lag.max = 30, type = "none")
lagselect$selection
lagselect$criteria
summary(lagselect,lagselect$selection,lagselect$criteria)
#---------------------------------
VARmodel=VAR(AC3diff, p = 7, type = "const", season=NULL, exog=NULL, ic = c("AIC", "HQ", "SC", "FPE"))
summary(VARmodel)
coefficients(VARmodel)
summary(VARmodel)$corres
print(VARmodel, digits = max(3, getOption("digits") - 3))
#---------------------------------
SerialCorrResiduals=serial.test(VARmodel, lags.pt = 8, type = "PT.adjusted") #serial correlation in the residuals  
print(SerialCorrResiduals )
#---------------------------------
#H0: that the residuals are homoscedastic
Hetrscdsty =arch.test(VARmodel,  multivariate.only = TRUE) # heteroscedasticity test 
Hetrscdsty

Nrmlt <- normality.test(VARmodel, multivariate.only = TRUE)
Nrmlt

stabilityTest <- stability(VARmodel)
dev.new(width=6, height=3)
par(col = "black", col.axis = "mediumpurple", mfrow = c(3,1))
plot(stabilityTest$stability$BTCAdjdiff, main="BTC in 1st difference")
plot(stabilityTest$stability$ETHAdjdiff, main="ETH in 1st difference")
plot(stabilityTest$stability$USDTAdjdiff,main="USDT in 1st difference")


StabilityTest =sctest(VARmodel) # p-value of stability test
StabilityTest
sink()

#----------------------------------------------------------------------------#forecast for 30 days 
N=30 
NN=nrow(AdjClosed3)
forecastVAR=predict(VARmodel,n.ahead=N, ci=0.95)
#--------------
Rbtc=numeric(NN+N); Reth=numeric(NN+N); Rusdt=numeric(NN+N);
#--------------insert historical data 
Rbtc[1:NN] =AdjClosed3[,1]; head(Rbtc)
Reth[1:NN] =AdjClosed3[,2]; head(Reth)
Rusdt[1:NN]=AdjClosed3[,3]; head(Rusdt)

#--------------predict levels from differences
Rbtc[(NN+1):(NN+N)]=Rbtc[1:NN]+  cumsum(forecastVAR$fcst[["BTCAdjdiff"]][,1]);Rbtc[(NN+1):(NN+N)]
Reth[(NN+1):(NN+N)]=Reth[1:NN]+  cumsum(forecastVAR$fcst[["ETHAdjdiff"]][,1]);head(Reth)
Rusdt[(NN+1):(NN+N)]=Rusdt[1:NN]+ cumsum(forecastVAR$fcst[["USDTAdjdiff"]][,1]);head(Rusdt)

#--------------transform to time series
Rbtc=ts(Rbtc)
Reth=ts(Reth)
Rusdt=ts(Rusdt)
Crypto3df = data.frame(BTC = Rbtc, ETH = Reth,USDT = Rusdt)
Dates = seq(from = as.Date("2018-01-01"), to = as.Date("2023-01-31"), by = 'day')
Crypto3df[,4]=Dates
(names(Crypto3df)[4] <- "Date")
rownames(Crypto3df) <- Crypto3df[,4]
Crypto3df <- Crypto3df[,-4]

#--------------
par(mar = c(4, 4, 2, 2) + 0.1)
par(mfrow = c(3,1))
dev.new(width=6, height=3)
plot(Rbtc,xlab='1Jan2018- 31Jan2023',   ylab="USD",   main="BTC  price forecast by VAR")
plot(Reth,  xlab='1Jan2018- 31Jan2023', ylab="USD",   main="ETH  price forecast by VAR")
plot(Rusdt, xlab='1Jan2018- 31Jan2023' ,ylab="USD",   main="USDT price forecast by VAR")

#----------------------------------------------------------------------------#impulse response function
sink("ImpulseResponse3ACdiff.txt")
#---------------------------------
irf_varBTC  <- irf(VARmodel, n.ahead = 30, ortho = F, impulse = "BTCAdjdiff", response = c("ETHAdjdiff","USDTAdjdiff"))
irf_varETH  <- irf(VARmodel, n.ahead = 30, ortho = F, impulse = "ETHAdjdiff", response = c("BTCAdjdiff","USDTAdjdiff"))
irf_varUSDT <- irf(VARmodel, n.ahead = 30, ortho = F, impulse = "USDTAdjdiff",response = c("ETHAdjdiff", "BTCAdjdiff"))

par(mfrow = c(3,2))
par(mar = c(1, 1, 1, 1))
dev.new(width=6, height=3)
plot(irf_varBTC)
plot(irf_varETH)
plot(irf_varUSDT)

#--------------------------forecast error variance decomposition of a VAR(p) for n.ahead steps.

#The variance decomposition indicates the amount of information each variable contributes to the other variables in the autoregression. 
#It determines how much of the forecast error variance of each of the variables can be explained by exogenous shocks to the other variables
par(mar = c(1, 1, 1, 1))
fevd_Mvar <- fevd(VARmodel, n.ahead = 30)
par(col = "mediumpurple", col.axis = "mediumpurple", mfrow = c(3,1), mar = c(2,2,2,2))
plot(fevd_Mvar)

sink()
#----------------------------------------------------------------------------
sink("CausalityVARmodelLR3.txt") 
causality(VARmodel, cause = "BTCAdjdiff")
causality(VARmodel, cause = "ETHAdjdiff")
causality(VARmodel, cause = "USDTAdjdiff")

sink()

#---------------------------------------------------------------------Transfer Entorpy
sink("TransferEntropy3ETH_BTC.txt")
teETH_BTC=transfer_entropy(x=LogRtrns3$ETH, y=LogRtrns3$BTC)
teETH_BTC

ETH_BTC=calc_ete(LogRtrns3$ETH, LogRtrns3$BTC)
ETH_BTC 

sink()
#--------------------
sink("TransferEntropy3ETH_USDT.txt")
teETH_USDT=transfer_entropy(x=LogRtrns3$ETH, y=LogRtrns3$USDT)
teETH_USDT

ETH_USDT=calc_ete(LogRtrns3$ETH, LogRtrns3$USDT)
ETH_USDT

sink() 
#--------------------
sink("TransferEntropy3BTC_USDT.txt")
teBTC_USD=transfer_entropy(x=LogRtrns3$BTC, y=LogRtrns3$USDT)
teBTC_USD

BTC_USDT=calc_ete(LogRtrns3$BTC, LogRtrns3$USDT)
BTC_USDT

sink()
#----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------plot returns with squared and absolute returns

par(mar = c(1, 1, 1, 1) )

PlotBTC = cbind(LogRtrns3$BTC, LogRtrns3$BTC^2, abs(LogRtrns3$BTC))
colnames(PlotBTC) = c("ReturnsBTC", "ReturnsBTC^2", "abs(ReturnsBTC)")
plot.zoo(PlotBTC, main="BTC Daily Returns", col="red") 

PlotETH = cbind(LogRtrns3$ETH, LogRtrns3$ETH^2, abs(LogRtrns3$ETH))
colnames(PlotETH) = c("ReturnsETH", "ReturnsETH^2", "abs(ReturnsETH)")
plot.zoo(PlotETH, main="ETH Daily Returns", col="blue") 

PlotUSDT = cbind(LogRtrns3$USDT, LogRtrns3$USDT^2, abs(LogRtrns3$USDT))
colnames(PlotUSDT) = c("ReturnsBTC", "ReturnsUSDT^2", "abs(ReturnsUSDT)")
plot.zoo(PlotUSDT, main="USDT Daily Returns", col="green") 
#----------------------------------------------------------------------------ADF
sink("TestLogRtrns3.txt")

ADF_ReturnsBTC = ur.df(LogRtrns3$BTC, type = "drift",selectlags = "AIC" )
summary(ADF_ReturnsBTC)

ADF_ReturnsETH= ur.df(LogRtrns3$ETH, type = "drift",selectlags = "AIC" )
summary(ADF_ReturnsETH)

ADF_ReturnsUSDT= ur.df(LogRtrns3$USDT, type = "drift",selectlags = "AIC" )
summary(ADF_ReturnsUSDT)
#----------------------------------------------------------------------------Normality
jarque.bera.test(LogRtrns3$BTC)
jarque.bera.test(LogRtrns3$ETH)
jarque.bera.test(LogRtrns3$USDT)
#----------------------------------------------------------------------------Heteroscecasticity
# Ljung Box.test to check auto correlation in square retruns
Box.test(LogRtrns3$BTC^2, type="Ljung-Box", lag = 12)
#ARCH LM Test
ArchTest(LogRtrns3$BTC, lags=12)

Box.test(LogRtrns3$ETH^2, type="Ljung-Box", lag = 12)
#ARCH LM Test
ArchTest(LogRtrns3$ETH, lags=12)

Box.test(LogRtrns3$USDT^2, type="Ljung-Box", lag = 12)
#ARCH LM Test
ArchTest(LogRtrns3$USDT, lags=12)


auto.arima(LogRtrns3$BTC)
auto.arima(LogRtrns3$ETH)
auto.arima(LogRtrns3$USDT)

sink()

#----------------------------------------------------------------------------------------------------------------------GARCH-DCC
sink("GARCH3.txt")

#Specify the model,variance.targeting = F 
spec1 = ugarchspec(    mean.model=list(armaOrder=c(1,0,2), include.mean=F),
                       variance.model=list(garchOrder=c(1,1),model="sGARCH"), 
                       distribution="sged")
spec1   

# DCC estimation
DCC_garch1<-dccspec(uspec = multispec(replicate(3, spec1)), dccOrder = c(1,1),distribution = "mvt")
DCC_garch1

DCC_garch1_mod<-dccfit(DCC_garch1, data = LogRtrns3) # fix the model
DCC_garch1_mod


AICdcc<-infocriteria(DCC_garch1_mod)[1]
AICdcc
#---------------------------------------------------------------------------
specBTC <- ugarchspec(  
  mean.model = list(armaOrder = c(1,0) , include.mean=T),
  variance.model = list(model = "sGARCH"),
  distribution.model = "sged")
specBTC

specETH = ugarchspec(
  mean.model = list(armaOrder = c(1,0) , include.mean=T),
  variance.model = list(model = "sGARCH"),
  distribution.model = "sstd")
specETH 

specUSDT= ugarchspec(
  mean.model = list(armaOrder = c(1,0), include.mean=T),
  variance.model = list(model = "eGARCH"),
  distribution.model = "sstd")
specUSDT

spec2 = c(specBTC, specETH,specUSDT)
spec2

# DCC estimation
DCC_garch2 = dccspec(uspec  = multispec( spec2 ), dccOrder = c(1,1), distribution = "mvt")
DCC_garch2

DCC_garch2_model<-dccfit(DCC_garch2, data = LogRtrns3) # fix the model
DCC_garch2_model

AICdcc<-infocriteria(DCC_garch2_model)[1]
AICdcc

corr=rcor(DCC_garch2_model);dim(corr)
cov=rcov(DCC_garch2_model);dim(cov)
corr[,,dim(corr)[3]]#last day correlation matrix
cov[,,dim(cov)[3]]#last day covariation matrix
#---------------
corrBTC_ETH=corr[1,2,]
corrBTC_USDT=corr[1,3,]
corrETH_USDT=corr[2,3,]

par(mfrow = c(3,1), mar = c(1, 3, 3, 1))
plot.ts(corrBTC_ETH, col="red",   main="Conditional Correlation between BTC and ETH")
plot.ts(corrBTC_USDT, col="blue", main="Conditional Correlation between BTC and USDT")
plot.ts(corrETH_USDT, col="green",main="Conditional Correlation between ETH and USDT")
#---------------
covBTC_ETH=cov[1,2,]
covBTC_USDT=cov[1,3,]
covETH_USDT=cov[2,3,]

par(mfrow = c(3,1), mar = c(1, 3, 3, 1))
plot.ts(covBTC_ETH, col="red",    main="Covariation between BTC and ETH")
plot.ts(covBTC_USDT, col="blue",  main="Covariation between BTC and USDT")
plot.ts(covETH_USDT, col="green", main="Covariation between ETH and USDT")
#---------------------------------------------------------------------------

DCC_garchETH_model<-dccfit(DCC_garch2, data = data.frame(LogRtrns3$ETH,LogRtrns3$BTC,LogRtrns3$USDT)) # fix the model
DCC_garchETH_model

DCC_garchUSDT_model<-dccfit(DCC_garch2, data = data.frame(LogRtrns3$USDT,LogRtrns3$ETH,LogRtrns3$BTC)) # fix the model
DCC_garchUSDT_model

sink()

#----------------------------------------------------------------------------------------------------------------------LSTM for bitcoin

#standarized the data using scale()
priceBTC <- scale(BTCdf$BTC_Adjusted)
priceBTC <- data.frame(priceBTC)
head(priceBTC)
nrow(priceBTC)
#-------------------------------------------------------------------------------------------------
#select hyperparameters
TimeSteps <- 32 # choose sequence length 8 
input_dim <- 1#input_dim <- 1
hidden_dim <- 32
num_layers <- 2
output_dim <- 1
batch_size = 21 #common factor of train and test sets
epochs <- 200 #epochs <- 200
window_size=TimeSteps -1
#-------------------------------------------------------------------------------------------------#train and test split
Test_sizeBTC  <- batch_size*14  # approx 19-20%% split 
Train_sizeBTC <- nrow(priceBTC) - Test_sizeBTC
SplitBTC=Test_sizeBTC/Train_sizeBTC 
SplitBTC
#-------------------------------------------------------------------------------------------------
priceBTC_ini <- as.matrix(priceBTC) # convert to matrix
dataBTC     <- array(dim = c(0, TimeSteps,1))

# create all possible sequences of length TimeSteps
for (i in 1:(length(priceBTC_ini) - TimeSteps)) {
  dataBTC <- rbind(dataBTC, priceBTC_ini[i:(i + TimeSteps - 1), ])
}

#divide data into train and test sets 
Train_XBTC <- dataBTC[1: Train_sizeBTC, 1:(TimeSteps - 1), drop = FALSE]
Train_YBTC <- dataBTC[1: Train_sizeBTC, TimeSteps, drop = FALSE]

Test_XBTC <- dataBTC[( Train_sizeBTC + 1):nrow(dataBTC), 1:(TimeSteps - 1), drop = FALSE]
Test_YBTC <- dataBTC[( Train_sizeBTC + 1):nrow(dataBTC), TimeSteps, drop = FALSE]

#-------------------------------------------------------------------------------------------------
# Reshape the training and test data to get a 3D tensor shape
Train_XBTC <- array_reshape(Train_XBTC, c(dim(Train_XBTC)[1], TimeSteps-1, input_dim))
Test_XBTC  <- array_reshape(Test_XBTC,  c(dim(Test_XBTC)[1],  TimeSteps-1, input_dim))

#-------------------------------------------------------------------------------------------------
# Define the LSTM LSTMmodel using Keras
LSTMmodel <- keras_model_sequential() %>%
  layer_lstm(units = hidden_dim, 
             return_sequences = TRUE, 
             input_shape = c(window_size, input_dim),activation = "relu") %>%
  layer_lstm(units = hidden_dim, activation = "relu") %>%
  layer_dense(units = output_dim)
summary(LSTMmodel)

# Compile the LSTMmodel using the mean squared error loss and the Adam optimizer
LSTMmodel %>% compile(  loss = 'mse',  optimizer = optimizer_adam( learning_rate = 0.01)) 

# Train the LSTMmodel on the training data
FitLSTM <- LSTMmodel %>% fit(Train_XBTC, Train_YBTC, epochs = epochs, batch_size = batch_size, verbose=1, validation_data = list(Test_XBTC, Test_YBTC)
)
plot(FitLSTM)
#-------------------------------------------------------------------------------------------------
# Extract predictions from the estimated LSTM model
Train_Y_predictedBTC <- LSTMmodel %>% predict(Train_XBTC)
Test_Y_predictedBTC  <- LSTMmodel %>% predict(Test_XBTC)

#-------------------------------------------------------------------------------------------------
# Rescale back the predictions and original values
Train_Y_predicted_rescaledBTC  <- Train_Y_predictedBTC * sd(BTCdf$BTC_Adjusted) + mean(BTCdf$BTC_Adjusted)
Train_Y_rescaledBTC            <- Train_YBTC           * sd(BTCdf$BTC_Adjusted) + mean(BTCdf$BTC_Adjusted)
Test_Y_predicted_rescaledBTC   <- Test_Y_predictedBTC  * sd(BTCdf$BTC_Adjusted) + mean(BTCdf$BTC_Adjusted)
Test_Y_rescaledBTC             <- Test_YBTC            * sd(BTCdf$BTC_Adjusted) + mean(BTCdf$BTC_Adjusted)

#-------------------------------------------------------------------------------------------------
# Shift the predicted values to start from where the training data predictions end
shift <- length(Train_Y_predicted_rescaledBTC)
Test_Y_predicted_rescaled_shiftedBTC <- c(rep(NA, shift), Test_Y_predicted_rescaledBTC[,1])
#-------------------------------------------------------------------------------------------------
# Plot the training and predicted values
plot(Train_Y_rescaledBTC, type = "l",main="Daily BTC Adjusted Closed prices", col = "blue", xlab = "Day", ylab = "Price", lwd=1)
lines(Train_Y_predicted_rescaledBTC, col = "red")
legend(x = "topleft", legend = c("Training Data", "Training Predictions"),  cex=0.7, col = c("blue", "red"), lwd = 1)
grid()
#-------------------------------------------------------------------------------------------------
# Plot the loss of training data
plot(FitLSTM$metrics$loss, type = "l",main="Traning Loss", xlab = "Epochs", ylab = "Loss", col = "blue",lwd=1)
grid()
#-------------------------------------------------------------------------------------------------
# Plot the training and predicted values
plot(BTCdf$BTC_Adjusted, type = "l", main="BTC Price Prediction ", xlab = "Day", ylab = "Price",col = "blue",lwd=1)
lines(Train_Y_predicted_rescaledBTC, col = "black",lwd=1)
lines(Test_Y_predicted_rescaled_shiftedBTC, type = "l",col = "red",lwd=1)
legend(x = "topleft", legend = c("Original Data", "Training Predictions","Testing Predictions"), cex=0.6, col = c("blue","black" ,"red"), lwd = 1)
grid()
#-------------------------------------------------------------------------------------------------
mse <- round(FitLSTM$metrics$val_loss[length(FitLSTM$metrics$val_loss)], 5)
mse
#Evaluate on training data
TrainEvaluateBTC=LSTMmodel %>% evaluate( Train_XBTC,  Train_YBTC) 
TrainEvaluateBTC
TestEvaluateBTC=LSTMmodel %>% evaluate(Test_XBTC, Test_YBTC) 
TestEvaluateBTC
#----------------------------------------------------------------------------------------------------------------------LSTM for ETH
priceETH <- scale(ETHdf$ETH_Adjusted)
priceETH <- data.frame(priceETH)
head(priceETH)
nrow(priceETH)
#-------------------------------------------------------------------------------------------------
#select hyperparameters
TimeSteps <- 32 # choose sequence length 8 
input_dim <- 1#input_dim <- 1
hidden_dim <- 32
num_layers <- 2
output_dim <- 1
batch_size = 21 #common factor of train and test sets
epochs <- 200 #epochs <- 200
window_size=TimeSteps -1
#-------------------------------------------------------------------------------------------------#train and test split
Test_sizeETH  <- batch_size*14  # approx 19-20%% split 
Train_sizeETH <- nrow(priceETH) - Test_sizeETH
SplitETH=Test_sizeETH/Train_sizeETH 
SplitETH
#-------------------------------------------------------------------------------------------------
priceETH_ini <- as.matrix(priceETH) # convert to matrix
dataETH     <- array(dim = c(0, TimeSteps,1))

# create all possible sequences of length TimeSteps
for (i in 1:(length(priceETH_ini) - TimeSteps)) {
  dataETH <- rbind(dataETH, priceETH_ini[i:(i + TimeSteps - 1), ])
}

#divide data into train and test sets 
Train_XETH <- dataETH[1: Train_sizeETH, 1:(TimeSteps - 1), drop = FALSE]
Train_YETH <- dataETH[1: Train_sizeETH, TimeSteps, drop = FALSE]

Test_XETH <- dataETH[( Train_sizeETH + 1):nrow(dataETH), 1:(TimeSteps - 1), drop = FALSE]
Test_YETH <- dataETH[( Train_sizeETH + 1):nrow(dataETH), TimeSteps, drop = FALSE]

#-------------------------------------------------------------------------------------------------
# Reshape the training and test data to get a 3D tensor shape
Train_XETH <- array_reshape(Train_XETH, c(dim(Train_XETH)[1], TimeSteps-1, input_dim))
Test_XETH  <- array_reshape(Test_XETH,  c(dim(Test_XETH)[1],  TimeSteps-1, input_dim))

#-------------------------------------------------------------------------------------------------
# Define the LSTM LSTMmodel using Keras
LSTMmodel <- keras_model_sequential() %>%
  layer_lstm(units = hidden_dim, 
             return_sequences = TRUE, 
             #input_shape = c(window_size, input_dim),activation = "relu") %>%
             input_shape = c(window_size, input_dim),activation = "sigmoid") %>%
  #  layer_lstm(units = hidden_dim, activation = "relu") %>%
  layer_lstm(units = hidden_dim, activation = "sigmoid") %>%
  #  bidirectional(layer_lstm(units = hidden_dim)) %>%
  layer_dense(units = output_dim)
summary(LSTMmodel)

# Compile the LSTMmodel using the mean squared error loss and the Adam optimizer
LSTMmodel %>% compile(  loss = 'mse',  optimizer = optimizer_adam( learning_rate = 0.01)) 

# Train the LSTMmodel on the training data
FitLSTM <- LSTMmodel %>% fit(Train_XETH, Train_YETH, epochs = epochs, batch_size = batch_size, verbose=1, validation_data = list(Test_XETH, Test_YETH)
                             #                             ,callbacks=callback_early_stopping(patience=20,verbose=1, monitor='val_loss') 
)
plot(FitLSTM)
#-------------------------------------------------------------------------------------------------
# Extract predictions from the estimated LSTM model
Train_Y_predictedETH <- LSTMmodel %>% predict(Train_XETH)
Test_Y_predictedETH  <- LSTMmodel %>% predict(Test_XETH)

#-------------------------------------------------------------------------------------------------
# Rescale back the predictions and original values
Train_Y_predicted_rescaledETH  <- Train_Y_predictedETH  * sd(ETHdf$ETH_Adjusted) + mean(ETHdf$ETH_Adjusted)
Train_Y_rescaledETH            <- Train_YETH           * sd(ETHdf$ETH_Adjusted) + mean(ETHdf$ETH_Adjusted)
Test_Y_predicted_rescaledETH   <- Test_Y_predictedETH  * sd(ETHdf$ETH_Adjusted) + mean(ETHdf$ETH_Adjusted)
Test_Y_rescaledETH             <- Test_YETH            * sd(ETHdf$ETH_Adjusted) + mean(ETHdf$ETH_Adjusted)

#-------------------------------------------------------------------------------------------------
# Shift the predicted values to start from where the training data predictions end
shift <- length(Train_Y_predicted_rescaledETH)
Test_Y_predicted_rescaled_shiftedETH <- c(rep(NA, shift), Test_Y_predicted_rescaledETH[,1])
#-------------------------------------------------------------------------------------------------
# Plot the training and predicted values
options(repr.plot.width=2, repr.plot.height=2)
plot(Train_Y_rescaledETH, type = "l",main="Daily ETH Adjusted Closed prices", col = "black", xlab = "Day", ylab = "Price", lwd=1)
lines(Train_Y_predicted_rescaledETH, col = "red")
legend(x = "topleft", legend = c("Training Data", "Training Predictions"),  cex=0.9, col = c("black", "red"), lwd = 1)
grid()
#-------------------------------------------------------------------------------------------------
# Plot the loss of training data
options(repr.plot.width=2, repr.plot.height=2)
plot(FitLSTM$metrics$loss, type = "l",main="Traning Loss", xlab = "Epochs", ylab = "Loss", col = "blue",lwd=1)
grid()
#-------------------------------------------------------------------------------------------------
# Plot the training and predicted values
options(repr.plot.width=2, repr.plot.height=2)
plot(ETHdf$ETH_Adjusted, type = "l", main="ETH Price Prediction ", xlab = "Day", ylab = "Price",col = "green",lwd=1)
lines(Train_Y_predicted_rescaledETH, col = "black",lwd=1)
lines(Test_Y_predicted_rescaled_shiftedETH, type = "l",col = "red",lwd=1)
legend(x = "topleft", legend = c("Original Data", "Training Predictions","Testing Predictions"), cex=0.6, col = c("green","black" ,"red"), lwd = 1)
grid()
#-------------------------------------------------------------------------------------------------
mse <- round(FitLSTM$metrics$val_loss[length(FitLSTM$metrics$val_loss)], 5)
mse
#Evaluate on training data
TrainEvaluateETH=LSTMmodel %>% evaluate( Train_XETH,  Train_YETH) 
TrainEvaluateETH
TestEvaluateETH=LSTMmodel %>% evaluate(Test_XETH, Test_YETH) 
TestEvaluateETH
#----------------------------------------------------------------------------------------------------------------------LSTM for USDT
#standarized the data using scale()
priceUSDT <- scale(USDTdf$USDT_Adjusted)
priceUSDT <- data.frame(priceUSDT)
head(priceUSDT)
nrow(priceUSDT)
#-------------------------------------------------------------------------------------------------
#select hyperparameters
TimeSteps <- 32 # choose sequence length 8 
input_dim <- 1#input_dim <- 1
hidden_dim <- 32
num_layers <- 2
output_dim <- 1
batch_size = 21 #common factor of train and test sets
epochs <- 200 #epochs <- 200
window_size=TimeSteps -1
#-------------------------------------------------------------------------------------------------#train and test split
Test_sizeUSDT  <- batch_size*14  # approx 19-20%% split 
Train_sizeUSDT <- nrow(priceUSDT) - Test_sizeUSDT
SplitUSDT=Test_sizeUSDT/Train_sizeUSDT 
SplitUSDT
#-------------------------------------------------------------------------------------------------
priceUSDT_ini <- as.matrix(priceUSDT) # convert to matrix
dataUSDT     <- array(dim = c(0, TimeSteps,1))

# create all possible sequences of length TimeSteps
for (i in 1:(length(priceUSDT_ini) - TimeSteps)) {
  dataUSDT <- rbind(dataUSDT, priceUSDT_ini[i:(i + TimeSteps - 1), ])
}

#divide data into train and test sets 
Train_XUSDT <- dataUSDT[1: Train_sizeUSDT, 1:(TimeSteps - 1), drop = FALSE]
Train_YUSDT <- dataUSDT[1: Train_sizeUSDT, TimeSteps, drop = FALSE]

Test_XUSDT <- dataUSDT[( Train_sizeUSDT + 1):nrow(dataUSDT), 1:(TimeSteps - 1), drop = FALSE]
Test_YUSDT <- dataUSDT[( Train_sizeUSDT + 1):nrow(dataUSDT), TimeSteps, drop = FALSE]

#-------------------------------------------------------------------------------------------------
# Reshape the training and test data to get a 3D tensor shape
Train_XUSDT <- array_reshape(Train_XUSDT, c(dim(Train_XUSDT)[1], TimeSteps-1, input_dim))
Test_XUSDT  <- array_reshape(Test_XUSDT,  c(dim(Test_XUSDT)[1],  TimeSteps-1, input_dim))

#-------------------------------------------------------------------------------------------------
# Define the LSTM LSTMmodel using Keras
LSTMmodel <- keras_model_sequential() %>%
  layer_lstm(units = hidden_dim, 
             return_sequences = TRUE, 
             input_shape = c(window_size, input_dim),activation = "relu") %>%
  #  input_shape = c(window_size, input_dim),activation = "sigmoid") %>%
  layer_lstm(units = hidden_dim, activation = "relu") %>%
  #  layer_lstm(units = hidden_dim, activation = "sigmoid") %>%
  #  bidirectional(layer_lstm(units = hidden_dim)) %>%
  layer_dense(units = output_dim)
summary(LSTMmodel)

# Compile the LSTMmodel using the mean squared error loss and the Adam optimizer
LSTMmodel %>% compile(  loss = 'mse',  optimizer = optimizer_adam( learning_rate = 0.01)) 

# Train the LSTMmodel on the training data
FitLSTM <- LSTMmodel %>% fit(Train_XUSDT, Train_YUSDT, epochs = epochs, batch_size = batch_size, verbose=1, validation_data = list(Test_XUSDT, Test_YUSDT)
                             #                             ,callbacks=callback_early_stopping(patience=20,verbose=1, monitor='val_loss') 
)
plot(FitLSTM)
#-------------------------------------------------------------------------------------------------
# Extract predictions from the estimated LSTM model
Train_Y_predictedUSDT <- LSTMmodel %>% predict(Train_XUSDT)
Test_Y_predictedUSDT  <- LSTMmodel %>% predict(Test_XUSDT)

#-------------------------------------------------------------------------------------------------
# Rescale back the predictions and original values
Train_Y_predicted_rescaledUSDT  <- Train_Y_predictedUSDT * sd(USDTdf$USDT_Adjusted) + mean(USDTdf$USDT_Adjusted)
Train_Y_rescaledUSDT            <- Train_YUSDT           * sd(USDTdf$USDT_Adjusted) + mean(USDTdf$USDT_Adjusted)
Test_Y_predicted_rescaledUSDT   <- Test_Y_predictedUSDT  * sd(USDTdf$USDT_Adjusted) + mean(USDTdf$USDT_Adjusted)
Test_Y_rescaledUSDT             <- Test_YUSDT            * sd(USDTdf$USDT_Adjusted) + mean(USDTdf$USDT_Adjusted)

#-------------------------------------------------------------------------------------------------
# Shift the predicted values to start from where the training data predictions end
shift <- length(Train_Y_predicted_rescaledUSDT)
Test_Y_predicted_rescaled_shiftedUSDT <- c(rep(NA, shift), Test_Y_predicted_rescaledUSDT[,1])
#-------------------------------------------------------------------------------------------------
# Plot the training and predicted values
options(repr.plot.width=2, repr.plot.height=2)
plot(Train_Y_rescaledUSDT, type = "l",main="Daily USDT Adjusted Closed prices", col = "black", xlab = "Day", ylab = "Price", lwd=1)
lines(Train_Y_predicted_rescaledUSDT, col = "red")
legend(x = "topleft", legend = c("Training Data", "Training Predictions"),  cex=0.9, col = c("black", "red"), lwd = 1)
grid()
#-------------------------------------------------------------------------------------------------
# Plot the loss of training data
options(repr.plot.width=2, repr.plot.height=2)
plot(FitLSTM$metrics$loss, type = "l",main="Traning Loss", xlab = "Epochs", ylab = "Loss", col = "blue",lwd=1)
grid()
#-------------------------------------------------------------------------------------------------
# Plot the training and predicted values
options(repr.plot.width=2, repr.plot.height=2)
plot(USDTdf$USDT_Adjusted, type = "l", main="USDT Price Prediction ", xlab = "Day", ylab = "Price",col = "green",lwd=1)
lines(Train_Y_predicted_rescaledUSDT, col = "black",lwd=1)
lines(Test_Y_predicted_rescaled_shiftedUSDT, type = "l",col = "red",lwd=1)
legend(x = "topleft", legend = c("Original Data", "Training Predictions","Testing Predictions"), cex=0.6, col = c("green","black" ,"red"), lwd = 1)
grid()
#-------------------------------------------------------------------------------------------------
mse <- round(FitLSTM$metrics$val_loss[length(FitLSTM$metrics$val_loss)], 5)
mse
#Evaluate on training data
TrainEvaluateUSDT=LSTMmodel %>% evaluate( Train_XUSDT,  Train_YUSDT) 
TrainEvaluateUSDT
TestEvaluateUSDT=LSTMmodel %>% evaluate(Test_XUSDT, Test_YUSDT) 
TestEvaluateUSDT
#----------------------------------------------------------------------------------------------------------------------ARIMA
Test_size  <- 21*14  # approx 19% split 
Train_size <- length(BTCdf$BTC_Adjusted) - Test_size
Split=Test_size/Train_size 
Split
#----------------------------------------------------------------------------------
PriceBTCa=as.matrix(BTCdf$BTC_Adjusted ) 
TrainBTC=data.frame(PriceBTCa[1:Train_size,]);nrow(TrainBTC)
TestBTC=data.frame(PriceBTCa[(Train_size+1):length(PriceBTCa),]);nrow(TestBTC)
#---------------------------------------------------------------------------------
#on train data
fitBTC <- auto.arima(TrainBTC)
forecastTrainBTC  <- window(fitted(fitBTC), start=1)
#on test data
refitBTC <- Arima(PriceBTCa, model=fitBTC)
#extract forecast on test data
forecastTestBTC <- window(fitted(refitBTC), start=1534)
#--------------------------------------
forecastedTrainBTC =data.frame(forecastTrainBTC)
forecastedTestBTC  =data.frame(forecastTestBTC)
#--------------------------------------
sink("ARIMAbtc.txt")
summary(fitBTC);summary(refitBTC)
summary(fitBTC)$residuals;summary(refitBTC)$residuals
round(accuracy(fitBTC),3);round(accuracy(refitBTC),3)


checkresiduals(fitBTC);checkresiduals(refitBTC);
adf.test(residuals(fitBTC))

checkresiduals(refitBTC);checkresiduals(refitBTC);
adf.test(residuals(refitBTC))
sink()
#--------------------------------------
par(mar = c(4, 4, 4, 4) + 0.1)
plot(BTCdf$BTC_Adjusted, type = "l", main="Daily BTC Adjusted Closed prices, USD", xlab = "Day", ylab = "Price",col = "green",lwd=1)
lines(forecastTrainBTC,  type = "l", col = "black",lwd=1)
lines(forecastTestBTC,   type = "l", col = "red",lwd=1)
legend(x = "topleft", legend = c("Original Data", "Training Predictions","Testing Predictions"), cex=0.7, col = c("green","black" ,"red"), lwd = 1)
grid()
#----------------------------------------------------------------------------------------------------------------------
PriceETHa=as.matrix(ETHdf$ETH_Adjusted ) 
TrainETH=data.frame(PriceETHa[1:Train_size,])

fitETH <- auto.arima(TrainETH)
forecastTrainETH  <- window(fitted(fitETH), start=1)
#on test data
refitETH <- Arima(PriceETHa, model=fitETH)
#extract forecast on test data
forecastTestETH <- window(fitted(refitETH), start=1534)
#--------------------------------------
sink("ARIMAeth.txt")
summary(fitETH);summary(refitETH)
round(accuracy(fitETH),3);round(accuracy(refitETH),3)

checkresiduals(fitETH);adf.test(residuals(fitETH))

checkresiduals(refitETH);adf.test(residuals(refitETH))
sink()
#--------------------------------------
par(mar = c(4, 4, 4, 4) + 0.1)
mfrow=c(1,1)

plot(ETHdf$ETH_Adjusted, type = "l", main="Daily ETH Adjusted Closed prices, USD", xlab = "Day", ylab = "Price",col = "green",lwd=1)
lines(forecastTrainETH,  type = "l", col = "black",lwd=1)
lines(forecastTestETH,   type = "l", col = "red",lwd=1)
legend(x = "topleft", legend = c("Original Data", "Training Predictions","Testing Predictions"), cex=0.7, col = c("green","black" ,"red"), lwd = 1)
grid()
#----------------------------------------------------------------------------------------------------------------------
PriceUSDTa=as.matrix(USDTdf$USDT_Adjusted ) 
TrainUSDT=data.frame(PriceUSDTa[1:Train_size,])


fitUSDT <- auto.arima(TrainUSDT)
forecastTrainUSDT <- window(fitted(fitUSDT), start=1)
#on test data
refitUSDT <- Arima(PriceUSDTa, model=fitUSDT)
#extract forecast on test data
forecastTestUSDT<- window(fitted(refitUSDT), start=1534)
#--------------------------------------
sink("ARIMAusdt.txt")
summary(fitUSDT);summary(refitUSDT)
round(accuracy(fitUSDT),3);round(accuracy(refitUSDT),3)

fitUSDT$results

checkresiduals(fitUSDT);adf.test(residuals(fitUSDT))
sink()
#--------------------------------------
par(mar = c(4, 4, 4, 4) + 0.1)
mfrow=c(1,1)

plot(USDTdf$USDT_Adjusted, type = "l", main="Daily USDT Adjusted Closed prices, USD", xlab = "Day", ylab = "Price",col = "green",lwd=1)
lines(forecastTrainUSDT,  type = "l", col = "black",lwd=1)
lines(forecastTestUSDT,   type = "l", col = "red",lwd=1)
legend(x = "topleft", legend = c("Original Data", "Training Predictions","Testing Predictions"), cex=0.7, col = c("green","black" ,"red"), lwd = 1)
grid()
#----------------------------------------------------------------------------------------------------------------------




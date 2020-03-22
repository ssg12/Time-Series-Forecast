####################################################################################
#The purpose of this project is to get you started with time series forecasting in R. 
#The template can be modified to handle new data sets and other forecasting methods 
#supported in the R package "forecast". 
# ####################################################################################

library("forecast") #load forecast library

##################Load data#####################
#Load the data 
demandData <- TimeSeriesData

# ###########Extract time series and plot#########
dataFreq= 4 #Data frequency of time series. Set to 12 and 4 for monthly and quaterly data, respectively
startEntry= c(2005,1) #Time stamp of first entry in time series e.g. c(2005,1) implies first quarter of 2005 if data frequency equals 4

blackplasticdemand <- ts(demandData$`Black Plastic Demand ('000 lbs)`, frequency=dataFreq,
                 start=startEntry) #create a time series

plot(blackplasticdemand,main = "Black plastic demand over the years",
    xlab="Year.Quarter",ylab="Black plastic demand") #plot time series.

clearplasticdemand <- ts(demandData$`Clear Plastic Demand ('000 lbs)`, frequency=dataFreq,
               start=startEntry) #create a time series

plot(clearplasticdemand,main = "Clear plastic demand over the years",
     xlab="Year.Quarter",ylab="clear plastic demand") #plot time series.

# ###########Decompose time series and plot#########
#Decompose methods return an object containing
clearplasticDecomp <- decompose(clearplasticdemand, type="multiplicative") #classical decomposition. Can be set to multiplicative or additive
plot(clearplasticDecomp) #plot decomposed time series

blackplasticDecomp <- decompose(blackplasticdemand, type="multiplicative") #classical decomposition. Can be set to multiplicative or additive
plot(blackplasticDecomp) #plot decomposed time series
# 
# ###########Prepare time series for forecasting#########
# ###We partition the time series into a training set for
# ###forecasting and a test set to evaluate accuracy
trainSetStart= c(2005,1) #training set start location in time series (typically the first entry)
trainSetEnd= c(2007,4) #training set end location in time series (typically covers 70% of time series)
testSetStart= c(2008,1) #test set start location in time series (typically location of entry after training set ends)
testSetEnd= c(2009,4) #test set end location in time series (typically end of time series)
# 
cleardemandTrain <- window(clearplasticdemand,start=trainSetStart,end=trainSetEnd) #extract training set
cleardemandTest <- window(clearplasticdemand,start=testSetStart,end=testSetEnd) #extract test set

blackdemandTrain <- window(blackplasticdemand,start=trainSetStart,end=trainSetEnd) #extract training set
blackdemandTest <- window(blackplasticdemand,start=testSetStart,end=testSetEnd) #extract test set
# 
# ###########Forecast#########
numForcPeriods = 8 #number of periods to forecast in the future (Note: If you are computing errors with respect to testing data then this value
                    #should be equal to the duration of the testing data)

AverageDemandForecastblack <- matrix(, nrow = numForcPeriods, ncol = 1)
for(t in 1:numForcPeriods){
  AverageDemandForecastblack[t] <- (blackplasticdemand[t]+blackplasticdemand[t+4]+blackplasticdemand[t+8])/3
}

AverageDemandForecastclear <- matrix(, nrow = numForcPeriods, ncol = 1)
for(t in 1:numForcPeriods){
  AverageDemandForecastclear[t] <- (clearplasticdemand[t]+clearplasticdemand[t+4]+clearplasticdemand[t+8])/3
}

clearHWForcModel <- HoltWinters(cleardemandTrain,seasonal="multiplicative") #Train Holt-Winters forecasting model. Can be additive or multiplicative
clearHWForecast <- forecast(clearHWForcModel, h=numForcPeriods, model = HoltWinters) #Foreacst using Holt-Winters model trained in previous step

blackHWForcModel <- HoltWinters(blackdemandTrain,seasonal="multiplicative") #Train Holt-Winters forecasting model. Can be additive or multiplicative
blackHWForecast <- forecast(blackHWForcModel, h=numForcPeriods, model = HoltWinters) #Foreacst using Holt-Winters model trained in previous step

plot(clearHWForecast, main="Plot for clear plastic forecast with 80% and 95%
     prediction intervals",xlab="Year.Quarter",
     ylab="Clear plastic Demand") #plot the training demand, and forecast with prediction intervals

lines(cleardemandTest,col=2) #add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2),
legend=c("Training Demand","Forecast","Testing Demand")) #create plot legend

plot(blackHWForecast, main="Plot for black plastic forecast with 80% and 95%
     prediction intervals",xlab="Year.Quarter",
     ylab="Black plastic Demand") #plot the training demand, and forecast with prediction intervals

lines(blackdemandTest,col=2) #add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2),
       legend=c("Training Demand","Forecast","Testing Demand")) #create plot legend

# ###########Analyze forecasting error#########
avgblackerror = AverageDemandForecastblack - blackdemandTest #difference between forecast and actual demand
avgblackAD = abs(avgblackerror) #absolute value of error

avgclearerror = AverageDemandForecastclear - cleardemandTest #difference between forecast and actual demand
avgclearAD = abs(avgclearerror) #absolute value of error

blackerror = blackHWForecast$mean - blackdemandTest #difference between forecast and actual demand
blackAD=abs(blackerror) #absolute value of error

clearerror = clearHWForecast$mean - cleardemandTest #difference between forecast and actual demand
clearAD=abs(clearerror) #absolute value of error

#Create empty vectors to store errors
blackMSE <- matrix(, nrow = numForcPeriods, ncol = 1)
blackMAD <- matrix(, nrow = numForcPeriods, ncol = 1)
blackMAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
blackbias <- matrix(, nrow = numForcPeriods, ncol = 1)
blackTS <- matrix(, nrow = numForcPeriods, ncol = 1)

clearMSE <- matrix(, nrow = numForcPeriods, ncol = 1)
clearMAD <- matrix(, nrow = numForcPeriods, ncol = 1)
clearMAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
clearbias <- matrix(, nrow = numForcPeriods, ncol = 1)
clearTS <- matrix(, nrow = numForcPeriods, ncol = 1)

avgblackMSE <- matrix(, nrow = numForcPeriods, ncol = 1)
avgblackMAD <- matrix(, nrow = numForcPeriods, ncol = 1)
avgblackMAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
avgblackbias <- matrix(, nrow = numForcPeriods, ncol = 1)
avgblackTS <- matrix(, nrow = numForcPeriods, ncol = 1)

avgclearMSE <- matrix(, nrow = numForcPeriods, ncol = 1)
avgclearMAD <- matrix(, nrow = numForcPeriods, ncol = 1)
avgclearMAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
avgclearbias <- matrix(, nrow = numForcPeriods, ncol = 1)
avgclearTS <- matrix(, nrow = numForcPeriods, ncol = 1)

#Label columns of matrices using name of error
colnames(blackMSE) <- "MSE"
colnames(blackMAD) <- "MAD"
colnames(blackMAPE) <- "MAPE"
colnames(blackbias) <- "bias"
colnames(blackTS) <- "TS"

colnames(clearMSE) <- "MSE"
colnames(clearMAD) <- "MAD"
colnames(clearMAPE) <- "MAPE"
colnames(clearbias) <- "bias"
colnames(clearTS) <- "TS"

colnames(avgblackerror) <- "Error"
colnames(avgblackAD) <- "AD"
colnames(avgblackMSE) <- "MSE"
colnames(avgblackMAD) <- "MAD"
colnames(avgblackMAPE) <- "MAPE"
colnames(avgblackbias) <- "bias"
colnames(avgblackTS) <- "TS"

colnames(avgclearerror) <- "Error"
colnames(avgclearAD) <- "AD"
colnames(avgclearMSE) <- "MSE"
colnames(avgclearMAD) <- "MAD"
colnames(avgclearMAPE) <- "MAPE"
colnames(avgclearbias) <- "bias"
colnames(avgclearTS) <- "TS"
# 
# #compute errors
for(t in 1:numForcPeriods){
  blackMSE[t] <- mean(blackerror[1:t]*blackerror[1:t])
  blackMAD[t] <- mean(blackAD[1:t])
  blackMAPE[t] <- mean(100*abs(blackerror[1:t]/blackdemandTest[1:t]))
  blackbias[t] <- sum(blackerror[1:t])
  blackTS[t]= blackbias[t]/blackMAD[t]
}

for(t in 1:numForcPeriods){
  clearMSE[t] <- mean(clearerror[1:t]*clearerror[1:t])
  clearMAD[t] <- mean(clearAD[1:t])
  clearMAPE[t] <- mean(100*abs(clearerror[1:t]/cleardemandTest[1:t]))
  clearbias[t] <- sum(clearerror[1:t])
  clearTS[t]= clearbias[t]/clearMAD[t]
}

for(t in 1:numForcPeriods){
  avgblackMSE[t] <- mean(avgblackerror[1:t]*avgblackerror[1:t])
  avgblackMAD[t] <- mean(avgblackAD[1:t])
  avgblackMAPE[t] <- mean(100*abs(avgblackerror[1:t]/blackdemandTest[1:t]))
  avgblackbias[t] <- sum(avgblackerror[1:t])
  avgblackTS[t]= avgblackbias[t]/avgblackMAD[t]
}

for(t in 1:numForcPeriods){
  avgclearMSE[t] <- mean(avgclearerror[1:t]*avgclearerror[1:t])
  avgclearMAD[t] <- mean(avgclearAD[1:t])
  avgclearMAPE[t] <- mean(100*abs(avgclearerror[1:t]/cleardemandTest[1:t]))
  avgclearbias[t] <- sum(avgclearerror[1:t])
  avgclearTS[t]= avgclearbias[t]/avgclearMAD[t]
}

#combined vectors into a dataframe, also appending year and quarter information in the first two columns
avgblackerror_Meas <- data.frame(floor(time(blackerror)),cycle(blackerror),blackdemandTest,AverageDemandForecastblack,avgblackerror,avgblackAD,avgblackMSE,avgblackMAD,avgblackMAPE,avgblackbias,avgblackTS)
colnames(avgblackerror_Meas)[1] <- "Year"
colnames(avgblackerror_Meas)[2] <- "Qtr"
colnames(avgblackerror_Meas)[3] <- "Actual demand"
colnames(avgblackerror_Meas)[4] <- "Forecast"

#combined vectors into a dataframe, also appending year and quarter information in the first two columns
avgclearerror_Meas <- data.frame(floor(time(clearerror)),cycle(clearerror),cleardemandTest,AverageDemandForecastclear,avgclearerror,avgclearAD,avgclearMSE,avgclearMAD,avgclearMAPE,avgclearbias,avgclearTS)
colnames(avgclearerror_Meas)[1] <- "Year"
colnames(avgclearerror_Meas)[2] <- "Qtr"
colnames(avgclearerror_Meas)[3] <- "Actual demand"
colnames(avgclearerror_Meas)[4] <- "Forecast"

#combined vectors into a dataframe, also appending year and quarter information in the first two columns
blackerror_Meas <- data.frame(floor(time(blackerror)),cycle(blackerror),blackdemandTest,blackHWForecast$mean,blackerror,blackAD,blackMSE,blackMAD,blackMAPE,blackbias,blackTS)
colnames(blackerror_Meas)[1] <- "Year"
colnames(blackerror_Meas)[2] <- "Qtr"
colnames(blackerror_Meas)[3] <- "Actual demand"
colnames(blackerror_Meas)[4] <- "Forecast"
colnames(blackerror_Meas)[5] <- "Error"
colnames(blackerror_Meas)[6] <- "AD"

#combined vectors into a dataframe, also appending year and quarter information in the first two columns
clearerror_Meas <- data.frame(floor(time(clearerror)),cycle(clearerror),cleardemandTest,clearHWForecast$mean,clearerror,clearAD,clearMSE,clearMAD,clearMAPE,clearbias,clearTS)
colnames(clearerror_Meas)[1] <- "Year"
colnames(clearerror_Meas)[2] <- "Qtr"
colnames(clearerror_Meas)[3] <- "Actual demand"
colnames(clearerror_Meas)[4] <- "Forecast"
colnames(clearerror_Meas)[5] <- "Error"
colnames(clearerror_Meas)[6] <- "AD"



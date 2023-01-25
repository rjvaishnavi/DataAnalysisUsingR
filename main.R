library(dplyr)
library(ggplot2)
library(imputeTS)
library(Hmisc)
library(lubridate)

# library required for forecasting 
library(forecast)
library(gtools)
library(readr)
library(fpp2)
library(TTR)



# library used for correlation plotting
library(corrplot)

#reading the data
data <- read.csv("../dataset_withNull.csv")

View(data)

# Converting the data types to proper types
data$Date <- as.Date(data$Date, "%Y-%m-%d")
#data$DateTime <- asPOSIXlt(data$DateTime, format = "%Y-%m-%d %H:%M")
data$DateTime <- as.POSIXct(data$DateTime, format = "%Y-%m-%d %H:%M:%S")
data$CO__GT <-as.numeric(data$CO__GT)
data$NMHC__GT <-as.numeric(data$NMHC__GT)
data$Nox__GT<-as.numeric(data$Nox__GT)

View(data)

summary(data) # before cleaning data


# Checking the most amount of null Values present in the data set to delete them
colSums(is.na(data))


# imputing median value for NA  values
data$CO__GT <- with(data, impute(data$CO__GT, median))
data$PT08_S1__CO <- with(data, impute(data$PT08_S1__CO, median))
data$NMHC__GT <- with(data, impute(data$NMHC__GT, median))
data$C6H6__GT <- with(data, impute(data$C6H6__GT, median))
data$PT08_S2__NMHC <- with(data, impute(data$PT08_S2__NMHC, median))
data$Nox__GT <- with(data, impute(data$Nox__GT, median))
data$PT08_S3__Nox <- with(data, impute(data$PT08_S3__Nox, median))
data$NO2__GT <- with(data, impute(data$N02__GT, median))
data$PT08_S4__NO2 <- with(data, impute(data$PT08_S4__NO2, median))
data$PT08_S5__O3 <- with(data, impute(data$PT08_S5__O3, median))
data$T <- with(data, impute(data$T, median))
data$RH <- with(data, impute(data$RH, median))
data$AH <- with(data, impute(data$AH, median))

# Checking the most amount of null Values present after imputing the data set to delete them
colSums(is.na(data))

summary(data) # after cleaning data


#testData <- subset(data, select = c( "DateTime", "CO__GT"))



#Plotting graphs for each attribute

ggplot(data, aes(x = DateTime, y = CO__GT)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )
ggplot(data, aes(x = DateTime, y = PT08_S1__CO)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )

ggplot(data, aes(x = DateTime, y = NMHC__GT)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )


ggplot(data, aes(x = DateTime, y = C6H6__GT)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )

ggplot(data, aes(x = DateTime, y = PT08_S2__NMHC)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )

ggplot(data, aes(x = DateTime, y = Nox__GT)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )
ggplot(data, aes(x = DateTime, y = PT08_S3__Nox)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )

ggplot(data, aes(x = DateTime, y = PT08_S4__NO2)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )

ggplot(data, aes(x = DateTime, y = PT08_S5__O3)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )

ggplot(data, aes(x = DateTime, y = T)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )

ggplot(data, aes(x = DateTime, y = RH)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )

ggplot(data, aes(x = DateTime, y = AH)) +
  geom_line() +
  scale_x_datetime( breaks = "1 month",
                    expand = c(0,0),
                    date_labels = '%m-%y',
  )




# finding the correlation between the attributes:
round(cor(data[c('CO__GT', 'PT08_S1__CO', 'NMHC__GT','C6H6__GT', 'PT08_S2__NMHC', 'Nox__GT', 'PT08_S3__Nox','PT08_S4__NO2', 'PT08_S5__O3', 'T', 'AH', 'RH')]), 2)
M <- cor(data[c('CO__GT', 'PT08_S1__CO', 'NMHC__GT','C6H6__GT', 'PT08_S2__NMHC', 'Nox__GT', 'PT08_S3__Nox','PT08_S4__NO2', 'PT08_S5__O3', 'T', 'AH', 'RH')])

head(round(M,2))

# as colour
corrplot(M, method="color")

# as number
corrplot(M, method="number")

# creating a subset of data based on the correlated values, with a correlation value greater than 0.89.

corr_data <- subset(data, select = c( 'DateTime','PT08_S1__CO','C6H6__GT', 'PT08_S2__NMHC','PT08_S5__O3'))



#finding the correlation of these values only:
corr2 <- cor(data[c('PT08_S1__CO','C6H6__GT', 'PT08_S2__NMHC','PT08_S5__O3')])

head(round(cor(data[c('PT08_S1__CO','C6H6__GT', 'PT08_S2__NMHC','PT08_S5__O3')]), 2))
# as number
corrplot(corr2, method="number")


#plotting histograms to see the frequency of values

h <- hist(corr_data$PT08_S1__CO,
          main="For PT08_S1__CO",
          xlab="Values of Sensor",
          col="darkmagenta",
          xlim=c(500,2500),
          ylim = c(0,2500),
          breaks=10
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h <- hist(corr_data$C6H6__GT,
          main="For C6H6__GT",
          xlab="Values in micrograms",
          col="darkmagenta",
          xlim=c(0,70),
          ylim = c(0,3500),
          breaks=10
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h <- hist(corr_data$PT08_S2__NMHC,
          main="For PT08_SS2__NMHC",
          xlab="Values of Sensor",
          col="darkmagenta",
          xlim=c(500,2500),
          ylim = c(0,3000),
          breaks=10
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

h <- hist(corr_data$PT08_S5__O3,
          main="For PT08_S5__O3",
          xlab="Values of Sensor",
          col="darkmagenta",
          xlim=c(500,3000),
          ylim = c(0,2500),
          breaks=10
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))


#making a new seperate datasets for each value:
newdata <- subset(data, select = c( 'Date','PT08_S1__CO','C6H6__GT', 'PT08_S2__NMHC','PT08_S5__O3'))

newdata$Date <- as.Date(newdata$Date)

newdata$time <- format(newdata$Date, format ="%Y-%m-%d")


str(newdata)

#  Aggregating each data type on days to get  mean average
co <- aggregate( PT08_S1__CO ~ time , newdata , mean )
c6 <- aggregate( C6H6__GT ~ time , newdata , mean )
nm <- aggregate( PT08_S2__NMHC ~ time , newdata , mean )
o3 <- aggregate( PT08_S5__O3 ~ time , newdata , mean )

#merging the data sets
fin = merge(x = co, y = c6, by = "time")
fin2 = merge(x = fin, y = nm, by = "time")
final = merge(x = fin2, y = o3, by = "time")
final
str(final)
fin3 <- subset(final, select = c('PT08_S1__CO','C6H6__GT', 'PT08_S2__NMHC','PT08_S5__O3'))
fin3


# Converting the data set to a TimeSeries Object:
tsdata = ts(fin3, start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
tsdata
autoplot(tsdata)
plot(tsdata)


# Predicting



dim(final)
x = 390 *0.8
x
y = 390 - x
y

train <- head(final,312L)
test <- tail(final, 78L)
head(train)
test

nrow(train); nrow(test)
head(tsdata)

#Mean Absolute Percentage Error (or MAPE)
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}


################### NAIVE FORCASTING ############################
#Naive forecasting: for PT08_S1__CO Levels:
tsdata = ts(test[,2], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
naive_mod <- naive(tsdata, h = y )
summary(naive_mod)
df = as.data.frame(naive_mod)
test$naiveco = df$`Point Forecast`
print("Naive Method: Error in PT08_S1__CO Levels:");mape(test$PT08_S1__CO, test$naiveco)

#Naive forecasting: for C6H6 Levels:
tsdata = ts(test[,3], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
naive_mod <- naive(tsdata, h = y )
summary(naive_mod)
df = as.data.frame(naive_mod)
test$naivec6 = df$`Point Forecast`
print("Naive Method: Error in C6H6__GT Levels:");mape(test$C6H6__GT, test$naivec6)

#Naive forecasting: for NMHC Levels:
tsdata = ts(test[,4], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
naive_mod <- naive(tsdata, h = y )
summary(naive_mod)
df = as.data.frame(naive_mod)
test$naivenm = df$`Point Forecast`
print("Naive Method: Error in PT08_S2__NMHC Levels:");mape(test$PT08_S2__NMHC, test$naivenm)


#Naive forecasting: for O3 Levels:
tsdata = ts(test[,5], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
naive_mod <- naive(tsdata, h = y )
summary(naive_mod)
df = as.data.frame(naive_mod)
test$naiveo3 = df$`Point Forecast`
print("Naive Method: Error in PT08_S5__O3 Levels:");mape(test$PT08_S5__O3, test$naiveo3)



################### SIMPLE EXPONENTIAL SMOOTHING ############################

#For CO levels:
tsdata = ts(test[,2], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
se_model <- ses(tsdata, h = y)
summary(se_model)
df = as.data.frame(se_model)
test$simplexpco = df$`Point Forecast`
print("Simple Exponential Smoothening: Error in PT08_S1__CO Levels:");mape(test$PT08_S1__CO, test$simplexpco)


#For C6H6
tsdata = ts(test[,3], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
se_model <- ses(tsdata, h = y)
summary(se_model)
df = as.data.frame(se_model)
test$simplexpc6 = df$`Point Forecast`
print("Simple Exponential Smoothening: Error in C6H6__GT Levels:");mape(test$C6H6__GT, test$simplexpc6)

#For NMHC
tsdata = ts(test[,4], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
se_model <- ses(tsdata, h =y)
summary(se_model)
df = as.data.frame(se_model)
test$simplexpnm = df$`Point Forecast`
print("Simple Exponential Smoothening: Error in PT08_S2__NMHC Levels:");mape(test$PT08_S2__NMHC, test$simplexpnm)

#For O3:
tsdata = ts(test[,5], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
se_model <- ses(tsdata, h = y)
summary(se_model)
df = as.data.frame(se_model)
test$simplexpo3 = df$`Point Forecast`
print("Simple Exponential Smoothening: Error in PT08_S5__O3 Levels:");mape(test$PT08_S5__O3, test$simplexpo3)







################# Holt's Trend Method #################################


#For CO levels:
tsdata = ts(test[,2], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
holt_model <- holt(tsdata, h = y)
summary(holt_model)
df_holt = as.data.frame(holt_model)
test$holtco = df_holt$`Point Forecast`
print("Holt's Trend Method: Error in PT08_S1__CO Levels:");mape(test$PT08_S1__CO, test$holtco)


#For C6H6
tsdata = ts(test[,3], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
holt_model <- holt(tsdata, h = y)
summary(holt_model)
df_holt = as.data.frame(holt_model)
test$holtc6 = df_holt$`Point Forecast`
print("Holt's Trend Method: Error in C6H6__GT Levels:");mape(test$C6H6__GT, test$holtc6)

#For NMHC
tsdata = ts(test[,4], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
holt_model <- holt(tsdata, h = y)
summary(holt_model)
df_holt = as.data.frame(holt_model)
test$holtnm = df_holt$`Point Forecast`
print("Holt's Trend Method: Error in PT08_S2__NMHC Levels:");mape(test$PT08_S2__NMHC, test$holtnm)

#For O3:
tsdata = ts(test[,5], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
holt_model <- holt(tsdata, h = y)
summary(holt_model)
df_holt = as.data.frame(holt_model)
test$holt03 = df_holt$`Point Forecast`
print("Holt's Trend Method: Error in PT08_S5__O3 Levels:");mape(test$PT08_S5__O3, test$holt03)



################# ARIMA Method #####################################

#For CO levels:
tsdata = ts(test[,2], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
arima_model <- auto.arima(tsdata)
summary(arima_model)
fore_arima = forecast::forecast(arima_model, h=y)
df_arima = as.data.frame(fore_arima)
test$arimaCO = df_arima$`Point Forecast`
print("ARIMA Method: Error in PT08_S1__CO Levels:"); mape(test$PT08_S1__CO, test$arimaCO)


#For C6H6
tsdata = ts(test[,3], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
arima_model <- auto.arima(tsdata)
summary(arima_model)
fore_arima = forecast::forecast(arima_model, h=y)
df_arima = as.data.frame(fore_arima)
test$arimac6 = df_arima$`Point Forecast`
print("ARIMA Method: Error in C6H6__GT Levels:"); mape(test$C6H6__GT, test$arimac6)


#For NMHC
tsdata = ts(test[,4], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
arima_model <- auto.arima(tsdata)
summary(arima_model)
fore_arima = forecast::forecast(arima_model, h=y)
df_arima = as.data.frame(fore_arima)
test$arimanm = df_arima$`Point Forecast`
print("ARIMA Method: Error in PT08_S2__NMHC Levels:"); mape(test$PT08_S2__NMHC, test$arimanm)


#For O3:
tsdata = ts(test[,5], start = c(2004,03,10), end = c(2005,04, 04), freq = 366)
arima_model <- auto.arima(tsdata)
summary(arima_model)
fore_arima = forecast::forecast(arima_model, h=y)
df_arima = as.data.frame(fore_arima)
test$arimao3 = df_arima$`Point Forecast`
print("ARIMA Method:Error in PT08_S5__O3 Levels:"); mape(test$PT08_S5__O3, test$arimao3)






########################### PLOTTING VALUES ###############################

############### PLOTTING VALUES SEPERATELY
# Plotting Holt's method against real values:

#For CO:
plt_aco <- subset(test, select = c('PT08_S1__CO','holtco'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)

# For c6H6
plt_aco <- subset(test, select = c('C6H6__GT','holtc6'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)

# For nm
plt_aco <- subset(test, select = c('PT08_S2__NMHC','holtnm'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)

# For o3
plt_aco <- subset(test, select = c('PT08_S5__O3','holt03'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)





# Plotting arima values against real values:

#For CO:
plt_aco <- subset(test, select = c('PT08_S1__CO',))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)

# For c6H6

plt_aco <- subset(test, select = c('C6H6__GT','arimac6'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)

# For nm
plt_aco <- subset(test, select = c('PT08_S2__NMHC','arimanm'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)

# For o3
plt_aco <- subset(test, select = c('PT08_S5__O3','arimao3'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)




######################### PLOTTING VALUES TOGETHER
# Plotting all the method's values against real values:

#For CO:
plt_aco <- subset(test, select = c('PT08_S1__CO','naiveco', 'simplexpco' ,'holtco','arimaCO'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)

# For c6H6
plt_aco <- subset(test, select = c('C6H6__GT','naivec6', 'simplexpc6' ,'holtc6' ,'arimac6'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)

# For nm
plt_aco <- subset(test, select = c('PT08_S2__NMHC','naivenm', 'simplexpnm' ,'holtnm', 'arimanm'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)

# For o3
plt_aco <- subset(test, select = c('PT08_S5__O3' ,'naiveo3', 'simplexpo3' ,'holt03', 'arimao3'))
values = ts(plt_aco, start = c(2005,02,25), end = c(2005,04, 04), freq = 366)
autoplot(values)






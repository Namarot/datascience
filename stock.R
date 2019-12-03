library(caret)
library(pracma)
library(dplyr)
library(skimr)
library(visdat)
library(DataExplorer)
library(inspectdf)
library(pROC)
library(class)


set.seed(12345)

setwd("/home/utku/ceng474/project")
hektas<-read.csv("datasets/HEKTS.IS.csv", stringsAsFactors = FALSE)
karsan<-read.csv("datasets/KARSN.IS.csv", stringsAsFactors = FALSE)
sabanci<-read.csv("datasets/SAHOL.IS.csv", stringsAsFactors = FALSE)
exchange<-read.csv("datasets/usd_try_exchange_complete.csv", stringsAsFactors = FALSE)
crude_oil<-read.csv("datasets/crude_oil_prices.csv", stringsAsFactors = FALSE)
bist100<-read.csv("datasets/bist100.csv", stringsAsFactors = FALSE)

#str(bist100)
#head(hektas)
#dim(hektas)
#glimpse(hektas)
#summary(hektas)
#skim(hektas)
#vis_miss(hektas)
#vis_dat(hektas)
exchange <- exchange[1:2]
exchange$Date <- as.Date(exchange$Date, "%b %d, %Y")
crude_oil <- crude_oil[1:2]
crude_oil$Date <- as.Date(crude_oil$Date, "%b %d, %Y")
bist100$Price <- as.numeric(gsub(",", "", bist100$Price))
bist100 <- bist100[1:2]
bist100$Date <- as.Date(bist100$Date, "%b %d, %Y")

hektas<-hektas[c("Date", "Close", "Volume")]
karsan<-karsan[c("Date", "Close", "Volume")]
sabanci<-sabanci[c("Date", "Close", "Volume")]

pre_process <- function(df){
  df$Close = as.numeric(df$Close)
  df$Volume = as.numeric(df$Volume)
  df$Date = as.Date(df$Date)
  df<-df[!(is.na(df$Close) | df$Close=="" | df$Close=="null"),]
  row.names(df) <- NULL
  return (df)
}
addUSD_Oil_BIST <- function(df){
  df$USD_TRY <- 0 
  df$CrudeOil <- 0
  df$BIST100 <- 0
  for (i in 1:length(df$USD_TRY)) {
    if(is.element(df$Date[i], exchange$Date)){
      df$USD_TRY[i] <- exchange$Price[exchange$Date == df$Date[i]]
    }else{
      df$USD_TRY[i] <- df$USD_TRY[i-1]
    }
    if(is.element(df$Date[i], crude_oil$Date)){
      df$CrudeOil[i] <- crude_oil$Price[crude_oil$Date == df$Date[i]]
    }else{
      df$CrudeOil[i] <- df$CrudeOil[i-1]
    }
    if(is.element(df$Date[i], bist100$Date)){
      df$BIST100[i] <- bist100$Price[bist100$Date == df$Date[i]]
    }else{
      df$BIST100[i] <- df$BIST100[i-1]
    }
  }
  df$CrudeOil <- df$CrudeOil * df$USD_TRY
  return(df)
}

hektas<-pre_process(hektas)
karsan<-pre_process(karsan)
sabanci<-pre_process(sabanci)
hektas<-addUSD_Oil_BIST(hektas)
karsan<-addUSD_Oil_BIST(karsan)
sabanci<-addUSD_Oil_BIST(sabanci)

write.csv(hektas, file="hektas2.csv", row.names = FALSE)
write.csv(karsan, file="karsan2.csv", row.names = FALSE)
write.csv(sabanci, file="sabanci2.csv", row.names = FALSE)

# Transforming stationary features into temporal via moving averages and rates of change

addEMA <- function(df){
  df$Close_EMA5 <- lag(movavg(df$Close, 5, "e"), 1)
  df$Close_EMA5[1] <- df$Close[1]
  
  df$Volume_EMA5 <- lag(movavg(df$Volume, 5, "e"), 1)
  df$Volume_EMA5[1] <- df$Volume[1]
  
  df$USD_TRY_EMA5 <- lag(movavg(df$USD_TRY, 5, "e"), 1)
  df$USD_TRY_EMA5[1] <- df$USD_TRY[1]
  
  df$CrudeOil_EMA5 <- lag(movavg(df$CrudeOil, 5, "e"), 1)
  df$CrudeOil_EMA5[1] <- df$CrudeOil[1]
  
  df$BIST_EMA5 <- lag(movavg(df$BIST100, 5, "e"), 1)
  df$BIST_EMA5[1] <- df$BIST100[1]
  return(df)
}
addLag <- function(df){
  df$Close_lag1<-lag(df$Close, 1)
  df$Close_lag2<-lag(df$Close, 2)
  df$Close_lag3<-lag(df$Close, 3)
  df$Close_lag4<-lag(df$Close, 4)
  df$Close_lag5<-lag(df$Close, 5)
  df$Close_lag1[1]<-df$Close[1]
  df$Close_lag2[1:2]<-df$Close[1]
  df$Close_lag3[1:3]<-df$Close[1]
  df$Close_lag4[1:4]<-df$Close[1]
  df$Close_lag5[1:5]<-df$Close[1]
  
  df$Volume_EMA5_lag1<-lag(df$Volume_EMA5, 1)
  df$Volume_EMA5_lag2<-lag(df$Volume_EMA5, 2)
  df$Volume_EMA5_lag3<-lag(df$Volume_EMA5, 3)
  df$Volume_EMA5_lag4<-lag(df$Volume_EMA5, 4)
  df$Volume_EMA5_lag5<-lag(df$Volume_EMA5, 5)
  df$Volume_EMA5_lag1[1]<-df$Volume_EMA5[1]
  df$Volume_EMA5_lag2[1:2]<-df$Volume_EMA5[1]
  df$Volume_EMA5_lag3[1:3]<-df$Volume_EMA5[1]
  df$Volume_EMA5_lag4[1:4]<-df$Volume_EMA5[1]
  df$Volume_EMA5_lag5[1:5]<-df$Volume_EMA5[1]
  
  
  df$USD_TRY_lag1<-lag(df$USD_TRY, 1)
  df$USD_TRY_lag2<-lag(df$USD_TRY, 2)
  df$USD_TRY_lag3<-lag(df$USD_TRY, 3)
  df$USD_TRY_lag4<-lag(df$USD_TRY, 4)
  df$USD_TRY_lag5<-lag(df$USD_TRY, 5)
  df$USD_TRY_lag1[1]<-df$USD_TRY[1]
  df$USD_TRY_lag2[1:2]<-df$USD_TRY[1]
  df$USD_TRY_lag3[1:3]<-df$USD_TRY[1]
  df$USD_TRY_lag4[1:4]<-df$USD_TRY[1]
  df$USD_TRY_lag5[1:5]<-df$USD_TRY[1]
  
  df$CrudeOil_lag1<-lag(df$CrudeOil, 1)
  df$CrudeOil_lag2<-lag(df$CrudeOil, 2)
  df$CrudeOil_lag3<-lag(df$CrudeOil, 3)
  df$CrudeOil_lag4<-lag(df$CrudeOil, 4)
  df$CrudeOil_lag5<-lag(df$CrudeOil, 5)
  df$CrudeOil_lag1[1]<-df$CrudeOil[1]
  df$CrudeOil_lag2[1:2]<-df$CrudeOil[1]
  df$CrudeOil_lag3[1:3]<-df$CrudeOil[1]
  df$CrudeOil_lag4[1:4]<-df$CrudeOil[1]
  df$CrudeOil_lag5[1:5]<-df$CrudeOil[1]
  
  df$BIST_lag1<-lag(df$BIST100, 1)
  df$BIST_lag2<-lag(df$BIST100, 2)
  df$BIST_lag3<-lag(df$BIST100, 3)
  df$BIST_lag4<-lag(df$BIST100, 4)
  df$BIST_lag5<-lag(df$BIST100, 5)
  df$BIST_lag1[1]<-df$BIST100[1]
  df$BIST_lag2[1:2]<-df$BIST100[1]
  df$BIST_lag3[1:3]<-df$BIST100[1]
  df$BIST_lag4[1:4]<-df$BIST100[1]
  df$BIST_lag5[1:5]<-df$BIST100[1]
  return(df)
}
addChange <- function(df){
  df$Close_Abs_Change <- df$Close - df$Close_lag1
  df$Close_ROC1 <- ((df$Close - df$Close_lag1)/df$Close_lag1)*100
  df$Close_ROC5 <- ((df$Close - df$Close_lag5)/df$Close_lag5)*100
  
  df$Volume_EMA5_ROC1 <- ((df$Volume_EMA5 - df$Volume_EMA5_lag1)/df$Volume_EMA5_lag1)*100
  df$Volume_EMA5_ROC5 <- ((df$Volume_EMA5 - df$Volume_EMA5_lag5)/df$Volume_EMA5_lag5)*100
  
  df$USD_TRY_ROC1 <- ((df$USD_TRY - df$USD_TRY_lag1)/df$USD_TRY_lag1)*100
  df$USD_TRY_ROC5 <- ((df$USD_TRY - df$USD_TRY_lag5)/df$USD_TRY_lag5)*100
  
  df$CrudeOil_ROC1 <- ((df$CrudeOil - df$CrudeOil_lag1)/df$CrudeOil_lag1)*100
  df$CrudeOil_ROC5 <- ((df$CrudeOil - df$CrudeOil_lag5)/df$CrudeOil_lag5)*100
  
  df$BIST_ROC1 <- ((df$BIST100 - df$BIST_lag1)/df$BIST_lag1)*100
  df$BIST_ROC5 <- ((df$BIST100 - df$BIST_lag5)/df$BIST_lag5)*100
  return(df)
}
addIncrease <- function(df){
  df$Increase <- 0
  for (i in 1:length(df$Close_Abs_Change)) {
    if(df$Close_Abs_Change[i] > 0){
      df$Increase[i] <- 1
    } else if(df$Close_Abs_Change[i] < 0){
      df$Increase[i] <- -1
    }
  }
  df$Increase <- make.names(df$Increase)
  return(df)
}
addMoreLag <- function(df){
  df$Close_ROC1_lag1<-lag(df$Close_ROC1, 1)
  df$Close_ROC1_lag1[1]<-df$Close_ROC1[1]
  df$Close_ROC5_lag1<-lag(df$Close_ROC5, 1)
  df$Close_ROC5_lag1[1]<-df$Close_ROC5[1]
  
  df$USD_TRY_ROC1_lag1<-lag(df$USD_TRY_ROC1, 1)
  df$USD_TRY_ROC1_lag1[1]<-df$USD_TRY_ROC1[1]
  df$USD_TRY_ROC5_lag1<-lag(df$USD_TRY_ROC5, 1)
  df$USD_TRY_ROC5_lag1[1]<-df$USD_TRY_ROC5[1]
  
  df$CrudeOil_ROC1_lag1<-lag(df$CrudeOil_ROC1, 1)
  df$CrudeOil_ROC1_lag1[1]<-df$CrudeOil_ROC1[1]
  df$CrudeOil_ROC5_lag1<-lag(df$CrudeOil_ROC5, 1)
  df$CrudeOil_ROC5_lag1[1]<-df$CrudeOil_ROC5[1]
  
  df$BIST_ROC1_lag1<-lag(df$BIST_ROC1, 1)
  df$BIST_ROC1_lag1[1]<-df$BIST_ROC1[1]
  df$BIST_ROC5_lag1<-lag(df$BIST_ROC5, 1)
  df$BIST_ROC5_lag1[1]<-df$BIST_ROC5[1]
  
  df$Increase_lag1<-lag(df$Increase, 1)
  df$Increase_lag1[1]<-df$Increase[1]
  
  df$Close_Abs_Change_lag1<-lag(df$Close_Abs_Change, 1)
  df$Close_Abs_Change_lag1[1]<-df$Close_Abs_Change[1]
  return(df)
}
applyAll <- function(df){
  df <- addEMA(df)
  df <- addLag(df)
  df <- addChange(df)
  df <- addIncrease(df)
  df <- addMoreLag(df)
  return(df)
}

hektas <- applyAll(hektas)
karsan <- applyAll(karsan)
sabanci <- applyAll(sabanci)
hektas$Increase <- as.factor(hektas$Increase)
hektas$Increase_lag1 <- as.factor(hektas$Increase_lag1)
karsan$Increase <- as.factor(karsan$Increase)
karsan$Increase_lag1 <- as.factor(karsan$Increase_lag1)
sabanci$Increase <- as.factor(sabanci$Increase)
sabanci$Increase_lag1 <- as.factor(sabanci$Increase_lag1)

final_cols <- c('Date', 'Close', 'Close_EMA5', 'Volume_EMA5', 'USD_TRY_EMA5', 'CrudeOil_EMA5', 'BIST_EMA5', 
         'Close_Abs_Change', 'Close_ROC1', 'Volume_EMA5_ROC1', 'Volume_EMA5_ROC5', 'Increase',
         'Close_ROC1_lag1', 'Close_ROC5_lag1', 'USD_TRY_ROC1_lag1', 'USD_TRY_ROC5_lag1',
         'CrudeOil_ROC1_lag1', 'CrudeOil_ROC5_lag1', 'BIST_ROC1_lag1', 'BIST_ROC5_lag1')

hektas_post <- hektas[final_cols]
karsan_post <- karsan[final_cols]
sabanci_post <- sabanci[final_cols]


write.csv(hektas_post, file="hektas3.csv", row.names = FALSE)
write.csv(karsan_post, file="karsan3.csv", row.names = FALSE)
write.csv(sabanci_post, file="sabanci3.csv", row.names = FALSE)

# Exploratory Plots

ggplot(hektas_post, aes(Date, Close)) + geom_line(color = "darkred", alpha = 0.5)
ggplot(hektas_post, aes(Date, Close_Abs_Change)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Difference")
ggplot(hektas_post, aes(Date, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference")

ggplot(hektas_post, aes(Close_EMA5, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5)  + ylab("Closing Price Percentage Difference") +
  xlab("Closing Price Exponential Moving Average")

ggplot(hektas_post, aes(Volume_EMA5, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("Volume EMA")
ggplot(hektas_post, aes(USD_TRY_EMA5, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("USD/TRY EMA")
ggplot(hektas_post, aes(CrudeOil_EMA5, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("Crude Oil Price EMA")
ggplot(hektas_post, aes(BIST_EMA5, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("BIST100 Index EMA")

ggplot(hektas_post, aes(Volume_EMA5_ROC5, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("5 day Rate of Change of Volume EMA")
ggplot(hektas_post, aes(USD_TRY_ROC5_lag1, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("5 day ROC of USD/TRY")
ggplot(hektas_post, aes(CrudeOil_ROC5_lag1, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("5 day ROC of Crude Oil Price")
ggplot(hektas_post, aes(BIST_ROC5_lag1, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("5 day ROC of BIST100 Index")

ggplot(hektas_post, aes(Volume_EMA5_ROC1, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("1 day Rate of Change of Volume EMA")
ggplot(hektas_post, aes(USD_TRY_ROC1_lag1, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("1 day ROC of USD/TRY")
ggplot(hektas_post, aes(CrudeOil_ROC1_lag1, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("1 day ROC of Crude Oil Price")
ggplot(hektas_post, aes(BIST_ROC1_lag1, Close_ROC1)) + geom_line(color = "darkred", alpha = 0.5) + ylab("Closing Price Percentage Difference") +
  xlab("1 day ROC of BIST100 Index")


trainingH<-hektas_post[1:4936,]
testingH<-hektas_post[4937:5057,]

# Traditional metrics can be misleading in Time Series Forecasting
# We'll see the results of simply using the Last Value as our prediction, then we'll compare other models to that.

SSE <- function(pred, obs){
  return(sum((obs - pred)^2))
}
SST <- function(obs){
  return(sum((obs - mean(obs))^2))
}
RSquare <- function(pred, obs){
  return(1 - (SSE(pred, obs)/SST(obs)))
}
MAPE <- function(pred, obs){
  return((sum(abs(obs-pred)/obs)/length(obs))*100)
}
RSquare(hektas$Close_lag1, hektas$Close)
RMSE(hektas$Close_lag1, hektas$Close)
MAPE(hektas$Close_lag1, hektas$Close)
ggplot(data = hektas, aes(x = Date, y=Close)) + geom_point(color ='blue') + 
  geom_line(color='red', data = hektas, aes(x=Date, y=Close_lag1)) + ylab("Close (Actual: Blue, Prediction: Red)")

RSquare(hektas$Close_lag1[4937:5057], hektas$Close[4937:5057])
RMSE(hektas$Close_lag1[4937:5057], hektas$Close[4937:5057])
MAPE(hektas$Close_lag1[4937:5057], hektas$Close[4937:5057])
temp<-hektas[4937:5057,]
ggplot(data = temp, aes(x = Date, y=Close)) + geom_line(color ='blue') + 
  geom_line(color='red', data = temp, aes(x=Date, y=Close_lag1))  + ylab("Close (Actual: Blue, Prediction: Red)")

RSquare(hektas$Close_Abs_Change_lag1, hektas$Close_Abs_Change)
RMSE(hektas$Close_Abs_Change_lag1, hektas$Close_Abs_Change)
ggplot(data = hektas, aes(x = Date, y=Close_Abs_Change)) + geom_line(color ='blue') + 
  geom_line(color='red', data = hektas, aes(x=Date, y=Close_Abs_Change_lag1)) + ylab("Closing Price Difference (Actual: Blue, Prediction: Red)")

RSquare(hektas$Close_Abs_Change_lag1[4937:5057], hektas$Close_Abs_Change[4937:5057])
RMSE(hektas$Close_Abs_Change_lag1[4937:5057], hektas$Close_Abs_Change[4937:5057])
ggplot(data = temp, aes(x = Date, y=Close_Abs_Change)) + geom_line(color ='blue') + 
  geom_line(color='red', data = temp, aes(x=Date, y=Close_Abs_Change_lag1)) + ylab("Closing Price Difference (Actual: Blue, Prediction: Red)")

RSquare(hektas_post$Close_ROC1_lag1, hektas_post$Close_ROC1)
RMSE(hektas_post$Close_ROC1_lag1, hektas_post$Close_ROC1)
ggplot(data = hektas, aes(x = Date, y=Close_ROC1)) + geom_line(color ='blue') + 
  geom_line(color='red', data = hektas, aes(x=Date, y=Close_ROC1_lag1))  + ylab("Closing Price % Difference (Actual: Blue, Prediction: Red)")

RSquare(hektas_post$Close_ROC1_lag1[4937:5057], hektas_post$Close_ROC1[4937:5057])
RMSE(hektas_post$Close_ROC1_lag1[4937:5057], hektas_post$Close_ROC1[4937:5057])
ggplot(data = temp, aes(x = Date, y=Close_ROC1)) + geom_line(color ='blue') + 
  geom_line(color='red', data = temp, aes(x=Date, y=Close_ROC1_lag1))   + ylab("Closing Price % Difference (Actual: Blue, Prediction: Red)")

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats=5, savePredictions = TRUE, classProbs = TRUE)

# Fit a linear regression line through all Close values in the training set, use it to predict Close values in the Test set


model1<-train(Close ~ Date, data = trainingH, 
              method = "lm",
              trControl= fitControl,
              na.action = na.omit)
model1
model1$finalModel$coefficients

ggplot() + geom_point(data=trainingH, aes(Date, Close), colour = "darkred") + 
  geom_abline(intercept = model1$finalModel$coefficients[1], slope = model1$finalModel$coefficients[2])

model1_pred <- data.frame(Close_Pred = predict(model1, testingH), Date=testingH$Date)
ggplot(data = hektas_post, aes(x = Date, y=Close)) + 
  geom_point(color ='blue') +
  geom_abline(intercept = model1$finalModel$coefficients[1], slope = model1$finalModel$coefficients[2], color='green') + 
  geom_line(color='red',data = model1_pred, aes(x=Date, y=Close_Pred)) 


SSE_hektas = sum((testingH$Close - model1_pred$Close_Pred)^2)
SST_hektas = sum((testingH$Close - mean(trainingH$Close))^2)
R_square1 = 1 - SSE_hektas/SST_hektas
RMSE1 = sqrt(SSE_hektas/length(model1_pred$Close_Pred))
R_square1
RMSE1
RSquare(model1_pred$Close_Pred, testingH$Close)
RMSE(model1_pred$Close_Pred, testingH$Close)
MAPE(model1_pred$Close_Pred, testingH$Close)



# Multiple Linear Regression, using Exponentional Moving Average and Rate of Change to predict Close

model2 <- train(Close ~ Close_EMA5 + Close_ROC5_lag1, data=trainingH,
                method="lm",
                trControl = fitControl,
                na.action = na.omit)
model2
summary(model2)
model2_pred <- data.frame(Close_Pred = predict(model2, testingH), Date=testingH$Date)
ggplot(data = testingH, aes(x = Date, y=Close)) + 
  geom_point(color ='blue') + geom_line(color='red',data = model2_pred, aes(x=Date, y=Close_Pred))

SSE_hektas2 = sum((testingH$Close - model2_pred$Close_Pred)^2)
SST_hektas2 = sum((testingH$Close - mean(trainingH$Close))^2)
R_square2 = 1 - SSE_hektas2/SST_hektas2
RMSE2 = sqrt(SSE_hektas2/length(model2_pred$Close_Pred))
R_square2
RMSE2
RSquare(model2_pred$Close_Pred, testingH$Close)
RMSE(model2_pred$Close_Pred, testingH$Close)
MAPE(model2_pred$Close_Pred, testingH$Close)

# Multiple Linear Regression, predicting rate of change (close(t) - close(t-1))/close(t-1) * 100

model3 <- train(Close_ROC1 ~ Close_EMA5 + Close_ROC5_lag1, data=trainingH,
                method="lm",
                trControl = fitControl,
                na.action = na.omit)
model3
summary(model3)
model3_train <- data.frame(Close_ROC1_Pred = model3$finalModel$fitted.values, Date=trainingH$Date)
ggplot(data = trainingH, aes(x = Date, y=Close_ROC1)) + geom_line(color="blue") + 
  geom_line(color='red', data = model3_train, aes(x=Date, y=Close_ROC1_Pred))
model3_pred <- data.frame(Close_ROC1_Pred = predict(model3, testingH), Date=testingH$Date, Close_ROC5_lag1=testingH$Close_ROC5_lag1)
ggplot(data = testingH, aes(x = Date, y=Close_ROC1)) + 
  geom_line(color ='blue') + geom_line(color='red',data = model3_pred, aes(x=Date, y=Close_ROC1_Pred))
ggplot(data = testingH, aes(x = Close_ROC5_lag1, y=Close_ROC1)) + 
  geom_point(color ='blue') + geom_line(color='red',data = model3_pred, aes(x=Close_ROC5_lag1, y=Close_ROC1_Pred))

SSE_hektas3 = sum((testingH$Close_ROC1 - model3_pred$Close_ROC1_Pred)^2)
SST_hektas3 = sum((testingH$Close_ROC1 - mean(trainingH$Close_ROC1))^2)
R_square3 = 1 - SSE_hektas3/SST_hektas3
RMSE3 = sqrt(SSE_hektas3/length(model3_pred$Close_ROC1_Pred))
R_square3
RMSE3
RSquare(model3_pred$Close_ROC1_Pred, testingH$Close_ROC1)
RMSE(model3_pred$Close_ROC1_Pred, testingH$Close_ROC1)

# Multiple Linear Regression, predicting rate of change, using more predictors
model4 <- train(Close_ROC1 ~ Close_EMA5 + Close_ROC1_lag1 + Close_ROC5_lag1 + Volume_EMA5 + USD_TRY_EMA5 + CrudeOil_EMA5 + BIST_EMA5 + 
                  USD_TRY_ROC5_lag1 + CrudeOil_ROC5_lag1 + BIST_ROC5_lag1, data=trainingH,
                method="lm",
                trControl = fitControl,
                na.action = na.omit)
model4
summary(model4)
model4_train <- data.frame(Close_ROC1_Pred = model4$finalModel$fitted.values, Date=trainingH$Date)
ggplot(data = trainingH, aes(x = Date, y=Close_ROC1)) + geom_line(color="blue") + 
  geom_line(color='red', data = model4_train, aes(x=Date, y=Close_ROC1_Pred))


model4_pred <- data.frame(Close_ROC1_Pred = predict(model4, testingH), Date=testingH$Date)
ggplot(data = testingH, aes(x = Date, y=Close_ROC1)) + 
  geom_line(color ='blue') + geom_line(color='red',data = model4_pred, aes(x=Date, y=Close_ROC1_Pred))

SSE_hektas4 = sum((testingH$Close_ROC1 - model4_pred$Close_ROC1_Pred)^2)
SST_hektas4 = sum((testingH$Close_ROC1 - mean(trainingH$Close_ROC1))^2)
R_square4 = 1 - SSE_hektas4/SST_hektas4
RMSE4 = sqrt(SSE_hektas4/length(model4_pred$Close_ROC1_Pred))
R_square4
RMSE4
RSquare(model4_pred$Close_ROC1_Pred, testingH$Close_ROC1)
RMSE(model4_pred$Close_ROC1_Pred, testingH$Close_ROC1)

# Classification
# Last Value
str(hektas)

confusionMatrix(hektas$Increase_lag1, hektas$Increase)

# Logistic Regression
cols <- c('Date', 'Close_EMA5', 'Volume_EMA5', 'USD_TRY_EMA5', 'CrudeOil_EMA5', 'BIST_EMA5', 
                'Increase', 'Close_ROC1_lag1', 'Close_ROC5_lag1', 'USD_TRY_ROC1_lag1', 'USD_TRY_ROC5_lag1',
                'CrudeOil_ROC1_lag1', 'CrudeOil_ROC5_lag1', 'BIST_ROC1_lag1', 'BIST_ROC5_lag1')
cols2 <- c('Close_EMA5', 'Close_ROC5_lag1', 'Volume_EMA5', 'Increase')
hektas_class <- hektas_post[cols]
hektas_class2 <- hektas_post[cols2]
trainingH_class<-hektas_class[1:4936,]
testingH_class<-hektas_class[4937:5057,]
trainingH_class2<-hektas_class2[1:4936,]
testingH_class2<-hektas_class2[4937:5057,]

model5 <- train(Increase ~ ., data=trainingH_class,
                method="vglmAdjCat",
                preProcess=c("scale","center"),
                trControl= fitControl,
                na.action = na.omit)
model5
pred5<-predict(model5, testingH_class[-7])
confusionMatrix(pred5 , testingH_class$Increase)
pred5_post <- predict(model5,  testingH_class[-7], type = "prob")
roc5_post <- roc(testingH_class$Increase, pred5_post$X1, smoothed = TRUE, 
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE, plot=TRUE, 
                 auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, 
                 print.auc=TRUE, show.thres=TRUE)
sens.ci5 <- ci.se(roc5_post)
plot(sens.ci5, type="shape", col="lightblue")
plot(sens.ci5, type="bars")

#
model6 <- train(Increase ~ ., data=trainingH_class2,
                method="vglmAdjCat",
                preProcess=c("scale","center"),
                trControl= fitControl,
                na.action = na.omit)
model6
pred6<-predict(model6, testingH_class2[-4])
confusionMatrix(pred6 , testingH_class2$Increase)
pred6_post <- predict(model6,  testingH_class2[-4], type = "prob")
roc6_post <- roc(testingH_class2$Increase, pred6_post$X1, smoothed = TRUE, 
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE, plot=TRUE, 
                 auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, 
                 print.auc=TRUE, show.thres=TRUE)
sens.ci6 <- ci.se(roc6_post)
plot(sens.ci6, type="shape", col="lightblue")
plot(sens.ci6, type="bars")

# Random Forest
trainingH_class_data <- trainingH_class[-7]
trainingH_class_labels <- trainingH_class[7]
str(trainingH_class_labels)
model7 <- train(Increase ~ ., data=trainingH_class2,
                method="rf",
                preProcess=c("scale","center"),
                trControl= fitControl,
                na.action = na.omit)

model7
pred7<-predict(model7, testingH_class2[-4])
confusionMatrix(pred7 , testingH_class2$Increase)
pred7_post <- predict(model7,  testingH_class2[-4], type = "prob")
roc7_post <- roc(testingH_class2$Increase, pred7_post$X1, smoothed = TRUE, 
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE, plot=TRUE, 
                 auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, 
                 print.auc=TRUE, show.thres=TRUE)
sens.ci7 <- ci.se(roc7_post)
plot(sens.ci7, type="shape", col="lightblue")
plot(sens.ci7, type="bars")

roc7_post <- multiclass.roc(testingH_class2$Increase, pred7_post$X1, direction = '<',
                 smoothed = TRUE, 
                 stratified=FALSE, plot=TRUE, 
                 auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, 
                 print.auc=TRUE, show.thres=TRUE)

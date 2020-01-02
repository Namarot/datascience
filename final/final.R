library(dplyr)
library(skimr)
library(visdat)
library(caret)
library(pROC)
library(class)
library(C50)
library(mlbench)

set.seed(123)

setwd("/home/utku/ceng474/final")
df<-read.csv('data/pulsar_stars.csv')

str(df)
head(df)

df$target_class <- make.names(df$target_class)
df$target_class <- as.factor(df$target_class)

# dropping highly correlated features (p > 0.9)
df_cols <- colnames(df)
rejected_cols <- c('Skewness.of.the.DM.SNR.curve', 'Skewness.of.the.integrated.profile')
df_cols %in% rejected_cols
df_cols<-df_cols[! df_cols %in% rejected_cols]


df_prep <- df[df_cols]

str(df_prep)
skim(df_prep)
vis_dat(df_prep)

par(mfrow=c(1,6))
for(i in 1:6) {
  boxplot(df_prep[,i], main=names(df_prep)[i])
}

inTrain = createDataPartition(y = df_prep$target_class, p = .8, list = FALSE)
training = df_prep[inTrain,]
testing = df_prep[-inTrain,]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats=5, savePredictions = TRUE, classProbs = TRUE)

# rpart
model1 <- train(target_class ~ ., data = training,
                method = "rpart",
                trControl = fitControl,
                na.action = na.omit)
model1
pred1 <- predict(model1, testing[, 1:6])
confusionMatrix(data = pred1, reference = testing$target_class)

pred1_probs <- predict(model1, testing[, 1:6], type = "prob")
roc1 <- multiclass.roc(testing$target_class, pred1_probs, plot=TRUE, 
                       auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, 
                       print.auc=TRUE)

par(mfrow=c(1,1))
roc1 <- roc(testing$target_class, pred1_probs$X0, smoothed = TRUE, 
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE, plot=TRUE, 
                 auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, 
                 print.auc=TRUE, show.thres=TRUE)
sens.ci1 <- ci.se(roc1)
plot(sens.ci1, type="shape", col="lightblue")
plot(sens.ci1, type="bars")

roc1[["auc"]]

# knn
model2 <- train(target_class ~ ., data = training, 
                method = "knn",
                trControl= fitControl,
                tuneLength = 10,
                na.action = na.omit)
model2
pred2 <- predict(model2, testing[, 1:6])
confusionMatrix(data = pred2, reference = testing$target_class)

pred2_probs <- predict(model2, testing[, 1:6], type = "prob")
dev.off()
roc2 <- roc(testing$target_class, pred2_probs$X0, smoothed = TRUE, 
            ci=TRUE, ci.alpha=0.9, stratified=FALSE, plot=TRUE, 
            auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, 
            print.auc=TRUE, show.thres=TRUE)
sens.ci2 <- ci.se(roc2)
plot(sens.ci2, type="shape", col="lightblue")
plot(sens.ci2, type="bars")
roc2[["auc"]]
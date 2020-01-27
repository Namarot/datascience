library(dplyr)
library(skimr)
library(visdat)
library(caret)
library(pROC)
library(class)
library(C50)
library(mlbench)
library(rpart)

set.seed(123)

setwd("/home/utku/ceng474/datascience/final")
df<-read.csv('data/pulsar_stars.csv')

str(df)
head(df)

df$target_class <- make.names(df$target_class)
df$target_class <- as.factor(df$target_class)

# dropping highly correlated features (p > 0.9)
df_cols <- colnames(df)
rejected_cols <- c('Skewness.of.the.DM.SNR.curve', 'Skewness.of.the.integrated.profile')
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
                preProcess=c("scale","center"),
                trControl = fitControl,
                na.action = na.omit)
model1
pred1 <- predict(model1, testing[, 1:6])
confusionMatrix(data = pred1, reference = testing$target_class)

pred1_probs <- predict(model1, testing[, 1:6], type = "prob")

dev.off()

roc1 <- roc(testing$target_class, pred1_probs$X1, 
            ci=TRUE,plot=TRUE, auc.polygon=TRUE, 
            max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE)

roc1[["auc"]]

# knn
model2 <- train(target_class ~ ., data = training, 
                method = "knn", 
                preProcess=c("scale","center"),
                trControl= fitControl,
                tuneLength = 10,
                na.action = na.omit)
model2
pred2 <- predict(model2, testing[, 1:6])
confusionMatrix(data = pred2, reference = testing$target_class)

pred2_probs <- predict(model2, testing[, 1:6], type = "prob")
dev.off()
roc2 <- roc(testing$target_class, pred2_probs$X1,
            ci=TRUE,plot=TRUE, auc.polygon=TRUE, 
            max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE)
roc2[["auc"]]
# C5.0
model3 <- train(target_class ~ ., data = training, 
                method = "C5.0", 
                preProcess=c("scale","center"),
                trControl= fitControl,
                na.action = na.omit)
model3
pred3 <- predict(model3, testing[, 1:6])
confusionMatrix(data = pred3, reference = testing$target_class)

pred3_probs <- predict(model3, testing[, 1:6], type = "prob")
dev.off()
roc3 <- roc(testing$target_class, pred3_probs$X1,
            ci=TRUE,plot=TRUE, auc.polygon=TRUE, 
            max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE)
roc3[["auc"]]
model3.foo <- C5.0(target_class ~ ., data = training, trials = 20)
plot(model3.foo)
# Logistic Regression
model4 <- train(target_class ~ ., data = training, 
                method = "vglmAdjCat", 
                preProcess=c("scale","center"),
                trControl= fitControl,
                na.action = na.omit)
model4
pred4 <- predict(model4, testing[, 1:6])
confusionMatrix(data = pred4, reference = testing$target_class)

pred4_probs <- predict(model4, testing[, 1:6], type = "prob")
dev.off()
roc4 <- roc(testing$target_class, pred4_probs$X1,
            ci=TRUE,plot=TRUE, auc.polygon=TRUE, 
            max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE)
roc4[["auc"]]

library(openintro)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(rsample)
library(vip)
setwd("U:/Univer/Anul 3/Analiza Datelor")
data<- read_excel("clients.xlsx")
glimpse(data)

data$PreferredLoginDevice <- as.factor(data$PreferredLoginDevice)
data$PreferredPaymentMode <- as.factor(data$PreferredPaymentMode)
data$Gender <- as.factor(data$Gender)
data$PreferedOrderCat <- as.factor(data$PreferedOrderCat)
data$MaritalStatus <- as.factor(data$MaritalStatus)
data$Churn <- as.factor(data$Churn)

data<- data[,-1]
glimpse(data)

df <- data %>% mutate_if(is.ordered, factor, ordered = F)

#Variabile de transformare pentru testare - Afost depistat ca acestea doar inrautatesc modelul
df$Complain <- as.factor(df$Complain)
df$SatisfactionScore <- as.factor(df$SatisfactionScore)
df$CityTier <- as.factor(df$CityTier)
glimpse(df)

glimpse(df)

#Centrarea datelor
data_centred <- scale(df, center = TRUE, scale = FALSE)


churn_split <- initial_split(df, prop = .7, strata = 'Churn')
churn_train <- training(churn_split)
churn_test <- testing(churn_split)


set.seed(123) 
cv_model3 <- train(
  Churn ~ .,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123) 
cv_model2 <- train(
  Churn ~ Tenure, 
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

summary( 
  resamples(
    list(
      model2 = cv_model2, model3 = cv_model3
    ))
)$statistics$Accuracy

pred_class <- predict(cv_model3, churn_train)
# create confusion matrix
confusionMatrix(
  data = relevel(pred_class, ref = 1),
  reference = relevel(churn_train$Churn, ref = 1)
)

pred_class2 <- predict(cv_model2, churn_train)
# create confusion matrix
confusionMatrix(
  data = relevel(pred_class2, ref = 1),
  reference = relevel(churn_train$Churn, ref = 1)
)

library(ROCR)
# Compute predicted probabilities
m1_prob <- predict(cv_model2, churn_train, type = 'prob')$'1'
m3_prob <- predict(cv_model3, churn_train, type = 'prob')$'1'

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, churn_train$Churn) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')
perf2 <- prediction(m3_prob, churn_train$Churn) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = 'black', lty = 2)
plot(perf2, add = TRUE, col = 'blue')
legend(0.6, 0.2, legend = c('ModelulUnivar', 'ModleToateVar'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.6)

### Feature interpretation
vip(cv_model3, num_features = 30)
summary(cv_model3)

model3 <- glm(
  Churn ~ ., family = 'binomial',
  data = churn_train
)

install.packages('car')
library(car)
summary(model3)
vif(model3)

coef(model3)

#Selectarea ficurilor
install.packages('MASS')
library(MASS)
mod_step <- stepAIC(model3, direction = 'backward', trace = FALSE)
mod_step


modelSelect<-glm(formula = Churn ~ Tenure + PreferredLoginDevice + CityTier + 
      WarehouseToHome + PreferredPaymentMode + Gender + NumberOfDeviceRegistered + 
      PreferedOrderCat + SatisfactionScore + MaritalStatus + NumberOfAddress + 
      Complain + OrderCount + DaySinceLastOrder + CashbackAmount, 
    family = "binomial", data = churn_train)
summary(modelSelect)


set.seed(123)
cv_modelSelect <- train(
  Churn ~ Tenure + PreferredLoginDevice + CityTier + 
    WarehouseToHome + PreferredPaymentMode + Gender + NumberOfDeviceRegistered + 
    PreferedOrderCat + SatisfactionScore + MaritalStatus + NumberOfAddress + 
    Complain + OrderCount + DaySinceLastOrder + CashbackAmount,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)
cv_modelSelectTest <- train(
  Churn ~ Tenure + PreferredLoginDevice + CityTier + 
    WarehouseToHome + PreferredPaymentMode + Gender + NumberOfDeviceRegistered + 
    PreferedOrderCat +  SatisfactionScore + MaritalStatus,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

pred_class <- predict(cv_modelSelect, churn_test)
# create confusion matrix
confmat<-confusionMatrix(
  data = relevel(pred_class, ref = 1),
  reference = relevel(churn_test$Churn, ref = 1)
)
confmat

install.packages('bootStepAIC')
library(bootStepAIC)
mod_boot<-boot.stepAIC(model3, churn_train, B=50)

mod_boot


#Efectuarea predictiilir
prediction <- predict(modelSelect, newdata = churn_test, type = "response")

binarPred<- ifelse(prediction>0.5,1,0)
accuracy <-mean(binarPred == churn_test$Churn)

accuracy

confusion_matrix <- confusionMatrix(as.factor(binarPred), as.factor(churn_test$Churn))
confusion_matrix
precision <- confmat$byClass['Pos Pred Value']
precision
recall <- confusion_matrix$byClass['Sensitivity']
recall
f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score

m1_prob <- predict(cv_modelSelect, churn_test, type = 'prob')$'1'
m3_prob <- predict(cv_modelSelectTest, churn_test, type = 'prob')$'1'

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, churn_test$Churn) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')
perf2 <- prediction(m3_prob, churn_test$Churn) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = 'black', lty = 2)
plot(perf2, add = TRUE, col = 'blue')
legend(0.8, 0.2, legend = c('cv_model1', 'cv_model3'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.6)


install.packages("pROC")
library(pROC)
roc_obj <- roc(churn_test$Churn, prediction)
auc <- auc(roc_obj)
plot(roc_obj, main = paste('AUC =', round(auc, 2)))







install.packages("smotefamily")
library(smotefamily)

#incercarea folosirii upSampling

formula <- Churn ~ Tenure + PreferredLoginDevice + CityTier + 
  WarehouseToHome + PreferredPaymentMode + Gender + NumberOfDeviceRegistered + 
  PreferedOrderCat + SatisfactionScore + MaritalStatus + NumberOfAddress + 
  Complain + OrderCount + DaySinceLastOrder + CashbackAmount

your_dataset_smote <- SMOTE(formula,data = churn_train, perc.over = 100, k = 5)

new <- upSample(churn_train, churn_train$Churn)
table(new$Churn)
glimpse(new)
new<-new[,-20]
glimpse(new)

cv_modelSelect2 <- train(
  Churn ~ Tenure + PreferredLoginDevice + CityTier + 
    WarehouseToHome + PreferredPaymentMode + Gender + NumberOfDeviceRegistered + 
    PreferedOrderCat + SatisfactionScore + MaritalStatus + NumberOfAddress + 
    Complain + OrderCount + DaySinceLastOrder + CashbackAmount,
  data = new,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

pred_class <- predict(cv_modelSelect2, churn_test)
# create confusion matrix
confmat<-confusionMatrix(
  data = relevel(pred_class, ref = 1),
  reference = relevel(churn_test$Churn, ref = 1)
)
confmat





#df<-as.numeric(df$Churn)
data<- read_excel("clients.xlsx")
data<- data[,-1]
data$PreferredLoginDevice <- as.factor(data$PreferredLoginDevice)
data$PreferredPaymentMode <- as.factor(data$PreferredPaymentMode)
data$Gender <- as.factor(data$Gender)
data$PreferedOrderCat <- as.factor(data$PreferedOrderCat)
data$MaritalStatus <- as.factor(data$MaritalStatus)
df <- data %>% mutate_if(is.ordered, factor, ordered = F)
dummy <- dummyVars(" ~ .", data=df)
glimpse(df)
#perform one-hot encoding on data frame
final_df <- data.frame(predict(dummy, newdata=df))
glimpse(final_df)
final_df$Churn <- as.factor(final_df$Churn)
final_df|>
  count(Churn)
smote <- SMOTE(final_df, final_df$Churn)
new <- smote$data
new
new|>
  count(Churn)
modeltest <- glm(
  Churn ~ ., family = 'binomial',
  data = churn_train
)
modeltest

churn_split <- initial_split(final_df, prop = .7, strata = 'Churn')
churn_train <- training(churn_split)
churn_test <- testing(churn_split)


cv_modelSelectNW <- train(
  Churn ~ .,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)
pred_class <- predict(cv_modelSelectNW, churn_test)
# create confusion matrix
confmat<-confusionMatrix(
  data = relevel(pred_class, ref = 1),
  reference = relevel(churn_test$Churn, ref = 1)
)
confmat




cv_modelSelectNW <- train(
  Churn ~ Tenure + PreferredLoginDevice + CityTier + 
    WarehouseToHome + PreferredPaymentMode + Gender + NumberOfDeviceRegistered + 
    PreferedOrderCat + SatisfactionScore + MaritalStatus + NumberOfAddress + 
    Complain + OrderCount + DaySinceLastOrder + CashbackAmount,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)
pred_class <- predict(cv_modelSelectNW, churn_test)
# create confusion matrix
confmat<-confusionMatrix(
  data = relevel(pred_class, ref = 1),
  reference = relevel(churn_test$Churn, ref = 1)
)
confmat
confmat<-confusionMatrix(
  data = relevel(pred_class, ref = 1),
  reference = relevel(churn_test$Churn, ref = 1)
)



m1_prob <- predict(cv_modelSelect, churn_test, type = 'prob')$'1'
m3_prob <- predict(cv_modelSelectNW, churn_test, type = 'prob')$'1'

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, churn_test$Churn) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')
perf2 <- prediction(m3_prob, churn_test$Churn) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = 'black', lty = 2)
plot(perf2, add = TRUE, col = 'blue')
legend(0.6, 0.2, legend = c('ModelulSimplu', 'ModleAntrenatPeSmote'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.6)



#Random forest
install.packages("randomForest")
library(randomForest)


churn_split <- initial_split(df, prop = .7, strata = 'Churn')
churn_train <- training(churn_split)
churn_test <- testing(churn_split)

set.seed(1)
modleRand<-randomForest(formula = Churn ~ Tenure + PreferredLoginDevice + CityTier + 
                          WarehouseToHome + PreferredPaymentMode + Gender + NumberOfDeviceRegistered + 
                          PreferedOrderCat + SatisfactionScore + MaritalStatus + NumberOfAddress + 
                          Complain + OrderCount + DaySinceLastOrder + CashbackAmount,
                        data=churn_train)

modleRand

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

set.seed(1234)
# Run the model
rf_default <- train(Churn ~ Tenure + PreferredLoginDevice + CityTier + 
                      WarehouseToHome + PreferredPaymentMode + Gender + NumberOfDeviceRegistered + 
                      PreferedOrderCat + SatisfactionScore + MaritalStatus + NumberOfAddress + 
                      Complain + OrderCount + DaySinceLastOrder + CashbackAmount,
                    data=churn_train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)
# Print the results
print(rf_default)

prediction<-predict(rf_default, churn_test)

confusionMatrix(prediction, churn_test$Churn)

plot(modleRand)

varImpPlot(modleRand) 

m1_prob <- predict(cv_modelSelect, churn_test, type = 'prob')$'1'
m3_prob <- predict(rf_default, churn_test, type = 'prob')$'1'

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, churn_test$Churn) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')
perf2 <- prediction(m3_prob, churn_test$Churn) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

perf1

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = 'red')
plot(perf2, add = TRUE, col = 'blue')
legend(0.6, 0.2, legend = c('RegresiaLogistica', 'RandomForest'),
       col = c('red', 'blue'), lty = 1:1, cex = 0.6)+
  title(main = "Performanta modelelor")




citation()





#Kaggle competition Titanic
#Goal: predict which passengers survived the tragedy
#Data: train.csv, test.csv

library(readr)
library(dplyr)
library(broom)
library(tidyverse)
library(rsample)
library(Metrics)
library(ranger)
library(caTools)
library(na.tools)
library(tidyimpute)

#training data set
train_all_column <- read_csv("train.csv")
train <- train_all_column %>% select(Survived, Sex, Age, Pclass, Embarked) %>%
  impute_at( .na=na.mean, .vars = 'Age') %>% #impute missing age with mean values
  drop_na() %>%
  mutate(Survived=factor(Survived), Sex=factor(Sex), Pclass=factor(Pclass), Embarked=factor(Embarked))

#Split training and testing data
set.seed(42)
data_split <- initial_split(train, prop = 0.75)
training_data <- training(data_split)
testing_data <- testing(data_split)
threshold <- 0.53

# Logistic regression without cross validation
model <- glm(Survived ~.,family=binomial(link='logit'),data=training_data)
summary(model)

#predicted survival for testing data
test_predicted <- predict(model, testing_data, type = "response") > threshold

#actual survived for testing data
test_actual <- testing_data$Survived ==1

#compare actual and predictd 
confusion_matrix <- table(test_actual, test_predicted)

#calculate the test accuracy, precision, recall
accuracy(test_actual, test_predicted)
precision(test_actual, test_predicted)
recall(test_actual, test_predicted)
print(paste("accuracy = ", accuracy(test_actual, test_predicted)))
print(paste("precision = ", precision(test_actual, test_predicted)))
print(paste("recall = ", recall(test_actual, test_predicted)))

#creat ROC curve
p <- predict(model, testing_data, type = "response")
auc_3age <- colAUC(p, testing_data[["Survived"]], plotROC = TRUE)


#Read in the new dataset for prediction 
newdata_all_column <- read_csv("test.csv") %>%
  impute_at( .na=na.mean, .vars = 'Age') %>% #impute missing age with mean values
  mutate(Sex=factor(Sex), Pclass=factor(Pclass), Embarked=factor(Embarked))

#Predicting survival for newdata data
results <- predict(model, newdata = newdata_all_column, type = "response") > threshold
results <- as.integer(results)
titanic_prediction <- select(newdata_all_column, PassengerId) %>%
  mutate(Survived = results)

write_csv(titanic_prediction, "titanic_prediction.csv", col_names = TRUE)





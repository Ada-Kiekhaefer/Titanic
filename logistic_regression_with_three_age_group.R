# Logistic regression with three age groups -------------------------------
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

train1 <- train %>% 
  mutate(Age_group = case_when(
    Age > 50 ~ "over50",
    Age > 15 ~ "15to50",
    TRUE ~ "0to15"))
train1 <- train1 %>%
  select(-Age) %>%
  mutate(Age_group = factor(Age_group, levels = c("0to15", "15to50", "over50")))

#Split training and testing data
set.seed(42)
data_split <- initial_split(train1, prop = 0.75)
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
confusion_matrix_3age <- table(test_actual, test_predicted)

#calculate the test accuracy, precision, recall
accuracy(test_actual, test_predicted)
precision(test_actual, test_predicted)
recall(test_actual, test_predicted)
print(paste("accuracy = ", accuracy(test_actual, test_predicted)))
print(paste("precision = ", precision(test_actual, test_predicted)))
print(paste("recall = ", recall(test_actual, test_predicted)))

#creat ROC curve
p <- predict(model, testing_data, type = "response")
auc <- colAUC(p, testing_data[["Survived"]], plotROC = TRUE)


#Read in the new dataset for prediction 
newdata_all_column <- read_csv("test.csv") %>%
  impute_at( .na=na.mean, .vars = 'Age') %>% #impute missing age with mean values
  mutate(Sex=factor(Sex), Pclass=factor(Pclass), Embarked=factor(Embarked))

newdata_with_age_group <- newdata_all_column %>%
  mutate(Age_group = case_when(
  Age > 50 ~ "over50",
  Age > 15 ~ "15to50",
  TRUE ~ "0to15"))
newdata_with_age_group <- newdata_with_age_group %>%
  select(-Age) %>%
  mutate(Age_group = factor(Age_group, levels = c("0to15", "15to50", "over50")))

#Predicting survival for newdata data
results <- predict(model, newdata = newdata_with_age_group, type = "response") > threshold
results <- as.integer(results)
titanic_prediction <- select(newdata_with_age_group, PassengerId) %>%
  mutate(Survived = results)

write_csv(titanic_prediction, "titanic_prediction_3age.csv", col_names = TRUE)





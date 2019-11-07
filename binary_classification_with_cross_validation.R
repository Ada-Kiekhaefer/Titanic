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

#cross validation dataframes
set.seed(42)
cv_split <- vfold_cv(training_data, v = 5)
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))

#Building cross validation logistic regression model
cv_model_lr <- cv_data %>%
  mutate(model = map(train, ~glm(formula = Survived ~ ., data = .x, 
                                 family = "binomial")))

#Evaluating the classification model 
cv_prep_lr <- cv_model_lr %>%
  mutate(validate_actual = map(validate, ~.x$Survived == 1),
         validate_predicted = map2(.x = model,
                                   .y = validate, .f = ~predict(.x, .y, type = "response") > threshold))

# Calculate the validate accuracy for each cross validation fold
cv_perf_lr_acc <- cv_prep_lr %>%
  mutate(validate_acc = map2_dbl(validate_actual, validate_predicted,
                                 ~accuracy(actual = .x, predicted = .y)))


#Classification with random forest
cv_tune <- cv_data %>%
  crossing(mtry = c(2, 3))
#Buil a cross validation model for each fold & mtry combination
cv_models_rf <- cv_tune %>%
  mutate(model = map2(train, mtry, ~ranger(formula = Survived ~ .,
                                           data = .x, mtry = .y,
                                           num.trees = 100, 
                                           seed = 42)))

cv_prep_rf <- cv_models_rf %>%
  mutate(validate_actual = map(validate, ~.x$Survived),
         validate_predicted = map2(.x = model, .y = validate,
                                   ~predict(.x, .y, 
                                            type = "response")$predictions > threshold))

#calculate accuracy for each cross validation fold
cv_perf_acc <- cv_prep_rf %>%
  mutate(acc = map2_dbl(.x = validate_actual,
                        .y = validate_predicted,
                        ~accuracy(actual = .x, predicted = .y)))

print(cv_perf_lr_acc$validate_acc)
print(mean(cv_perf_lr_acc$validate_acc))

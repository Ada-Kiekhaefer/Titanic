func_lr_model <- function(data, equation, threshold = 0.5) {
  library(broom)
  library(tidyverse)
  library(rsample)
  #Split training and testing data
  set.seed(42)
  data_split <- initial_split(data, prop = 0.7)
  training_data <- training(data_split)
  testing_data <- testing(data_split)

  ### Build the logistic regression model using all training data
  lr_all_train <- glm(formula = equation, data = training_data,
                      family = "binomial")
  model_summary <- summary(lr_all_train)
  #actual survived for testing data
  test_actual <- testing_data$Survived ==1
  
  #predicted survival for testing data
  test_predicted <- predict(lr_all_train, testing_data, type = "response") > threshold
  prop_predicted <- predict(lr_all_train, testing_data, type = "response")
  #compare actual and predictd 
  table <- table(test_actual, test_predicted)
  
  #calculate the test accuracy, precision, recall
  test_accuracy <- accuracy(test_actual, test_predicted)
  test_precision <- precision(test_actual, test_predicted)
  test_recall <- recall(test_actual, test_predicted)
  
  return(list(lr_model = lr_all_train, model_summary, prop_predicted = prop_predicted, table, accuracy = test_accuracy, precision = test_precision, recall = test_recall))
  
}
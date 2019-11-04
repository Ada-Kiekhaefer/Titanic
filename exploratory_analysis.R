# exploratory analysis of the survivors -----------------------------------
library(readr)
library(ggplot2)
library(dplyr)

train_all_column <- read_csv("train.csv")
names(train_all_column)


# investigate survival rate between male and female ------------------------------------

ggplot(train_all_column, aes(x = factor(Sex))) +
  geom_bar() +
  facet_wrap(~Survived, labeller = as_labeller(c(`0` = "Not survived", `1` = "Survived"))) +
  labs(title = "Survival rate between male and female") +
  theme(plot.title = element_text(hjust = 0.5))


# histogram of age versus survival counts ---------------------------------

ggplot(train_all_column, aes(x = Age)) +
  geom_histogram() +
  labs(title = "Histogram of age versus survival counts") +
  theme(plot.title = element_text(hjust = 0.5))


# Calculate survival rate among age groups of each gender -----------------

age_0to15_percent_survival <- train_all_column %>%
  filter(Age > 0 & Age <= 15) %>%
  group_by(Sex) %>%
  summarize(number_of_passengers = n(), survived = sum(Survived), percent_survived = sum(Survived)*100/n())

age_15to50_percent_survival <- train_all_column %>%
  filter(Age >15 & Age <= 50) %>%
  group_by(Sex) %>%
  summarize(number_of_passengers = n(), survived = sum(Survived), percent_survived = sum(Survived)*100/n())

age_over50_percentsurvival <- train_all_column %>%
  filter(Age > 50) %>%
  group_by(Sex) %>%
  summarize(number_of_passengers = n(), survived = sum(Survived), percent_survived = sum(Survived)*100/n())

percent_survived <- bind_rows(age_0to15_percent_survival, age_15to50_percent_survival, age_over50_percentsurvival)
age_group <- tibble(age_group = c("0to15", "0to15", "15to50", "15to50", "over50", "over50"))
percent_survived <- bind_cols(percent_survived, age_group)


# plot of percent survival of male and female among different age groups ----------

ggplot(percent_survived) +
  geom_bar(aes(x=age_group,y=percent_survived),stat="identity") +
  facet_wrap(~Sex) +
  labs(title = "Percent survival of male and female among different age groups") +
  theme(plot.title = element_text(hjust = 0.5))

# clear environment
rm(list = ls())

# load needed libraries
library(readr)
library(dplyr)
library(tidyr)
library(randomForest)
library(caret)

# set your working directory
#setwd()

# load and inspect data
df_cleaned <- read_csv("nba_rookie_data_cleaned.csv")
glimpse(df_cleaned)

# convert target variable to factor
df_cleaned$was_all_star <- as.factor(df_cleaned$was_all_star)

# drop columns not useful for prediction
df_modeling <- df_cleaned %>%
  select(-Name, -`seasons_selected_as_all_star`, -`teams_selected_on_as_all_star`)
glimpse(df_modeling)

# handle missing values and other cleaning
df_modeling <- df_modeling %>% drop_na()
glimpse(df_modeling)
names(df_modeling) <- make.names(names(df_modeling))
df_modeling

# train test split
set.seed(222)
ind <- createDataPartition(df_modeling$was_all_star, p = 0.8, list = FALSE)
train <- df_modeling[ind, ]
test <- df_modeling[-ind, ]
train

# train random forest model
rf_model <- randomForest(was_all_star ~ ., data = train, ntree = 500, mtry = 7, importance = TRUE)
print(rf_model)

# test random forest model
pred <- predict(rf_model, newdata = test)
confusionMatrix(pred, test$was_all_star)

# feature importance
varImpPlot(rf_model)

# References
# https://www.r-bloggers.com/2021/04/random-forest-in-r/

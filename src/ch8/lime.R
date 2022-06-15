# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)

# Load Data
employee_attrition_tbl <- read_csv("new_raw_data/Business Decisions with Machine Learning/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl    <- read_excel("new_raw_data/Business Decisions with Machine Learning/data_definitions.xlsx", sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")

employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# Split into test and train
set.seed(seed = 1113)
split_obj <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)

# Assign training and test data
train_readable_tbl <- training(split_obj)
test_readable_tbl  <- testing(split_obj)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>% 
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel("04_Modeling/h20_models/StackedEnsemble_BestOfFamily_AutoML_20200903_144246")
automl_leader


# 3. LIME ----

# 3.1 Making Predictions ----

predictions_tbl <- automl_leader %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )

predictions_tbl


test_tbl %>%
  slice(1) %>%
  glimpse()

# 3.2 Single Explanation ----

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
    model           = automl_leader,
    bin_continuous  = TRUE,
    n_bins          = 4,
    quantile_bins   = TRUE
  )

explainer

?lime::explain

explanation <- test_tbl %>%
  slice(1) %>%
  select(-Attrition) %>%
  lime::explain(
    
    # Pass our explainer object
    explainer = explainer,
    # Because it is a binary classification model: 1
    n_labels   = 1,
    # number of features to be returned
    n_features = 8,
    # number of localized linear models
    n_permutations = 5000,
    # Let's start with 1
    kernel_width   = 1
  )

explanation

explanation %>%
  as.tibble() %>%
  select(feature:prediction) 
g <- plot_features(explanation = explanation, ncol = 1)


# 3.3 Multiple Explanations ----

explanation <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer = explainer,
    n_labels   = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width   = 0.5
  )

explanation %>%
  as.tibble()

plot_features(explanation, ncol = 4)

plot_explanations(explanation)
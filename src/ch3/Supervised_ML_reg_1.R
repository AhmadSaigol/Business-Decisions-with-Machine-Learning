library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(broom.mixed) # for converting bayesian models to tidy tibbles

# Data set
bike_data_tbl <- readRDS("new_raw_data/Business Decisions with Machine Learning/bike_orderlines.rds")

ggplot(bike_data_tbl,
       aes(x = price, 
           y = weight, 
           group = category_1, 
           col = category_1)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_manual(values=c("#2dc6d6", "#d65a2d", "#d6af2d", "#8a2dd6", "#FF0000"))

#build model
weight ~ price * category_1
linear_reg()

lm_mod <- linear_reg() %>% 
  set_engine("lm")

lm_mod
## Linear Regression Model Specification (regression)
## 
## Computational engine: lm

lm_fit <- lm_mod %>% 
  fit(weight ~ price * category_1, 
      data = bike_data_tbl)
tidy(lm_fit)

#prediction
new_points <- expand.grid(price = 20000, 
                          category_1 = c("E-Bikes", "Hybrid / City", "Mountain", "Road"))
new_points

mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred


conf_int_pred <- predict(lm_fit, 
                         new_data = new_points, 
                         type = "conf_int")
conf_int_pred
## # A tibble: 4 x 2
##   .pred_lower .pred_upper
##         <dbl>       <dbl>
## 1       17.9        20.7 
## 2        9.04       10.8 
## 3       12.8        13.7 
## 4        7.88        8.74

# Now combine: 
plot_data <- new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# and plot:
ggplot(plot_data, aes(x = category_1)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "Bike weight", x = "Category") 

#model with different engine
# set the prior distribution
prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)

# make the parsnip model
bayes_mod <- linear_reg() %>% 
  set_engine("stan",
             prior_intercept = prior_dist, 
             prior = prior_dist) 

# train the model
bayes_fit <-  bayes_mod %>% 
  fit(weight ~ price * category_1, 
      data = bike_data_tbl)

print(bayes_fit, digits = 5)

tidy(bayes_fit, conf.int = TRUE)

bayes_plot_data <- 
  new_points %>%
  bind_cols(predict(bayes_fit, new_data = new_points)) %>% 
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

ggplot(bayes_plot_data, aes(x = category_1)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) + 
  labs(y = "Bike weight") + 
  ggtitle("Bayesian model with t(1) prior distribution")

#preprocessing
set.seed(123)

flight_data <- 
  flights %>% 
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) in the recipe below
    date = as.Date(time_hour)
  ) %>% 
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)

flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))

flight_data %>% 
  skimr::skim(dest, carrier) 
glimpse(flight_data)

#data splitting
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(555)
# Put 3/4 of the data into the training set 
data_split <- initial_split(flight_data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) 

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") 

summary(flights_rec)

flight_data %>% 
  distinct(date) %>% 
  mutate(numeric_date = as.numeric(date)) 

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
  step_rm(date)

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>% 
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
  step_rm(date) %>% 
  step_dummy(all_nominal(), -all_outcomes())

test_data %>% 
  distinct(dest) %>% 
  anti_join(train_data)

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>% 
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
  step_rm(date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors())

predict(flights_fit, test_data)
flights_pred <- 
  predict(flights_fit, test_data, type = "prob") %>% 
  bind_cols(test_data %>% select(arr_delay, time_hour, flight)) 

# The data look like: 
flights_pred

flights_pred %>% 
  roc_curve(truth = arr_delay, .pred_late) %>% 
  autoplot()

flights_pred %>% 
  roc_auc(truth = arr_delay, .pred_late)

#evaluating
library(tidymodels) # for the rsample package, along with the rest of tidymodels

# Helper packages
library(modeldata)  

data(cells, package = "modeldata")
cells

cells %>% 
  count(class) %>% 
  mutate(prop = n/sum(n))

set.seed(123)
cell_split <- initial_split(cells %>% select(-case), 
                            strata = class)

cell_train <- training(cell_split)
cell_test  <- testing(cell_split)

nrow(cell_train)
## 1515
nrow(cell_train)/nrow(cells)
## 0.7503715

# training set proportions by class
cell_train %>% 
  count(class) %>% 
  mutate(prop = n/sum(n))
## # A tibble: 2 x 3
##   class     n  prop
##   <fct> <int> <dbl>
## 1 PS      975 0.644
## 2 WS      540 0.356  

# test set proportions by class
cell_test %>% 
  count(class) %>% 
  mutate(prop = n/sum(n))

rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

set.seed(234)
rf_fit <- 
  rf_mod %>% 
  fit(class ~ ., data = cell_train)
rf_fit

rf_training_pred <- 
  predict(rf_fit, cell_train) %>% 
  bind_cols(predict(rf_fit, cell_train, type = "prob")) %>% 
  # Add the true outcome data back in
  bind_cols(cell_train %>% 
              select(class))

rf_training_pred %>%                # training set predictions
  roc_auc(truth = class, .pred_PS)
## # A tibble: 1 x 3
##   .metric .estimator .estimate
##   <chr>   <chr>          <dbl>
## 1 roc_auc binary          1.00
rf_training_pred %>%                # training set predictions
  accuracy(truth = class, .pred_class)

rf_testing_pred <- 
  predict(rf_fit, cell_test) %>% 
  bind_cols(predict(rf_fit, cell_test, type = "prob")) %>% 
  bind_cols(cell_test %>% select(class))

rf_testing_pred %>%                   # test set predictions
  roc_auc(truth = class, .pred_PS)
## # A tibble: 1 x 3
##   .metric .estimator .estimate
##   <chr>   <chr>          <dbl>
## 1 roc_auc binary         0.909

rf_testing_pred %>%                   # test set predictions
  accuracy(truth = class, .pred_class)

set.seed(345)
folds <- vfold_cv(cell_train, v = 10)
folds

rf_wf <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_formula(class ~ .)

set.seed(456)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)

rf_fit_rs

collect_metrics(rf_fit_rs)

rf_testing_pred %>%                   # test set predictions
  roc_auc(truth = class, .pred_PS)
## # A tibble: 1 x 3
##   .metric .estimator .estimate
##   <chr>   <chr>          <dbl>
## 1 roc_auc binary         0.909
rf_testing_pred %>%                   # test set predictions
  accuracy(truth = class, .pred_class)

#tuning
library(tidymodels)  # for the tune package, along with the rest of tidymodels

# Helper packages
library(modeldata)   # for the cells data
library(vip)  


set.seed(123)
cell_split <- initial_split(cells %>% select(-case), 
                            strata = class)
cell_train <- training(cell_split)
cell_test  <- testing(cell_split)


tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

tune_spec

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

tree_grid

tree_grid %>% 
  count(tree_depth)

set.seed(234)
cell_folds <- vfold_cv(cell_train)


set.seed(345)

tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(class ~ .)

tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = cell_folds,
    grid = tree_grid
  )

tree_res


tree_res %>% 
  collect_metrics()

tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)


tree_res %>%
  show_best("roc_auc")

best_tree <- tree_res %>%
  select_best("roc_auc")

best_tree

final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)

final_wf


final_tree <- 
  final_wf %>%
  fit(data = cell_train) 

final_tree

library(vip)

final_tree %>% 
  pull_workflow_fit() %>% 
  vip()

final_fit <- 
  final_wf %>%
  last_fit(cell_split) 

final_fit %>%
  collect_metrics()
## # A tibble: 2 x 3
##   .metric  .estimator .estimate
##   <chr>    <chr>          <dbl>
## 1 accuracy binary         0.802
## 2 roc_auc  binary         0.860

final_fit %>%
  collect_predictions() %>% 
  roc_curve(class, .pred_PS) %>% 
  autoplot()

args(decision_tree)


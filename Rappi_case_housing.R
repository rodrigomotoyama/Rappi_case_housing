# Pacotes ------------------------------------------------------------------
library(tidymodels)
library(tidyverse)
library(vip)
library(skimr)

# CARREGAR A BASE ----------------------------------------------------------
path <- "house_sales.csv"
housing <- read_csv(path)

housing_initial_split <- housing %>% initial_split(3/4)
housing_train <- housing_initial_split %>% training()
housing_test <- housing_initial_split %>% testing()



# DATAPREP --------------------------------------------------------------
housing_recipe <- recipe(price ~ ., housing_initial_split) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) 

# MODELO ------------
housing_model <- rand_forest(
  mtry = tune(),
  trees = 150,
  min_n = tune()
) %>% 
  set_mode("regression") %>% 
  set_engine("randomForest")

# WORKFLOW -----------------------
housing_wf <- workflow() %>% 
  add_model(housing_model) %>% 
  add_recipe(housing_recipe)


# TUNAGEM DE HIPERPARAMETROS ----------------------------
housing_grid <- expand.grid(
  mtry = c(4, 6, 8, 10, 12),
  # trees = c(100, 120, 150, 175, 200),
  min_n = c(15, 20, 25, 30, 35, 40)
)

housing_resamples <- vfold_cv(housing_train, v = 3)

housing_rf_tune_grid <- tune_grid(
  housing_wf,
  resamples = housing_resamples,
  grid = housing_grid,
  metrics = metric_set(
    rmse
  )
)

#INSPEÇÃO DA TUNAGEM ----------------------
autoplot(housing_rf_tune_grid)
collect_metrics(housing_rf_tune_grid)
show_best(housing_rf_tune_grid, "rmse")

#FINALIZAÇÃO DO WORKFLOW
hiperparameters <- select_best(housing_rf_tune_grid, metric = "rmse")

housing_final_workflow <- finalize_workflow(housing_wf, 
                                          parameters = hiperparameters)

housing_last_fit <- last_fit(housing_final_workflow, split = housing_initial_split)

housing_test_preds <- collect_predictions(housing_last_fit)

# VISUALIZAÇÃO DAS PREVISÕES NA BASE DE TREINO-------------
housing_test_preds %>%
  # filter(x > 0) %>%
  ggplot() +
  geom_point(aes(.pred, price)) +
  geom_abline(slope = 1, intercept = 0, colour = "purple", size = 1) +
  theme_bw()

housing_test_preds %>%
  ggplot(aes(.pred, ((price)-(.pred)))) +
  geom_point() +
  geom_smooth(se = FALSE)

housing_fit <- housing_final_workflow %>% fit(data = housing)

vip::vi(housing_fit$fit$fit) %>%
  mutate(
    abs_importance = abs(Importance),
    Variable = fct_reorder(Variable, abs_importance)
  ) %>%
  # arrange(desc(mportance)) %>% 
  head(15) %>% 
  ggplot(aes(x = abs_importance, y = Variable)) +
  geom_col()

saveRDS(housing_fit, file = "housing_random_forest.rds")

# housing_fit$fit$fit %>% plot
# qq <- housing_fit %>% fit(data = housing_test)
# qq$fit$fit$fit




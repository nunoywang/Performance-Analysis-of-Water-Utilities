# --- 0) Packages ----
library(tidyverse)
library(readxl)
library(tidymodels)
library(janitor)
library(vip)
library(themis)

set.seed(123)

df <- read_excel("C:/Users/nunow/OneDrive/Documentos/IST LMAC/Data Mining/Projeto/df_2023_final.xlsx") |> clean_names()

df_baixa <- df |>
  filter(tolower(tipo_sistema) == "baixa" | tipo_sistema == "Baixa") |>
  mutate(across(where(is.character), ~ ifelse(. %in% c("NR","nr","N/R"), NA, .)))

# Drop identifiers, target columns and unwanted columns from predictors
drop_cols <- c("empresa",
               "tipo_sistema",
               "ramo_atividade",
               "i_identificacao_da_entidade_gestora",
               "v_identificacao_da_entidade_gestora",
               "i_resposta_a_reclamacoes_sugestoes_e_pedidos_de_informacao_telefonicos",
               "v_volume_de_agua_medido_nos_3_meses_com_maiores_volumes",
               "v_volume_de_agua_medido_nos_3_meses_com_menores_volumes",
               "v_custo_de_substituicao",
               "v_ramais_afetados_por_falhas_no_abastecimento"
               )

# Variables to test WITH vs WITHOUT
vars_nrw_key <- c("v_agua_entrada_no_sistema","v_agua_nao_faturada")
vars_rl_key  <- c("v_comprimento_total_de_condutas","v_ramais_de_ligacao","v_perdas_reais")

# Function to prepare dataset for each target
prep_xy <- function(data, target, remove_keys = FALSE, key_vars = character()) {
  # rows with target present
  d <- data |> filter(!is.na(.data[[target]]))
  
  # drop only the other target
  others <- setdiff(c("nrw_class","rl_class"), target)
  
  # keep target first, then drop unwanted predictors
  d <- d |>
    select(all_of(target), everything()) |>
    select(-any_of(c(drop_cols, others)))
  
  if (remove_keys) d <- d |> select(-any_of(key_vars))
  
  # set stable levels for outcome
  levs <- c("Good","Medium","Unsatisfactory")
  d[[target]] <- factor(d[[target]], levels = levs)
  
  # if any label is outside levels, drop those rows
  if (any(is.na(d[[target]]))) {
    d <- d |> filter(!is.na(.data[[target]]))
  }
  d
}

# KNN trainer
knn_train_eval <- function(dat, target, k_grid = 3:45, vfolds = 5) {
  # Split data into training data and test data
  set.seed(123)
  vfolds <- min(vfolds, min(table(dat[[target]])))
  split <- initial_split(dat, strata = all_of(target), prop = 0.8)
  train <- training(split)
  test  <- testing(split)
  
  # Recipe
  rec <- recipe(reformulate(termlabels = setdiff(names(dat), target), response = target),
                data = train) |>
    step_nzv(all_predictors()) |> # Remove near-zero variance predictors (almost constant)
    step_impute_median(all_numeric_predictors()) |> # Replace missing numeric values with median
    step_impute_mode(all_nominal_predictors()) |> # Replace missing categorical values with mode
    step_other(all_nominal_predictors(), threshold = 0.02) |> # Group rare factor levels (<2%) into "other"
    step_novel(all_nominal_predictors()) |> # Handle unseen categorical levels in new data during prediction
    step_dummy(all_nominal_predictors()) |> # Convert categorical predictors into dummy (one-hot) variables
    step_zv(all_predictors()) |> # Drop any predictors that became constant after dummying
    #step_corr(all_numeric_predictors(), threshold = 0.9) |>   # Remove highly correlated vars
    #step_pca(all_numeric_predictors(), threshold = 0.9) |>    # Reduce via PCA
    step_smote(all_outcomes()) |> # Balance class distribution by synthetic oversampling (SMOTE)
    step_normalize(all_numeric_predictors()) # Normalize numeric predictors
  
  # Model spec + grid
  knn_spec <- nearest_neighbor(neighbors = tune(), 
                               weight_func = "inv", 
                               dist_power = 1) |>
    set_engine("kknn") |>
    set_mode("classification")
  
  wf <- workflow() |> add_model(knn_spec) |> add_recipe(rec)
  metrics_set <- metric_set(accuracy, kap, bal_accuracy, f_meas)
  folds <- vfold_cv(train, v = vfolds, strata = all_of(target))
  res <- tune_grid(
    wf, resamples = folds, grid = tibble(neighbors = k_grid),
    metrics = metrics_set, 
    control = control_grid(save_pred = TRUE)
  )
  best <- select_best(res, metric = "bal_accuracy")
  final_wf <- finalize_workflow(wf, best)
  fit <- fit(final_wf, data = train)
  
  # Test performance
  preds <- predict(fit, test, type = "prob") |>
    bind_cols(predict(fit, test, type = "class")) |>
    bind_cols(test |> select(all_of(target)))
  
  colnames(preds)[ncol(preds)-1] <- ".pred_class"
  
  levs <- levels(preds[[target]])
  
  prob_cols <- paste0(".pred_", levs)
  
  test_metrics <- metrics_set(preds, truth = !!sym(target), estimate = .pred_class)
  
  # Compute log-loss
  test_logloss <- yardstick::mn_log_loss(
    preds,
    truth = !!rlang::sym(target),
    !!!rlang::syms(prob_cols)
  )
  
  list(
    best_k = best$neighbors,
    fit = fit,
    test_metrics = test_metrics,
    test_logloss = test_logloss,
    confusion = conf_mat(preds, truth = !!sym(target), estimate = .pred_class),
    vip = tryCatch(vip::vi(fit$fit$fit$fit, method = "permute"), error = function(e) NULL)
  )
}

# KNN for NRW
# All predictors
dat_nrw_all <- prep_xy(df_baixa, target = "nrw_class",
                       remove_keys = FALSE, key_vars = vars_nrw_key)
nrw_all <- knn_train_eval(dat_nrw_all, target = "nrw_class")

# Without key vars (v_agua_entrada_no_sistema, v_agua_nao_faturada)
dat_nrw_drop <- prep_xy(df_baixa, target = "nrw_class",
                        remove_keys = TRUE, key_vars = vars_nrw_key)
nrw_drop <- knn_train_eval(dat_nrw_drop, target = "nrw_class")

cat("\n--- NRW results ---\n")
cat("With key vars, best k =", nrw_all$best_k, "\n")
print(nrw_all$test_metrics)
print(nrw_all$test_logloss)
print(nrw_all$confusion)

cat("\nWithout key vars, best k =", nrw_drop$best_k, "\n")
print(nrw_drop$test_metrics)
print(nrw_drop$test_logloss)
print(nrw_drop$confusion)

# KNN for Real Losses
# All predictors (incl. key vars)
dat_rl_all <- prep_xy(df_baixa, target = "rl_class",
                      remove_keys = FALSE, key_vars = vars_rl_key)
rl_all <- knn_train_eval(dat_rl_all, target = "rl_class")

# Without key vars (v_comprimento_total_de_condutas, v_ramais_de_ligacao, v_perdas_reais)
dat_rl_drop <- prep_xy(df_baixa, target = "rl_class",
                       remove_keys = TRUE, key_vars = vars_rl_key)
rl_drop <- knn_train_eval(dat_rl_drop, target = "rl_class")

cat("\n--- Real Losses results ---\n")
cat("With key vars, best k =", rl_all$best_k, "\n")
print(rl_all$test_metrics)
print(rl_all$test_logloss)
print(rl_all$confusion)

cat("\nWithout key vars, best k =", rl_drop$best_k, "\n")
print(rl_drop$test_metrics)
print(rl_drop$test_logloss)
print(rl_drop$confusion)

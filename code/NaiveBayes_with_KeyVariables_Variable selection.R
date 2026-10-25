suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(infotheo)
  library(e1071)
  library(caret)
})

# ---- Load and scope ----
FILE_PATH <- "df_2023_final.xlsx"
df <- readxl::read_excel(FILE_PATH)
if ("Tipo_Sistema" %in% names(df)) df <- df %>% filter(str_detect(tolower(Tipo_Sistema), "baixa"))
stopifnot(all(c("NRW_class","RL_class") %in% names(df)))
y_nrw <- factor(df$NRW_class)
y_rl  <- factor(df$RL_class)

# ---- Candidate pools ----
HARD_EXCLUDE   <- c("Empresa", "Tipo_Sistema", "NRW_class", "RL_class")
ALL_CANDIDATES <- setdiff(names(df), HARD_EXCLUDE)
NUM_CANDS <- ALL_CANDIDATES[vapply(df[ALL_CANDIDATES], is.numeric, logical(1))]
CAT_WHITELIST <- intersect(c("V_Modelo_de_gestão","V_Tipologia_da_área_de_intervenção"), names(df))
CAT_CANDS <- CAT_WHITELIST[!vapply(df[CAT_WHITELIST], is.numeric, logical(1))]
TOTAL_CANDS <- unique(c(NUM_CANDS, CAT_CANDS))
if (!length(TOTAL_CANDS)) stop("No candidate features after exclusions.")

# ---- Required key variables (must be included) ----
KEY_NRW <- c("V_Água_entrada_no_sistema", "I_Água_não_faturada")
KEY_RL  <- c("I_Perdas_reais_de_água", "V_Comprimento_total_de_condutas", "V_Ramais_de_ligação")
if (length(setdiff(KEY_NRW, names(df)))) stop("Missing NRW keys in data.")
if (length(setdiff(KEY_RL,  names(df)))) stop("Missing RL keys in data.")

# ---- Fixed levels for the two categorical features ----
MODEL_GESTAO_LEVELS <- c(
  "Delegação (empresa municipal ou intermunicipal)",
  "Concessão (concessão municipal)",
  "Delegação (parceria Estado/ municípios)",
  "Concessão (concessão multimunicipal)",
  "Gestão direta (serviço municipal)",
  "Delegação (empresa estatal)",
  "Gestão direta (serviço municipalizado ou intermunicipalizado)"
)
TIPOLOGIA_LEVELS <- c(
  "Área predominantemente urbana",
  "Área predominantemente rural",
  "Área mediamente urbana"
)
CAT_MISS  <- "__MISSING__"
CAT_NOVEL <- "__NOVEL__"

# ---- Fold-wise preprocessing (numeric + categorical) ----
prep_fit_mixed <- function(Xtr, num_cols, cat_cols) {
  med <- mu <- sdv <- NULL
  if (length(num_cols)) {
    Xn <- as.matrix(Xtr[, num_cols, drop = FALSE])
    med <- apply(Xn, 2, function(v) if (all(is.na(v))) 0 else stats::median(v, na.rm = TRUE))
    for (j in seq_along(med)) {
      idx <- is.na(Xn[, j]); if (any(idx)) Xn[idx, j] <- med[j]
    }
    mu  <- apply(Xn, 2, mean)
    sdv <- apply(Xn, 2, sd); sdv[sdv == 0 | is.na(sdv)] <- 1
  }
  levels_list <- list()
  if (length(cat_cols)) {
    for (cn in cat_cols) {
      v <- as.character(Xtr[[cn]]); v[is.na(v) | trimws(v)==""] <- CAT_MISS
      if (cn == "V_Modelo_de_gestão") {
        lv <- c(MODEL_GESTAO_LEVELS, CAT_MISS, CAT_NOVEL)
      } else if (cn == "V_Tipologia_da_área_de_intervenção") {
        lv <- c(TIPOLOGIA_LEVELS, CAT_MISS, CAT_NOVEL)
      } else {
        lv <- unique(c(v, CAT_MISS, CAT_NOVEL))
      }
      levels_list[[cn]] <- lv
    }
  }
  list(num=list(cols=num_cols, med=med, mu=mu, sd=sdv),
       cat=list(cols=cat_cols, levels=levels_list))
}
prep_apply_mixed <- function(X, fit) {
  out <- data.frame(row.names = seq_len(nrow(X)))
  if (length(fit$num$cols)) {
    Xn <- as.matrix(X[, fit$num$cols, drop = FALSE])
    for (j in seq_along(fit$num$med)) {
      idx <- is.na(Xn[, j]); if (any(idx)) Xn[idx, j] <- fit$num$med[j]
    }
    Xn_df <- as.data.frame(scale(Xn, center = fit$num$mu, scale = fit$num$sd))
    colnames(Xn_df) <- fit$num$cols
    out <- cbind(out, Xn_df)
  }
  if (length(fit$cat$cols)) {
    for (cn in fit$cat$cols) {
      v <- as.character(X[[cn]])
      v[is.na(v) | trimws(v)==""] <- CAT_MISS
      lv <- fit$cat$levels[[cn]]
      v[!(v %in% lv)] <- CAT_NOVEL
      out[[cn]] <- factor(v, levels = lv)
    }
  }
  out
}

# ---- Metrics ----
bal_acc <- function(y_true, y_pred) {
  tab <- table(y_true, y_pred)
  rec <- diag(prop.table(tab, 1))
  mean(rec, na.rm = TRUE)
}
logloss_mc <- function(y_true, prob, eps = 1e-15) {
  y_true <- factor(y_true); L <- levels(y_true)
  if (is.data.frame(prob)) prob <- as.matrix(prob)
  prob <- pmin(pmax(prob, eps), 1 - eps)
  rs <- rowSums(prob); prob[rs == 0 | is.na(rs), ] <- 1/length(L)
  prob <- prob / rowSums(prob)
  -mean(log(prob[cbind(seq_along(y_true), as.integer(y_true))]))
}

# ---- OOF evaluation for Naive Bayes with mixed features and LogLoss ----
nb_oof_eval_mixed <- function(X, y, seed = 42) {
  set.seed(seed)
  folds <- caret::createFolds(y, k = 5, list = TRUE, returnTrain = TRUE)
  L <- levels(y)
  y_pred   <- factor(rep(NA_character_, length(y)), levels = L)
  prob_oof <- matrix(NA_real_, nrow = length(y), ncol = length(L)); colnames(prob_oof) <- L
  for (tr_idx in folds) {
    vl_idx <- setdiff(seq_len(nrow(X)), tr_idx)
    Xtr <- X[tr_idx, , drop = FALSE]; ytr <- y[tr_idx]
    Xvl <- X[vl_idx, , drop = FALSE]
    num_cols <- names(Xtr)[vapply(Xtr, is.numeric, logical(1))]
    cat_cols <- setdiff(names(Xtr), num_cols)
    fit  <- prep_fit_mixed(Xtr, num_cols, cat_cols)
    Xtr2 <- prep_apply_mixed(Xtr, fit)
    Xvl2 <- prep_apply_mixed(Xvl, fit)
    mdl <- e1071::naiveBayes(x = Xtr2, y = ytr, laplace = 1)
    y_pred[vl_idx] <- predict(mdl, Xvl2, type = "class")
    pr <- predict(mdl, Xvl2, type = "raw")
    pr_full <- matrix(0, nrow = nrow(pr), ncol = length(L)); colnames(pr_full) <- L
    common <- intersect(colnames(pr), L)
    if (length(common)) pr_full[, common] <- as.matrix(pr[, common, drop = FALSE])
    rs <- rowSums(pr_full); pr_full[rs == 0 | is.na(rs), ] <- 1/length(L)
    pr_full <- pr_full / rowSums(pr_full)
    prob_oof[vl_idx, ] <- pr_full
  }
  list(
    oof_pred = y_pred,
    oof_prob = prob_oof,
    oof_acc  = mean(y_pred == y),
    oof_bacc = bal_acc(y, y_pred),
    oof_logloss = logloss_mc(y, prob_oof)
  )
}

# ---- MI helpers (numeric discretized; categorical direct) ----
safe_discretize <- function(v, nbins = 5) {
  if (all(is.na(v))) return(rep(1L, length(v)))
  v2 <- v; v2[is.na(v2)] <- median(v2, na.rm = TRUE)
  uq <- unique(v2); if (length(uq) < 2) return(rep(1L, length(v2)))
  k <- max(2, min(nbins, length(uq) - 1))
  out <- tryCatch(
    infotheo::discretize(data.frame(x = v2), disc = "equalfreq", nbins = k)[, 1],
    error = function(e) cut(v2, breaks = k, labels = FALSE, include.lowest = TRUE)
  )
  as.integer(out)
}
mi_for_cols_mixed <- function(df_cols, y, nbins = 5) {
  y_fac <- factor(y)
  ms <- sapply(names(df_cols), function(cn) {
    col <- df_cols[[cn]]
    if (is.numeric(col)) {
      bins <- safe_discretize(col, nbins = nbins)
      as.numeric(infotheo::mutinformation(X = factor(bins), Y = y_fac))
    } else {
      fac <- as.factor(ifelse(is.na(col) | trimws(as.character(col))=="", CAT_MISS, as.character(col)))
      as.numeric(infotheo::mutinformation(X = fac, Y = y_fac))
    }
  })
  out <- data.frame(feature = names(df_cols), mi = as.numeric(ms))
  out[order(-out$mi), , drop = FALSE]
}

print_mi_overview_mixed <- function(task_name, key_vars, pool_df, y, nbins = 5, top_k = 15) {
  mi_keys <- mi_for_cols_mixed(df[, key_vars, drop = FALSE], y, nbins = nbins)
  cat(sprintf("\n[%s] MI of required key variables:\n", task_name))
  print(mi_keys, row.names = FALSE)
  mi_pool <- mi_for_cols_mixed(pool_df, y, nbins = nbins)
  cat(sprintf("\n[%s] Top-%d MI features among NON-key variables:\n", task_name, top_k))
  print(utils::head(mi_pool, top_k), row.names = FALSE)
  mi_pool
}

# ---- Search: include keys, add top-j non-key MI (mixed), report LogLoss ----
search_include_keys_mixed <- function(task_name, y, key_vars, jmax = 20, seed = 42) {
  if (!all(key_vars %in% TOTAL_CANDS)) stop(sprintf("[%s] keys not in candidate set.", task_name))
  pool <- setdiff(TOTAL_CANDS, key_vars)
  pool_df <- df[, pool, drop = FALSE]
  mi_pool <- print_mi_overview_mixed(task_name, key_vars, pool_df, y, nbins = 5, top_k = 15)
  results <- list(); jmax <- min(jmax, nrow(mi_pool))
  for (j in 0:jmax) {
    extras <- if (j == 0) character(0) else mi_pool$feature[1:j]
    feats  <- unique(c(key_vars, extras))
    Xsel   <- df[, feats, drop = FALSE]
    oof <- nb_oof_eval_mixed(Xsel, y, seed = seed)
    cat(sprintf("[%s][Include Keys] +%02d extras -> OOF Acc=%.3f, OOF BA=%.3f, OOF LogLoss=%.4f\n",
                task_name, j, oof$oof_acc, oof$oof_bacc, oof$oof_logloss))
    results[[length(results)+1]] <- list(j=j, features=feats, oof=oof)
  }
  scores <- data.frame(
    BA  = sapply(results, function(r) r$oof$oof_bacc),
    NLL = sapply(results, function(r) r$oof$oof_logloss),
    Acc = sapply(results, function(r) r$oof$oof_acc),
    Len = sapply(results, function(r) length(r$features))
  )
  best_idx <- order(scores$BA, -scores$NLL, scores$Acc, -scores$Len, decreasing = TRUE)[1]
  best <- results[[best_idx]]
  cm <- table(Truth = y, Pred = best$oof$oof_pred)
  list(results = results, best = best, cm = cm)
}

# ---- Run Version A (keys enforced; categoricals allowed; LogLoss reported) ----
cat("=== VERSION A (include keys; allow two categorical features; report LogLoss) ===\n")

nrw_A <- search_include_keys_mixed("NRW_class", y_nrw, KEY_NRW, jmax = 20, seed = 42)
cat("\n[NRW_A] Best: +", nrw_A$best$j, " extras\n", sep = ""); print(nrw_A$cm)
cat("\n[NRW_A] Best feature set:\n"); print(nrw_A$best$features)

rl_A  <- search_include_keys_mixed("RL_class",  y_rl,  KEY_RL,  jmax = 20, seed = 42)
cat("\n[RL_A]  Best: +", rl_A$best$j, " extras\n", sep = ""); print(rl_A$cm)
cat("\n[RL_A]  Best feature set:\n"); print(rl_A$best$features)


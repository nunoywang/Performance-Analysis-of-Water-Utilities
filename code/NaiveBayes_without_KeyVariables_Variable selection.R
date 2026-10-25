suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(infotheo)
  library(e1071)
  library(caret)
})

# ---- Load data----
FILE_PATH <- "df_2023_final.xlsx"
df <- readxl::read_excel(FILE_PATH)

# ---- Keep Baixa rows; targets ----
if ("Tipo_Sistema" %in% names(df)) {
  df <- df %>% filter(str_detect(tolower(Tipo_Sistema), "baixa"))
}
stopifnot(all(c("NRW_class","RL_class") %in% names(df)))
y_nrw <- factor(df$NRW_class)
y_rl  <- factor(df$RL_class)

# ---- Candidate pools ----
HARD_EXCLUDE   <- c("Empresa", "Tipo_Sistema", "NRW_class", "RL_class")
ALL_CANDIDATES <- setdiff(names(df), HARD_EXCLUDE)

NUM_CANDIDATES <- ALL_CANDIDATES[vapply(df[ALL_CANDIDATES], is.numeric, logical(1))]

CAT_WHITELIST <- intersect(
  c("V_Modelo_de_gestão", "V_Tipologia_da_área_de_intervenção"),
  names(df)
)
CAT_CANDIDATES <- CAT_WHITELIST[!vapply(df[CAT_WHITELIST], is.numeric, logical(1))]

TOTAL_CANDIDATES <- unique(c(NUM_CANDIDATES, CAT_CANDIDATES))
if (!length(TOTAL_CANDIDATES)) stop("No candidate features after exclusions.")

# ---- Task-specific keys to EXCLUDE (correct names) ----
KEY_NRW <- c("V_Água_entrada_no_sistema", "I_Água_não_faturada")
KEY_RL  <- c("I_Perdas_reais_de_água", "V_Comprimento_total_de_condutas", "V_Ramais_de_ligação")

# ---- Enumerations for the two categorical columns (levels are fixed) ----
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

# ---- Guards ----
assert_exclusion <- function(task_name, pool, keys) {
  leaked <- intersect(pool, keys)
  if (length(leaked)) stop(sprintf("[%s] exclusion failed; still in pool: %s",
                                   task_name, paste(leaked, collapse = ", ")))
}

# ---- Fold-wise preprocessing (numeric: median+standardize; categorical: fixed levels, miss/novel buckets) ----
CAT_MISS  <- "__MISSING__"
CAT_NOVEL <- "__NOVEL__"

prep_fit_mixed <- function(Xtr, num_cols, cat_cols) {
  # numeric stats
  med <- mu <- sdv <- NULL
  if (length(num_cols)) {
    Xn  <- as.matrix(Xtr[, num_cols, drop = FALSE])
    med <- apply(Xn, 2, function(v) if (all(is.na(v))) 0 else stats::median(v, na.rm = TRUE))
    for (j in seq_along(med)) {
      idx <- is.na(Xn[, j]); if (any(idx)) Xn[idx, j] <- med[j]
    }
    mu  <- apply(Xn, 2, mean)
    sdv <- apply(Xn, 2, sd); sdv[sdv == 0 | is.na(sdv)] <- 1
  }
  # categorical levels (use fixed enumerations when available)
  levels_list <- list()
  if (length(cat_cols)) {
    for (cn in cat_cols) {
      v <- as.character(Xtr[[cn]])
      v[is.na(v) | trimws(v)==""] <- CAT_MISS
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
  # numeric
  if (length(fit$num$cols)) {
    Xn <- as.matrix(X[, fit$num$cols, drop = FALSE])
    for (j in seq_along(fit$num$med)) {
      idx <- is.na(Xn[, j]); if (any(idx)) Xn[idx, j] <- fit$num$med[j]
    }
    Xn <- scale(Xn, center = fit$num$mu, scale = fit$num$sd)
    out <- cbind(out, as.data.frame(Xn))
    names(out)[ncol(out)-(ncol(Xn)-1):ncol(out)] <- fit$num$cols
  }
  # categorical
  if (length(fit$cat$cols)) {
    for (cn in fit$cat$cols) {
      v <- as.character(X[[cn]])
      v[is.na(v) | trimws(v)=="" ] <- CAT_MISS
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
  y_true <- factor(y_true)
  L <- levels(y_true)
  if (is.data.frame(prob)) prob <- as.matrix(prob)
  prob <- pmin(pmax(prob, eps), 1 - eps)
  rs <- rowSums(prob)
  prob[rs == 0 | is.na(rs), ] <- 1/length(L)
  prob <- prob / rowSums(prob)
  idx <- cbind(seq_len(length(y_true)), as.integer(y_true))
  -mean(log(prob[idx]))
}

# ---- 5-fold OOF evaluation for Naive Bayes with mixed features and LogLoss ----
nb_oof_eval_mixed <- function(X, y, seed = 42) {
  set.seed(seed)
  folds <- caret::createFolds(y, k = 5, list = TRUE, returnTrain = TRUE)
  L <- levels(y)
  y_pred   <- factor(rep(NA_character_, length(y)), levels = L)
  prob_oof <- matrix(NA_real_, nrow = length(y), ncol = length(L))
  colnames(prob_oof) <- L
  
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
    pr_full <- matrix(0, nrow = nrow(pr), ncol = length(L))
    colnames(pr_full) <- L
    common <- intersect(colnames(pr), L)
    if (length(common)) pr_full[, common] <- as.matrix(pr[, common, drop = FALSE])
    rs <- rowSums(pr_full)
    pr_full[rs == 0 | is.na(rs), ] <- 1/length(L)
    pr_full <- pr_full / rowSums(pr_full)
    
    prob_oof[vl_idx, ] <- pr_full
  }
  
  acc  <- mean(y_pred == y)
  bacc <- bal_acc(y, y_pred)
  ll   <- logloss_mc(y, prob_oof)
  list(oof_pred = y_pred, oof_prob = prob_oof, oof_acc = acc, oof_bacc = bacc, oof_logloss = ll)
}

# ---- Discretization helper for numeric MI ----
safe_discretize <- function(v, nbins = 5) {
  if (all(is.na(v))) return(rep(1L, length(v)))
  v2 <- v; v2[is.na(v2)] <- median(v2, na.rm = TRUE)
  uq <- unique(v2)
  if (length(uq) < 2) return(rep(1L, length(v2)))
  k <- max(2, min(nbins, length(uq) - 1))
  out <- tryCatch(
    infotheo::discretize(data.frame(x = v2), disc = "equalfreq", nbins = k)[, 1],
    error = function(e) cut(v2, breaks = k, labels = FALSE, include.lowest = TRUE)
  )
  as.integer(out)
}

# ---- MI ranking for mixed features ----
mi_rank_mixed <- function(X_pool, y, nbins = 5, task_name = "TASK") {
  y_fac <- factor(y)
  mi_scores <- sapply(names(X_pool), function(cn) {
    col <- X_pool[[cn]]
    if (is.numeric(col)) {
      bins <- safe_discretize(col, nbins = nbins)
      as.numeric(infotheo::mutinformation(X = factor(bins), Y = y_fac))
    } else {
      fac <- as.factor(ifelse(is.na(col) | trimws(as.character(col))=="", "__MISSING__", as.character(col)))
      as.numeric(infotheo::mutinformation(X = fac, Y = y_fac))
    }
  })
  mi_df <- data.frame(feature = names(X_pool), mi = as.numeric(mi_scores))
  mi_df <- mi_df[order(-mi_df$mi), , drop = FALSE]
  cat(sprintf("\n[%s] Top-15 MI features (mixed):\n", task_name))
  print(utils::head(mi_df, 15), row.names = FALSE)
  mi_df
}

# ---- Search WITHOUT task keys; incrementally add top-j MI features (mixed) ----
search_exclude_keys_mixed <- function(task_name, y, key_vars, jmax = 25, seed = 42) {
  pool <- setdiff(TOTAL_CANDIDATES, key_vars)
  assert_exclusion(task_name, pool, key_vars)
  X_pool <- df[, pool, drop = FALSE]
  
  mi_df <- mi_rank_mixed(X_pool, y, nbins = 5, task_name = task_name)
  
  results <- list()
  jmax <- min(jmax, nrow(mi_df))
  for (j in 1:jmax) {
    feats <- mi_df$feature[1:j]
    Xsel  <- df[, feats, drop = FALSE]
    oof <- nb_oof_eval_mixed(Xsel, y, seed = seed)
    cat(sprintf("[%s][Exclude Keys] top-%02d -> OOF Acc=%.3f, OOF BA=%.3f, OOF LogLoss=%.4f\n",
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
  list(mi = mi_df, results = results, best = best, cm = cm)
}

# ---- Run updated B-version (mixed) for both tasks ----
cat("=== VERSION B (exclude keys; allow two categorical features; report LogLoss) ===\n")

nrw_B <- search_exclude_keys_mixed("NRW_class", y_nrw, KEY_NRW, jmax = 25, seed = 42)
cat("\n[NRW_B] Best: top-", nrw_B$best$j, " features\n", sep = ""); print(nrw_B$cm)
cat("\n[NRW_B] Best feature set:\n"); print(nrw_B$best$features)

rl_B  <- search_exclude_keys_mixed("RL_class",  y_rl,  KEY_RL,  jmax = 25, seed = 42)
cat("\n[RL_B]  Best: top-", rl_B$best$j, " features\n", sep = ""); print(rl_B$cm)
cat("\n[RL_B]  Best feature set:\n"); print(rl_B$best$features)

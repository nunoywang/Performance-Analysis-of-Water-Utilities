library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(forcats)
library(stringr)
library(factoextra)
library(broom)
library(interactions)
library(car)
library(sandwich)
library(lmtest)
library(leaps)

df <- read_excel("df_2023_final.xlsx")

# ------------------------
# 1. Preparing data
# ------------------------

#Rename ERSAR classes to keep originals
df <- df %>%
  rename(
    NRW_ERSAR_class = NRW_class,
    RL_ERSAR_class  = RL_class
  )

#Ensure numeric columns are numeric
num_cols <- c(
  "V_Água_não_faturada",
  "V_Água_entrada_no_sistema",
  "V_Água_faturada",
  "V_Perdas_reais",
  "V_Ramais_de_ligação",
  "V_Comprimento_total_de_condutas",
  "I_Densidade_de_ramais"
)

#Coerce safely where present
for (cc in intersect(num_cols, names(df))) {
  df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
}

#Avoid dividing by zero
df <- df %>%
  mutate(
    V_Ramais_de_ligação = ifelse(V_Ramais_de_ligação <= 0, NA_real_, V_Ramais_de_ligação),
    V_Comprimento_total_de_condutas = ifelse(V_Comprimento_total_de_condutas <= 0, NA_real_, V_Comprimento_total_de_condutas),
    V_Água_entrada_no_sistema = ifelse(V_Água_entrada_no_sistema <= 0, NA_real_, V_Água_entrada_no_sistema)
  )

# ------------------------
# 2. Explanatory variables derived from ERSAR data.
#    Used to interpret NRW and RL performance and support
# ------------------------
df <- df %>%
  mutate(
    # connection density (ramais/km)
    conn_density = V_Ramais_de_ligação / V_Comprimento_total_de_condutas,
    
    # NRW percent (just for reference; ERSAR already has % indicator, but keep it)
    NRW_pct = 100 * (V_Água_entrada_no_sistema - V_Água_faturada) / V_Água_entrada_no_sistema
  )

# ------------------------
# 3. New Indicators
# ------------------------

# 3.1 NRW "intensity" indicator:
# - Baixa & I_Densidade_de_ramais >= 20  => NRW per connection per day (L/conn/day)
# - Alta OR (Baixa & density < 20)      => NRW per km per day (m3/km/day)
df <- df %>%
  mutate(
    NRW_new = case_when(
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais >= 20 ~
        (V_Água_não_faturada * 1000) / (V_Ramais_de_ligação * 365), # L/conn/day
      
      Tipo_Sistema == "Alta" |
        (Tipo_Sistema == "Baixa" & I_Densidade_de_ramais < 20) ~
        V_Água_não_faturada / (V_Comprimento_total_de_condutas * 365), # m3/km/day
      
      TRUE ~ NA_real_
    ),
    NRW_new_unit = case_when(
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais >= 20 ~ "L/conn/day",
      Tipo_Sistema == "Alta" | (Tipo_Sistema == "Baixa" & I_Densidade_de_ramais < 20) ~ "m3/km/day",
      TRUE ~ NA_character_
    )
  )

# 3.2 Real Losses: two NEW indicators
# - For Alta: RL per km per day (m3/km/day)
# - For Baixa: CRLI computed from:
#     RL per conn per day (L/conn/day) and RL per km per day (m3/km/day)
#     Convert m3/km/day -> L/km/day by *1000 for geometric mean consistency
df <- df %>%
  mutate(
    RL_km_day = V_Perdas_reais / (V_Comprimento_total_de_condutas * 365), # m3/km/day
    RL_conn_day_L = (V_Perdas_reais * 1000) / (V_Ramais_de_ligação * 365), # L/conn/day (only meaningful for Baixa)
    
    # CRLI only for Baixa
    CRLI = case_when(
      Tipo_Sistema == "Baixa" ~ sqrt(RL_conn_day_L * (RL_km_day * 1000)), # sqrt( L/conn/day * L/km/day )
      TRUE ~ NA_real_
    ),
    
    # The NEW Real Loss indicator we will classify on:
    RL_new = case_when(
      Tipo_Sistema == "Alta"  ~ RL_km_day,   # m3/km/day
      Tipo_Sistema == "Baixa" ~ CRLI,        # CRLI
      TRUE ~ NA_real_
    ),
    RL_new_unit = case_when(
      Tipo_Sistema == "Alta"  ~ "m3/km/day",
      Tipo_Sistema == "Baixa" ~ "CRLI",
      TRUE ~ NA_character_
    )
  )

# ------------------------
# 4. EDA quick checks
# ------------------------

#Plot NRW_new by unit/type
ggplot(df, aes(x = NRW_new)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ NRW_new_unit, scales = "free") +
  labs(title = "Distribution of NEW NRW indicator", x = "NRW_new", y = "Count")

ggplot(df, aes(x = Tipo_Sistema, y = NRW_new)) +
  geom_boxplot(fill="steelblue", alpha=0.7) + 
  geom_text_repel(
    data = df %>%
      group_by(NRW_new_unit, Tipo_Sistema) %>%
      filter(NRW_new > quantile(NRW_new, 0.95, na.rm = TRUE)),
    aes(label = Empresa),
    size = 3,
    color = "blue",
    max.overlaps = Inf
  ) +
  labs(
    x = "Type of System",
    y = "Non Revenue Water",
    title = "Non Revenue Water boxplot"
  ) +
  facet_wrap(~NRW_new_unit)+
  theme_minimal() #outliers for low density are only identified if we do this

ggplot(subset(df,df$Empresa!="INFRATRÓIA"),aes(x=RL_new,y=NRW_new, colour = as.factor(NRW_new_unit)))+
  geom_point()+
  facet_wrap(~Tipo_Sistema)

#Plot CRLI by area typology (Baixa only)
ggplot(df %>% filter(Tipo_Sistema == "Baixa"),
       aes(x = fct_infreq(V_Tipologia_da_área_de_intervenção), y = CRLI)) +
  geom_boxplot() +
  geom_text_repel(
    data = df %>%
      group_by(V_Tipologia_da_área_de_intervenção) %>%
      mutate(
        Q1 = quantile(CRLI, 0.25, na.rm = TRUE),
        Q3 = quantile(CRLI, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1
      ) %>%
      filter(CRLI > (Q3 + 1.5 * IQR) |
               CRLI < (Q1 - 1.5 * IQR)),
    aes(label = Empresa),
    size = 3,
    color = "blue"
  ) +
  labs(
    x = "Area tipology",
    y = "Real Losses",
    title = "Real Losses boxplot"
  ) +
  labs(title = "CRLI by Area Typology (Baixa)", x = "Area typology", y = "CRLI")+
  theme_minimal()

#Plot Distribution of Real Losses per km per day
ggplot(df %>% filter(is.finite(RL_km_day)),
       aes(x = RL_km_day)) +
  geom_histogram(bins = 30, fill = "grey40", color = "white") +
  facet_wrap(~ Tipo_Sistema, scales = "free") +
  labs(
    title = "Distribution of Real Losses per km per day",
    subtitle = "Shown separately for Alta and Baixa systems",
    x = "Real losses per km per day (m³/km/day)",
    y = "Number of utilities"
  )

# ------------------------
# 5. Identify explanatory variables for new indicators
# ------------------------
# 5.1 Correlations with numeric explanatory vars
candidate_expl <- c(
  "conn_density",
  "I_Densidade_de_ramais",
  "I_Acessibilidade_física_do_serviço",
  "I_Ocorrência_de_falhas_no_abastecimento",
  "I_Reabilitação_de_condutas",
  "I_Ramais_afetados_por_falhas_no_abastecimento",
  "I_Modelo_de_gestão",
  "V_Tipologia_da_área_de_intervenção",
  "V_Comprimento_total_de_condutas",
  "V_Comprimento_médio_de_condutas_com_mais_de_10_anos",
  "V_Condutas_reabilitadas_nos_últimos_cinco_anos",
  "V_Ramais_de_ligação",
  "V_Falhas_no_abastecimento",
  "V_Ramais_afetados_por_falhas_no_abastecimento",
  "V_Avarias_em_condutas",
  "V_Água_entrada_no_sistema",
  "V_Água_faturada",
  "V_Perdas_reais",
  "V_Perdas_de_água_por_erros_de_medição",
  "V_Consumo_não_faturado_não_medido",
  "V_Consumo_não_faturado_medido",
  "V_Gastos_totais",
  "V_Pessoal_em_outsourcing_afeto_ao_serviço_de_abastecimento_de_água",
  "V_Rendimentos_tarifários",
  "I_Adesão_ao_serviço",
  "V_Alojamentos_existentes",
  "Custo_de_substituição",
  "V_Índice_de_conhecimento_infraestrutural",
  "V_Índice_de_gestão_patrimonial_de_infraestruturas",
  "V_Pessoal_afeto_ao_serviço_de_abastecimento_de_água"
)

# Keep only those that exist
candidate_expl <- intersect(candidate_expl, names(df))

# Correlation table helper
corr_table <- function(data, y, xs) {
  out <- lapply(xs, function(xn) {
    x <- data[[xn]]
    yy <- data[[y]]
    ok <- is.finite(x) & is.finite(yy)
    if (sum(ok) < 10) return(NULL)
    data.frame(
      y = y,
      x = xn,
      cor = cor(yy[ok], x[ok]),
      n = sum(ok)
    )
  })
  bind_rows(out) %>% arrange(desc(abs(cor)))
}

corr_NRW <- corr_table(df, "NRW_new", candidate_expl)
corr_RL  <- corr_table(df, "RL_new",  candidate_expl)

print(head(corr_NRW, 20))
print(head(corr_RL, 20))

# 5.2 Simple regression
model_df <- df %>%
  filter(is.finite(NRW_new), is.finite(V_Comprimento_total_de_condutas), is.finite(V_Água_entrada_no_sistema)) %>%
  mutate(
    log_len = log(V_Comprimento_total_de_condutas),
    log_in  = log(V_Água_entrada_no_sistema),
    log_mo = log(V_Gastos_totais),
    log_pr = log(V_Perdas_reais),
    log_c = log1p(V_Consumo_não_faturado_não_medido)
  )

m0 <- lm(NRW_new ~ log_len + log_in + V_Tipologia_da_área_de_intervenção + Tipo_Sistema, data = model_df)
summary(m0)

m1 <- lm(NRW_new ~ V_Índice_de_conhecimento_infraestrutural + I_Ocorrência_de_falhas_no_abastecimento + log_len + log_in + I_Modelo_de_gestão + V_Tipologia_da_área_de_intervenção + Tipo_Sistema + I_Ocorrência_de_falhas_no_abastecimento*log_len, data = model_df)
summary(m1)
par(mfrow = c(2,2))
plot(m1, main = "m1") #normality looks good
dwtest(m1) #correlation satisfied
vif(m1) #no multicollinearity

coeftest(m1, vcov = vcovHC(m1, type = "HC1")) #to account for heterocedasticity

m2 <- lm(sqrt(NRW_new) ~ log_len + log_in + I_Ocorrência_de_falhas_no_abastecimento + V_Tipologia_da_área_de_intervenção + I_Modelo_de_gestão + Tipo_Sistema*log_in + I_Ocorrência_de_falhas_no_abastecimento*log_len, data = model_df)
summary(m2)
plot(m2, main = "m2")

m21 <- lm(log(NRW_new) ~ log_len + log_in + I_Ocorrência_de_falhas_no_abastecimento + V_Tipologia_da_área_de_intervenção + Tipo_Sistema + I_Modelo_de_gestão + Tipo_Sistema*log_in + I_Ocorrência_de_falhas_no_abastecimento*log_len, data = model_df)
summary(m21)
plot(m21, main = "m21")

m3 <- lm(RL_new ~ I_Ocorrência_de_falhas_no_abastecimento + log_len + log_in + log_mo + Tipo_Sistema + V_Tipologia_da_área_de_intervenção + I_Ocorrência_de_falhas_no_abastecimento*log_len, data = model_df)
summary(m3)
plot(m3, main = "m3")
dwtest(m3)

m31 <- lm(log(RL_new) ~ I_Acessibilidade_física_do_serviço + I_Ocorrência_de_falhas_no_abastecimento + log_len + log_in + log_mo + Tipo_Sistema + V_Tipologia_da_área_de_intervenção, data = model_df)
summary(m31)
plot(m31, , main = "m31")

m4 <- lm(RL_new ~ V_Índice_de_conhecimento_infraestrutural + I_Ocorrência_de_falhas_no_abastecimento + log_len + log_in + log(V_Rendimentos_tarifários) + I_Adesão_ao_serviço + Tipo_Sistema + V_Tipologia_da_área_de_intervenção + I_Ocorrência_de_falhas_no_abastecimento*log_len, data = model_df)
summary(m4)
plot(m4, main = "m4")
dwtest(m4) #correlation not satisfied but we can work with this
vif(m4) #no multicollinearity

coeftest(m4, vcov = vcovHC(m4, type = "HC1")) #to account for heterocedasticity

m4b <- lm(RL_new ~   I_Acessibilidade_física_do_serviço + I_Ocorrência_de_falhas_no_abastecimento + log_len + log_in + log1p(V_Rendimentos_tarifários) + I_Adesão_ao_serviço + I_Densidade_de_ramais + V_Tipologia_da_área_de_intervenção, data = subset(model_df, model_df$Tipo_Sistema=="Baixa"))
summary(m4b)#to check connections variables
plot(m4b, main = "m4b")
dwtest(m4)

m41 <- lm(log(RL_new) ~ I_Acessibilidade_física_do_serviço + V_Índice_de_conhecimento_infraestrutural + I_Ocorrência_de_falhas_no_abastecimento + log_len + log_in + log(V_Rendimentos_tarifários) + I_Adesão_ao_serviço + Tipo_Sistema + V_Tipologia_da_área_de_intervenção, data = model_df)
summary(m41)
plot(m41, main = "m41") #Adjusted R^2 é muito melhor mas a interpretação perde-se
dwtest(m41)
# ------------------------
# 6. Thresholds and Classification
# ------------------------

# 6.1 Cutpoints generator (quantiles for calibration)
get_cuts <- function(x, probs = c(0.25, 0.75)) {
  x <- x[is.finite(x)]
  if (length(x) < 10) return(c(NA_real_, NA_real_))
  as.numeric(quantile(x, probs = probs, na.rm = TRUE, type = 7))
}


# 6.2 Schemes to test
schemes <- list(
  quartiles = c(0.25, 0.75),
  terciles  = c(1/3, 2/3),
  p20_p80   = c(0.20, 0.80),
  p40_p80   = c(0.40, 0.80)  # renamed from "quintiles" to what it actually is
)


# 6.3 Compute numeric cut tables by unit and scheme
compute_cuts_by_unit_scheme <- function(df, value_col, unit_col, schemes) {
  base <- df %>%
    filter(is.finite(.data[[value_col]]), !is.na(.data[[unit_col]])) %>%
    group_by(.data[[unit_col]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    rename(unit = .data[[unit_col]])
  
  cuts_all <- lapply(names(schemes), function(nm) {
    probs <- schemes[[nm]]
    
    cuts_tbl <- df %>%
      filter(is.finite(.data[[value_col]]), !is.na(.data[[unit_col]])) %>%
      group_by(.data[[unit_col]]) %>%
      summarise(
        cut1 = get_cuts(.data[[value_col]], probs = probs)[1],
        cut2 = get_cuts(.data[[value_col]], probs = probs)[2],
        .groups = "drop"
      ) %>%
      rename(unit = .data[[unit_col]]) %>%
      mutate(
        scheme = nm,
        p1 = probs[1],
        p2 = probs[2]
      )
    
    cuts_tbl
  })
  
  cuts_all <- bind_rows(cuts_all) %>%
    left_join(base, by = "unit") %>%
    relocate(scheme, p1, p2, unit, n, cut1, cut2)
  
  cuts_all
}


# 6.4 Classify using FIXED numeric cuts
apply_fixed_cuts <- function(df, value_col, unit_col, cuts_tbl, out_class_col) {
  # cuts_tbl must have: scheme, unit, cut1, cut2
  df %>%
    left_join(cuts_tbl %>% select(scheme, unit, cut1, cut2),
              by = setNames("unit", unit_col)) %>%
    mutate(
      "{out_class_col}" := case_when(
        !is.finite(.data[[value_col]]) ~ NA_character_,
        is.na(cut1) | is.na(cut2)      ~ NA_character_,
        .data[[value_col]] <= cut1     ~ "Good",
        .data[[value_col]] <= cut2     ~ "Medium",
        .data[[value_col]] >  cut2     ~ "Unsatisfactory",
        TRUE                           ~ NA_character_
      ),
      "{out_class_col}" := factor(.data[[out_class_col]],
                                  levels = c("Good","Medium","Unsatisfactory"))
    ) %>%
    select(-cut1, -cut2)
}


# 6.5 Comparison helpers

# Agreement vs ERSAR
agreement_vs_ersar <- function(df, ersar_col, new_col) {
  df %>%
    filter(!is.na(.data[[ersar_col]]), !is.na(.data[[new_col]])) %>%
    summarise(agreement = mean(.data[[ersar_col]] == .data[[new_col]])) %>%
    pull(agreement)
}

# Row-wise % distribution of Proposed within each ERSAR class
compare_to_ersar <- function(df, ersar_col, new_col) {
  df %>%
    filter(!is.na(.data[[ersar_col]]), !is.na(.data[[new_col]])) %>%
    count(.data[[ersar_col]], .data[[new_col]]) %>%
    group_by(.data[[ersar_col]]) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    rename(ERSAR = 1, Proposed = 2)
}

# Class balance (% in Good/Medium/Unsatisfactory)
class_balance <- function(df, class_col) {
  df %>%
    filter(!is.na(.data[[class_col]])) %>%
    count(.data[[class_col]]) %>%
    mutate(pct = n / sum(n)) %>%
    rename(Class = 1)
}


# 6.6 Build cut tables + evaluation tables for all schemes
run_calibration <- function(df, value_col, unit_col, ersar_col, schemes, prefix) {
  
  # 1) Calibration: compute numeric cuts (this is what you will "freeze")
  cuts_tbl <- compute_cuts_by_unit_scheme(df, value_col, unit_col, schemes)
  
  # 2) For each scheme: classify using the fixed cut values + build comparison tables
  eval_summary <- list()
  eval_conf_long <- list()
  eval_balance_long <- list()
  
  for (nm in names(schemes)) {
    
    # scheme-specific cut table
    cuts_nm <- cuts_tbl %>% filter(scheme == nm)
    
    class_col <- paste0(prefix, "_", nm)  # keep all class columns for later inspection
    
    tmp <- df %>%
      rename(unit = .data[[unit_col]]) %>%
      apply_fixed_cuts(
        value_col = value_col,
        unit_col  = "unit",
        cuts_tbl  = cuts_nm,
        out_class_col = class_col
      ) %>%
      rename("{unit_col}" := unit)
    
    # agreement
    agree <- agreement_vs_ersar(tmp, ersar_col, class_col)
    
    # confusion (row-wise %)
    conf <- compare_to_ersar(tmp, ersar_col, class_col) %>%
      mutate(scheme = nm)
    
    # class balance
    bal <- class_balance(tmp, class_col) %>%
      mutate(scheme = nm)
    
    eval_summary[[nm]] <- data.frame(
      scheme = nm,
      p1 = schemes[[nm]][1],
      p2 = schemes[[nm]][2],
      agreement = agree
    )
    
    eval_conf_long[[nm]] <- conf
    eval_balance_long[[nm]] <- bal
    
    # keep column in df
    df <- tmp
  }
  
  # Tables for comparison
  conf_wide <- bind_rows(eval_conf_long) %>%
    select(scheme, ERSAR, Proposed, pct) %>%
    pivot_wider(names_from = Proposed, values_from = pct, values_fill = 0) %>%
    arrange(scheme, ERSAR)
  
  balance_wide <- bind_rows(eval_balance_long) %>%
    select(scheme, Class, pct) %>%
    pivot_wider(names_from = Class, values_from = pct, values_fill = 0) %>%
    arrange(scheme)
  
  list(
    df = df,                               # df with all scheme class columns appended
    cuts_tbl = cuts_tbl,                   # numeric cut values (these are what you'd freeze)
    summary = bind_rows(eval_summary) %>% arrange(desc(agreement)),
    conf_wide = conf_wide,                 # ERSAR -> Proposed row-wise % table
    balance_wide = balance_wide            # Proposed class balance per scheme
  )
}


# 6.7 Run calibration for NRW and RL

# NRW: fixed cuts table + evaluation tables
nrw_cal <- run_calibration(
  df       = df,
  value_col= "NRW_new",
  unit_col = "NRW_new_unit",
  ersar_col= "NRW_ERSAR_class",
  schemes  = schemes,
  prefix   = "NRW_new_class"
)

df <- nrw_cal$df

# RL: fixed cuts table + evaluation tables
rl_cal <- run_calibration(
  df       = df,
  value_col= "RL_new",
  unit_col = "RL_new_unit",
  ersar_col= "RL_ERSAR_class",
  schemes  = schemes,
  prefix   = "RL_new_class"
)

df <- rl_cal$df

# ERSAR class balance
# Generic helper
ersar_class_balance <- function(df, ersar_col) {
  df %>%
    filter(!is.na(.data[[ersar_col]])) %>%
    count(.data[[ersar_col]]) %>%
    mutate(pct = n / sum(n)) %>%
    rename(Class = 1) %>%
    select(Class, pct) %>%
    tidyr::pivot_wider(
      names_from = Class,
      values_from = pct,
      values_fill = 0
    )
}

# NRW - ERSAR class balance
nrw_ersar_balance <- ersar_class_balance(df, "NRW_ERSAR_class") %>%
  mutate(scheme = "ERSAR") %>%
  relocate(scheme)

nrw_balance_all <- bind_rows(
  nrw_ersar_balance,
  nrw_cal$balance_wide
)

# RL - ERSAR class balance
rl_ersar_balance <- ersar_class_balance(df, "RL_ERSAR_class") %>%
  mutate(scheme = "ERSAR") %>%
  relocate(scheme)

rl_balance_all <- bind_rows(
  rl_ersar_balance,
  rl_cal$balance_wide
)

cat("\n--- NRW: Summary (agreement only; calibration year) ---\n")
print(nrw_cal$summary)

cat("\n--- NRW: Fixed numeric cut values (by scheme & unit) ---\n")
print(nrw_cal$cuts_tbl)

cat("\n--- NRW: ERSAR vs Proposed (row-wise %) ---\n")
print(nrw_cal$conf_wide)

cat("\n--- NRW: Proposed class balance (%) ---\n")
print(nrw_balance_all)

cat("\n--- RL: Summary (agreement only; calibration year) ---\n")
print(rl_cal$summary)

cat("\n--- RL: Fixed numeric cut values (by scheme & unit) ---\n")
print(rl_cal$cuts_tbl)

cat("\n--- RL: ERSAR vs Proposed (row-wise %) ---\n")
print(rl_cal$conf_wide)

cat("\n--- RL: Proposed class balance (%) ---\n")
print(rl_balance_all)

# 6.8 Hybrid scheme

# Helper: quartile cuts by unit
compute_quartile_cuts_by_unit <- function(df, value_col, unit_col, min_n = 10) {
  df %>%
    filter(!is.na(.data[[unit_col]]), is.finite(.data[[value_col]])) %>%
    group_by(.data[[unit_col]]) %>%
    summarise(
      n_all = n(),
      q25 = if (n_all >= min_n) as.numeric(quantile(.data[[value_col]], 0.25, na.rm = TRUE, type = 7)) else NA_real_,
      q75 = if (n_all >= min_n) as.numeric(quantile(.data[[value_col]], 0.75, na.rm = TRUE, type = 7)) else NA_real_,
      .groups = "drop"
    )
}

# Compute cut2 from ERSAR-Unsatisfactory + cut1 from non-unsat, by unit
compute_hybrid_cuts_by_unit <- function(df, value_col, unit_col, ersar_col,
                                        p_good = 0.50,
                                        cut2_method = c("min_unsat", "q_unsat"),
                                        q_unsat = 0.10,
                                        min_n = 10) {
  
  cut2_method <- match.arg(cut2_method)
  
  df2 <- df %>%
    filter(!is.na(.data[[unit_col]]),
           !is.na(.data[[ersar_col]]),
           is.finite(.data[[value_col]]))
  
  # cut2: boundary for Unsatisfactory from ERSAR
  cut2_tbl <- df2 %>%
    filter(.data[[ersar_col]] == "Unsatisfactory") %>%
    group_by(.data[[unit_col]]) %>%
    summarise(
      n_unsat = n(),
      cut2 = case_when(
        n_unsat < min_n ~ NA_real_,
        cut2_method == "min_unsat" ~ min(.data[[value_col]], na.rm = TRUE),
        cut2_method == "q_unsat"   ~ as.numeric(quantile(.data[[value_col]],
                                                         probs = q_unsat, na.rm = TRUE, type = 7)),
        TRUE ~ NA_real_
      ),
      .groups = "drop"
    )
  
  # cut1: Good/Medium split inside the non-unsat region
  cut1_tbl <- df2 %>%
    filter(.data[[ersar_col]] != "Unsatisfactory") %>%
    group_by(.data[[unit_col]]) %>%
    summarise(
      n_non_unsat = n(),
      cut1 = if (n_non_unsat < min_n) NA_real_
      else as.numeric(quantile(.data[[value_col]], probs = p_good, na.rm = TRUE, type = 7)),
      .groups = "drop"
    )
  
  full_join(cut1_tbl, cut2_tbl, by = unit_col)
}

# Build FINAL cuts: Hybrid where possible, else Quartiles
compute_final_hybrid_cuts <- function(df, value_col, unit_col, ersar_col,
                                      p_good = 0.50,
                                      cut2_method = "q_unsat",
                                      q_unsat = 0.10,
                                      min_n = 10,
                                      fallback_min_n = 10) {
  
  hybrid <- compute_hybrid_cuts_by_unit(
    df, value_col, unit_col, ersar_col,
    p_good = p_good,
    cut2_method = cut2_method,
    q_unsat = q_unsat,
    min_n = min_n
  )
  
  quart <- compute_quartile_cuts_by_unit(
    df, value_col, unit_col,
    min_n = fallback_min_n
  )
  
  # join by unit col name 
  out <- hybrid %>%
    left_join(quart, by = unit_col) %>%
    mutate(
      final_cut1 = dplyr::coalesce(cut1, q25),
      final_cut2 = dplyr::coalesce(cut2, q75),
      method = case_when(
        !is.na(cut1) & !is.na(cut2) ~ "Hybrid",
        (is.na(cut1) | is.na(cut2)) & !is.na(final_cut1) & !is.na(final_cut2) ~ "Quartiles_fallback",
        TRUE ~ "Insufficient_data"
      )
    ) %>%
    select(
      !!sym(unit_col),
      n_non_unsat, cut1,
      n_unsat, cut2,
      n_all, q25, q75,
      final_cut1, final_cut2,
      method
    )
  
  out
}

# Apply FINAL fixed cuts
apply_final_cuts <- function(df, value_col, unit_col, final_cuts_tbl, out_col) {
  
  cuts_tbl <- final_cuts_tbl %>%
    dplyr::rename(unit_tmp = !!rlang::sym(unit_col)) %>%
    dplyr::select(unit_tmp, final_cut1, final_cut2, method)
  
  df %>%
    dplyr::rename(unit_tmp = !!rlang::sym(unit_col)) %>%
    dplyr::left_join(cuts_tbl, by = "unit_tmp", suffix = c("", "_cuts")) %>%
    dplyr::mutate(
      "{out_col}" := dplyr::case_when(
        !is.finite(.data[[value_col]]) ~ NA_character_,
        is.na(final_cut1) | is.na(final_cut2) ~ NA_character_,
        .data[[value_col]] <= final_cut1 ~ "Good",
        .data[[value_col]] <= final_cut2 ~ "Medium",
        .data[[value_col]] >  final_cut2 ~ "Unsatisfactory",
        TRUE ~ NA_character_
      ),
      "{out_col}" := factor(.data[[out_col]], levels = c("Good","Medium","Unsatisfactory"))
    ) %>%
    dplyr::rename("{unit_col}" := unit_tmp) %>%
    dplyr::select(-final_cut1, -final_cut2, -method)
}


# -------------------------
# NRW: final hybrid cuts (q_unsat = 0.25) + fallback quartiles for NA units
# -------------------------
nrw_hybrid_final_cuts <- compute_final_hybrid_cuts(
  df,
  value_col = "NRW_new",
  unit_col  = "NRW_new_unit",
  ersar_col = "NRW_ERSAR_class",
  p_good    = 0.50,
  cut2_method = "q_unsat",
  q_unsat   = 0.25,
  min_n     = 10,
  fallback_min_n = 10
)

df <- apply_final_cuts(
  df,
  value_col = "NRW_new",
  unit_col  = "NRW_new_unit",
  final_cuts_tbl = nrw_hybrid_final_cuts,
  out_col   = "NRW_hybrid_class"
)

# -------------------------
# RL: final hybrid cuts (q_unsat = 0.10) + fallback quartiles for NA units
# -------------------------
rl_hybrid_final_cuts <- compute_final_hybrid_cuts(
  df,
  value_col = "RL_new",
  unit_col  = "RL_new_unit",
  ersar_col = "RL_ERSAR_class",
  p_good    = 0.50,
  cut2_method = "q_unsat",
  q_unsat   = 0.10,
  min_n     = 10,
  fallback_min_n = 10
)

df <- apply_final_cuts(
  df,
  value_col = "RL_new",
  unit_col  = "RL_new_unit",
  final_cuts_tbl = rl_hybrid_final_cuts,
  out_col   = "RL_hybrid_class"
)

# -------------------------
# OUTPUTS
# -------------------------
cat("\n--- NRW: Hybrid (final) class balance ---\n")
print(class_balance(df, "NRW_hybrid_class"))

cat("\n--- NRW: Hybrid (final) cutoff table (Hybrid + Quartiles fallback) ---\n")
print(nrw_hybrid_final_cuts)

cat("\n--- NRW: ERSAR vs Hybrid (final) (row-wise %) ---\n")
print(compare_to_ersar(df, "NRW_ERSAR_class", "NRW_hybrid_class"))

cat("\n--- RL: Hybrid (final) class balance ---\n")
print(class_balance(df, "RL_hybrid_class"))

cat("\n--- RL: Hybrid (final) cutoff table (Hybrid + Quartiles fallback) ---\n")
print(rl_hybrid_final_cuts)

cat("\n--- RL: ERSAR vs Hybrid (final) (row-wise %) ---\n")
print(compare_to_ersar(df, "RL_ERSAR_class", "RL_hybrid_class"))

# ------------------------
# 7. Save outputs
# ------------------------
write.csv(df, "df_2023_new.csv", row.names = FALSE)



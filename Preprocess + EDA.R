library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2)
library(ggrepel)


#Download filtered data (AA type and relevant variables and indicators) there are some variables that don't exist for 2020
data_2023_I <- read_excel("~/MMA/MEDM/Dados_2023_filtered.xlsx", sheet = "Indicadores_2023")
data_2023_I <- data_2023_I %>%
  mutate(Indicador = paste0("I_", Indicador))
data_2023_V <- read_excel("~/MMA/MEDM/Dados_2023_filtered.xlsx", sheet = "Dados_2023")
data_2023_V <- data_2023_V %>%
  mutate(Variavel = paste0("V_", Variavel))

#aggregate into one dataset both indicators and variables
indicators_wide_2023 <- data_2023_I %>%
  select(Empresa, `Tipo Sistema`, Indicador, Valor) %>%
  pivot_wider(names_from = Indicador, values_from = Valor, id_cols = c(Empresa, `Tipo Sistema`),values_fn = first)

variables_wide_2023 <- data_2023_V %>%
  select(Empresa, `Tipo Sistema`, Variavel, Valor) %>%
  pivot_wider(names_from = Variavel, values_from = Valor, id_cols = c(Empresa, `Tipo Sistema`))

df_2023_final <- indicators_wide_2023 %>%
  left_join(variables_wide_2023, by = c("Empresa", "Tipo Sistema"))

#Compute the classifiers NRW and RL 
# Replace spaces with underscores in column names
names(df_2023_final) <- gsub(" ", "_", names(df_2023_final))

#all the variables are chr we can't have them like this to perform eda
df_2023_final <- df_2023_final %>%
  mutate(
    I_Modelo_de_gestão = factor(I_Modelo_de_gestão),
    V_Modelo_de_gestão = factor(V_Modelo_de_gestão),
    V_Tipologia_da_área_de_intervenção = factor(V_Tipologia_da_área_de_intervenção),
    Tipo_Sistema = factor(Tipo_Sistema, levels = c("Baixa", "Alta"))
  )

exclude_cols <- c(
  "Empresa", "Tipo_Sistema", 
  "I_Identificação_da_entidade_gestora", "I_Modelo_de_gestão",
  "V_Identificação_da_entidade_gestora", "V_Modelo_de_gestão",
  "V_Tipologia_da_área_de_intervenção"
)

df_2023_final <- df_2023_final %>%
  mutate(
    across(
      .cols = !all_of(exclude_cols),
      ~as.numeric(replace(., . %in% c("NR", "NA"), NA))
    )
  )
df_2023_final <- df_2023_final %>%
  mutate(
    NRW_class = case_when(
      Tipo_Sistema == "Alta"   & I_Água_não_faturada >= 0   & I_Água_não_faturada <= 5.0   ~ "Good",
      Tipo_Sistema == "Alta"   & I_Água_não_faturada > 5.0  & I_Água_não_faturada <= 7.5   ~ "Medium",
      Tipo_Sistema == "Alta"   & I_Água_não_faturada > 7.5                          ~ "Unsatisfactory",
      Tipo_Sistema == "Baixa" & I_Água_não_faturada >= 0   & I_Água_não_faturada <= 20.0  ~ "Good",
      Tipo_Sistema == "Baixa" & I_Água_não_faturada > 20.0 & I_Água_não_faturada <= 30.0  ~ "Medium",
      Tipo_Sistema == "Baixa" & I_Água_não_faturada > 30.0                         ~ "Unsatisfactory",
      TRUE ~ NA_character_
    )
  )

df_2023_final <- df_2023_final %>%
  mutate(
    RL_class = case_when(
      Tipo_Sistema == "Alta"   & I_Perdas_reais_de_água >= 0   & I_Perdas_reais_de_água <= 5.0   ~ "Good",
      Tipo_Sistema == "Alta"   & I_Perdas_reais_de_água > 5.0  & I_Perdas_reais_de_água <= 7.5   ~ "Medium",
      Tipo_Sistema == "Alta"   & I_Perdas_reais_de_água > 7.5                          ~ "Unsatisfactory",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais < 20 & I_Perdas_reais_de_água >= 0   & I_Perdas_reais_de_água <= 3.0  ~ "Good",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais < 20 & I_Perdas_reais_de_água > 3.0 & I_Perdas_reais_de_água <= 5.0  ~ "Medium",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais < 20 & I_Perdas_reais_de_água > 5.0 ~ "Unsatisfactory",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais >= 20 & I_Perdas_reais_de_água >= 0   & I_Perdas_reais_de_água <= 100.0  ~ "Good",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais >= 20 & I_Perdas_reais_de_água > 100.0 & I_Perdas_reais_de_água <= 150.0  ~ "Medium",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais >= 20 & I_Perdas_reais_de_água > 150.0  ~ "Unsatisfactory",
      TRUE ~ NA_character_
    )
  )
#Remove NA and check how many WU were lost in the process
nrow_before <- nrow(df_2023_final)
df_2023_final <- df_2023_final %>%
  filter(!is.na(I_Água_não_faturada), !is.na(I_Perdas_reais_de_água))
nrow_after <- nrow(df_2023_final)

cat("Removed", nrow_before - nrow_after, "rows with missing NRW or RL values.\n")
#repeat for 2020
data_2020_I <- read_excel("~/MMA/MEDM/Dados_2020_filtered.xlsx", sheet = "Indicadores_2020")
data_2020_I <- data_2020_I %>%
  mutate(Indicador = paste0("I_", Indicador))
data_2020_V <- read_excel("~/MMA/MEDM/Dados_2020_filtered.xlsx", sheet = "Dados_2020")
data_2020_V <- data_2020_V %>%
  mutate(Variavel = paste0("V_", Variavel))

indicators_wide_2020 <- data_2020_I %>%
  select(Empresa, TipoSistema, Indicador, Valor) %>%
  pivot_wider(names_from = Indicador, values_from = Valor, id_cols = c(Empresa, TipoSistema))

variables_wide_2020 <- data_2020_V %>%
  select(Empresa, TipoSistema, Variavel, Valor) %>%
  pivot_wider(names_from = Variavel, values_from = Valor, id_cols = c(Empresa, TipoSistema))

df_2020_final <- indicators_wide_2020 %>%
  left_join(variables_wide_2020, by = c("Empresa", "TipoSistema"))
#desnecessário para 2020 apenas útil para EDA
# Replace spaces with underscores in column names
names(df_2020_final)[names(df_2020_final) == "TipoSistema"] <- "Tipo Sistema"
names(df_2020_final) <- gsub(" ", "_", names(df_2020_final))

#all the variables are chr we can't have them like this to perform eda
df_2020_final <- df_2020_final %>%
  mutate(
    I_Modelo_de_gestão = factor(I_Modelo_de_gestão),
    V_Modelo_de_gestão = factor(V_Modelo_de_gestão),
    V_Tipologia_da_área_de_intervenção = factor(V_Tipologia_da_área_de_intervenção),
    Tipo_Sistema = factor(Tipo_Sistema, levels = c("Baixa", "Alta"))
  )

exclude_cols <- c(
  "Empresa", "Tipo_Sistema", 
  "I_Identificação_da_entidade_gestora", "I_Modelo_de_gestão",
  "V_Identificação_da_entidade_gestora", "V_Modelo_de_gestão",
  "V_Tipologia_da_área_de_intervenção"
)

df_2020_final <- df_2020_final %>%
  mutate(
    across(
      .cols = !all_of(exclude_cols),
      ~as.numeric(replace(., . %in% c("NR", "NA"), NA))
    )
  )
df_2020_final <- df_2020_final %>%
  mutate(
    NRW_class = case_when(
      Tipo_Sistema == "Alta"   & I_Água_não_faturada >= 0   & I_Água_não_faturada <= 5.0   ~ "Good",
      Tipo_Sistema == "Alta"   & I_Água_não_faturada > 5.0  & I_Água_não_faturada <= 7.5   ~ "Medium",
      Tipo_Sistema == "Alta"   & I_Água_não_faturada > 7.5                          ~ "Unsatisfactory",
      Tipo_Sistema == "Baixa" & I_Água_não_faturada >= 0   & I_Água_não_faturada <= 20.0  ~ "Good",
      Tipo_Sistema == "Baixa" & I_Água_não_faturada > 20.0 & I_Água_não_faturada <= 30.0  ~ "Medium",
      Tipo_Sistema == "Baixa" & I_Água_não_faturada > 30.0                         ~ "Unsatisfactory",
      TRUE ~ NA_character_
    )
  )

df_2020_final <- df_2020_final %>%
  mutate(
    RL_class = case_when(
      Tipo_Sistema == "Alta"   & I_Perdas_reais_de_água >= 0   & I_Perdas_reais_de_água <= 5.0   ~ "Good",
      Tipo_Sistema == "Alta"   & I_Perdas_reais_de_água > 5.0  & I_Perdas_reais_de_água <= 7.5   ~ "Medium",
      Tipo_Sistema == "Alta"   & I_Perdas_reais_de_água > 7.5                          ~ "Unsatisfactory",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais < 20 & I_Perdas_reais_de_água >= 0   & I_Perdas_reais_de_água <= 3.0  ~ "Good",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais < 20 & I_Perdas_reais_de_água > 3.0 & I_Perdas_reais_de_água <= 5.0  ~ "Medium",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais < 20 & I_Perdas_reais_de_água > 5.0 ~ "Unsatisfactory",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais >= 20 & I_Perdas_reais_de_água >= 0   & I_Perdas_reais_de_água <= 100.0  ~ "Good",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais >= 20 & I_Perdas_reais_de_água > 100.0 & I_Perdas_reais_de_água <= 150.0  ~ "Medium",
      Tipo_Sistema == "Baixa" & I_Densidade_de_ramais >= 20 & I_Perdas_reais_de_água > 150.0  ~ "Unsatisfactory",
      TRUE ~ NA_character_
    )
  )

nrow_before <- nrow(df_2020_final)
df_2020_final <- df_2020_final %>%
  filter(!is.na(I_Água_não_faturada), !is.na(I_Perdas_reais_de_água))
nrow_after <- nrow(df_2020_final)

cat("Removed", nrow_before - nrow_after, "rows with missing NRW or RL values.\n")

#--------------------------------------------EDA-------------------------------------------------#
str(df_2023_final)
df_2023_final <- df_2023_final %>%
  mutate(
    NRW_class = factor(NRW_class, levels = c("Unsatisfactory", "Medium", "Good"), ordered = TRUE),
    RL_class  = factor(RL_class, levels = c("Unsatisfactory", "Medium", "Good"), ordered = TRUE)
  )

#check for missing values in columns
missing_summary <- df_2023_final %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%        # count NAs per column
  pivot_longer(everything(), names_to = "Column", values_to = "NA_count") %>%
  mutate(
    Total = nrow(df_2023_final),                              # total number of rows
    NA_pct = (NA_count / Total) * 100                         # percent missing
  ) %>%
  arrange(desc(NA_pct))                                       # sort descending by missing %

# View the summary
print(missing_summary, n=35)
#remove columns with a lot of NA's
cols_to_drop <- missing_summary %>%
  filter(NA_pct > 60) %>%
  pull(Column)

df_2023_final <- df_2023_final %>%
  select(-all_of(cols_to_drop))

#check for missing values in rows: Is there WU with a lot of missing values?
row_missing_summary <- df_2023_final %>%
  rowwise() %>%
  summarise(
    Empresa = Empresa,
    Tipo_Sistema = Tipo_Sistema,
    NA_count = sum(is.na(c_across(where(is.numeric)))),
    Total = length(c_across(where(is.numeric))),
    NA_pct = (NA_count / Total) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(NA_pct))

print(row_missing_summary, n = 125)                            

#summary and median imputation in numeric variables
summary(df_2023_final)
df_2023_final <- df_2023_final %>%
  group_by(Tipo_Sistema) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
    )
  ) %>%
  ungroup()
summary(df_2023_final) #there are only 10 alta and there are structural na's

df_2020_final <- df_2020_final %>%
  mutate(
    NRW_class = factor(NRW_class, levels = c("Unsatisfactory", "Medium", "Good"), ordered = TRUE),
    RL_class  = factor(RL_class, levels = c("Unsatisfactory", "Medium", "Good"), ordered = TRUE)
  )
#1Houve uma melhoria nos classificadores?
summary(df_2020_final$NRW_class)
summary(df_2023_final$NRW_class)
summary(df_2020_final$RL_class) #3 empresas sem densidade de ramais
df_2020_final <- df_2020_final %>%
  filter(!is.na(RL_class))
#before proceed check NA in 2020
missing_summary1 <- df_2020_final %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%        # count NAs per column
  pivot_longer(everything(), names_to = "Column", values_to = "NA_count") %>%
  mutate(
    Total = nrow(df_2020_final),                              # total number of rows
    NA_pct = (NA_count / Total) * 100                         # percent missing
  ) %>%
  arrange(desc(NA_pct))                                       # sort descending by missing %

# View the summary
print(missing_summary1, n=35)
cols_to_drop1 <- missing_summary1 %>%
  filter(NA_pct > 60) %>%
  pull(Column)

df_2020_final <- df_2020_final %>%
  select(-all_of(cols_to_drop1)) 

row_missing_summary1 <- df_2020_final %>%
  rowwise() %>%
  summarise(
    Empresa = Empresa,
    Tipo_Sistema = Tipo_Sistema,
    NA_count = sum(is.na(c_across(where(is.numeric)))),
    Total = length(c_across(where(is.numeric))),
    NA_pct = (NA_count / Total) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(NA_pct))

print(row_missing_summary1, n = 105) 
#same imputation in 2020
summary(df_2020_final)

df_2020_final <- df_2020_final %>%
  group_by(Tipo_Sistema) %>% # optional: impute separately for Alta/Baixa
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
    )
  ) %>%
  ungroup()
summary(df_2020_final) #there are only 10 alta, #novamente há muitos NA's nas variáveis relacionadas com € e a mediana pode ou não alterar

write_xlsx(df_2023_final, "df_2023_final.xlsx")
write_xlsx(df_2020_final, "df_2020_final.xlsx")
#1.Como estão distribuídas as variáveis de resposta? Existem outliers ou valores extremos?
tapply(df_2023_final[c("I_Água_não_faturada","I_Perdas_reais_de_água")],df_2023_final$Tipo_Sistema, summary)

ggplot(df_2023_final, aes(x = I_Perdas_reais_de_água)) +
  geom_histogram(bins = round(1+log2(nrow(df_2023_final))), fill = "steelblue", color="black") +
  facet_wrap(~Tipo_Sistema, scales = "free_y") +
  labs(x = "Real Water losses(L/ramal·dia)", y = "Nº WU", title = "Distribution of Real losses per system type")+
  theme_minimal()
#cauda direita acentuada para baixa um valor extremo: INFRATROIA - existem falhas no abastecimento e avarias em condutas mas não há reabilitação
ggplot(df_2023_final, aes(x = I_Água_não_faturada)) +
  geom_histogram( bins = round(1+log2(nrow(df_2023_final))),fill = "steelblue", color="black") +
  facet_wrap(~Tipo_Sistema, scales = "free_y") +
  labs(x = "Non Revenue Water(%)", y = "Nº WU", title = "Distribution of Non Revenue Water per system type")+
  theme_minimal()
#para baixa está bastante normalizada para alta temos poucas observações (10) e uma delas foge à regra: ICOVI deve-se maioritariamente ao V_Consumo não faturado medido 

#outliers
ggplot(df_2023_final, aes(x = Tipo_Sistema, y = I_Perdas_reais_de_água)) +
  geom_boxplot(fill="steelblue", alpha=0.7) + 
  geom_text_repel(
    data = df_2023_final %>%
      group_by(Tipo_Sistema) %>%
      mutate(
        Q1 = quantile(I_Perdas_reais_de_água, 0.25, na.rm = TRUE),
        Q3 = quantile(I_Perdas_reais_de_água, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1
      ) %>%
      filter(I_Perdas_reais_de_água > (Q3 + 1.5 * IQR) |
               I_Perdas_reais_de_água < (Q1 - 1.5 * IQR)),
    aes(label = Empresa),
    size = 3,
    color = "blue"
  ) +
  labs(
    x = "Type of System",
    y = "Real Water losses (L/ramal·dia)",
    title = "Real water losses boxplot"
  ) +
  theme_minimal()
#para baixa são identificados 5 outliers: INFRATROIA já visto; CM de Castelo de Paiva - avarias em condutas; SMAS de Almada - pouca reabilitação?; SIMAR de Loures e Odivelas - problema de reabilitação?; CM de Lagos - muitos ramais afetados e pouca reabilitação
#para alta é identificado 1 outlier: EPAL - parece ser por não existir reabilitação

ggplot(df_2023_final, aes(x = Tipo_Sistema, y = I_Água_não_faturada)) +
  geom_boxplot(fill="steelblue", alpha=0.7) + 
  geom_text_repel(
    data = df_2023_final %>%
      group_by(Tipo_Sistema) %>%
      mutate(
        Q1 = quantile(I_Água_não_faturada, 0.25, na.rm = TRUE),
        Q3 = quantile(I_Água_não_faturada, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1
      ) %>%
      filter(I_Água_não_faturada > (Q3 + 1.5 * IQR) |
               I_Água_não_faturada < (Q1 - 1.5 * IQR)),
    aes(label = Empresa),
    size = 3,
    color = "blue"
  ) +
  labs(
    x = "Type of System",
    y = "Non Revenue Water(%)",
    title = "Non Revenue Water boxplot"
  ) +
  theme_minimal()
#identificado 1 outlier para baixa: CM de Vila Nova de Cerveira - perdas reais de água
#identificado o mesmo outlier ICOVI

#Comparação com 2020
#juntar e criar uma variável para ano. Vamos analisar aquilo que existe nos dois datasets Empresa e Colunas
common_cols <- intersect(names(df_2020_final), names(df_2023_final))
common_empresas <- intersect(df_2020_final$Empresa, df_2023_final$Empresa)

df_common <- bind_rows(
  df_2020_final %>%
    filter(Empresa %in% common_empresas) %>%
    select(all_of(common_cols)) %>%
    mutate(Year = 2020),
  
  df_2023_final %>%
    filter(Empresa %in% common_empresas) %>%
    select(all_of(common_cols)) %>%
    mutate(Year = 2023)
)
df_outliers <- df_common %>%
  group_by(Year, Tipo_Sistema) %>%
  mutate(
    Q1 = quantile(I_Água_não_faturada, 0.25),
    Q3 = quantile(I_Água_não_faturada, 0.75),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    is_outlier = I_Água_não_faturada < lower | I_Água_não_faturada > upper
  ) %>%
  filter(is_outlier)
ggplot(df_common, aes(x = as.factor(Year), y = I_Água_não_faturada, fill = as.factor(Year))) +
  geom_boxplot() +
  facet_wrap(~Tipo_Sistema) +
  geom_text_repel(
    data = df_outliers,
    aes(label = Empresa),  # replace with your column for names
    color = "red",
    nudge_x = 0.2,
    size = 3
  ) +
  theme_bw() +
  labs(y = "Non Revenue Water", x = "Year", title="Evolution of Non Revenue Water boxplots")+
  theme(legend.position = "none")

#parece-me igual, os outliers são os mesmos
ggplot(df_common, aes(x = as.factor(Year), y = I_Perdas_reais_de_água, fill = as.factor(Year))) +
  geom_boxplot() +
  facet_wrap(~Tipo_Sistema)+
  labs(y="Real losses", x="Year", title="Evolution of Real losses boxplots")+
  theme_bw()+
  theme(legend.position = "none")
#alguém piorou bastante (INFRATRÓIA)! De resto não aparentam grandes mudanças

#2. Empresas em 2023 com %altas de NRW também têm mais perdas de água? Se não de onde vêm esses valores?
#3. NRW e RL mudaram? Diferenças nos Indicadores?
ggplot(df_2023_final)+
  geom_point(aes(x=I_Perdas_reais_de_água,y=I_Água_não_faturada, color=Tipo_Sistema), alpha=0.5)+
  scale_x_log10()+ 
  labs(x="Real losses (L/ramal.dia)", y="Non Revenue Water (%)", title="Relationship between NRW and real losses")+#because of the right tail
  theme_minimal()+
  theme(legend.position = "top")
df_2023_final %>%
  group_by(Tipo_Sistema) %>%
  summarise(
    cor = cor(I_Perdas_reais_de_água, I_Água_não_faturada, use = "complete.obs")
  )#alta tem poucas observações resultados têm que ser analisados com cuidado
  #empresa pode perder muita água mas receber pouca em comparação com as outras logo ao ínicio ou ser uma empresa mais pequena e portanto I_Perdas_reais_de_água vai ser baixo quando comparado com outras
#enquanto que I_Água_não_faturada é uma %, I_Perdas_reais_de_água depende da estrutura(ramais e densidade) e tamanho(comprimento de condutas) 
#em vez de usar I_Perdas_reais_de_água (l/(ramal ou km.dia)) usamos uma %: quanta água entrada no sistema é que se perde realmente?
df_plot <- df_2023_final %>%
  mutate(
    perc_RL = 100 * V_Perdas_reais / V_Água_entrada_no_sistema,
    diff = I_Água_não_faturada - perc_RL,
    label = ifelse(diff >= 20, Empresa, NA),
    highlight = diff >= 20  # TRUE/FALSE
  ) #pelo menos 80% de I_Água_não_faturada vem de perdas reais

ggplot(df_plot, aes(x = perc_RL, y = I_Água_não_faturada)) +
  # all points
  geom_point(
    aes(color = Tipo_Sistema),
    alpha = 0.5
  ) +
  # highlight points: add a visible shape/border
  geom_point(
    data = subset(df_plot, highlight),
    aes(color = Tipo_Sistema),
    shape = 21, stroke = 1.2, fill = "white"
  ) +
  # add labels
  geom_text_repel(
    data = subset(df_plot, highlight),
    aes(label = label),
    size = 3,
    max.overlaps = 20
  ) +
  # dashed diagonal line: quanto mais longe menos NRW se deve a perdas reais
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(
    x = "Real losses (% of input water)",
    y = "Non-revenue water (%)",
    title = "Share of Real Losses in NRW by System Type"
  )
#em 193 empresas pelo menos 80% da água não faturada vem de perdas reais
#apenas 1 alta aparenta ter valores altos de NRW e que não vêm de perdas reais - já vimos que vêm de consumo não faturado medido
#12 baixas têm valores altos de NRW que não são completamente explicados por perdas reais: portanto podem vir de perdas aparentes ou de consumo autorizado e não faturado
#CM de Évora: 73% vem de consumo não faturado; 14% erros de medição;13% perdas reais - RL_class: Good
#CM de Barreiro: 44% vem de consumo não faturado;11% erros de medição;5% uso não autorizado; 40% perdas reais - RL_Class:Medium
#CM de Vila Velha de Ródão: 50% vem de consumo não faturado; 10% erros de medição;40% perdas reais - RL_Class:Good 
#CM de Moita: 45% vem de consumo não faturado; 8% erros de medição; 47% perdas reais: RL_Class: Unsatisfactory
#CM de Pombal: 33% vem de consumo não faturado;8% uso não autorizado; 7% erros de medição; 52% perdas reais - RL_Class: Good 
#CM de Macedo de Cavaleiros: 28% vem de consumo não faturado; 18% não autorizado; 6% erros de medição; 48% perdas reais - RL_Class: Medium
#CM de Golegã: 61% vem de consumo não faturado;4% erros de medição; 34% perdas reais - RL_Class: Medium
#CM de Barrancos: 47% vem de consumo não faturado; 3% erros de medição; 50% perdas reais - RL_Class: Good
#CM de Anadia: 44% vem de consumo não faturado;4% erros de medição; 51% perdas reais - RL_Class: Unsatisfactory
#CM de Ponte de Barca: 34% vem de consumo não faturado; 5% erros de medição; 60% perdas reais - RL_Class: Medium
#CM de Boticas: 33% vem de consumo não faturado; 4% erros de medição; 63% perdas reais - RL_Class: Good
#CM de Miranda do Douro: 31% vem de consumo não faturado; 3% erros de medição; 66% perdas reais - RL_Class: Unsatisfactory
a<-c("CM de Évora", "CM de Barreiro","CM de Vila Velha de Ródão","CM de Moita", "CM de Pombal", "CM de Macedo de Cavaleiros", "CM de Golegã","CM de Barrancos", "CM de Anadia","CM de Ponte da Barca","CM de Boticas", "CM de Miranda do Douro")
ggplot(subset(df_2023_final, Empresa %in% a), aes(x=(V_Perdas_de_água_por_erros_de_medição/V_Água_entrada_no_sistema)*100,y=I_Água_não_faturada))+
  geom_point()+
  geom_text_repel(aes(label = Empresa))
ggplot(subset(df_2023_final, Empresa %in% a), aes(x=((V_Consumo_não_faturado_medido+V_Consumo_não_faturado_não_medido)/V_Água_entrada_no_sistema)*100,y=I_Água_não_faturada))+
  geom_point()+
  geom_text_repel(aes(label = Empresa))
ggplot(subset(df_2023_final, Empresa %in% a), aes(x=V_Consumo_não_faturado_medido+V_Consumo_não_faturado_não_medido+V_Perdas_de_água_por_erros_de_medição,y=V_Água_não_faturada))+
  geom_point()+
  geom_text_repel(aes(label = Empresa), size=3)
ggplot(subset(df_2023_final, Empresa %in% a), aes(x=V_Perdas_de_água_por_erros_de_medição,y=V_Água_não_faturada))+
  geom_point()+
  geom_text_repel(aes(label = Empresa), size=3)
#tratar tudo como % para o tamanho da wu não influenciar
df_long <- df_2023_final %>%
  filter(Empresa %in% a) %>%
  mutate(
    Consumo_nao_faturado = V_Consumo_não_faturado_medido + V_Consumo_não_faturado_não_medido,
    Perdas_Aparentes = V_Uso_não_autorizado + V_Perdas_de_água_por_erros_de_medição,
    Perdas_Reais = V_Perdas_reais
  ) %>%
  select(Empresa, Consumo_nao_faturado, Perdas_Aparentes, Perdas_Reais) %>%
  pivot_longer(
    cols = c(Consumo_nao_faturado, Perdas_Aparentes, Perdas_Reais),
    names_to = "Tipo",
    values_to = "Valor"
  ) %>%
  group_by(Empresa) %>%
  mutate(Percent = Valor / sum(Valor) * 100)
ggplot(df_long, aes(x = Empresa, y = Valor, fill = Tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 2) +
  labs(x = "WU", y = "Volume", fill = "Tipo", title="Distribution of NRW") +
  scale_fill_manual(
    values = c("Consumo_nao_faturado" = "coral", "Perdas_Aparentes" = "deepskyblue2", "Perdas_Reais" = "limegreen"),
    labels = c("Unbilled authorized consumption",
               "Apparent losses",
               "Real losses")
  ) +
  theme_minimal() +
  theme(legend.position="top",axis.text.x = element_text(angle = 35, hjust = 1))
#apenas 4 têm consumo não faturado autorizado superior

df_2023_final %>%
  group_by(Tipo_Sistema) %>%
  summarise(
    cor = cor((V_Perdas_reais/V_Água_entrada_no_sistema)*100, I_Água_não_faturada, use = "complete.obs")
  )
#uma empresa pode ter muita água não faturada vinda de perdas reais e mesmo assim tendo em conta o tamanho da sua rede e a densidade de ramais ser eficiente no que toca a perdas de água
#já vimos que a água não faturada provém na maioria das empresas de perdas reais (apenas 4 baixas e 1 alta têm o consumo não faturado como factor dominante)
df_compare_nrw <- df_common %>%
  select(Empresa, Year, Tipo_Sistema, I_Água_não_faturada) %>%
  pivot_wider(names_from = Year, values_from = I_Água_não_faturada) %>%
  mutate(
    Change = `2023` - `2020`,
    Direction = case_when(
      Change > 0 ~ "Worse NRW",
      Change < 0 ~ "Better NRW",
      TRUE ~ "No change"
    ),
    highlight = abs(Change) >= 15,
    label = ifelse(abs(Change) >= 15, Empresa, NA),
  )
df_compare_nrw %>%
  group_by(Tipo_Sistema, Direction) %>%
  summarise(n = n()) %>%
  mutate(Percent = n / sum(n) * 100)
#houve ligeiramente mais a melhorar para baixa
ggplot(df_compare_nrw, aes(x = `2020`, y = `2023`, color = Tipo_Sistema)) +
  geom_point(alpha = 0.7) +
  geom_point(
    data = subset(df_compare_nrw, highlight),
    shape = 22, stroke = 1.2, fill = "white",
    show.legend = FALSE
  )+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "2020 NRW (%)",
    y = "2023 NRW (%)",
    title = "Change in Non-Revenue Water (2020 vs 2023)"
  ) +
  theme_minimal()

print(df_compare_nrw %>%
        filter(highlight) %>%
        select(Empresa, Tipo_Sistema, Change) %>%
        arrange(desc(abs(Change))), n=30)

#2.1 WU que tiveram grandes variações de NRW também tiveram na sua composição? Foi porque melhoraram as perdas reais que a %NRW desceu?
#a diferença na % pode vir: perdas reais, perdas aparentes, consumo não faturado
df_long_2020 <- df_2020_final %>%
  mutate(
    Year = 2020,
    Consumo_nao_faturado = V_Consumo_não_faturado_medido + V_Consumo_não_faturado_não_medido,
    Perdas_Aparentes = V_Uso_não_autorizado + V_Perdas_de_água_por_erros_de_medição,
    Perdas_Reais = V_Perdas_reais
  ) %>%
  select(Empresa, Year, Consumo_nao_faturado, Perdas_Aparentes, Perdas_Reais)

df_long_2023 <- df_2023_final %>%
  mutate(
    Year = 2023,
    Consumo_nao_faturado = V_Consumo_não_faturado_medido + V_Consumo_não_faturado_não_medido,
    Perdas_Aparentes = V_Uso_não_autorizado + V_Perdas_de_água_por_erros_de_medição,
    Perdas_Reais = V_Perdas_reais
  ) %>%
  select(Empresa, Year, Consumo_nao_faturado, Perdas_Aparentes, Perdas_Reais)
df_long_both <- bind_rows(df_long_2020, df_long_2023)
# Add direction info (Better/Worse) to the NRW composition dataset
df_long_both <- df_long_both %>%
  left_join(df_compare_nrw %>% select(Empresa, Direction), by = "Empresa")

highlighted_WU <- df_compare_nrw %>%
  filter(highlight) %>%
  pull(Empresa)

df_long_both <- df_long_both %>%
  filter(Empresa %in% highlighted_WU)
df_long_both <- df_long_both %>%
  pivot_longer(cols = c(Consumo_nao_faturado, Perdas_Aparentes, Perdas_Reais),
               names_to = "Tipo", values_to = "Valor") %>%
  group_by(Empresa, Year) %>%
  mutate(Percent = Valor / sum(Valor) * 100)

# Plot for WUs that improved (↓ NRW)
ggplot(subset(df_long_both, Direction == "Better NRW"),
       aes(x = Empresa, y = Percent, fill = Tipo)) +
  geom_bar(stat = "identity", position = "stack", width=0.6) +
  facet_wrap(~ Year, ncol = 1, labeller = label_both) +
  labs(
    x = "Water Utility",
    y = "Share of NRW (%)",
    fill = "NRW",
    title = "WU with Improved NRW (2020 vs 2023)"
  ) +
  scale_fill_manual(
    values = c("Consumo_nao_faturado" = "coral", "Perdas_Aparentes" = "limegreen", "Perdas_Reais" = "deepskyblue3"),
    labels = c("Unbilled authorized consumption",
               "Apparent losses",
               "Real losses")
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 25, hjust = 1, size = 9)
  )
#aqueles que melhoram NRW melhoram tanto RL como CANF
# Plot for WUs that worsened (↑ NRW)
ggplot(subset(df_long_both, Direction == "Worse NRW"),
       aes(x = Empresa, y = Percent, fill = Tipo)) +
  geom_bar(stat = "identity", position = "stack", width=0.7) +
  facet_wrap(~ Year, ncol = 1, labeller = label_both) +
  labs(
    x = "Water Utility",
    y = "Share of NRW (%)",
    fill = "NRW",
    title = "WU with Worsened NRW (2020 vs 2023)"
  ) +
  scale_fill_manual(
    values = c("Consumo_nao_faturado" = "coral", "Perdas_Aparentes" = "limegreen", "Perdas_Reais" = "deepskyblue3"),
    labels = c("Unbilled authorized consumption",
               "Apparent losses",
               "Real losses")
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 25, hjust = 1, size = 9)
  )
#aqueles que pioram a NRW também pioram RL
#Portanto perdas reais e consumo não faturado é o que altera. Será que reabilitação e propriedades da rede da WU também interferem?
vars_for_boxplots <- c(
  "I_Reabilitação_de_condutas",
  "I_Ocorrência_de_falhas_no_abastecimento",
  "I_Ocorrência_de_avarias_em_condutas",
  "V_Comprimento_médio_de_condutas_com_mais_de_10_anos",
  "V_Ramais_de_ligação",
  "V_Perdas_reais",
  "V_Água_faturada",
  "V_Perdas_reais_ratio",
  "V_Água_faturada_ratio",
  "V_Outros_componentes_ratio",
  "V_Rendimentos_tarifários"
)

#Testar também rácios: apenas o volume de água é influenciado pelo tamanho da empresa
df_2023_final <- df_2023_final %>%
  mutate(
    V_Perdas_reais_ratio = V_Perdas_reais / V_Água_entrada_no_sistema,
    V_Água_faturada_ratio = V_Água_faturada / V_Água_entrada_no_sistema,
    V_Outros_componentes_ratio = (
      V_Consumo_não_faturado_não_medido +
        V_Consumo_não_faturado_medido +
        V_Perdas_de_água_por_erros_de_medição +
        V_Uso_não_autorizado
    ) / V_Água_entrada_no_sistema
  )
variable_labels <- c(
  V_Água_faturada_ratio = "Ratio Revenue Water",
  I_Reabilitação_de_condutas = "Pipe Rehabilitation",
  V_Perdas_reais_ratio = "Real losses ratio",
  V_Rendimentos_tarifários = "Financial component",
  V_Comprimento_médio_de_condutas_com_mais_de_10_anos = "Average length of pipes\nwith >10y ",
  V_Outros_componentes_ratio = "Unbilled authorized and\nApparent losses ratio",
  V_Água_faturada = "Revenue Water",
  I_Ocorrência_de_avarias_em_condutas = "Incidence of Pipe Breaks",
  I_Ocorrência_de_falhas_no_abastecimento = "Incidence of Failures",
  V_Ramais_de_ligação = "Nº service connections",
  V_Perdas_reais = "Real Losses"
)
# Reshape to long format
df_long <- df_2023_final %>%
  select(NRW_class, Tipo_Sistema, all_of(vars_for_boxplots)) %>%
  pivot_longer(
    cols = all_of(vars_for_boxplots),
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(df_long %>%
         filter(Variable %in% c("V_Perdas_reais","V_Água_faturada",
                                "V_Perdas_reais_ratio","V_Água_faturada_ratio",
                                "V_Outros_componentes_ratio","V_Rendimentos_tarifários"),
                Tipo_Sistema == "Baixa"),
       aes(x = NRW_class, y = Value, fill = NRW_class)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y",labeller = labeller(Variable = variable_labels)) +
  scale_fill_manual(values = c("Unsatisfactory" = "firebrick1",
                               "Medium" = "deepskyblue",
                               "Good" = "springgreen")) +
  labs(title = "Retail systems – Volume variables by NRW class",
       x = "NRW class", y = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 35, hjust = 1))
ggplot(df_long %>%
         filter(Variable %in% c("I_Reabilitação_de_condutas",
                                "I_Ocorrência_de_falhas_no_abastecimento",
                                "I_Ocorrência_de_avarias_em_condutas",
                                "V_Comprimento_médio_de_condutas_com_mais_de_10_anos",
                                "V_Ramais_de_ligação"),
                Tipo_Sistema == "Baixa"),
       aes(x = NRW_class, y = Value, fill = NRW_class)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y",labeller = labeller(Variable = variable_labels)) +
  scale_fill_manual(values = c("Unsatisfactory" = "firebrick1",
                               "Medium" = "deepskyblue",
                               "Good" = "springgreen")) +
  labs(title = "Retail systems – Network structure and rehabilitation variables by NRW class",
       x = "NRW class", y = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_long %>%
         filter(Variable %in% c("V_Perdas_reais","V_Água_faturada",
                                "V_Perdas_reais_ratio","V_Água_faturada_ratio",
                                "V_Outros_componentes_ratio","V_Rendimentos_tarifários"),
                Tipo_Sistema == "Alta"),
       aes(x = NRW_class, y = Value, fill = NRW_class)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y",labeller = labeller(Variable = variable_labels)) +
  scale_fill_manual(values = c("Unsatisfactory" = "firebrick1",
                               "Medium" = "deepskyblue",
                               "Good" = "springgreen")) +
  labs(title = "Bulk systems – Volume variables by NRW class",
       x = "NRW class", y = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 35, hjust = 1))
ggplot(df_long %>%
         filter(Variable %in% c("I_Reabilitação_de_condutas",
                                "I_Ocorrência_de_falhas_no_abastecimento",
                                "I_Ocorrência_de_avarias_em_condutas",
                                "V_Comprimento_médio_de_condutas_com_mais_de_10_anos"),
                Tipo_Sistema == "Alta"),
       aes(x = NRW_class, y = Value, fill = NRW_class)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y",labeller = labeller(Variable = variable_labels)) +
  scale_fill_manual(values = c("Unsatisfactory" = "firebrick1",
                               "Medium" = "deepskyblue",
                               "Good" = "springgreen")) +
  labs(title = "Bulk systems – Network structure and rehabilitation variables by NRW class",
       x = "NRW class", y = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))
#Alta: poucas observações para cada classe
#Baixa: sobreposição da maioria dos boxplots: ninguém faz reabilitação e toda a gente sofre de falhas;
#Os rácios parecem ajudar mais que os volumes em si; 
#o rácio rl+unbilled authorized é muito pequeno para a maioria das empresas e portanto é o que ajuda menos
#Rendimentos parece ajudar a distinguir insatisfatório de médio
ggplot(df_common, aes(x = as.factor(Year), fill = NRW_class)) +
  geom_bar(position = "dodge") +
  labs(y = "Proportion", title = "Change in Real Losses classification (2020–2023)")+
  facet_wrap(~Tipo_Sistema)
#insatisfatório subiu e o good também na baixa. Na alta muitos do insatisfatório passaram para medium

#3.Avaliação das WU que apresentaram uma grande variação de RL entre 2020 e 2023
df_compare_rl <- df_common %>%
  select(Empresa, Year, Tipo_Sistema, I_Perdas_reais_de_água) %>%
  pivot_wider(names_from = Year, values_from = I_Perdas_reais_de_água) %>%
  mutate(
    Change = `2023` - `2020`,
    Direction = case_when(
      Change > 0 ~ "Worse RL",
      Change < 0 ~ "Better RL",
      TRUE ~ "No change"
    ),
    highlight = abs(Change) >= 75,
    label = ifelse(abs(Change) >= 75, Empresa, NA),
  )
df_compare_rl %>%
  group_by(Tipo_Sistema, Direction) %>%
  summarise(n = n()) %>%
  mutate(Percent = n / sum(n) * 100)
#novamente ligeiramente mais wu que melhoraram
ggplot(df_compare_rl, aes(x = `2020`, y = `2023`, color = Tipo_Sistema)) +
  geom_point(alpha = 0.7) +
  geom_point(
    data = subset(df_compare_rl, highlight),
    shape = 22, stroke = 1.2, fill = "white",
    show.legend = FALSE
  )+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "2020 RL",
    y = "2023 RL",
    title = "Change in Real Losses (2020 vs 2023)"
  ) +
  theme_minimal()

print(df_compare_rl %>%
        filter(highlight) %>%
        select(Empresa, Tipo_Sistema, Change) %>%
        arrange(desc(abs(Change))),n=40)
#WU do tipo baixa identificadas
selected_wu <- c(
  "CM de Anadia", "CM de Macedo de Cavaleiros", "CM de Serpa",
  "CM de São Brás de Alportel", "CM de Moimenta da Beira", "CM de Santiago do Cacém",
  "CM de Mangualde", "CM de Vale de Cambra", "CM de Moura", "CM de Sines",
  "CM de Sobral de Monte Agraço", "CM de Alvito", "CM de Amares", "CM de Chaves",
  "SMAS de Almada", "AMBIOLHÃO", "CM de Lagoa", "CM de Condeixa-a-Nova",
  "CM de Entroncamento", "CM de Silves", "CM de Valpaços", "CM de Seixal", "SIMAS de Oeiras e Amadora",
  "CM de Vila Nova de Foz Coa", "SM de Abrantes", "CM de Lousada", "CM de Ponte da Barca",
  "CM de Loulé", "CM de Moita", "CM de Proença-a-Nova", "CM de Castro Marim","CM de Oleiros", "CM de Melgaço","CM de Tabuaço", "CM de Miranda do Douro" 
)#removed INFRATROIA por ser alta e ter um range de valores diferentes: ocorrências tanto de falhas como condutas dispararam mas reabilitação está a zeros

df_selected <- df_common %>%
  filter(Empresa %in% selected_wu) %>%
  select(
    Empresa, Year, Tipo_Sistema, 
    I_Perdas_reais_de_água, I_Reabilitação_de_condutas,
    V_Avarias_em_condutas, V_Condutas_reabilitadas_nos_últimos_cinco_anos,
    V_Comprimento_total_de_condutas, V_Falhas_no_abastecimento,
    I_Cobertura_dos_gastos,I_Ocorrência_de_avarias_em_condutas,
    I_Ocorrência_de_falhas_no_abastecimento, V_Índice_de_conhecimento_infraestrutural,
    V_Perdas_reais
  )
# Define improvement based on the change in real losses
df_change <- df_selected %>%
  group_by(Empresa) %>%
  summarise(change_RL = diff(I_Perdas_reais_de_água)) %>%
  mutate(Direction = ifelse(change_RL < 0, "Improved", "Worsened"))

# Merge this classification back
df_selected <- df_selected %>%
  left_join(df_change, by = "Empresa")

df_long_selected <- df_selected %>%
  select(-change_RL) %>%
  pivot_longer(
    cols = -c(Empresa, Year, Tipo_Sistema, Direction),
    names_to = "Variable",
    values_to = "Value"
  )
df_summary <- df_long_selected %>%
  group_by(Direction, Year, Variable) %>%
  summarise(median_value = median(Value, na.rm = TRUE))

variable_labels <- c(
  I_Perdas_reais_de_água = "Real Water\nLosses",
  I_Reabilitação_de_condutas = "Pipe\nRehabilitation",
  V_Avarias_em_condutas = "Pipe Breaks",
  V_Condutas_reabilitadas_nos_últimos_cinco_anos = "Pipes\nRehabilitated (5y)",
  V_Comprimento_total_de_condutas = "Total Pipe Length",
  V_Falhas_no_abastecimento = "Supply Failures",
  I_Cobertura_dos_gastos = "Cost Coverage\nIndex",
  I_Ocorrência_de_avarias_em_condutas = "Incidence of\nPipe Breaks",
  I_Ocorrência_de_falhas_no_abastecimento = "Incidence of\nFailures",
  V_Índice_de_conhecimento_infraestrutural = "Infrastructure\nKnowledge Index",
  V_Perdas_reais = "Real Losses"
)

ggplot(subset(df_summary, Direction=="Improved"), aes(x = as.factor(Year), y = median_value, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(
    ~ Variable,
    scales = "free_y",
    labeller = labeller(Variable = variable_labels)
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9, face = "bold"), # readable facet labels
    # tilt x-axis if needed
    legend.position = "none"
  ) +
  labs(
    title = "Median Evolution of Key Indicators",subtitle = "WU with Improved RL",
    y = "Median Value",
    x = "Year"
  )

ggplot(subset(df_summary, Direction=="Worsened"), aes(x = as.factor(Year), y = median_value, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(
    ~ Variable,
    scales = "free_y",
    labeller = labeller(Variable = variable_labels)
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9, face = "bold"), # readable facet labels
    # tilt x-axis if needed
    legend.position = "none"
  ) +
  labs(
    title = "Median Evolution of Key Indicators",subtitle = "WU with Worsened RL",
    y = "Median Value",
    x = "Year"
  )
#para WU que sofreram grandes variações de real losses parece que foi a reabilitação que mudou
#será que dá para generalizar para o dataset inteiro?

#3. Como é que o tamanho da empresa ou a densidade afeta as perdas reais? 
#3.1 Tamanho da empresas - V_Comprimento_total_de_condutas, v_Comprimento_médio_de_condutas_com_mais_de_10_anos, I_Densidade_de_ramais, V_Ramais_de_ligação
df_2023_final %>%
  filter(Tipo_Sistema == "Alta") %>%
  summarise(across(c(V_Comprimento_total_de_condutas, 
                     V_Comprimento_médio_de_condutas_com_mais_de_10_anos),
                   ~ cor(V_Perdas_reais, ., method="spearman", use="complete.obs")))
df_2023_final %>%
  filter(Tipo_Sistema == "Baixa") %>%
  summarise(across(c(V_Comprimento_total_de_condutas, 
                     V_Comprimento_médio_de_condutas_com_mais_de_10_anos, V_Ramais_de_ligação, I_Densidade_de_ramais),
                   ~ cor(V_Perdas_reais, ., method="spearman", use="complete.obs")))
df_2023_final %>%
  filter(Tipo_Sistema == "Alta") %>%
  summarise(across(c(V_Comprimento_total_de_condutas, 
                     V_Comprimento_médio_de_condutas_com_mais_de_10_anos),
                   ~ cor(I_Perdas_reais_de_água, ., method="spearman", use="complete.obs")))
df_2023_final %>%
  filter(Tipo_Sistema == "Baixa") %>%
  summarise(across(c(V_Comprimento_total_de_condutas, 
                     V_Comprimento_médio_de_condutas_com_mais_de_10_anos, V_Ramais_de_ligação, I_Densidade_de_ramais),
                   ~ cor(I_Perdas_reais_de_água, ., method="spearman", use="complete.obs")))
#há uma mínima relação entre o tamanho/densidade da empresa com a sua eficácia em controlar perdas de água. De facto, I_Perdas_reais_de_água está normalizado para tal não acontecer
#Claramente que quanto mais condutas/ramais mais perdas reais de água. A densidade parece não afetar a quantidade de água perdida

#4. Como é que reabilitação de condutas e avarias afetam as perdas de água/eficiência em controlar as perdas reais?
#V_Comprimento_médio_de_condutas_com_mais_de_10_anos, V_Avarias_em_condutas,V_Condutas_reabilitadas_nos_últimos_5_anos, I_Reabilitação_de_condutas, I_Ocorrência_de_avarias_em_condutas, I_Índice_de_valor_da_infraestrutura
df_2023_final %>%
  filter(Tipo_Sistema == "Baixa") %>%
  summarise(across(c(V_Avarias_em_condutas,V_Condutas_reabilitadas_nos_últimos_cinco_anos, I_Reabilitação_de_condutas, I_Ocorrência_de_avarias_em_condutas, I_Índice_de_valor_da_infraestrutura ),
                   ~ cor(I_Perdas_reais_de_água, ., method="spearman", use="complete.obs")))

ggplot(df_2023_final)+
  geom_point(aes(x=log1p(V_Falhas_no_abastecimento),y=I_Perdas_reais_de_água,color=RL_class))+
  theme_minimal()
ggplot(df_2023_final)+
  geom_point(aes(x=I_Ocorrência_de_avarias_em_condutas,y=I_Perdas_reais_de_água,color=RL_class))+
  theme_minimal()
ggplot(df_2023_final) +
  geom_point(aes(x = I_Reabilitação_de_condutas, y = I_Perdas_reais_de_água, color=Tipo_Sistema), alpha = 0.6) +
  labs(x = "Pipe rehabilitation rate (%/year)",
       y = "Real losses (L/ramal·day)",
       title = "Relationship between rehabilitation and real losses") +
  theme_minimal()
ggplot(df_2023_final, aes(x=V_Água_faturada/V_Água_entrada_no_sistema,y=I_Perdas_reais_de_água,color=RL_class))+
  geom_point()+
  labs(x="Revenue Water (%)", y="Real losses Indicator", title="Relationship between real losses and revenue water")+
  theme_minimal()
#há sobreposição entre classes

#Pode ser que apenas uma variável não seja suficiente para distinguir mas sim uma interação entre variáveis
ggplot(df_2023_final %>% filter(Tipo_Sistema == "Baixa"),
       aes(x = log1p(I_Ocorrência_de_avarias_em_condutas), 
           y = log1p(I_Perdas_reais_de_água), 
           color = RL_class)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  theme_minimal()

ggplot(df_2023_final)+
  geom_point(aes(y=log1p(V_Avarias_em_condutas), x=log1p(V_Condutas_reabilitadas_nos_últimos_cinco_anos), color= RL_class, shape=Tipo_Sistema), size=2)+
  theme_minimal()
ggplot(df_2023_final)+
  geom_point(aes(y=log1p(I_Ocorrência_de_avarias_em_condutas), x=I_Reabilitação_de_condutas, color= RL_class, shape=Tipo_Sistema))+
  theme_minimal()
ggplot(df_2023_final)+
  geom_point(aes(y=log1p(V_Falhas_no_abastecimento), x=V_Índice_de_conhecimento_infraestrutural, color= RL_class, shape=Tipo_Sistema))+
  theme_minimal()
ggplot(df_2023_final)+
  geom_point(aes(y=log1p(V_Falhas_no_abastecimento), x=log1p(V_Comprimento_médio_de_condutas_com_mais_de_10_anos), color= RL_class, shape=Tipo_Sistema))+
  theme_minimal()

ggplot(df_2023_final %>% filter(Tipo_Sistema == "Baixa"), 
       aes(x = log1p(I_Ramais_afetados_por_falhas_no_abastecimento), 
           y = log1p(V_Falhas_no_abastecimento), 
           color = RL_class)) +
  geom_point(alpha = 0.6) +
  theme_minimal()
df_2023_final %>%
  filter(Tipo_Sistema == "Baixa") %>%
  group_by(RL_class) %>%
  summarise(
    median_rehab = median(I_Reabilitação_de_condutas, na.rm = TRUE),
    median_failures = median(I_Ramais_afetados_por_falhas_no_abastecimento, na.rm = TRUE),
    median_value_index = median(I_Índice_de_valor_da_infraestrutura, na.rm = TRUE),
    median_f = median(V_Falhas_no_abastecimento, na.rm = TRUE)
  )
vars_for_boxplots <- c(
  "V_Rendimentos_tarifários",
  "I_Acessibilidade_física_do_serviço",
  "I_Água_não_faturada",
  "V_Água_faturada",
  "V_Índice_de_conhecimento_infraestrutural",
  "I_Índice_de_valor_da_infraestrutura",
  "V_Índice_de_gestão_patrimonial_de_infraestruturas",
  "V_Gastos_totais",
  "I_Reabilitação_de_condutas",
  "V_Falhas_no_abastecimento",
  "V_Condutas_reabilitadas_nos_últimos_cinco_anos",
  "V_Avarias_em_condutas",
  "V_Perdas_reais",
  "V_Comprimento_médio_de_condutas_com_mais_de_10_anos",
  "V_Comprimento_total_de_condutas"
)

var_labels <- c(
  V_Rendimentos_tarifários = "Tariff Revenues",
  I_Acessibilidade_física_do_serviço = "Physical Service Accessibility",
  I_Água_não_faturada = "Non-Revenue Water",
  V_Água_faturada = "Billed Water",
  V_Índice_de_conhecimento_infraestrutural = "Infrastructure Knowledge Index",
  I_Índice_de_valor_da_infraestrutura = "Infrastructure Value Index",
  V_Gastos_totais = "Total Expenses",
  I_Reabilitação_de_condutas = "Pipeline Rehabilitation Index",
  V_Falhas_no_abastecimento = "Supply Failures",
  V_Condutas_reabilitadas_nos_últimos_cinco_anos = "Pipelines Rehabilitated\n(Last 5 Years)",
  V_Avarias_em_condutas = "Pipe Breaks",
  V_Perdas_reais = "Real Losses",
  V_Comprimento_médio_de_condutas_com_mais_de_10_anos = "Avg. Length of Pipes >10 Years",
  V_Comprimento_total_de_condutas = "Total Pipe Length", 
  V_Índice_de_gestão_patrimonial_de_infraestruturas = "Infrastructure Management Index"
)

# Reshape to long format
df_long <- df_2023_final %>%
  select(RL_class, Tipo_Sistema, all_of(vars_for_boxplots)) %>%
  pivot_longer(
    cols = all_of(vars_for_boxplots),
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(df_long %>%
         filter(Variable %in% c("I_Reabilitação_de_condutas",
                                "V_Avarias_em_condutas",
                                "V_Comprimento_total_de_condutas",
                                "V_Comprimento_médio_de_condutas_com_mais_de_10_anos",
                                "V_Falhas_no_abastecimento",
                                "V_Condutas_reabilitadas_nos_últimos_cinco_anos"),
                Tipo_Sistema == "Alta"),
       aes(x = RL_class, y = Value, fill = RL_class)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y",labeller = labeller(Variable = var_labels)) +
  scale_fill_manual(values = c("Unsatisfactory" = "firebrick1",
                               "Medium" = "deepskyblue",
                               "Good" = "springgreen")) +
  labs(title = "Bulk systems – Network structure and rehabilitation variables by RL class",
       x = "RL class", y = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_long %>%
         filter(Variable %in% c("I_Acessibilidade_física_do_serviço",
                                "I_Água_não_faturada",
                                "I_Índice_de_valor_da_infraestrutura",
                                "V_Água_faturada",
                                "V_Gastos_totais",
                                "V_Índice_de_conhecimento_infraestrutural",
                                "V_Índice_de_gestão_patrimonial_de_infraestruturas",
                                "V_Perdas_reais",
                                "V_Rendimentos_tarifários"),
                Tipo_Sistema == "Alta"),
       aes(x = RL_class, y = Value, fill = RL_class)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y",labeller = labeller(Variable = var_labels)) +
  scale_fill_manual(values = c("Unsatisfactory" = "firebrick1",
                               "Medium" = "deepskyblue",
                               "Good" = "springgreen")) +
  labs(title = "Bulk systems – Indexes, Volumes and Financial variables by RL class",
       x = "RL class", y = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_long %>%
         filter(Variable %in% c("I_Reabilitação_de_condutas",
                                "V_Avarias_em_condutas",
                                "V_Comprimento_total_de_condutas",
                                "V_Comprimento_médio_de_condutas_com_mais_de_10_anos",
                                "V_Falhas_no_abastecimento",
                                "V_Condutas_reabilitadas_nos_últimos_cinco_anos"),
                Tipo_Sistema == "Baixa"),
       aes(x = RL_class, y = log1p(Value), fill = RL_class)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y",labeller = labeller(Variable = var_labels)) +
  scale_fill_manual(values = c("Unsatisfactory" = "firebrick1",
                               "Medium" = "deepskyblue",
                               "Good" = "springgreen")) +
  labs(title = "Retail systems – Network structure and rehabilitation variables by RL class",
       x = "RL class", y = "log1p(Value)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_long %>%
         filter(Variable %in% c("I_Acessibilidade_física_do_serviço",
                                "I_Água_não_faturada",
                                "I_Índice_de_valor_da_infraestrutura",
                                "V_Água_faturada",
                                "V_Gastos_totais",
                                "V_Índice_de_conhecimento_infraestrutural",
                                "V_Índice_de_gestão_patrimonial_de_infraestruturas",
                                "V_Perdas_reais",
                                "V_Rendimentos_tarifários"),
                Tipo_Sistema == "Baixa"),
       aes(x = RL_class, y = log1p(Value), fill = RL_class)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y",labeller = labeller(Variable = var_labels)) +
  scale_fill_manual(values = c("Unsatisfactory" = "firebrick1",
                               "Medium" = "deepskyblue",
                               "Good" = "springgreen")) +
  labs(title = "Retail systems – Indexes, Volumes and Financial variables by RL class",
       x = "RL class", y = "log1p(Value)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

#Reabilitação e nº de avarias não parecem influenciar as classes: é de facto estranho mas praticamente ninguém reabilita; e o facto de teres mais avarias não parece indicar que és mau

#Houve uma melhoria nos classificadores? Nota-se diferença entre alta e baixa? 
ggplot(df_common, aes(x = as.factor(Year), fill = RL_class)) +
  geom_bar(position = "dodge") +
  labs(y = "Proportion", title = "Change in Real Losses classification (2020–2023)")+
  facet_wrap(~Tipo_Sistema)
#Medium subiu e as outras duas desceram: significa que houve algumas que eram más e conseguiram subir e outras que por outro lado acabaram por descer. A descida de insatisfatório parece maior para alta
ggplot(df_common, aes(x = as.factor(Year), fill = NRW_class)) +
  geom_bar(position = "dodge") +
  labs(y = "Proportion", title = "Change in NRW classification (2020–2023)")+
  facet_wrap(~Tipo_Sistema)
#insatisfatório subiu e o good também na baixa. Na alta muitos do insatisfatório passaram para medium

#Reabilitação é uma coisa que requer tempo. Provavelmente foi por isso que não deu relação em 2023: uma empresa que reabilitou em 2020 verá melhorias em 2023?
df_diff <- df_common %>%
  group_by(Empresa, Tipo_Sistema) %>%
  summarise(
    delta_rehab = diff(V_Condutas_reabilitadas_nos_últimos_cinco_anos),
    real_losses = I_Perdas_reais_de_água
  )
ggplot(df_diff, aes(x = delta_rehab, y = real_losses, color=Tipo_Sistema)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
df_class_change <- df_common %>%
  select(Empresa,Tipo_Sistema, Year, RL_class) %>%
  pivot_wider(names_from = Year, values_from = RL_class, names_prefix = "RL_") %>%
  mutate(change = paste(RL_2020, "→", RL_2023))
ggplot(df_class_change, aes(x = change, fill=Tipo_Sistema)) +
  geom_bar(position = "dodge") +
  labs(title = "Transitions between RL classes (2020–2023)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
# Compute rehab change manually
df_rehab <- df_common %>%
  select(Empresa, Year, I_Reabilitação_de_condutas, I_Perdas_reais_de_água, RL_class) %>%
  group_by(Empresa) %>%
  summarize(
    rehab_2020 = I_Reabilitação_de_condutas[Year == 2020],
    rehab_2023 = I_Reabilitação_de_condutas[Year == 2023],
    losses_2020 = I_Perdas_reais_de_água[Year == 2020],
    losses_2023 = I_Perdas_reais_de_água[Year == 2023],
    rl_class_2023 = RL_class[Year == 2023]
  ) %>%
  ungroup() %>%
  mutate(
    rehab_change = rehab_2023 - rehab_2020,
    losses_change = losses_2023 - losses_2020
  )

# Plot change in rehab vs change in losses
ggplot(df_rehab, aes(x = rehab_change, y = losses_change, color = rl_class_2023)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    x = "Change in Rehabilitation (2023 - 2020)",
    y = "Change in Real Losses (2023 - 2020)",
    title = "Impact of Rehabilitation Changes on Real Losses"
  ) +
  theme_minimal()



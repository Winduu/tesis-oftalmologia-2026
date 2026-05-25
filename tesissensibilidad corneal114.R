# # =====================================================
# TESIS: SENSIBILIDAD CORNEAL EN PACIENTES CON DM2
#
# =====================================================

# =====================================================
# 1. PAQUETES
# =====================================================
# Dentro de tu proyecto en RStudio:
usethis::use_git()        # Inicializa Git local
usethis::use_github()     # Crea el repositorio en GitHub y sube todo
library(MASS)
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(naniar)
library(skimr)
library(gtsummary)
library(flextable)
library(officer)
library(writexl)
library(readxl)
library(broom)
library(ggplot2)

# =====================================================
# 2. CARPETAS DE SALIDA
# =====================================================

dir.create("graficos", showWarnings = FALSE)
dir.create("tablas", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

# =====================================================
# 3. CARGA Y LIMPIEZA INICIAL
# =====================================================

df <- read_csv("base_sensibilidad_corneal.csv") %>%
  clean_names() %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(
    where(is.character),
    ~ case_when(
      .x %in% c("si", "sí", "SI", "Sí", "SÍ") ~ "Si",
      .x %in% c("no", "NO", "No") ~ "NO",
      TRUE ~ .x
    )
  ))

# =====================================================
# 4. CONVERSIÓN DE VARIABLES
# =====================================================

df <- df %>%
  mutate(
    edad = as.numeric(edad),
    
    fecha_de_evaluacion = dmy(fecha_de_evaluacion),
    
    tiempo_de_evolucion_de_diabetes =
      as.numeric(tiempo_de_evolucion_de_diabetes),
    
    hb_a1c_num = as.numeric(gsub("%", "", hb_a1c)),
    
    glucemia_num = as.numeric(
      str_trim(gsub("mg/dL|mg/dl|mg/dl|/dL|/dl", "", glucemia_reciente))
    ),
    
    sexo = factor(sexo),
    tratamiento = factor(tratamiento),
    
    hipertension_arterial = factor(hipertension_arterial, levels = c("NO", "Si")),
    dislipidemia = factor(dislipidemia, levels = c("NO", "Si")),
    neuropatia_periferica_diabetica = factor(neuropatia_periferica_diabetica, levels = c("NO", "Si")),
    nefropatia_diabetica = factor(nefropatia_diabetica, levels = c("NO", "Si")),
    tabaquismo = factor(tabaquismo, levels = c("NO", "Si")),
    
    retinopatia_diabetica_od = factor(retinopatia_diabetica_od),
    retinopatia_diabetica_os = factor(retinopatia_diabetica_os),
    
    central_od = as.numeric(central_od),
    central_os = as.numeric(central_os),
    
    pio_od = as.numeric(pio_od),
    pio_os = as.numeric(pio_os),
    but_od = as.numeric(but_od),
    but_os = as.numeric(but_os)
  )

# =====================================================
# 5. VARIABLES DERIVADAS
# =====================================================

df <- df %>%
  mutate(
    sensibilidad_minima = pmin(central_od, central_os, na.rm = TRUE),
    
    severidad_sensibilidad = case_when(
      sensibilidad_minima == 3 ~ "Normal",
      sensibilidad_minima == 2 ~ "Leve",
      sensibilidad_minima == 1 ~ "Moderada",
      sensibilidad_minima == 0 ~ "Severa"
    ),
    
    severidad_sensibilidad = factor(
      severidad_sensibilidad,
      levels = c("Normal", "Leve", "Moderada", "Severa"),
      ordered = TRUE
    ),
    
    sensibilidad_alterada = case_when(
      sensibilidad_minima == 3 ~ "NO",
      sensibilidad_minima < 3 ~ "Si"
    ),
    
    sensibilidad_alterada = factor(
      sensibilidad_alterada,
      levels = c("NO", "Si")
    ),
    
    control_glucemico = case_when(
      hb_a1c_num < 7 ~ "Buen control",
      hb_a1c_num >= 7 & hb_a1c_num < 9 ~ "Control subóptimo",
      hb_a1c_num >= 9 ~ "Mal control"
    ),
    
    control_glucemico = factor(
      control_glucemico,
      levels = c("Buen control", "Control subóptimo", "Mal control"),
      ordered = TRUE
    ),
    
    glucemia_categoria = case_when(
      glucemia_num < 126 ~ "Menor de 126 mg/dL",
      glucemia_num >= 126 & glucemia_num < 200 ~ "126-199 mg/dL",
      glucemia_num >= 200 ~ "≥200 mg/dL"
    ),
    
    glucemia_categoria = factor(
      glucemia_categoria,
      levels = c("Menor de 126 mg/dL", "126-199 mg/dL", "≥200 mg/dL"),
      ordered = TRUE
    )
  )

# =====================================================
# 6. VALIDACIÓN INICIAL
# =====================================================

str(df)
summary(df)
colSums(is.na(df))

table(df$severidad_sensibilidad)
table(df$sensibilidad_alterada)
table(df$control_glucemico)
table(df$glucemia_categoria)

vis_miss(df)
miss_var_summary(df)
skim(df)

# =====================================================
# 7. EXPORTAR BASE LIMPIA
# =====================================================

write.csv(
  df,
  "resultados/base_sensibilidad_corneal_113_limpia.csv",
  row.names = FALSE
)

write_xlsx(
  list("dataset_limpio" = df),
  "resultados/base_sensibilidad_corneal_113_limpia.xlsx"
)

saveRDS(
  df,
  "resultados/base_sensibilidad_corneal_113_limpia.rds"
)

# =====================================================
# 8. TABLA 1: DESCRIPTIVA GENERAL
# =====================================================

tabla1 <- df %>%
  dplyr::select(
    edad,
    sexo,
    tiempo_de_evolucion_de_diabetes,
    hb_a1c_num,
    glucemia_num,
    hipertension_arterial,
    dislipidemia,
    neuropatia_periferica_diabetica,
    nefropatia_diabetica,
    severidad_sensibilidad
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no",
    label = list(
      edad ~ "Edad (años)",
      sexo ~ "Sexo",
      tiempo_de_evolucion_de_diabetes ~ "Tiempo de evolución DM2 (años)",
      hb_a1c_num ~ "HbA1c (%)",
      glucemia_num ~ "Glucemia reciente (mg/dL)",
      hipertension_arterial ~ "Hipertensión arterial",
      dislipidemia ~ "Dislipidemia",
      neuropatia_periferica_diabetica ~ "Neuropatía periférica diabética",
      nefropatia_diabetica ~ "Nefropatía diabética",
      severidad_sensibilidad ~ "Severidad de sensibilidad corneal"
    )
  ) %>%
  bold_labels()

tabla1

####categorización HbA1c
df <- df %>%
  mutate(
    control_glucemico_hba1c = case_when(
      hb_a1c_num < 7 ~ "Controlado",
      hb_a1c_num >= 7 & hb_a1c_num < 9 ~ "Mal controlado",
      hb_a1c_num >= 9 ~ "Muy mal controlado",
      TRUE ~ NA_character_
    ),
    control_glucemico_hba1c = factor(
      control_glucemico_hba1c,
      levels = c("Controlado", "Mal controlado", "Muy mal controlado"),
      ordered = TRUE
    )
  )
table(df$control_glucemico_hba1c, useNA = "ifany")

round(prop.table(table(df$control_glucemico_hba1c)) * 100, 1)
names(df)

table(df$control_glucemico_hba1c, useNA = "ifany")

round(prop.table(table(df$control_glucemico_hba1c)) * 100, 1)
cat("Distribución de control glucémico por HbA1c\n\n")

tabla_hba1c <- table(df$control_glucemico_hba1c)

print(tabla_hba1c)

cat("\nPorcentajes:\n")

print(round(prop.table(tabla_hba1c) * 100, 1))

# =====================================================
# 9. TABLA 2: INFERENCIAL POR SEVERIDAD
# =====================================================

tabla2 <- df %>%
  dplyr::select(
    edad,
    hb_a1c_num,
    glucemia_num,
    tiempo_de_evolucion_de_diabetes,
    neuropatia_periferica_diabetica,
    severidad_sensibilidad
  ) %>%
  tbl_summary(
    by = severidad_sensibilidad,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no",
    label = list(
      edad ~ "Edad (años)",
      hb_a1c_num ~ "HbA1c (%)",
      glucemia_num ~ "Glucemia reciente (mg/dL)",
      tiempo_de_evolucion_de_diabetes ~ "Tiempo de evolución DM2 (años)",
      neuropatia_periferica_diabetica ~ "Neuropatía periférica diabética"
    )
  ) %>%
  add_p() %>%
  bold_labels()

tabla2

doc2 <- read_docx() %>%
  body_add_par(
    "Tabla 2. Variables clínicas según severidad de sensibilidad corneal",
    style = "heading 1"
  ) %>%
  body_add_flextable(as_flex_table(tabla2))

print(
  doc2,
  target = "tablas/tabla2_severidad_sensibilidad.docx"
)

kruskal.test(
  severidad_sensibilidad ~ control_glucemico_hba1c,
  data = df
)
# =====================================================
# 10. NORMALIDAD
# =====================================================

sink("resultados/shapiro_tests.txt")
tryCatch({
  shapiro.test(df$edad)
  shapiro.test(df$hb_a1c_num)
  shapiro.test(df$glucemia_num)
  shapiro.test(df$tiempo_de_evolucion_de_diabetes)
}, finally = sink())

# =====================================================
# 11. GRÁFICOS DE NORMALIDAD
# =====================================================

png("graficos/histograma_hba1c.png", width = 3000, height = 2000, res = 300)
hist(df$hb_a1c_num, main = "Histograma HbA1c", xlab = "HbA1c (%)")
dev.off()

png("graficos/qqplot_hba1c.png", width = 3000, height = 2000, res = 300)
qqnorm(df$hb_a1c_num)
qqline(df$hb_a1c_num)
dev.off()

png("graficos/histograma_glucemia.png", width = 3000, height = 2000, res = 300)
hist(df$glucemia_num, main = "Histograma glucemia", xlab = "Glucemia (mg/dL)")
dev.off()

png("graficos/qqplot_glucemia.png", width = 3000, height = 2000, res = 300)
qqnorm(df$glucemia_num)
qqline(df$glucemia_num)
dev.off()

# =====================================================
# 12. CORRELACIONES DE SPEARMAN
# =====================================================

sink("resultados/correlaciones_spearman.txt")
tryCatch({
  cor.test(
    df$tiempo_de_evolucion_de_diabetes,
    as.numeric(df$severidad_sensibilidad),
    method = "spearman"
  )
  
  cor.test(
    df$hb_a1c_num,
    as.numeric(df$severidad_sensibilidad),
    method = "spearman"
  )
  
  cor.test(
    df$glucemia_num,
    as.numeric(df$severidad_sensibilidad),
    method = "spearman"
  )
}, finally = sink())

# =====================================================
# 13. COMPARACIÓN BINARIA
# Sensibilidad normal vs alterada
# =====================================================

sink("resultados/analisis_binario_sensibilidad.txt")
tryCatch({
  wilcox.test(tiempo_de_evolucion_de_diabetes ~ sensibilidad_alterada, data = df)
  wilcox.test(hb_a1c_num ~ sensibilidad_alterada, data = df)
  wilcox.test(glucemia_num ~ sensibilidad_alterada, data = df)
  wilcox.test(edad ~ sensibilidad_alterada, data = df)
  fisher.test(table(df$sensibilidad_alterada, df$neuropatia_periferica_diabetica))
}, finally = sink())

grafico_mann_whitney <- ggplot(
  df,
  aes(
    x = sensibilidad_alterada,
    y = tiempo_de_evolucion_de_diabetes
  )
) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(
    title = "Tiempo de evolución de DM2 según sensibilidad corneal",
    x = "Sensibilidad corneal",
    y = "Tiempo de evolución DM2 (años)"
  ) +
  theme_minimal(base_size = 14)

grafico_mann_whitney

ggsave(
  "graficos/mann_whitney_tiempo_dm2.png",
  grafico_mann_whitney,
  width = 10,
  height = 7,
  dpi = 300
)

# =====================================================
# 14. REGRESIÓN LOGÍSTICA BINARIA
# =====================================================

modelo_binario <- glm(
  sensibilidad_alterada ~
    tiempo_de_evolucion_de_diabetes +
    hb_a1c_num +
    glucemia_num +
    neuropatia_periferica_diabetica +
    edad,
  data = df,
  family = binomial
)

summary(modelo_binario)

or_binario <- exp(cbind(
  OR = coef(modelo_binario),
  confint.default(modelo_binario)
))

or_binario

write.csv(
  or_binario,
  "resultados/or_modelo_logistico_binario.csv"
)

sink("resultados/modelo_logistico_binario.txt")
tryCatch({
  summary(modelo_binario)
  or_binario
}, finally = sink())

# =====================================================
# 15. REGRESIÓN LOGÍSTICA ORDINAL
# =====================================================

levels(df$severidad_sensibilidad)

modelo_ordinal <- polr(
  severidad_sensibilidad ~
    tiempo_de_evolucion_de_diabetes +
    hb_a1c_num +
    glucemia_num +
    neuropatia_periferica_diabetica +
    edad,
  data = df,
  Hess = TRUE
)

summary(modelo_ordinal)

coef_table <- coef(summary(modelo_ordinal))

p_values <- pnorm(
  abs(coef_table[, "t value"]),
  lower.tail = FALSE
) * 2

coef_table <- cbind(
  coef_table,
  "p value" = p_values
)

coef_table

or_ordinal <- exp(cbind(
  OR = coef(modelo_ordinal),
  confint.default(modelo_ordinal)
))

or_ordinal

write.csv(coef_table, "resultados/coeficientes_modelo_ordinal.csv")
write.csv(or_ordinal, "resultados/or_modelo_ordinal.csv")

sink("resultados/modelo_logistico_ordinal.txt")
tryCatch({
  summary(modelo_ordinal)
  coef_table
  or_ordinal
}, finally = sink())

# =====================================================
# 16. FIGURAS PUBLICABLES
# =====================================================

grafico_tiempo <- ggplot(
  df,
  aes(
    x = severidad_sensibilidad,
    y = tiempo_de_evolucion_de_diabetes
  )
) +
  geom_boxplot() +
  labs(
    title = "Tiempo de evolución de DM2 según severidad de sensibilidad corneal",
    x = "Severidad de sensibilidad corneal",
    y = "Tiempo de evolución DM2 (años)"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/boxplot_tiempo_evolucion_vs_severidad.png",
  grafico_tiempo,
  width = 10,
  height = 7,
  dpi = 300
)

grafico_hba1c <- ggplot(
  df,
  aes(
    x = severidad_sensibilidad,
    y = hb_a1c_num
  )
) +
  geom_boxplot() +
  labs(
    title = "HbA1c según severidad de sensibilidad corneal",
    x = "Severidad de sensibilidad corneal",
    y = "HbA1c (%)"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/boxplot_hba1c_vs_severidad.png",
  grafico_hba1c,
  width = 10,
  height = 7,
  dpi = 300
)

grafico_neuropatia <- ggplot(
  df,
  aes(
    x = neuropatia_periferica_diabetica,
    fill = sensibilidad_alterada
  )
) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("NO" = "#1F3B73", "Si" = "#4F81BD")
  ) +
  labs(
    title = "Neuropatía periférica y sensibilidad corneal alterada",
    x = "Neuropatía periférica diabética",
    y = "Proporción",
    fill = "Sensibilidad alterada"
  ) +
  theme_minimal(base_size = 14)

grafico_neuropatia

ggsave(
  "graficos/neuropatia_vs_sensibilidad.png",
  grafico_neuropatia,
  width = 10,
  height = 7,
  dpi = 300
)

# =====================================================
# 17. FOREST PLOT MODELO ORDINAL
# =====================================================

forest_data <- tidy(modelo_ordinal) %>%
  filter(
    term %in% c(
      "tiempo_de_evolucion_de_diabetes",
      "hb_a1c_num",
      "glucemia_num",
      "neuropatia_periferica_diabeticaSi",
      "edad"
    )
  ) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    term = case_match(
      term,
      "tiempo_de_evolucion_de_diabetes" ~ "Tiempo evolución DM2",
      "hb_a1c_num" ~ "HbA1c",
      "glucemia_num" ~ "Glucemia",
      "neuropatia_periferica_diabeticaSi" ~ "Neuropatía periférica",
      "edad" ~ "Edad"
    )
  )

forest_plot <- ggplot(
  forest_data,
  aes(
    x = term,
    y = OR,
    ymin = conf.low,
    ymax = conf.high
  )
) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_flip() +
  labs(
    title = "Modelo de regresión logística ordinal",
    x = "",
    y = "Odds Ratio"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/forest_plot_modelo_ordinal.png",
  forest_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# =====================================================
# 18. CORRELACIÓN GRÁFICA
# =====================================================

grafico_correlacion <- ggplot(
  df,
  aes(
    x = tiempo_de_evolucion_de_diabetes,
    y = as.numeric(severidad_sensibilidad)
  )
) +
  geom_jitter(width = 0.3, height = 0.1) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("Normal", "Leve", "Moderada", "Severa")
  ) +
  labs(
    title = "Tiempo de evolución DM2 y severidad de sensibilidad corneal",
    x = "Tiempo evolución DM2 (años)",
    y = "Severidad de sensibilidad corneal"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/correlacion_tiempo_dm2_severidad.png",
  grafico_correlacion,
  width = 10,
  height = 7,
  dpi = 300
)

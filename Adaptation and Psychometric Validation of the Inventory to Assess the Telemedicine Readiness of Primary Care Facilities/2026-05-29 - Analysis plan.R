########################
#1. Call libraries
########################
library(tidyverse)
library(psych)
library(openxlsx)
library(lavaan)
library(readxl)
library(dplyr)
library(car)
library(semTools)
library(semPlot)
library(haven)
library(readr)
library(readxl)
library(ggplot2)
library(polycor)
library(psych) 
library(tidyr)
library(janitor)
library(survey)
library(haven)













########################
# 2. Base y criterios de inclusión (AJUSTADO) 
########################

library(dplyr)
library(tibble)
library(openxlsx)
library(haven)

Database <- read_dta("madurez_validacion_consolidado.dta")

# Mueve columnas al final si existen
Database <- Database %>%
  select(-any_of(c("p15_1", "p15_2", "p22_1", "p22_2", "p57_1", "p57_2")),
         everything(),
         any_of(c("p15_1", "p15_2", "p22_1", "p22_2", "p57_1", "p57_2")))

# Convertir variables etiquetadas de Stata a formato usable
Database <- Database %>%
  mutate(
    profesion_chr = as.character(haven::as_factor(profesion))
  )

# Revisar rápidamente cómo quedó profesion
cat("Clase original de profesion:\n")
print(class(Database$profesion))

cat("\nPrimeros valores de profesion convertida:\n")
print(head(unique(Database$profesion_chr), 20))

# Profesiones de salud
profesionales_salud <- c(
  "Enfermería", "Medicina", "Nutrición", "Obstetricia",
  "Odontología", "Psicología", "Quimica-Farmacéutica",
  "Técnico(a)/Auxiliar de Enfermería", "Tecnología Médica",
  "Trabajador(a) social"
)



# Filtrar SOLO profesiones de salud + categoria I-1 a I-4
Database_salud <- Database %>%
  filter(
    as.character(haven::as_factor(profesion)) %in% profesionales_salud,
    as.character(haven::as_factor(categoria)) %in% c("I-1", "I-2", "I-3", "I-4")
  )

cat("\nN total en Database:", nrow(Database), "\n")
cat("N después de filtrar profesiones de salud:", nrow(Database_salud), "\n")

cat("\nTabla de profesion en Database_salud:\n")
print(table(Database_salud$profesion_chr, useNA = "ifany"))




# -----------------------------------
# Base para Tabla 1:
# NO filtrar por complete.cases p1:p85
# -----------------------------------
Database_tbl1 <- Database_salud

# -----------------------------------
# Base para EFA/CFA u otros análisis:
# sí filtrar completos en p1:p85
# -----------------------------------
preguntas <- paste0("p", 1:85)
preguntas_ok <- intersect(preguntas, names(Database_salud))

Database_salud_completo <- Database_salud %>%
  filter(if_all(all_of(preguntas_ok), ~ !is.na(.x)))

cat("\nN para Table 1:", nrow(Database_tbl1), "\n")
cat("N para análisis completos p1:p85:", nrow(Database_salud_completo), "\n")



# Revisión de expertos SOLO en la base completa
drops <- c(
  "p4",
  "p7",
  "p8",
  "p11",
  "p10",
  "p9",
  "p12",
  "p13",
  "p15",
  "p15_1",
  "p18",
  "p19",
  "p22_1",
  "p24",
  "p36",
  "p42",
  "p44",
  "p45",
  "p50",
  "p50_1",
  "p50_2",
  "p50_3",
  "p50_4",
  "p51",
  "p53",
  "p56",
  "p57_1",
  "p57_2",
  "p77",
  "p78",
  "p79",
  "p80",
  "p81",
  "p82",
  "p83",
  "p84",
  "p85"
)


Database_salud_completo <- Database_salud_completo %>%
  select(-any_of(drops))

names(Database_salud_completo)

########################
# 3. Table 1 sin ponderación - AJUSTADO CON LABELS EN INGLÉS
########################

df <- Database_salud_completo

if (nrow(df) == 0) {
  stop("La base para Table 1 quedó vacía. Revisa los valores de profesion_chr.")
}

# =========================
# Funciones auxiliares
# =========================

to_chr_labelled <- function(x) {
  if (inherits(x, "haven_labelled")) {
    as.character(haven::as_factor(x))
  } else {
    as.character(x)
  }
}

to_numeric_code <- function(x) {
  suppressWarnings(as.numeric(x))
}

clean_text <- function(x) {
  x <- to_chr_labelled(x)
  x <- trimws(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  tolower(x)
}

# =========================
# Crear variables dicotomizadas a nivel de evaluados
# =========================

df <- df %>%
  mutate(
    cargo_code = to_numeric_code(cargo),
    tiempo_ipress_code = to_numeric_code(tiempo_ipress),
    
    telemedicine_role = case_when(
      cargo_code %in% c(1, 2) ~ "Responsible/coordinator of telemedicine",
      cargo_code %in% c(3, 4) ~ "Not responsible/coordinator of telemedicine",
      TRUE ~ NA_character_
    ),
    
    time_at_ipress_group = case_when(
      tiempo_ipress_code %in% c(1, 2) ~ "Less than 6 months",
      tiempo_ipress_code %in% c(3, 4) ~ "6 months or more",
      TRUE ~ NA_character_
    )
  )

# Verificación rápida de las nuevas variables
cat("\nTelemedicine role:\n")
print(table(df$telemedicine_role, useNA = "ifany"))

cat("\nTime at IPRESS group:\n")
print(table(df$time_at_ipress_group, useNA = "ifany"))

# =========================
# Recodificar variables categóricas con labels académicos en inglés
# =========================

df <- df %>%
  mutate(
    categoria_chr = to_chr_labelled(categoria),
    ioarr_chr = clean_text(ioarr),
    quintil_code = to_numeric_code(quintil),
    escenario_code = to_numeric_code(escenario),
    densidad_chr = clean_text(densidad_cat),
    
    categoria = case_when(
      categoria_chr %in% c("I-1", "I-2", "I-3", "I-4") ~ categoria_chr,
      TRUE ~ categoria_chr
    ),
    
    ioarr = case_when(
      ioarr_chr %in% c("si", "sí", "yes", "1") ~ "Yes",
      ioarr_chr %in% c("no", "0", "2") ~ "No",
      TRUE ~ to_chr_labelled(ioarr)
    ),
    
    quintil = case_when(
      quintil_code == 1 ~ "Quintile 1 (poorest)",
      quintil_code == 2 ~ "Quintile 2",
      quintil_code == 3 ~ "Quintile 3",
      quintil_code == 4 ~ "Quintile 4",
      quintil_code == 5 ~ "Quintile 5 (richest)",
      grepl("1", clean_text(quintil)) ~ "Quintile 1 (poorest)",
      grepl("2", clean_text(quintil)) ~ "Quintile 2",
      grepl("3", clean_text(quintil)) ~ "Quintile 3",
      grepl("4", clean_text(quintil)) ~ "Quintile 4",
      grepl("5", clean_text(quintil)) ~ "Quintile 5 (richest)",
      TRUE ~ to_chr_labelled(quintil)
    ),
    
    escenario = case_when(
      escenario_code == 1 ~ "Large metropolis",
      escenario_code == 2 ~ "Regional metropolis",
      escenario_code == 3 ~ "Intermediate cities",
      escenario_code == 4 ~ "Provincial capitals",
      escenario_code == 5 ~ "Other urban",
      escenario_code == 6 ~ "Rural",
      grepl("large|gran|grande", clean_text(escenario)) ~ "Large metropolis",
      grepl("regional", clean_text(escenario)) ~ "Regional metropolis",
      grepl("intermediate|intermedia", clean_text(escenario)) ~ "Intermediate cities",
      grepl("capital", clean_text(escenario)) ~ "Provincial capitals",
      grepl("other|otro", clean_text(escenario)) ~ "Other urban",
      grepl("rural", clean_text(escenario)) ~ "Rural",
      TRUE ~ to_chr_labelled(escenario)
    ),
    
    densidad_cat = case_when(
      grepl("adecu|adequate", densidad_chr) ~ "Adequate",
      grepl("baja|low|deficit|insuf", densidad_chr) ~ "Low",
      densidad_chr %in% c("1") ~ "Adequate",
      densidad_chr %in% c("0", "2") ~ "Low",
      TRUE ~ to_chr_labelled(densidad_cat)
    ),
    
    telemedicine_role = factor(
      telemedicine_role,
      levels = c(
        "Responsible/coordinator of telemedicine",
        "Not responsible/coordinator of telemedicine"
      )
    ),
    
    time_at_ipress_group = factor(
      time_at_ipress_group,
      levels = c(
        "Less than 6 months",
        "6 months or more"
      )
    ),
    
    categoria = factor(
      categoria,
      levels = c("I-1", "I-2", "I-3", "I-4")
    ),
    
    ioarr = factor(
      ioarr,
      levels = c("Yes", "No")
    ),
    
    quintil = factor(
      quintil,
      levels = c(
        "Quintile 1 (poorest)",
        "Quintile 2",
        "Quintile 3",
        "Quintile 4",
        "Quintile 5 (richest)"
      )
    ),
    
    escenario = factor(
      escenario,
      levels = c(
        "Large metropolis",
        "Regional metropolis",
        "Intermediate cities",
        "Provincial capitals",
        "Other urban",
        "Rural"
      )
    ),
    
    densidad_cat = factor(
      densidad_cat,
      levels = c("Adequate", "Low")
    )
  ) %>%
  select(-cargo_code, -tiempo_ipress_code,
         -categoria_chr, -ioarr_chr, -quintil_code,
         -escenario_code, -densidad_chr)

# =========================
# Variables de interés
# =========================

# Nivel de establecimiento
cat_vars_facility <- c(
  "categoria",
  "ioarr",
  "quintil",
  "escenario",
  "densidad_cat"
)

# Nivel de evaluados
cat_vars_evaluated <- c(
  "telemedicine_role",
  "time_at_ipress_group"
)

cat_vars <- c(cat_vars_facility, cat_vars_evaluated)

num_vars <- c(
  "pob_asignada",
  "medicocirujano",
  "medicoespecialista",
  "profesionalsalud",
  "tecnicosasistenciales",
  "total",
  "densidad_rhus"
)

cat_vars_ok <- intersect(cat_vars, names(df))
num_vars_ok <- intersect(num_vars, names(df))

# Convertir numéricas a numeric
for (v in num_vars_ok) {
  df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
}

cat("\nVariables categóricas encontradas:\n")
print(cat_vars_ok)

cat("\nVariables numéricas encontradas:\n")
print(num_vars_ok)

# =========================
# Labels académicos para la tabla
# =========================

var_labels <- c(
  categoria = "Health level of complexity",
  ioarr = "IOARR program beneficiary",
  quintil = "Wealth quintile",
  escenario = "Geographic area",
  densidad_cat = "Health workforce density level*",
  telemedicine_role = "Telemedicine responsibility role",
  time_at_ipress_group = "Time working at the IPRESS",
  
  pob_asignada = "Assigned population",
  medicocirujano = "General practitioners (physicians)**",
  medicoespecialista = "Medical specialists**",
  profesionalsalud = "Other health professionals**",
  tecnicosasistenciales = "Health technician and auxiliary staff**",
  total = "Total health personnel**",
  densidad_rhus = "Health workforce density (per 10,000 pop.)"
)

# =========================
# Categóricas: n (% [95% CI])
# =========================

summary_cat_simple <- function(data, var) {
  x <- data[[var]]
  x <- x[!is.na(x)]
  
  if (length(x) == 0) return(NULL)
  
  tb_n <- as.data.frame(table(x), stringsAsFactors = FALSE)
  names(tb_n) <- c("Category", "n")
  
  n_total <- sum(tb_n$n)
  
  ci_mat <- t(sapply(tb_n$n, function(k) {
    ci <- suppressWarnings(prop.test(k, n_total)$conf.int)
    c(li = ci[1], ls = ci[2])
  }))
  
  tb_out <- tb_n %>%
    mutate(
      Variable = unname(var_labels[var]),
      prop_val = n / n_total,
      li = ci_mat[, "li"],
      ls = ci_mat[, "ls"],
      `n (% [95% CI])` = sprintf(
        "%d (%.1f [%.1f, %.1f])",
        n,
        prop_val * 100,
        li * 100,
        ls * 100
      ),
      `Median [IQR]` = ""
    ) %>%
    select(
      Variable,
      Category,
      `n (% [95% CI])`,
      `Median [IQR]`
    )
  
  tb_out
}

table_cat <- bind_rows(
  lapply(cat_vars_ok, function(v) summary_cat_simple(df, v))
)

# =========================
# Numéricas: mediana [Q1, Q3]
# =========================

summary_num_simple <- function(data, var) {
  x <- data[[var]]
  x <- x[!is.na(x)]
  
  if (length(x) == 0) return(NULL)
  
  mediana <- median(x, na.rm = TRUE)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  tibble(
    Variable = unname(var_labels[var]),
    Category = "",
    `n (% [95% CI])` = "",
    `Median [IQR]` = sprintf("%.2f [%.2f, %.2f]", mediana, q1, q3)
  )
}

table_num <- bind_rows(
  lapply(num_vars_ok, function(v) summary_num_simple(df, v))
)

# =========================
# Unir y exportar
# =========================

tabla1_final <- bind_rows(
  table_cat,
  table_num
)

write.xlsx(
  tabla1_final,
  "Tabla1_Caracteristicas_EESS_sin_ponderacion.xlsx",
  rowNames = FALSE
)

print(tabla1_final)




























########################
#4. Dividir bases EFA / CFA
########################

# Fijar semilla para hacer la aleatorización replicable
set.seed(1234)  # Puedes cambiar el número si quieres otra aleatorización

# Crear un vector aleatorio de TRUE/FALSE para dividir la base
group_assignment <- sample(c(TRUE, FALSE), size = nrow(Database_salud_completo), replace = TRUE)

# Crear las dos bases
Database_EFA <- Database_salud_completo[group_assignment, ]  # Los que salieron TRUE
Database_CFA <- Database_salud_completo[!group_assignment, ] # Los que salieron FALSE

# Revisar tamaños
cat("Tamaño EFA:", nrow(Database_EFA), "\n")
cat("Tamaño CFA:", nrow(Database_CFA), "\n")





names(Database_CFA)
names(Database_EFA)









############################################################
# 5. AFE + AFC AUTOMÁTICO - SOLO MODELOS UNIDIMENSIONALES
# AFE desde Database_EFA
# CFA desde Database_CFA
#
# AJUSTES PRINCIPALES:
# - Mantiene los mismos nombres de objetos/tablas usados después.
# - Ejecuta análisis paralelo solo como diagnóstico descriptivo.
# - Fuerza la extracción AFE a 1 factor por dimensión.
# - Fuerza el CFA a modelos unidimensionales por dimensión.
# - No prueba modelos correlacionados, bifactor ni de segundo orden.
# - Si una dimensión queda con < 3 ítems, se marca como colapsada.
# - Exporta resultados a Excel con las mismas hojas/objetos.
############################################################

############################################################
# 5.0. Paquetes requeridos
############################################################

required_packages <- c(
  "psych",
  "GPArotation",
  "lavaan",
  "dplyr",
  "purrr",
  "tibble",
  "haven",
  "openxlsx",
  "tidyr"
)

missing_packages <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Instala los paquetes faltantes antes de correr el análisis: ",
    paste0("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))")
  )
}

suppressPackageStartupMessages({
  library(psych)
  library(GPArotation)
  library(lavaan)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(haven)
  library(openxlsx)
  library(tidyr)
})

############################################################
# 5.1. Definir ítems por dimensión
############################################################

items_by_scale <- list(
  dim1 = paste0("p", 1:34),
  dim2 = paste0("p", 35:46),
  dim3 = paste0("p", 47:62),
  dim4 = paste0("p", 63:69),
  dim5 = paste0("p", 70:76)
)

scale_labels <- c(
  dim1 = "Dimensión 1: p1-p34",
  dim2 = "Dimensión 2: p35-p46",
  dim3 = "Dimensión 3: p47-p62",
  dim4 = "Dimensión 4: p63-p69",
  dim5 = "Dimensión 5: p70-p76"
)

############################################################
# 5.2. Parámetros generales
############################################################

min_loading <- 0.40
min_diff <- 0.20

# Requisito conservador para que un factor sea estimable en CFA
min_items_per_factor <- 3

n_iter_parallel <- 100
plot_parallel <- TRUE

# Análisis paralelo:
# "both" muestra factores comunes y componentes principales.
parallel_fa_type <- "both"

# Decisión para número de modelos a probar:
# En esta versión se fuerza el análisis unidimensional.
# El análisis paralelo se conserva como diagnóstico, pero no determina k.
parallel_decision_source <- "unidimensional_only"
force_unidimensional_models <- TRUE

efa_rotation_multidim <- "oblimin"
efa_fm <- "wls"

# CFA
cfa_estimator <- "WLSMV"
cfa_parameterization <- "theta"

# Diagnósticos
latent_overlap_cutoff <- 0.85
latent_overlap_severe_cutoff <- 0.95
mi_resid_cutoff <- 10
resid_cor_cutoff <- 0.10

output_excel <- "AFE_CFA_unidimensional_only_results.xlsx"

############################################################
# 5.3. Funciones auxiliares
############################################################

to_numeric_safe <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  
  if (is.factor(x)) {
    out <- suppressWarnings(as.numeric(as.character(x)))
    if (all(is.na(out)) && !all(is.na(x))) out <- as.numeric(x)
    return(out)
  }
  
  if (is.character(x)) {
    return(suppressWarnings(as.numeric(x)))
  }
  
  suppressWarnings(as.numeric(x))
}

make_items_df <- function(data, items) {
  items_ok <- intersect(items, names(data))
  
  if (length(items_ok) < min_items_per_factor) {
    stop(
      "Dimensión colapsada: hay menos de ",
      min_items_per_factor,
      " ítems disponibles."
    )
  }
  
  df <- data[, items_ok, drop = FALSE]
  df <- as.data.frame(lapply(df, to_numeric_safe))
  names(df) <- items_ok
  
  keep_variability <- sapply(df, function(x) {
    x2 <- x[!is.na(x)]
    length(unique(x2)) >= 2
  })
  
  df <- df[, keep_variability, drop = FALSE]
  
  if (ncol(df) < min_items_per_factor) {
    stop(
      "Dimensión colapsada: hay menos de ",
      min_items_per_factor,
      " ítems con variabilidad."
    )
  }
  
  df
}

round_numeric_df <- function(df, digits = 4) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
}

add_missing_cols <- function(df, cols) {
  for (cc in cols) {
    if (!cc %in% names(df)) df[[cc]] <- NA
  }
  df
}

ensure_empty_cols <- function(df, cols) {
  if (is.null(df) || ncol(df) == 0) {
    out <- as_tibble(setNames(rep(list(logical()), length(cols)), cols))
    return(out)
  }
  
  for (cc in cols) {
    if (!cc %in% names(df)) df[[cc]] <- NA
  }
  
  df
}

safe_fit_measure <- function(fit, primary, fallback = NULL) {
  out <- tryCatch(
    lavaan::fitMeasures(fit, primary),
    error = function(e) NA_real_
  )
  out <- as.numeric(out[1])
  
  if (is.na(out) && !is.null(fallback)) {
    out <- tryCatch(
      lavaan::fitMeasures(fit, fallback),
      error = function(e) NA_real_
    )
    out <- as.numeric(out[1])
  }
  
  out
}

safe_lavinspect <- function(fit, what, default = NA) {
  tryCatch(
    lavaan::lavInspect(fit, what),
    error = function(e) default
  )
}

get_parameter_estimates_safe <- function(fit) {
  pe <- tryCatch(
    lavaan::parameterEstimates(
      fit,
      standardized = TRUE,
      ci = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(pe)) return(tibble())
  
  pe <- as_tibble(pe)
  
  add_missing_cols(
    pe,
    c(
      "lhs", "op", "rhs",
      "est", "se", "z", "pvalue",
      "std.lv", "std.all", "std.nox"
    )
  )
}

paste_items_by_factor <- function(factor_items) {
  if (length(factor_items) == 0) return(NA_character_)
  
  paste(
    purrr::imap_chr(
      factor_items,
      ~ paste0(.y, ": ", paste(.x, collapse = ", "))
    ),
    collapse = " | "
  )
}

empty_tbl <- function(...) {
  tibble(...)
}

############################################################
# 5.4. Análisis paralelo
############################################################

choose_parallel_k <- function(suggested_fa, suggested_pc) {
  if (exists("force_unidimensional_models") && isTRUE(force_unidimensional_models)) {
    return(1L)
  }
  
  suggested_fa <- suppressWarnings(as.integer(suggested_fa))
  suggested_pc <- suppressWarnings(as.integer(suggested_pc))
  
  if (is.na(suggested_fa) || suggested_fa < 1) suggested_fa <- NA_integer_
  if (is.na(suggested_pc) || suggested_pc < 1) suggested_pc <- NA_integer_
  
  if (parallel_decision_source == "fa") {
    out <- suggested_fa
  } else if (parallel_decision_source == "pc") {
    out <- suggested_pc
  } else if (parallel_decision_source == "both_max") {
    out <- suppressWarnings(max(c(suggested_fa, suggested_pc), na.rm = TRUE))
    if (is.infinite(out)) out <- NA_integer_
  } else {
    stop("parallel_decision_source debe ser 'fa', 'pc' o 'both_max'.")
  }
  
  if (is.na(out) || out < 1) out <- 1L
  
  as.integer(out)
}

run_parallel_analysis <- function(df_efa, scale_id, scale_label) {
  cat("\n====================================================\n")
  cat("Análisis paralelo:", scale_label, "\n")
  cat("====================================================\n")
  
  par_out <- suppressWarnings(
    psych::fa.parallel(
      df_efa,
      fa = parallel_fa_type,
      cor = "poly",
      n.iter = n_iter_parallel,
      show.legend = FALSE,
      plot = plot_parallel
    )
  )
  
  suggested_fa <- suppressWarnings(as.integer(par_out$nfact))
  suggested_pc <- suppressWarnings(as.integer(par_out$ncomp))
  
  suggested_final <- choose_parallel_k(
    suggested_fa = suggested_fa,
    suggested_pc = suggested_pc
  )
  
  max_extractable <- max(1, ncol(df_efa) - 1)
  max_k_to_test <- min(suggested_final, max_extractable)
  
  if (max_k_to_test < 1) max_k_to_test <- 1
  
  cat("Factores sugeridos por FA:", suggested_fa, "\n")
  cat("Componentes sugeridos por PC:", suggested_pc, "\n")
  cat("Criterio usado:", parallel_decision_source, "\n")
  cat("Modelos AFE a probar: solo modelo unidimensional (k = 1)\n")
  
  list(
    par_out = par_out,
    suggested_fa = suggested_fa,
    suggested_pc = suggested_pc,
    decision_source = parallel_decision_source,
    suggested_final = suggested_final,
    max_extractable = max_extractable,
    max_k_to_test = max_k_to_test
  )
}

############################################################
# 5.5. Sintaxis lavaan
############################################################

build_unidimensional_syntax <- function(factor_items) {
  factor_items <- factor_items[sapply(factor_items, length) >= min_items_per_factor]
  
  if (length(factor_items) != 1) {
    return(NA_character_)
  }
  
  paste0(
    names(factor_items)[1],
    " =~ ",
    paste(factor_items[[1]], collapse = " + ")
  )
}

build_correlated_syntax <- function(factor_items) {
  factor_items <- factor_items[sapply(factor_items, length) >= min_items_per_factor]
  
  if (length(factor_items) < 2) {
    return(NA_character_)
  }
  
  factor_lines <- purrr::imap_chr(
    factor_items,
    ~ paste0(.y, " =~ ", paste(.x, collapse = " + "))
  )
  
  cov_lines <- apply(
    combn(names(factor_items), 2),
    2,
    function(z) paste0(z[1], " ~~ ", z[2])
  )
  
  paste(c(factor_lines, cov_lines), collapse = "\n")
}

build_bifactor_syntax <- function(factor_items, scale_id) {
  factor_items <- factor_items[sapply(factor_items, length) >= min_items_per_factor]
  
  if (length(factor_items) < 2) {
    return(NA_character_)
  }
  
  all_items <- unique(unlist(factor_items))
  general_factor <- paste0(scale_id, "_G")
  
  general_line <- paste0(
    general_factor,
    " =~ ",
    paste(all_items, collapse = " + ")
  )
  
  specific_lines <- purrr::imap_chr(
    factor_items,
    ~ paste0(.y, " =~ ", paste(.x, collapse = " + "))
  )
  
  orthogonal_general <- paste0(
    general_factor,
    " ~~ 0*",
    names(factor_items)
  )
  
  orthogonal_specific <- apply(
    combn(names(factor_items), 2),
    2,
    function(z) paste0(z[1], " ~~ 0*", z[2])
  )
  
  paste(
    c(
      general_line,
      specific_lines,
      orthogonal_general,
      orthogonal_specific
    ),
    collapse = "\n"
  )
}

build_second_order_syntax <- function(factor_items, scale_id) {
  factor_items <- factor_items[sapply(factor_items, length) >= min_items_per_factor]
  
  if (length(factor_items) < 2) {
    return(NA_character_)
  }
  
  first_order_lines <- purrr::imap_chr(
    factor_items,
    ~ paste0(.y, " =~ ", paste(.x, collapse = " + "))
  )
  
  so_factor <- paste0(scale_id, "_SO")
  first_order_names <- names(factor_items)
  
  # Con 2 factores de primer orden, se igualan las cargas para ayudar a la identificación.
  if (length(first_order_names) == 2) {
    so_line <- paste0(
      so_factor,
      " =~ a*",
      first_order_names[1],
      " + a*",
      first_order_names[2]
    )
  } else {
    so_line <- paste0(
      so_factor,
      " =~ ",
      paste(first_order_names, collapse = " + ")
    )
  }
  
  paste(c(first_order_lines, so_line), collapse = "\n")
}

build_model_syntax <- function(factor_items, scale_id, model_type) {
  if (model_type == "unidimensional") {
    return(build_unidimensional_syntax(factor_items))
  }
  
  if (model_type == "correlated_factors") {
    return(build_correlated_syntax(factor_items))
  }
  
  if (model_type == "bifactor") {
    return(build_bifactor_syntax(factor_items, scale_id))
  }
  
  if (model_type == "second_order") {
    return(build_second_order_syntax(factor_items, scale_id))
  }
  
  stop("model_type no reconocido: ", model_type)
}

get_model_types_to_try <- function(k) {
  # Versión restringida: solo se estima el modelo unidimensional.
  # Se conserva el nombre de la función porque otros bloques lo usan.
  return("unidimensional")
}

############################################################
# 5.6. Convertir AFE en estructura para CFA
############################################################

extract_loadings_matrix <- function(efa_obj, n_factors) {
  L <- as.matrix(unclass(efa_obj$loadings))
  L <- L[, seq_len(n_factors), drop = FALSE]
  storage.mode(L) <- "numeric"
  L
}

efa_loadings_to_long <- function(efa_obj, scale_id, model_id, n_factors) {
  L <- extract_loadings_matrix(efa_obj, n_factors)
  colnames(L) <- paste0(scale_id, "_F", seq_len(n_factors))
  
  as.data.frame(L) %>%
    rownames_to_column("item") %>%
    pivot_longer(
      cols = -item,
      names_to = "factor",
      values_to = "loading"
    ) %>%
    mutate(
      scale_id = scale_id,
      model_id = model_id,
      n_factors_efa = n_factors,
      abs_loading = abs(loading),
      .before = 1
    )
}

efa_to_cfa_structure <- function(
    efa_obj,
    scale_id,
    model_id,
    n_factors
) {
  L <- extract_loadings_matrix(efa_obj, n_factors)
  absL <- abs(L)
  
  colnames(L) <- paste0(scale_id, "_F", seq_len(n_factors))
  colnames(absL) <- colnames(L)
  
  max_factor_number <- apply(absL, 1, which.max)
  max_loading <- absL[cbind(seq_len(nrow(absL)), max_factor_number)]
  
  second_loading <- if (n_factors >= 2) {
    apply(absL, 1, function(z) sort(z, decreasing = TRUE)[2])
  } else {
    rep(NA_real_, nrow(absL))
  }
  
  diff_to_second <- if (n_factors >= 2) {
    max_loading - second_loading
  } else {
    rep(NA_real_, nrow(absL))
  }
  
  assigned_factor <- paste0(scale_id, "_F", max_factor_number)
  
  keep_loading <- max_loading >= min_loading
  
  keep_diff <- if (n_factors == 1) {
    rep(TRUE, nrow(absL))
  } else {
    diff_to_second > min_diff
  }
  
  keep_initial <- keep_loading & keep_diff
  
  decisions <- tibble(
    scale_id = scale_id,
    model_id = model_id,
    n_factors_efa = n_factors,
    item = rownames(absL),
    assigned_factor_number = max_factor_number,
    assigned_factor = assigned_factor,
    max_loading = max_loading,
    second_loading = second_loading,
    diff_to_second = diff_to_second,
    keep_loading = keep_loading,
    keep_diff = keep_diff,
    keep_initial = keep_initial
  )
  
  factor_counts <- decisions %>%
    filter(keep_initial) %>%
    count(assigned_factor, name = "n_items_factor")
  
  valid_factors <- factor_counts %>%
    filter(n_items_factor >= min_items_per_factor)
  
  decisions <- decisions %>%
    left_join(factor_counts, by = "assigned_factor") %>%
    mutate(
      n_items_factor = ifelse(is.na(n_items_factor), 0, n_items_factor),
      keep_final = keep_initial & assigned_factor %in% valid_factors$assigned_factor,
      decision = case_when(
        !keep_loading ~ paste0("Eliminado: carga máxima < ", min_loading),
        n_factors >= 2 & keep_loading & !keep_diff ~ paste0("Eliminado: diferencia entre cargas <= ", min_diff),
        keep_initial & n_items_factor < min_items_per_factor ~ paste0(
          "Eliminado: factor colapsado con menos de ",
          min_items_per_factor,
          " ítems"
        ),
        keep_final ~ "Conservado",
        TRUE ~ "Eliminado"
      )
    )
  
  kept_items <- decisions %>%
    filter(keep_final) %>%
    arrange(assigned_factor_number, item)
  
  factor_items <- split(kept_items$item, kept_items$assigned_factor)
  factor_items <- factor_items[sapply(factor_items, length) >= min_items_per_factor]
  
  n_factors_retained <- length(factor_items)
  n_items_retained <- length(unique(unlist(factor_items)))
  
  collapsed <- FALSE
  collapse_reason <- NA_character_
  
  if (n_items_retained < min_items_per_factor) {
    collapsed <- TRUE
    collapse_reason <- paste0(
      "Modelo colapsado: menos de ",
      min_items_per_factor,
      " ítems conservados."
    )
  } else if (n_factors_retained < n_factors) {
    collapsed <- TRUE
    collapse_reason <- paste0(
      "Modelo colapsado: AFE solicitó ",
      n_factors,
      " factor(es), pero solo ",
      n_factors_retained,
      " factor(es) tienen al menos ",
      min_items_per_factor,
      " ítems después de aplicar cargas >= ",
      min_loading,
      " y diferencia > ",
      min_diff,
      "."
    )
  }
  
  list(
    scale_id = scale_id,
    model_id = model_id,
    n_factors_efa = n_factors,
    decisions = decisions,
    factor_items = factor_items,
    n_factors_retained = n_factors_retained,
    n_items_retained = n_items_retained,
    collapsed = collapsed,
    collapse_reason = collapse_reason
  )
}

############################################################
# 5.7. Correr AFE por dimensión
############################################################

run_efa_for_scale <- function(data_efa, scale_id, scale_label, items) {
  cat("\n====================================================\n")
  cat("AFE:", scale_label, "\n")
  cat("Base usada: Database_EFA\n")
  cat("====================================================\n")
  
  df_efa <- tryCatch(
    make_items_df(data_efa, items),
    error = function(e) e
  )
  
  if (inherits(df_efa, "error")) {
    msg <- df_efa$message
    cat("No se ejecuta AFE:", msg, "\n")
    
    return(list(
      scale_id = scale_id,
      scale_label = scale_label,
      n_items_available = 0,
      scale_collapsed = TRUE,
      scale_collapse_reason = msg,
      parallel = NULL,
      efa_objects = list(),
      cfa_structures = list(),
      efa_loadings_long = list()
    ))
  }
  
  par_result <- run_parallel_analysis(df_efa, scale_id, scale_label)
  max_k <- 1L
  
  efa_objects <- list()
  cfa_structures <- list()
  efa_loadings_long <- list()
  
  for (k in seq_len(max_k)) {
    model_id <- paste0(scale_id, "_", k, "D")
    
    cat("\n----------------------------------------------------\n")
    cat("Modelo AFE:", model_id, "\n")
    cat("----------------------------------------------------\n")
    
    efa_obj <- tryCatch(
      psych::fa(
        df_efa,
        nfactors = k,
        rotate = ifelse(k == 1, "none", efa_rotation_multidim),
        fm = efa_fm,
        cor = "poly"
      ),
      error = function(e) e
    )
    
    if (inherits(efa_obj, "error")) {
      cat("Falló el AFE:", efa_obj$message, "\n")
      next
    }
    
    print(efa_obj$loadings, cutoff = min_loading)
    
    efa_objects[[model_id]] <- efa_obj
    
    efa_loadings_long[[model_id]] <- efa_loadings_to_long(
      efa_obj = efa_obj,
      scale_id = scale_id,
      model_id = model_id,
      n_factors = k
    )
    
    cfa_structures[[model_id]] <- efa_to_cfa_structure(
      efa_obj = efa_obj,
      scale_id = scale_id,
      model_id = model_id,
      n_factors = k
    )
    
    cat("\nÍtems conservados para CFA:\n")
    print(cfa_structures[[model_id]]$factor_items)
    
    if (isTRUE(cfa_structures[[model_id]]$collapsed)) {
      cat("\nModelo no estimable en CFA:\n")
      cat(cfa_structures[[model_id]]$collapse_reason, "\n")
    }
  }
  
  list(
    scale_id = scale_id,
    scale_label = scale_label,
    n_items_available = ncol(df_efa),
    scale_collapsed = FALSE,
    scale_collapse_reason = NA_character_,
    parallel = par_result,
    efa_objects = efa_objects,
    cfa_structures = cfa_structures,
    efa_loadings_long = efa_loadings_long
  )
}

############################################################
# 5.8. Ejecutar AFE
############################################################

efa_runs <- purrr::imap(
  items_by_scale,
  function(items, scale_id) {
    run_efa_for_scale(
      data_efa = Database_EFA,
      scale_id = scale_id,
      scale_label = scale_labels[[scale_id]],
      items = items
    )
  }
)

parallel_summary_final <- bind_rows(
  lapply(efa_runs, function(x) {
    if (isTRUE(x$scale_collapsed) || is.null(x$parallel)) {
      return(tibble(
        scale_id = x$scale_id,
        scale_label = x$scale_label,
        n_items_available = x$n_items_available,
        scale_collapsed = TRUE,
        scale_collapse_reason = x$scale_collapse_reason,
        parallel_suggested_fa = NA_integer_,
        parallel_suggested_pc = NA_integer_,
        parallel_decision_source = parallel_decision_source,
        parallel_suggested_final = NA_integer_,
        max_extractable = NA_integer_,
        max_k_tested = 0,
        models_tested_in_efa = NA_character_
      ))
    }
    
    tibble(
      scale_id = x$scale_id,
      scale_label = x$scale_label,
      n_items_available = x$n_items_available,
      scale_collapsed = FALSE,
      scale_collapse_reason = NA_character_,
      parallel_suggested_fa = x$parallel$suggested_fa,
      parallel_suggested_pc = x$parallel$suggested_pc,
      parallel_decision_source = x$parallel$decision_source,
      parallel_suggested_final = x$parallel$suggested_final,
      max_extractable = x$parallel$max_extractable,
      max_k_tested = x$parallel$max_k_to_test,
      models_tested_in_efa = paste0(
        x$scale_id,
        "_",
        seq_len(x$parallel$max_k_to_test),
        "D",
        collapse = ", "
      )
    )
  })
)

cfa_structures <- unlist(
  lapply(efa_runs, function(x) x$cfa_structures),
  recursive = FALSE
)

if (length(cfa_structures) > 0) {
  names(cfa_structures) <- vapply(
    cfa_structures,
    function(x) x$model_id,
    character(1)
  )
}

efa_loadings_final <- bind_rows(
  unlist(
    lapply(efa_runs, function(x) x$efa_loadings_long),
    recursive = FALSE
  )
) %>%
  round_numeric_df(4)

efa_item_decisions_final <- bind_rows(
  lapply(cfa_structures, function(x) x$decisions)
) %>%
  round_numeric_df(4)

efa_model_summary_final <- bind_rows(
  lapply(cfa_structures, function(x) {
    tibble(
      scale_id = x$scale_id,
      model_id = x$model_id,
      n_factors_efa = x$n_factors_efa,
      n_factors_retained_for_cfa = x$n_factors_retained,
      n_items_retained_for_cfa = x$n_items_retained,
      collapsed = x$collapsed,
      collapse_reason = x$collapse_reason,
      items_by_factor = paste_items_by_factor(x$factor_items)
    )
  })
)

cat("\nResumen del análisis paralelo:\n")
print(parallel_summary_final)

cat("\nResumen de modelos AFE generados:\n")
print(efa_model_summary_final)

cat("\nDecisiones por ítem en AFE:\n")
print(efa_item_decisions_final)

############################################################
# 5.9. Preparar datos CFA
############################################################

prepare_cfa_data <- function(data_cfa, items) {
  items_ok <- intersect(items, names(data_cfa))
  
  if (length(items_ok) < min_items_per_factor) {
    stop(
      "Menos de ",
      min_items_per_factor,
      " ítems del modelo están presentes en Database_CFA."
    )
  }
  
  df <- data_cfa[, items_ok, drop = FALSE]
  df <- as.data.frame(lapply(df, to_numeric_safe))
  names(df) <- items_ok
  
  keep_variability <- sapply(df, function(x) {
    x2 <- x[!is.na(x)]
    length(unique(x2)) >= 2
  })
  
  df <- df[, keep_variability, drop = FALSE]
  
  if (ncol(df) < min_items_per_factor) {
    stop(
      "Menos de ",
      min_items_per_factor,
      " ítems tienen variabilidad en Database_CFA."
    )
  }
  
  df
}

############################################################
# 5.10. Extracciones de CFA
############################################################

extract_fit_summary <- function(
    fit,
    scale_id,
    model_id,
    cfa_model_id,
    model_type,
    syntax,
    n_items,
    n_factors_efa
) {
  converged <- isTRUE(safe_lavinspect(fit, "converged", FALSE))
  post_check <- isTRUE(safe_lavinspect(fit, "post.check", FALSE))
  
  pe <- get_parameter_estimates_safe(fit)
  
  heywood_flag <- FALSE
  
  if (nrow(pe) > 0) {
    heywood_flag <- any(
      pe$op == "~~" &
        pe$lhs == pe$rhs &
        !is.na(pe$est) &
        pe$est < 0
    )
  }
  
  tibble(
    scale_id = scale_id,
    model_id = model_id,
    cfa_model_id = cfa_model_id,
    model_type = model_type,
    n_factors_efa = n_factors_efa,
    n_items = n_items,
    cfa_converged = converged,
    post_check = post_check,
    heywood_flag = heywood_flag,
    chisq = safe_fit_measure(fit, "chisq.scaled", "chisq"),
    df = safe_fit_measure(fit, "df.scaled", "df"),
    pvalue = safe_fit_measure(fit, "pvalue.scaled", "pvalue"),
    cfi = safe_fit_measure(fit, "cfi.scaled", "cfi"),
    tli = safe_fit_measure(fit, "tli.scaled", "tli"),
    rmsea = safe_fit_measure(fit, "rmsea.scaled", "rmsea"),
    rmsea_ci_lower = safe_fit_measure(fit, "rmsea.ci.lower.scaled", "rmsea.ci.lower"),
    rmsea_ci_upper = safe_fit_measure(fit, "rmsea.ci.upper.scaled", "rmsea.ci.upper"),
    srmr = safe_fit_measure(fit, "srmr"),
    aic = safe_fit_measure(fit, "aic"),
    bic = safe_fit_measure(fit, "bic"),
    model_syntax = syntax,
    cfa_status = ifelse(converged, "converged", "not_converged"),
    cfa_note = ifelse(
      converged,
      NA_character_,
      "lavaan no reportó convergencia para este modelo."
    )
  )
}

extract_standardized_loadings <- function(fit, scale_id, model_id, cfa_model_id, model_type) {
  pe <- get_parameter_estimates_safe(fit)
  
  if (nrow(pe) == 0) return(tibble())
  
  pe %>%
    filter(op == "=~") %>%
    transmute(
      scale_id = scale_id,
      model_id = model_id,
      cfa_model_id = cfa_model_id,
      model_type = model_type,
      factor = lhs,
      item = rhs,
      est = est,
      se = se,
      z = z,
      pvalue = pvalue,
      std_loading = std.all,
      abs_std_loading = abs(std.all)
    )
}

extract_latent_correlations <- function(fit, scale_id, model_id, cfa_model_id, model_type) {
  cor_lv <- tryCatch(
    lavaan::lavInspect(fit, "cor.lv"),
    error = function(e) NULL
  )
  
  if (is.null(cor_lv)) return(tibble())
  if (is.list(cor_lv)) cor_lv <- cor_lv[[1]]
  if (is.null(dim(cor_lv)) || ncol(cor_lv) < 2) return(tibble())
  
  pairs <- t(combn(colnames(cor_lv), 2))
  
  tibble(
    scale_id = scale_id,
    model_id = model_id,
    cfa_model_id = cfa_model_id,
    model_type = model_type,
    factor_1 = pairs[, 1],
    factor_2 = pairs[, 2],
    correlation = mapply(
      function(a, b) cor_lv[a, b],
      pairs[, 1],
      pairs[, 2]
    ),
    abs_correlation = abs(correlation),
    latent_overlap_flag = abs_correlation >= latent_overlap_cutoff,
    latent_overlap_severe_flag = abs_correlation >= latent_overlap_severe_cutoff
  )
}

extract_residual_mi <- function(
    fit,
    scale_id,
    model_id,
    cfa_model_id,
    model_type,
    observed_items
) {
  mi <- tryCatch(
    lavaan::modificationIndices(
      fit,
      standardized = TRUE,
      sort. = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(mi)) return(tibble())
  
  mi <- as_tibble(mi)
  mi <- add_missing_cols(
    mi,
    c("lhs", "op", "rhs", "mi", "epc", "sepc.lv", "sepc.all", "sepc.nox")
  )
  
  mi %>%
    filter(
      op == "~~",
      lhs %in% observed_items,
      rhs %in% observed_items,
      lhs != rhs,
      mi >= mi_resid_cutoff
    ) %>%
    arrange(desc(mi)) %>%
    transmute(
      scale_id = scale_id,
      model_id = model_id,
      cfa_model_id = cfa_model_id,
      model_type = model_type,
      item_1 = lhs,
      item_2 = rhs,
      mi = mi,
      epc = epc,
      sepc_lv = sepc.lv,
      sepc_all = sepc.all,
      sepc_nox = sepc.nox
    )
}

extract_high_residual_correlations <- function(fit, scale_id, model_id, cfa_model_id, model_type) {
  res <- tryCatch(
    lavaan::lavResiduals(fit, type = "cor"),
    error = function(e) NULL
  )
  
  if (is.null(res)) return(tibble())
  
  R <- NULL
  
  if (!is.null(res$cov)) {
    R <- res$cov
  } else if (!is.null(res[[1]]$cov)) {
    R <- res[[1]]$cov
  }
  
  if (is.null(R) || is.null(dim(R)) || ncol(R) < 2) return(tibble())
  
  pairs <- t(combn(colnames(R), 2))
  
  out <- tibble(
    scale_id = scale_id,
    model_id = model_id,
    cfa_model_id = cfa_model_id,
    model_type = model_type,
    item_1 = pairs[, 1],
    item_2 = pairs[, 2],
    residual_correlation = mapply(
      function(a, b) R[a, b],
      pairs[, 1],
      pairs[, 2]
    ),
    abs_residual_correlation = abs(residual_correlation)
  )
  
  out %>%
    filter(abs_residual_correlation >= resid_cor_cutoff) %>%
    arrange(desc(abs_residual_correlation))
}

############################################################
# 5.11. Métricas bifactor
############################################################

empty_bifactor_metrics <- function() {
  tibble::tibble(
    ECV_general = NA_real_,
    PUC = NA_real_,
    omega_hierarchical = NA_real_,
    omega_total = NA_real_,
    omega_relative = NA_real_,
    ECV_specific_mean = NA_real_,
    ECV_specific_max = NA_real_
  )
}

compute_bifactor_indices <- function(fit, factor_items, scale_id) {
  
  general_factor <- paste0(scale_id, "_G")
  all_items <- unique(unlist(factor_items))
  specific_factors <- names(factor_items)
  all_factors <- c(general_factor, specific_factors)
  
  pe <- get_parameter_estimates_safe(fit)
  
  if (nrow(pe) == 0) {
    return(empty_bifactor_metrics())
  }
  
  loading_tbl <- pe %>%
    dplyr::filter(
      op == "=~",
      lhs %in% all_factors,
      rhs %in% all_items
    ) %>%
    dplyr::select(lhs, rhs, std.all)
  
  if (nrow(loading_tbl) == 0) {
    return(empty_bifactor_metrics())
  }
  
  L <- matrix(
    0,
    nrow = length(all_items),
    ncol = length(all_factors),
    dimnames = list(all_items, all_factors)
  )
  
  for (i in seq_len(nrow(loading_tbl))) {
    L[loading_tbl$rhs[i], loading_tbl$lhs[i]] <- loading_tbl$std.all[i]
  }
  
  common_var_by_factor <- colSums(L^2, na.rm = TRUE)
  total_common_var <- sum(common_var_by_factor, na.rm = TRUE)
  
  ECV_general <- ifelse(
    total_common_var > 0,
    common_var_by_factor[general_factor] / total_common_var,
    NA_real_
  )
  
  ECV_specific_values <- common_var_by_factor[specific_factors] / total_common_var
  
  n_items <- length(all_items)
  total_pairs <- choose(n_items, 2)
  
  within_specific_pairs <- sum(
    sapply(
      factor_items,
      function(x) {
        if (length(x) >= 2) {
          choose(length(x), 2)
        } else {
          0
        }
      }
    )
  )
  
  PUC <- ifelse(
    total_pairs > 0,
    (total_pairs - within_specific_pairs) / total_pairs,
    NA_real_
  )
  
  resid_tbl <- pe %>%
    dplyr::filter(
      op == "~~",
      lhs == rhs,
      lhs %in% all_items
    ) %>%
    dplyr::select(lhs, std.all)
  
  theta <- rep(NA_real_, length(all_items))
  names(theta) <- all_items
  
  if (nrow(resid_tbl) > 0) {
    theta[resid_tbl$lhs] <- resid_tbl$std.all
  }
  
  missing_theta <- is.na(theta)
  
  if (any(missing_theta)) {
    theta[missing_theta] <- 1 - rowSums(
      L[missing_theta, , drop = FALSE]^2,
      na.rm = TRUE
    )
  }
  
  theta[theta < 0] <- 0
  
  Sigma_std <- L %*% t(L) + diag(theta, nrow = length(theta))
  total_score_var <- sum(Sigma_std, na.rm = TRUE)
  
  general_score_var <- sum(L[, general_factor], na.rm = TRUE)^2
  common_score_var <- sum(colSums(L, na.rm = TRUE)^2, na.rm = TRUE)
  
  omega_hierarchical <- ifelse(
    total_score_var > 0,
    general_score_var / total_score_var,
    NA_real_
  )
  
  omega_total <- ifelse(
    total_score_var > 0,
    common_score_var / total_score_var,
    NA_real_
  )
  
  omega_relative <- ifelse(
    !is.na(omega_total) && omega_total > 0,
    omega_hierarchical / omega_total,
    NA_real_
  )
  
  tibble::tibble(
    ECV_general = as.numeric(ECV_general),
    PUC = as.numeric(PUC),
    omega_hierarchical = as.numeric(omega_hierarchical),
    omega_total = as.numeric(omega_total),
    omega_relative = as.numeric(omega_relative),
    ECV_specific_mean = ifelse(
      length(ECV_specific_values) > 0,
      mean(as.numeric(ECV_specific_values), na.rm = TRUE),
      NA_real_
    ),
    ECV_specific_max = ifelse(
      length(ECV_specific_values) > 0,
      max(as.numeric(ECV_specific_values), na.rm = TRUE),
      NA_real_
    )
  )
}

############################################################
# 5.12. Correr CFA por estructura derivada del AFE
############################################################

run_cfa_for_structure <- function(structure) {
  scale_id <- structure$scale_id
  model_id <- structure$model_id
  k <- structure$n_factors_efa
  factor_items <- structure$factor_items
  
  base_items <- unique(unlist(factor_items))
  
  if (isTRUE(structure$collapsed)) {
    return(list(
      fit_rows = tibble(
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = paste0(model_id, "_not_estimable"),
        model_type = NA_character_,
        n_factors_efa = k,
        n_items = length(base_items),
        cfa_converged = FALSE,
        post_check = FALSE,
        heywood_flag = NA,
        chisq = NA_real_,
        df = NA_real_,
        pvalue = NA_real_,
        cfi = NA_real_,
        tli = NA_real_,
        rmsea = NA_real_,
        rmsea_ci_lower = NA_real_,
        rmsea_ci_upper = NA_real_,
        srmr = NA_real_,
        aic = NA_real_,
        bic = NA_real_,
        model_syntax = NA_character_,
        cfa_status = "not_estimable_collapsed",
        cfa_note = structure$collapse_reason
      ),
      syntax_rows = tibble(
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = paste0(model_id, "_not_estimable"),
        model_type = NA_character_,
        model_syntax = NA_character_,
        syntax_status = "not_created",
        syntax_note = structure$collapse_reason
      ),
      loadings = tibble(),
      lat_cor = tibble(),
      residual_mi = tibble(),
      residual_cor = tibble(),
      bifactor_metrics = tibble(),
      fit_objects = list()
    ))
  }
  
  df_cfa <- tryCatch(
    prepare_cfa_data(Database_CFA, base_items),
    error = function(e) e
  )
  
  if (inherits(df_cfa, "error")) {
    return(list(
      fit_rows = tibble(
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = paste0(model_id, "_not_estimable_cfa_data"),
        model_type = NA_character_,
        n_factors_efa = k,
        n_items = length(base_items),
        cfa_converged = FALSE,
        post_check = FALSE,
        heywood_flag = NA,
        chisq = NA_real_,
        df = NA_real_,
        pvalue = NA_real_,
        cfi = NA_real_,
        tli = NA_real_,
        rmsea = NA_real_,
        rmsea_ci_lower = NA_real_,
        rmsea_ci_upper = NA_real_,
        srmr = NA_real_,
        aic = NA_real_,
        bic = NA_real_,
        model_syntax = NA_character_,
        cfa_status = "not_estimable_cfa_data",
        cfa_note = df_cfa$message
      ),
      syntax_rows = tibble(
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = paste0(model_id, "_not_estimable_cfa_data"),
        model_type = NA_character_,
        model_syntax = NA_character_,
        syntax_status = "not_created",
        syntax_note = df_cfa$message
      ),
      loadings = tibble(),
      lat_cor = tibble(),
      residual_mi = tibble(),
      residual_cor = tibble(),
      bifactor_metrics = tibble(),
      fit_objects = list()
    ))
  }
  
  model_types <- get_model_types_to_try(k)
  
  fit_rows <- list()
  syntax_rows <- list()
  loading_rows <- list()
  latcor_rows <- list()
  mi_rows <- list()
  resid_rows <- list()
  bifactor_rows <- list()
  fit_objects <- list()
  
  for (model_type in model_types) {
    cfa_model_id <- paste0(model_id, "_", model_type)
    
    syntax <- tryCatch(
      build_model_syntax(
        factor_items = factor_items,
        scale_id = scale_id,
        model_type = model_type
      ),
      error = function(e) NA_character_
    )
    
    if (is.na(syntax) || nchar(syntax) == 0) {
      syntax_rows[[cfa_model_id]] <- tibble(
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = cfa_model_id,
        model_type = model_type,
        model_syntax = NA_character_,
        syntax_status = "not_created",
        syntax_note = "Sintaxis no creada por estructura insuficiente."
      )
      
      fit_rows[[cfa_model_id]] <- tibble(
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = cfa_model_id,
        model_type = model_type,
        n_factors_efa = k,
        n_items = length(base_items),
        cfa_converged = FALSE,
        post_check = FALSE,
        heywood_flag = NA,
        chisq = NA_real_,
        df = NA_real_,
        pvalue = NA_real_,
        cfi = NA_real_,
        tli = NA_real_,
        rmsea = NA_real_,
        rmsea_ci_lower = NA_real_,
        rmsea_ci_upper = NA_real_,
        srmr = NA_real_,
        aic = NA_real_,
        bic = NA_real_,
        model_syntax = NA_character_,
        cfa_status = "not_estimable_syntax",
        cfa_note = "Sintaxis no creada por estructura insuficiente."
      )
      
      next
    }
    
    syntax_rows[[cfa_model_id]] <- tibble(
      scale_id = scale_id,
      model_id = model_id,
      cfa_model_id = cfa_model_id,
      model_type = model_type,
      model_syntax = syntax,
      syntax_status = "created",
      syntax_note = NA_character_
    )
    
    cat("\n----------------------------------------------------\n")
    cat("CFA:", cfa_model_id, "\n")
    cat("----------------------------------------------------\n")
    cat(syntax, "\n")
    
    fit <- tryCatch(
      lavaan::cfa(
        model = syntax,
        data = df_cfa,
        ordered = names(df_cfa),
        estimator = cfa_estimator,
        parameterization = cfa_parameterization,
        std.lv = TRUE,
        warn = TRUE
      ),
      error = function(e) e
    )
    
    if (inherits(fit, "error")) {
      fit_rows[[cfa_model_id]] <- tibble(
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = cfa_model_id,
        model_type = model_type,
        n_factors_efa = k,
        n_items = ncol(df_cfa),
        cfa_converged = FALSE,
        post_check = FALSE,
        heywood_flag = NA,
        chisq = NA_real_,
        df = NA_real_,
        pvalue = NA_real_,
        cfi = NA_real_,
        tli = NA_real_,
        rmsea = NA_real_,
        rmsea_ci_lower = NA_real_,
        rmsea_ci_upper = NA_real_,
        srmr = NA_real_,
        aic = NA_real_,
        bic = NA_real_,
        model_syntax = syntax,
        cfa_status = "not_converged_error",
        cfa_note = fit$message
      )
      
      next
    }
    
    fit_objects[[cfa_model_id]] <- fit
    
    fit_rows[[cfa_model_id]] <- extract_fit_summary(
      fit = fit,
      scale_id = scale_id,
      model_id = model_id,
      cfa_model_id = cfa_model_id,
      model_type = model_type,
      syntax = syntax,
      n_items = ncol(df_cfa),
      n_factors_efa = k
    )
    
    loading_rows[[cfa_model_id]] <- extract_standardized_loadings(
      fit = fit,
      scale_id = scale_id,
      model_id = model_id,
      cfa_model_id = cfa_model_id,
      model_type = model_type
    )
    
    latcor_rows[[cfa_model_id]] <- extract_latent_correlations(
      fit = fit,
      scale_id = scale_id,
      model_id = model_id,
      cfa_model_id = cfa_model_id,
      model_type = model_type
    )
    
    mi_rows[[cfa_model_id]] <- extract_residual_mi(
      fit = fit,
      scale_id = scale_id,
      model_id = model_id,
      cfa_model_id = cfa_model_id,
      model_type = model_type,
      observed_items = names(df_cfa)
    )
    
    resid_rows[[cfa_model_id]] <- extract_high_residual_correlations(
      fit = fit,
      scale_id = scale_id,
      model_id = model_id,
      cfa_model_id = cfa_model_id,
      model_type = model_type
    )
    
    if (model_type == "bifactor") {
      bifactor_rows[[cfa_model_id]] <- compute_bifactor_indices(
        fit = fit,
        factor_items = factor_items,
        scale_id = scale_id
      ) %>%
        mutate(
          scale_id = scale_id,
          model_id = model_id,
          cfa_model_id = cfa_model_id,
          model_type = model_type,
          .before = 1
        )
    }
  }
  
  list(
    fit_rows = bind_rows(fit_rows),
    syntax_rows = bind_rows(syntax_rows),
    loadings = bind_rows(loading_rows),
    lat_cor = bind_rows(latcor_rows),
    residual_mi = bind_rows(mi_rows),
    residual_cor = bind_rows(resid_rows),
    bifactor_metrics = bind_rows(bifactor_rows),
    fit_objects = fit_objects
  )
}

############################################################
# 5.13. Ejecutar CFA
############################################################

cfa_runs <- lapply(cfa_structures, run_cfa_for_structure)

cfa_fit_final <- bind_rows(lapply(cfa_runs, function(x) x$fit_rows)) %>%
  round_numeric_df(4)

cfa_model_syntax_final <- bind_rows(lapply(cfa_runs, function(x) x$syntax_rows))

cfa_standardized_loadings_final <- bind_rows(lapply(cfa_runs, function(x) x$loadings)) %>%
  round_numeric_df(4) %>%
  ensure_empty_cols(
    c(
      "scale_id", "model_id", "cfa_model_id", "model_type",
      "factor", "item", "est", "se", "z", "pvalue",
      "std_loading", "abs_std_loading"
    )
  )

cfa_latent_correlations_final <- bind_rows(lapply(cfa_runs, function(x) x$lat_cor)) %>%
  round_numeric_df(4) %>%
  ensure_empty_cols(
    c(
      "scale_id", "model_id", "cfa_model_id", "model_type",
      "factor_1", "factor_2", "correlation", "abs_correlation",
      "latent_overlap_flag", "latent_overlap_severe_flag"
    )
  )

cfa_residual_mi_final <- bind_rows(lapply(cfa_runs, function(x) x$residual_mi)) %>%
  round_numeric_df(4) %>%
  ensure_empty_cols(
    c(
      "scale_id", "model_id", "cfa_model_id", "model_type",
      "item_1", "item_2", "mi", "epc", "sepc_lv", "sepc_all", "sepc_nox"
    )
  )

cfa_residual_correlations_final <- bind_rows(lapply(cfa_runs, function(x) x$residual_cor)) %>%
  round_numeric_df(4) %>%
  ensure_empty_cols(
    c(
      "scale_id", "model_id", "cfa_model_id", "model_type",
      "item_1", "item_2", "residual_correlation", "abs_residual_correlation"
    )
  )

bifactor_metrics_final <- bind_rows(lapply(cfa_runs, function(x) x$bifactor_metrics)) %>%
  round_numeric_df(4) %>%
  ensure_empty_cols(
    c(
      "scale_id", "model_id", "cfa_model_id", "model_type",
      "ECV_general", "PUC", "omega_hierarchical", "omega_total",
      "omega_relative", "ECV_specific_mean", "ECV_specific_max"
    )
  )

cfa_fit_objects <- unlist(
  lapply(cfa_runs, function(x) x$fit_objects),
  recursive = FALSE
)

############################################################
# 5.14. Diagnósticos finales
# VERSIÓN CORREGIDA PARA MODELOS UNIDIMENSIONALES
#
# Motivo del ajuste:
# En modelos unidimensionales, cfa_latent_correlations_final puede quedar vacío
# porque no existen correlaciones entre factores latentes. Cuando una tabla está
# vacía, algunas columnas clave pueden quedar como logical en lugar de character,
# generando error en left_join().
############################################################

# ==========================================================
# 5.14.1. Funciones auxiliares para asegurar tipos
# ==========================================================

key_cols_cfa <- c("scale_id", "model_id", "cfa_model_id", "model_type")

ensure_cfa_key_types <- function(df, key_cols = key_cols_cfa) {
  
  if (is.null(df) || !is.data.frame(df)) {
    df <- tibble::tibble()
  } else {
    df <- tibble::as_tibble(df)
  }
  
  for (kk in key_cols) {
    if (!kk %in% names(df)) {
      df[[kk]] <- character()
    }
    df[[kk]] <- as.character(df[[kk]])
  }
  
  df
}

# Asegurar que las columnas clave tengan el mismo tipo en todas las tablas
cfa_fit_final <- ensure_cfa_key_types(cfa_fit_final)

cfa_standardized_loadings_final <- ensure_cfa_key_types(
  cfa_standardized_loadings_final
)

cfa_latent_correlations_final <- ensure_cfa_key_types(
  cfa_latent_correlations_final
)

cfa_residual_correlations_final <- ensure_cfa_key_types(
  cfa_residual_correlations_final
)

# Asegurar variables lógicas en cfa_fit_final
cfa_fit_final <- cfa_fit_final %>%
  mutate(
    cfa_converged = as.logical(cfa_converged),
    post_check = as.logical(post_check),
    heywood_flag = as.logical(heywood_flag)
  )

# ==========================================================
# 5.14.2. Diagnóstico de cargas factoriales
# ==========================================================

if (
  nrow(cfa_standardized_loadings_final) == 0 ||
  !"abs_std_loading" %in% names(cfa_standardized_loadings_final)
) {
  
  loading_diagnostics <- cfa_fit_final %>%
    select(all_of(key_cols_cfa)) %>%
    distinct() %>%
    mutate(
      min_abs_std_loading = NA_real_,
      max_abs_std_loading = NA_real_,
      n_loadings_below_40 = NA_integer_,
      n_loadings_above_1 = NA_integer_
    )
  
} else {
  
  loading_diagnostics <- cfa_standardized_loadings_final %>%
    mutate(
      abs_std_loading = suppressWarnings(as.numeric(abs_std_loading))
    ) %>%
    group_by(across(all_of(key_cols_cfa))) %>%
    summarise(
      min_abs_std_loading = ifelse(
        all(is.na(abs_std_loading)),
        NA_real_,
        suppressWarnings(min(abs_std_loading, na.rm = TRUE))
      ),
      max_abs_std_loading = ifelse(
        all(is.na(abs_std_loading)),
        NA_real_,
        suppressWarnings(max(abs_std_loading, na.rm = TRUE))
      ),
      n_loadings_below_40 = sum(abs_std_loading < 0.40, na.rm = TRUE),
      n_loadings_above_1 = sum(abs_std_loading > 1, na.rm = TRUE),
      .groups = "drop"
    )
}

loading_diagnostics <- ensure_cfa_key_types(loading_diagnostics)

# ==========================================================
# 5.14.3. Diagnóstico de correlaciones latentes
# En modelos unidimensionales esta tabla suele estar vacía.
# ==========================================================

if (
  nrow(cfa_latent_correlations_final) == 0 ||
  !"abs_correlation" %in% names(cfa_latent_correlations_final)
) {
  
  latent_diagnostics <- cfa_fit_final %>%
    select(all_of(key_cols_cfa)) %>%
    distinct() %>%
    mutate(
      max_abs_latent_correlation = NA_real_,
      any_latent_overlap_85 = FALSE,
      any_latent_overlap_95 = FALSE
    )
  
} else {
  
  latent_diagnostics <- cfa_latent_correlations_final %>%
    mutate(
      abs_correlation = suppressWarnings(as.numeric(abs_correlation)),
      latent_overlap_flag = as.logical(latent_overlap_flag),
      latent_overlap_severe_flag = as.logical(latent_overlap_severe_flag)
    ) %>%
    group_by(across(all_of(key_cols_cfa))) %>%
    summarise(
      max_abs_latent_correlation = ifelse(
        all(is.na(abs_correlation)),
        NA_real_,
        suppressWarnings(max(abs_correlation, na.rm = TRUE))
      ),
      any_latent_overlap_85 = any(latent_overlap_flag, na.rm = TRUE),
      any_latent_overlap_95 = any(latent_overlap_severe_flag, na.rm = TRUE),
      .groups = "drop"
    )
}

latent_diagnostics <- ensure_cfa_key_types(latent_diagnostics)

# ==========================================================
# 5.14.4. Diagnóstico de correlaciones residuales
# ==========================================================

if (
  nrow(cfa_residual_correlations_final) == 0 ||
  !"abs_residual_correlation" %in% names(cfa_residual_correlations_final)
) {
  
  residual_diagnostics <- cfa_fit_final %>%
    select(all_of(key_cols_cfa)) %>%
    distinct() %>%
    mutate(
      max_abs_residual_correlation = NA_real_,
      n_high_residual_correlations = 0L
    )
  
} else {
  
  residual_diagnostics <- cfa_residual_correlations_final %>%
    mutate(
      abs_residual_correlation = suppressWarnings(as.numeric(abs_residual_correlation))
    ) %>%
    group_by(across(all_of(key_cols_cfa))) %>%
    summarise(
      max_abs_residual_correlation = ifelse(
        all(is.na(abs_residual_correlation)),
        NA_real_,
        suppressWarnings(max(abs_residual_correlation, na.rm = TRUE))
      ),
      n_high_residual_correlations = dplyr::n(),
      .groups = "drop"
    )
}

residual_diagnostics <- ensure_cfa_key_types(residual_diagnostics)

# ==========================================================
# 5.14.5. Diagnóstico final integrado
# ==========================================================

cfa_diagnostics_final <- cfa_fit_final %>%
  left_join(
    loading_diagnostics,
    by = key_cols_cfa
  ) %>%
  left_join(
    latent_diagnostics,
    by = key_cols_cfa
  ) %>%
  left_join(
    residual_diagnostics,
    by = key_cols_cfa
  ) %>%
  mutate(
    n_loadings_above_1 = ifelse(
      is.na(n_loadings_above_1),
      0L,
      as.integer(n_loadings_above_1)
    ),
    n_loadings_below_40 = ifelse(
      is.na(n_loadings_below_40),
      0L,
      as.integer(n_loadings_below_40)
    ),
    n_high_residual_correlations = ifelse(
      is.na(n_high_residual_correlations),
      0L,
      as.integer(n_high_residual_correlations)
    ),
    any_latent_overlap_85 = ifelse(
      is.na(any_latent_overlap_85),
      FALSE,
      as.logical(any_latent_overlap_85)
    ),
    any_latent_overlap_95 = ifelse(
      is.na(any_latent_overlap_95),
      FALSE,
      as.logical(any_latent_overlap_95)
    ),
    admissible_model = cfa_converged == TRUE &
      post_check == TRUE &
      heywood_flag == FALSE &
      n_loadings_above_1 == 0,
    warning_note = case_when(
      !is.na(cfa_status) & cfa_status != "converged" ~ cfa_note,
      heywood_flag == TRUE ~ "Modelo con posible Heywood: varianza residual negativa.",
      post_check == FALSE ~ "Modelo converge, pero no pasa post.check.",
      n_loadings_above_1 > 0 ~ "Carga estandarizada > 1.",
      any_latent_overlap_95 == TRUE ~ "Correlación latente >= .95: posible redundancia factorial.",
      any_latent_overlap_85 == TRUE ~ "Correlación latente >= .85: revisar solapamiento factorial.",
      TRUE ~ NA_character_
    )
  ) %>%
  round_numeric_df(4)

cat("\nDiagnósticos CFA finales corregidos:\n")
print(cfa_diagnostics_final)



############################################################
# 5.15. Exportar resultados
############################################################

wb <- createWorkbook()

addWorksheet(wb, "Parallel_summary")
writeData(wb, "Parallel_summary", parallel_summary_final)

addWorksheet(wb, "EFA_model_summary")
writeData(wb, "EFA_model_summary", efa_model_summary_final)

addWorksheet(wb, "EFA_loadings_long")
writeData(wb, "EFA_loadings_long", efa_loadings_final)

addWorksheet(wb, "EFA_item_decisions")
writeData(wb, "EFA_item_decisions", efa_item_decisions_final)

addWorksheet(wb, "CFA_model_syntax")
writeData(wb, "CFA_model_syntax", cfa_model_syntax_final)

addWorksheet(wb, "CFA_fit_comparison")
writeData(wb, "CFA_fit_comparison", cfa_fit_final)

addWorksheet(wb, "CFA_std_loadings")
writeData(wb, "CFA_std_loadings", cfa_standardized_loadings_final)

addWorksheet(wb, "CFA_latent_correlations")
writeData(wb, "CFA_latent_correlations", cfa_latent_correlations_final)

addWorksheet(wb, "CFA_residual_MI")
writeData(wb, "CFA_residual_MI", cfa_residual_mi_final)

addWorksheet(wb, "CFA_residual_correlations")
writeData(wb, "CFA_residual_correlations", cfa_residual_correlations_final)

addWorksheet(wb, "Bifactor_metrics")
writeData(wb, "Bifactor_metrics", bifactor_metrics_final)

addWorksheet(wb, "CFA_diagnostics")
writeData(wb, "CFA_diagnostics", cfa_diagnostics_final)

saveWorkbook(wb, output_excel, overwrite = TRUE)

############################################################
# 5.16. Mensaje final
############################################################

cat("\n====================================================\n")
cat("ANÁLISIS AFE + CFA COMPLETADO\n")
cat("====================================================\n")
cat("Archivo exportado:", output_excel, "\n\n")

cat("Hojas exportadas:\n")
cat("- Parallel_summary: número sugerido por FA y PC; se fuerza k = 1 para el análisis.\n")
cat("- EFA_model_summary: factores retenidos, ítems retenidos y modelos colapsados.\n")
cat("- EFA_loadings_long: cargas AFE en formato largo.\n")
cat("- EFA_item_decisions: decisión por ítem según carga, diferencia y colapso factorial.\n")
cat("- CFA_model_syntax: sintaxis lavaan de cada modelo estimado/no estimado.\n")
cat("- CFA_fit_comparison: índices de ajuste CFA.\n")
cat("- CFA_std_loadings: cargas estandarizadas.\n")
cat("- CFA_latent_correlations: correlaciones latentes.\n")
cat("- CFA_residual_MI: índices de modificación para errores correlacionados.\n")
cat("- CFA_residual_correlations: residuos correlacionales altos.\n")
cat("- Bifactor_metrics: ECV, PUC y omegas para modelos bifactor.\n")
cat("- CFA_diagnostics: convergencia, Heywood, cargas > 1 y alertas.\n")

cat("\nCriterios aplicados:\n")
cat("- Carga mínima:", min_loading, "\n")
cat("- Diferencia mínima entre cargas:", min_diff, "\n")
cat("- Mínimo de ítems por factor en CFA:", min_items_per_factor, "\n")
cat("- Criterio aplicado para modelos:", parallel_decision_source, "\n")





























############################################################
# 6. CFA ITERATIVO POR ERRORES CORRELACIONADOS
#    SOLO MODELOS UNIDIMENSIONALES
# Base: Database_CFA
# Estructuras CFA: cfa_structures, generadas en la sección 5
#
# Regla:
# 1) Ajusta únicamente el modelo CFA unidimensional de cada dominio.
# 2) Si no cumple criterios de ajuste:
#    CFI > .95, TLI > .95, RMSEA < .08, SRMR < .08
# 3) Identifica el mayor índice de modificación para error correlacionado:
#    item_i ~~ item_j
# 4) Entre esos dos ítems, elimina el ítem con mayor carga factorial
#    estandarizada absoluta.
# 5) Recorre el CFA hasta lograr ajuste adecuado o hasta que el dominio
#    quede con < 3 ítems.
#
# Salida:
# CFA_correlated_errors_iterative_unidimensional_results.xlsx
############################################################

############################################################
# 6.0. Paquetes mínimos
############################################################

required_packages_ce <- c(
  "lavaan",
  "psych",
  "dplyr",
  "purrr",
  "tibble",
  "tidyr",
  "openxlsx",
  "haven"
)

missing_packages_ce <- required_packages_ce[
  !sapply(required_packages_ce, requireNamespace, quietly = TRUE)
]

if (length(missing_packages_ce) > 0) {
  stop(
    "Instala los paquetes faltantes antes de correr esta sección: ",
    paste0(
      "install.packages(c('",
      paste(missing_packages_ce, collapse = "', '"),
      "'))"
    )
  )
}

suppressPackageStartupMessages({
  library(lavaan)
  library(psych)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(tidyr)
  library(openxlsx)
  library(haven)
})

############################################################
# 6.1. Verificaciones
############################################################

if (!exists("Database_CFA")) {
  stop("No existe Database_CFA. Corre primero la sección donde divides Database_EFA y Database_CFA.")
}

if (!exists("cfa_structures")) {
  stop("No existe cfa_structures. Corre primero la sección 5 de AFE + AFC automático.")
}

if (!exists("min_items_per_factor")) {
  min_items_per_factor <- 3
}

############################################################
# 6.2. Parámetros del procedimiento
############################################################

# Criterios de ajuste
fit_cfi_cutoff_ce   <- 0.95
fit_tli_cutoff_ce   <- 0.95
fit_rmsea_cutoff_ce <- 0.08
fit_srmr_cutoff_ce  <- 0.08

# Estimación CFA
cfa_estimator_ce <- if (exists("cfa_estimator")) cfa_estimator else "WLSMV"
cfa_parameterization_ce <- if (exists("cfa_parameterization")) cfa_parameterization else "theta"

# Regla para escoger el error correlacionado:
# "mi" = mayor índice de modificación
# "sepc_all" = mayor EPC estandarizado absoluto
correlated_error_selection_ce <- "mi"

# Regla para eliminar ítem dentro del par con mayor error correlacionado:
# El usuario solicitó eliminar el ítem con mayor carga factorial.
delete_item_rule_ce <- "higher_loading"

# Número máximo de eliminaciones por modelo
max_item_deletions_ce <- 30

# Archivo de salida
output_excel_ce <- "CFA_correlated_errors_iterative_unidimensional_results.xlsx"

# Etiquetas de dominios
domain_labels_ce <- c(
  dim1 = "Organizational Readiness",
  dim2 = "Processes",
  dim3 = "Digital Environment",
  dim4 = "Human Resources",
  dim5 = "Regulatory Issues"
)

############################################################
# 6.3. Funciones auxiliares generales
############################################################

to_numeric_safe_ce <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  
  if (is.factor(x)) {
    out <- suppressWarnings(as.numeric(as.character(x)))
    if (all(is.na(out)) && !all(is.na(x))) out <- as.numeric(x)
    return(out)
  }
  
  if (is.character(x)) {
    return(suppressWarnings(as.numeric(x)))
  }
  
  suppressWarnings(as.numeric(x))
}

round_numeric_df_ce <- function(df, digits = 4) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
}

safe_fit_measure_ce <- function(fit, primary, fallback = NULL) {
  out <- tryCatch(
    lavaan::fitMeasures(fit, primary),
    error = function(e) NA_real_
  )
  
  out <- suppressWarnings(as.numeric(out[1]))
  
  if (is.na(out) && !is.null(fallback)) {
    out <- tryCatch(
      lavaan::fitMeasures(fit, fallback),
      error = function(e) NA_real_
    )
    out <- suppressWarnings(as.numeric(out[1]))
  }
  
  out
}

safe_lavinspect_ce <- function(fit, what, default = NA) {
  tryCatch(
    lavaan::lavInspect(fit, what),
    error = function(e) default
  )
}

get_domain_label_ce <- function(scale_id) {
  if (scale_id %in% names(domain_labels_ce)) {
    return(unname(domain_labels_ce[scale_id]))
  }
  scale_id
}

paste_items_ce <- function(x) {
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = ", ")
}

paste_items_by_factor_ce <- function(factor_items) {
  if (length(factor_items) == 0) return(NA_character_)
  
  paste(
    purrr::imap_chr(
      factor_items,
      ~ paste0(.y, ": ", paste(.x, collapse = ", "))
    ),
    collapse = " | "
  )
}

number_word_ce <- function(n) {
  words <- c(
    "One", "Two", "Three", "Four", "Five",
    "Six", "Seven", "Eight", "Nine", "Ten"
  )
  
  if (!is.na(n) && n >= 1 && n <= length(words)) {
    return(words[n])
  }
  
  as.character(n)
}

model_type_label_ce <- function(model_type, n_factors) {
  nw <- number_word_ce(n_factors)
  
  dplyr::case_when(
    model_type == "unidimensional" ~ "One-factor model",
    model_type == "correlated_factors" ~ paste0(nw, "-factor correlated model"),
    model_type == "bifactor" ~ paste0(nw, "-factor bifactor model"),
    model_type == "second_order" ~ paste0(nw, "-factor second-order model"),
    TRUE ~ model_type
  )
}

############################################################
# 6.4. Construcción de sintaxis lavaan
############################################################

build_unidimensional_syntax_ce <- function(factor_items) {
  if (length(factor_items) != 1) return(NA_character_)
  
  paste0(
    names(factor_items)[1],
    " =~ ",
    paste(factor_items[[1]], collapse = " + ")
  )
}

build_correlated_syntax_ce <- function(factor_items) {
  if (length(factor_items) < 2) return(NA_character_)
  
  factor_lines <- purrr::imap_chr(
    factor_items,
    ~ paste0(.y, " =~ ", paste(.x, collapse = " + "))
  )
  
  cov_lines <- apply(
    combn(names(factor_items), 2),
    2,
    function(z) paste0(z[1], " ~~ ", z[2])
  )
  
  paste(c(factor_lines, cov_lines), collapse = "\n")
}

build_bifactor_syntax_ce <- function(factor_items, scale_id) {
  if (length(factor_items) < 2) return(NA_character_)
  
  all_items <- unique(unlist(factor_items))
  general_factor <- paste0(scale_id, "_G")
  
  general_line <- paste0(
    general_factor,
    " =~ ",
    paste(all_items, collapse = " + ")
  )
  
  specific_lines <- purrr::imap_chr(
    factor_items,
    ~ paste0(.y, " =~ ", paste(.x, collapse = " + "))
  )
  
  orthogonal_general <- paste0(
    general_factor,
    " ~~ 0*",
    names(factor_items)
  )
  
  orthogonal_specific <- apply(
    combn(names(factor_items), 2),
    2,
    function(z) paste0(z[1], " ~~ 0*", z[2])
  )
  
  paste(
    c(
      general_line,
      specific_lines,
      orthogonal_general,
      orthogonal_specific
    ),
    collapse = "\n"
  )
}

build_second_order_syntax_ce <- function(factor_items, scale_id) {
  if (length(factor_items) < 2) return(NA_character_)
  
  first_order_lines <- purrr::imap_chr(
    factor_items,
    ~ paste0(.y, " =~ ", paste(.x, collapse = " + "))
  )
  
  so_factor <- paste0(scale_id, "_SO")
  first_order_names <- names(factor_items)
  
  if (length(first_order_names) == 2) {
    so_line <- paste0(
      so_factor,
      " =~ a*",
      first_order_names[1],
      " + a*",
      first_order_names[2]
    )
  } else {
    so_line <- paste0(
      so_factor,
      " =~ ",
      paste(first_order_names, collapse = " + ")
    )
  }
  
  paste(c(first_order_lines, so_line), collapse = "\n")
}

build_model_syntax_ce <- function(factor_items, scale_id, model_type) {
  if (model_type == "unidimensional") {
    return(build_unidimensional_syntax_ce(factor_items))
  }
  
  if (model_type == "correlated_factors") {
    return(build_correlated_syntax_ce(factor_items))
  }
  
  if (model_type == "bifactor") {
    return(build_bifactor_syntax_ce(factor_items, scale_id))
  }
  
  if (model_type == "second_order") {
    return(build_second_order_syntax_ce(factor_items, scale_id))
  }
  
  stop("model_type no reconocido: ", model_type)
}

get_model_types_to_try_ce <- function(factor_items) {
  # Versión restringida: solo se ejecuta el modelo unidimensional.
  # Se conserva el nombre de la función porque el flujo posterior lo usa.
  return("unidimensional")
}

############################################################
# 6.5. Preparar datos CFA
############################################################

prepare_cfa_data_ce <- function(data, items) {
  items_ok <- intersect(items, names(data))
  
  if (length(items_ok) < min_items_per_factor) {
    stop(
      "Menos de ",
      min_items_per_factor,
      " ítems están presentes en Database_CFA."
    )
  }
  
  df <- data[, items_ok, drop = FALSE]
  df <- as.data.frame(lapply(df, to_numeric_safe_ce))
  names(df) <- items_ok
  
  keep_variability <- sapply(df, function(x) {
    x2 <- x[!is.na(x)]
    length(unique(x2)) >= 2
  })
  
  df <- df[, keep_variability, drop = FALSE]
  
  if (ncol(df) < min_items_per_factor) {
    stop(
      "Menos de ",
      min_items_per_factor,
      " ítems tienen variabilidad en Database_CFA."
    )
  }
  
  df
}

############################################################
# 6.6. Validar estructura antes de correr CFA
############################################################

check_estimable_structure_ce <- function(factor_items, model_type) {
  n_factors <- length(factor_items)
  factor_counts <- lengths(factor_items)
  
  if (n_factors == 0) {
    return(list(
      estimable = FALSE,
      reason = "not_converged_less_than_3_items_per_factor: no hay factores disponibles."
    ))
  }
  
  if (any(factor_counts < min_items_per_factor)) {
    bad <- names(factor_counts)[factor_counts < min_items_per_factor]
    bad_txt <- paste0(
      bad,
      " (",
      factor_counts[bad],
      " ítems)"
    )
    
    return(list(
      estimable = FALSE,
      reason = paste0(
        "not_converged_less_than_3_items_per_factor: ",
        "uno o más factores quedaron con menos de ",
        min_items_per_factor,
        " ítems: ",
        paste(bad_txt, collapse = "; ")
      )
    ))
  }
  
  if (model_type == "unidimensional" && n_factors != 1) {
    return(list(
      estimable = FALSE,
      reason = "not_converged_invalid_structure: el modelo unidimensional requiere exactamente 1 factor."
    ))
  }
  
  if (model_type != "unidimensional" && n_factors < 2) {
    return(list(
      estimable = FALSE,
      reason = paste0(
        "not_converged_less_than_3_items_per_factor: ",
        "el modelo multidimensional quedó con menos de 2 factores estimables."
      )
    ))
  }
  
  list(
    estimable = TRUE,
    reason = NA_character_
  )
}

############################################################
# 6.7. Ajuste CFA seguro
############################################################

fit_cfa_ce <- function(model_syntax, df_cfa) {
  tryCatch(
    lavaan::cfa(
      model = model_syntax,
      data = df_cfa,
      ordered = names(df_cfa),
      estimator = cfa_estimator_ce,
      parameterization = cfa_parameterization_ce,
      std.lv = TRUE,
      warn = TRUE
    ),
    error = function(e) e
  )
}

extract_fit_row_ce <- function(
    fit,
    scale_id,
    model_id,
    cfa_model_id,
    model_type,
    iteration,
    factor_items,
    eliminated_items,
    model_syntax
) {
  converged <- isTRUE(safe_lavinspect_ce(fit, "converged", FALSE))
  post_check <- isTRUE(safe_lavinspect_ce(fit, "post.check", TRUE))
  
  pe <- tryCatch(
    lavaan::parameterEstimates(fit, standardized = TRUE, ci = FALSE),
    error = function(e) NULL
  )
  
  heywood_flag <- FALSE
  
  if (!is.null(pe) && nrow(pe) > 0) {
    heywood_flag <- any(
      pe$op == "~~" &
        pe$lhs == pe$rhs &
        !is.na(pe$est) &
        pe$est < 0
    )
  }
  
  chisq <- safe_fit_measure_ce(fit, "chisq.scaled", "chisq")
  df <- safe_fit_measure_ce(fit, "df.scaled", "df")
  pvalue <- safe_fit_measure_ce(fit, "pvalue.scaled", "pvalue")
  cfi <- safe_fit_measure_ce(fit, "cfi.scaled", "cfi")
  tli <- safe_fit_measure_ce(fit, "tli.scaled", "tli")
  rmsea <- safe_fit_measure_ce(fit, "rmsea.scaled", "rmsea")
  rmsea_lower <- safe_fit_measure_ce(
    fit,
    "rmsea.ci.lower.scaled",
    "rmsea.ci.lower"
  )
  rmsea_upper <- safe_fit_measure_ce(
    fit,
    "rmsea.ci.upper.scaled",
    "rmsea.ci.upper"
  )
  srmr <- safe_fit_measure_ce(fit, "srmr")
  
  fit_adequate <- isTRUE(converged) &&
    !is.na(cfi) && !is.na(tli) &&
    !is.na(rmsea) && !is.na(srmr) &&
    cfi > fit_cfi_cutoff_ce &&
    tli > fit_tli_cutoff_ce &&
    rmsea < fit_rmsea_cutoff_ce &&
    srmr < fit_srmr_cutoff_ce
  
  all_items <- unique(unlist(factor_items))
  
  tibble(
    Domain = get_domain_label_ce(scale_id),
    scale_id = scale_id,
    model_id = model_id,
    cfa_model_id = cfa_model_id,
    iteration = iteration,
    `Domain and factor structure` = model_type_label_ce(
      model_type,
      length(factor_items)
    ),
    model_type = model_type,
    n_factors = length(factor_items),
    `Retained items` = length(all_items),
    `Items eliminados` = ifelse(
      length(eliminated_items) == 0,
      "-",
      paste(eliminated_items, collapse = ", ")
    ),
    chisq = chisq,
    df = df,
    pvalue = pvalue,
    CFI = cfi,
    TLI = tli,
    RMSEA = rmsea,
    RMSEA_lower = rmsea_lower,
    RMSEA_upper = rmsea_upper,
    `RMSEA (90% CI)` = ifelse(
      is.na(rmsea),
      NA_character_,
      sprintf("%.3f (%.3f-%.3f)", rmsea, rmsea_lower, rmsea_upper)
    ),
    SRMR = srmr,
    cfa_converged = converged,
    post_check = post_check,
    heywood_flag = heywood_flag,
    fit_adequate = fit_adequate,
    factor_items = paste_items_by_factor_ce(factor_items),
    Model = model_syntax,
    cfa_status = dplyr::case_when(
      fit_adequate ~ "adequate_fit",
      converged ~ "converged_not_adequate_fit",
      TRUE ~ "not_converged"
    ),
    cfa_note = dplyr::case_when(
      fit_adequate ~ "Modelo alcanzó criterios de ajuste.",
      converged ~ "Modelo convergió, pero no alcanzó criterios de ajuste.",
      TRUE ~ "lavaan no reportó convergencia."
    )
  )
}

fit_is_adequate_ce <- function(fit_row) {
  isTRUE(fit_row$fit_adequate[1])
}

############################################################
# 6.8. Cargas estandarizadas por ítem
############################################################

extract_item_loadings_ce <- function(fit, observed_items) {
  pe <- tryCatch(
    lavaan::parameterEstimates(fit, standardized = TRUE, ci = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(pe) || nrow(pe) == 0) {
    return(tibble())
  }
  
  pe %>%
    as_tibble() %>%
    filter(
      op == "=~",
      rhs %in% observed_items
    ) %>%
    mutate(
      abs_std_loading = abs(std.all)
    ) %>%
    group_by(item = rhs) %>%
    arrange(desc(abs_std_loading), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      item,
      factor = lhs,
      std_loading = std.all,
      abs_std_loading = abs_std_loading
    )
}

############################################################
# 6.9. Mayor error correlacionado por índices de modificación
############################################################

extract_top_correlated_error_ce <- function(fit, observed_items) {
  mi <- tryCatch(
    lavaan::modificationIndices(
      fit,
      standardized = TRUE,
      sort. = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(mi) || nrow(mi) == 0) {
    return(tibble())
  }
  
  mi <- mi %>%
    as_tibble() %>%
    filter(
      op == "~~",
      lhs %in% observed_items,
      rhs %in% observed_items,
      lhs != rhs
    ) %>%
    mutate(
      abs_sepc_all = abs(sepc.all),
      selection_value = dplyr::case_when(
        correlated_error_selection_ce == "mi" ~ mi,
        correlated_error_selection_ce == "sepc_all" ~ abs_sepc_all,
        TRUE ~ mi
      )
    ) %>%
    arrange(desc(selection_value), desc(mi), desc(abs_sepc_all))
  
  if (nrow(mi) == 0) {
    return(tibble())
  }
  
  mi %>%
    slice(1) %>%
    transmute(
      item_1 = lhs,
      item_2 = rhs,
      correlated_error_metric = correlated_error_selection_ce,
      correlated_error_value = selection_value,
      mi = mi,
      epc = epc,
      sepc_lv = sepc.lv,
      sepc_all = sepc.all,
      abs_sepc_all = abs_sepc_all
    )
}

find_item_factor_ce <- function(item, factor_items) {
  hits <- names(factor_items)[sapply(factor_items, function(x) item %in% x)]
  
  if (length(hits) == 0) return(NA_character_)
  
  paste(hits, collapse = "; ")
}

choose_item_to_delete_ce <- function(top_pair, loadings, factor_items) {
  pair_items <- c(top_pair$item_1[1], top_pair$item_2[1])
  
  pair_loads <- loadings %>%
    filter(item %in% pair_items) %>%
    select(item, factor, std_loading, abs_std_loading)
  
  pair_loads <- tibble(item = pair_items) %>%
    left_join(pair_loads, by = "item") %>%
    mutate(
      abs_std_loading = ifelse(is.na(abs_std_loading), -Inf, abs_std_loading)
    )
  
  if (delete_item_rule_ce == "higher_loading") {
    deleted <- pair_loads %>%
      arrange(desc(abs_std_loading)) %>%
      slice(1)
  } else if (delete_item_rule_ce == "lower_loading") {
    deleted <- pair_loads %>%
      arrange(abs_std_loading) %>%
      slice(1)
  } else {
    stop("delete_item_rule_ce debe ser 'higher_loading' o 'lower_loading'.")
  }
  
  tibble(
    item_1 = pair_items[1],
    item_2 = pair_items[2],
    item_1_loading = pair_loads$abs_std_loading[pair_loads$item == pair_items[1]],
    item_2_loading = pair_loads$abs_std_loading[pair_loads$item == pair_items[2]],
    deleted_item = deleted$item[1],
    deleted_item_loading = deleted$abs_std_loading[1],
    deleted_item_factor = find_item_factor_ce(deleted$item[1], factor_items),
    decision_rule = delete_item_rule_ce
  )
}

############################################################
# 6.10. Confiabilidad: alfa y omega por factor
############################################################

compute_alpha_omega_one_ce <- function(data, items) {
  items_ok <- intersect(items, names(data))
  
  if (length(items_ok) < min_items_per_factor) {
    return(tibble(
      n_items_reliability = length(items_ok),
      alpha = NA_real_,
      omega = NA_real_
    ))
  }
  
  df_items <- data[, items_ok, drop = FALSE]
  df_items <- as.data.frame(lapply(df_items, to_numeric_safe_ce))
  names(df_items) <- items_ok
  
  alpha_val <- tryCatch(
    {
      out <- psych::alpha(
        df_items,
        check.keys = FALSE,
        warnings = FALSE
      )
      as.numeric(out$total$raw_alpha)
    },
    error = function(e) NA_real_
  )
  
  omega_val <- tryCatch(
    {
      out <- suppressWarnings(
        psych::omega(
          df_items,
          nfactors = 1,
          plot = FALSE,
          warnings = FALSE
        )
      )
      as.numeric(out$omega.tot)
    },
    error = function(e) NA_real_
  )
  
  tibble(
    n_items_reliability = length(items_ok),
    alpha = alpha_val,
    omega = omega_val
  )
}

compute_reliability_long_ce <- function(data, factor_items) {
  all_items <- unique(unlist(factor_items))
  
  total_rel <- compute_alpha_omega_one_ce(data, all_items) %>%
    mutate(
      reliability_factor = "Total",
      reliability_items = paste_items_ce(all_items),
      .before = 1
    )
  
  factor_rel <- purrr::imap_dfr(
    factor_items,
    function(items, factor_name) {
      compute_alpha_omega_one_ce(data, items) %>%
        mutate(
          reliability_factor = factor_name,
          reliability_items = paste_items_ce(items),
          .before = 1
        )
    }
  )
  
  bind_rows(total_rel, factor_rel)
}

reliability_wide_ce <- function(rel_long) {
  out <- list()
  
  total <- rel_long %>% filter(reliability_factor == "Total")
  
  out[["Total alfa"]] <- ifelse(nrow(total) == 0, NA_real_, total$alpha[1])
  out[["Total omega"]] <- ifelse(nrow(total) == 0, NA_real_, total$omega[1])
  
  factors <- rel_long %>%
    filter(reliability_factor != "Total")
  
  if (nrow(factors) > 0) {
    for (i in seq_len(nrow(factors))) {
      out[[paste0("F", i, " alfa")]] <- factors$alpha[i]
      out[[paste0("F", i, " omega")]] <- factors$omega[i]
      out[[paste0("F", i, " items reliability")]] <- factors$reliability_items[i]
    }
  }
  
  as_tibble(out)
}

############################################################
# 6.11. Correr un modelo iterativo
############################################################

run_one_iterative_cfa_ce <- function(structure, model_type) {
  scale_id <- structure$scale_id
  model_id <- structure$model_id
  
  cfa_model_id <- paste(
    model_id,
    model_type,
    "correlated_error_iterative",
    sep = "__"
  )
  
  factor_items_current <- structure$factor_items
  eliminated_items <- character(0)
  
  step_rows <- list()
  elimination_rows <- list()
  loading_rows <- list()
  reliability_rows <- list()
  syntax_rows <- list()
  
  iteration <- 0
  
  repeat {
    estimable_check <- check_estimable_structure_ce(
      factor_items = factor_items_current,
      model_type = model_type
    )
    
    current_items <- unique(unlist(factor_items_current))
    
    if (!isTRUE(estimable_check$estimable)) {
      syntax_now <- tryCatch(
        build_model_syntax_ce(
          factor_items = factor_items_current,
          scale_id = scale_id,
          model_type = model_type
        ),
        error = function(e) NA_character_
      )
      
      step_rows[[length(step_rows) + 1]] <- tibble(
        Domain = get_domain_label_ce(scale_id),
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = cfa_model_id,
        iteration = iteration,
        `Domain and factor structure` = model_type_label_ce(
          model_type,
          length(factor_items_current)
        ),
        model_type = model_type,
        n_factors = length(factor_items_current),
        `Retained items` = length(current_items),
        `Items eliminados` = ifelse(
          length(eliminated_items) == 0,
          "-",
          paste(eliminated_items, collapse = ", ")
        ),
        chisq = NA_real_,
        df = NA_real_,
        pvalue = NA_real_,
        CFI = NA_real_,
        TLI = NA_real_,
        RMSEA = NA_real_,
        RMSEA_lower = NA_real_,
        RMSEA_upper = NA_real_,
        `RMSEA (90% CI)` = NA_character_,
        SRMR = NA_real_,
        cfa_converged = FALSE,
        post_check = FALSE,
        heywood_flag = NA,
        fit_adequate = FALSE,
        factor_items = paste_items_by_factor_ce(factor_items_current),
        Model = syntax_now,
        item_deleted_after_fit = NA_character_,
        correlated_with_item = NA_character_,
        correlated_error_metric = NA_character_,
        correlated_error_value = NA_real_,
        correlated_error_mi = NA_real_,
        correlated_error_epc = NA_real_,
        correlated_error_sepc_all = NA_real_,
        deleted_item_loading = NA_real_,
        step_status = "stopped_not_estimable",
        cfa_status = "not_converged_less_than_3_items_per_factor",
        cfa_note = estimable_check$reason
      )
      
      break
    }
    
    model_syntax <- build_model_syntax_ce(
      factor_items = factor_items_current,
      scale_id = scale_id,
      model_type = model_type
    )
    
    syntax_rows[[length(syntax_rows) + 1]] <- tibble(
      Domain = get_domain_label_ce(scale_id),
      scale_id = scale_id,
      model_id = model_id,
      cfa_model_id = cfa_model_id,
      iteration = iteration,
      model_type = model_type,
      factor_items = paste_items_by_factor_ce(factor_items_current),
      Model = model_syntax
    )
    
    df_cfa <- tryCatch(
      prepare_cfa_data_ce(Database_CFA, current_items),
      error = function(e) e
    )
    
    if (inherits(df_cfa, "error")) {
      step_rows[[length(step_rows) + 1]] <- tibble(
        Domain = get_domain_label_ce(scale_id),
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = cfa_model_id,
        iteration = iteration,
        `Domain and factor structure` = model_type_label_ce(
          model_type,
          length(factor_items_current)
        ),
        model_type = model_type,
        n_factors = length(factor_items_current),
        `Retained items` = length(current_items),
        `Items eliminados` = ifelse(
          length(eliminated_items) == 0,
          "-",
          paste(eliminated_items, collapse = ", ")
        ),
        chisq = NA_real_,
        df = NA_real_,
        pvalue = NA_real_,
        CFI = NA_real_,
        TLI = NA_real_,
        RMSEA = NA_real_,
        RMSEA_lower = NA_real_,
        RMSEA_upper = NA_real_,
        `RMSEA (90% CI)` = NA_character_,
        SRMR = NA_real_,
        cfa_converged = FALSE,
        post_check = FALSE,
        heywood_flag = NA,
        fit_adequate = FALSE,
        factor_items = paste_items_by_factor_ce(factor_items_current),
        Model = model_syntax,
        item_deleted_after_fit = NA_character_,
        correlated_with_item = NA_character_,
        correlated_error_metric = NA_character_,
        correlated_error_value = NA_real_,
        correlated_error_mi = NA_real_,
        correlated_error_epc = NA_real_,
        correlated_error_sepc_all = NA_real_,
        deleted_item_loading = NA_real_,
        step_status = "stopped_data_error",
        cfa_status = "not_converged_data_error",
        cfa_note = df_cfa$message
      )
      
      break
    }
    
    fit <- fit_cfa_ce(model_syntax, df_cfa)
    
    if (inherits(fit, "error")) {
      step_rows[[length(step_rows) + 1]] <- tibble(
        Domain = get_domain_label_ce(scale_id),
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = cfa_model_id,
        iteration = iteration,
        `Domain and factor structure` = model_type_label_ce(
          model_type,
          length(factor_items_current)
        ),
        model_type = model_type,
        n_factors = length(factor_items_current),
        `Retained items` = length(current_items),
        `Items eliminados` = ifelse(
          length(eliminated_items) == 0,
          "-",
          paste(eliminated_items, collapse = ", ")
        ),
        chisq = NA_real_,
        df = NA_real_,
        pvalue = NA_real_,
        CFI = NA_real_,
        TLI = NA_real_,
        RMSEA = NA_real_,
        RMSEA_lower = NA_real_,
        RMSEA_upper = NA_real_,
        `RMSEA (90% CI)` = NA_character_,
        SRMR = NA_real_,
        cfa_converged = FALSE,
        post_check = FALSE,
        heywood_flag = NA,
        fit_adequate = FALSE,
        factor_items = paste_items_by_factor_ce(factor_items_current),
        Model = model_syntax,
        item_deleted_after_fit = NA_character_,
        correlated_with_item = NA_character_,
        correlated_error_metric = NA_character_,
        correlated_error_value = NA_real_,
        correlated_error_mi = NA_real_,
        correlated_error_epc = NA_real_,
        correlated_error_sepc_all = NA_real_,
        deleted_item_loading = NA_real_,
        step_status = "stopped_lavaan_error",
        cfa_status = "not_converged_lavaan_error",
        cfa_note = fit$message
      )
      
      break
    }
    
    fit_row <- extract_fit_row_ce(
      fit = fit,
      scale_id = scale_id,
      model_id = model_id,
      cfa_model_id = cfa_model_id,
      model_type = model_type,
      iteration = iteration,
      factor_items = factor_items_current,
      eliminated_items = eliminated_items,
      model_syntax = model_syntax
    )
    
    loadings_now <- extract_item_loadings_ce(
      fit = fit,
      observed_items = names(df_cfa)
    ) %>%
      mutate(
        Domain = get_domain_label_ce(scale_id),
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = cfa_model_id,
        model_type = model_type,
        iteration = iteration,
        .before = 1
      )
    
    loading_rows[[length(loading_rows) + 1]] <- loadings_now
    
    rel_long_now <- compute_reliability_long_ce(
      data = Database_CFA,
      factor_items = factor_items_current
    ) %>%
      mutate(
        Domain = get_domain_label_ce(scale_id),
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = cfa_model_id,
        model_type = model_type,
        iteration = iteration,
        .before = 1
      )
    
    reliability_rows[[length(reliability_rows) + 1]] <- rel_long_now
    
    rel_wide_now <- reliability_wide_ce(rel_long_now)
    
    # Si el modelo ya cumple criterios de ajuste, se detiene.
    if (fit_is_adequate_ce(fit_row)) {
      fit_row <- fit_row %>%
        mutate(
          item_deleted_after_fit = NA_character_,
          correlated_with_item = NA_character_,
          correlated_error_metric = NA_character_,
          correlated_error_value = NA_real_,
          correlated_error_mi = NA_real_,
          correlated_error_epc = NA_real_,
          correlated_error_sepc_all = NA_real_,
          deleted_item_loading = NA_real_,
          step_status = "fit_criteria_met"
        ) %>%
        bind_cols(rel_wide_now)
      
      step_rows[[length(step_rows) + 1]] <- fit_row
      break
    }
    
    # Si no convergió, no se usa MI para eliminar.
    if (!isTRUE(fit_row$cfa_converged[1])) {
      fit_row <- fit_row %>%
        mutate(
          item_deleted_after_fit = NA_character_,
          correlated_with_item = NA_character_,
          correlated_error_metric = NA_character_,
          correlated_error_value = NA_real_,
          correlated_error_mi = NA_real_,
          correlated_error_epc = NA_real_,
          correlated_error_sepc_all = NA_real_,
          deleted_item_loading = NA_real_,
          step_status = "stopped_not_converged"
        ) %>%
        bind_cols(rel_wide_now)
      
      step_rows[[length(step_rows) + 1]] <- fit_row
      break
    }
    
    # Si llegó al máximo de eliminaciones.
    if (iteration >= max_item_deletions_ce) {
      fit_row <- fit_row %>%
        mutate(
          item_deleted_after_fit = NA_character_,
          correlated_with_item = NA_character_,
          correlated_error_metric = NA_character_,
          correlated_error_value = NA_real_,
          correlated_error_mi = NA_real_,
          correlated_error_epc = NA_real_,
          correlated_error_sepc_all = NA_real_,
          deleted_item_loading = NA_real_,
          step_status = "stopped_max_item_deletions"
        ) %>%
        bind_cols(rel_wide_now)
      
      step_rows[[length(step_rows) + 1]] <- fit_row
      break
    }
    
    top_pair <- extract_top_correlated_error_ce(
      fit = fit,
      observed_items = names(df_cfa)
    )
    
    if (nrow(top_pair) == 0) {
      fit_row <- fit_row %>%
        mutate(
          item_deleted_after_fit = NA_character_,
          correlated_with_item = NA_character_,
          correlated_error_metric = NA_character_,
          correlated_error_value = NA_real_,
          correlated_error_mi = NA_real_,
          correlated_error_epc = NA_real_,
          correlated_error_sepc_all = NA_real_,
          deleted_item_loading = NA_real_,
          step_status = "stopped_no_correlated_error_available"
        ) %>%
        bind_cols(rel_wide_now)
      
      step_rows[[length(step_rows) + 1]] <- fit_row
      break
    }
    
    delete_decision <- choose_item_to_delete_ce(
      top_pair = top_pair,
      loadings = loadings_now,
      factor_items = factor_items_current
    )
    
    item_to_delete <- delete_decision$deleted_item[1]
    
    elimination_row <- bind_cols(
      tibble(
        Domain = get_domain_label_ce(scale_id),
        scale_id = scale_id,
        model_id = model_id,
        cfa_model_id = cfa_model_id,
        model_type = model_type,
        iteration = iteration,
        next_iteration = iteration + 1
      ),
      top_pair,
      delete_decision
    ) %>%
      mutate(
        retained_items_before_deletion = length(current_items),
        retained_items_after_deletion = length(setdiff(current_items, item_to_delete)),
        eliminated_items_after_deletion = paste(
          c(eliminated_items, item_to_delete),
          collapse = ", "
        )
      )
    
    elimination_rows[[length(elimination_rows) + 1]] <- elimination_row
    
    fit_row <- fit_row %>%
      mutate(
        item_deleted_after_fit = item_to_delete,
        correlated_with_item = ifelse(
          item_to_delete == top_pair$item_1[1],
          top_pair$item_2[1],
          top_pair$item_1[1]
        ),
        correlated_error_metric = top_pair$correlated_error_metric[1],
        correlated_error_value = top_pair$correlated_error_value[1],
        correlated_error_mi = top_pair$mi[1],
        correlated_error_epc = top_pair$epc[1],
        correlated_error_sepc_all = top_pair$sepc_all[1],
        deleted_item_loading = delete_decision$deleted_item_loading[1],
        step_status = "item_deleted_and_model_refit"
      ) %>%
      bind_cols(rel_wide_now)
    
    step_rows[[length(step_rows) + 1]] <- fit_row
    
    # Actualizar estructura: eliminar ítem y volver a correr.
    eliminated_items <- c(eliminated_items, item_to_delete)
    
    factor_items_current <- lapply(
      factor_items_current,
      function(x) setdiff(x, item_to_delete)
    )
    
    iteration <- iteration + 1
  }
  
  list(
    steps = bind_rows(step_rows),
    eliminations = bind_rows(elimination_rows),
    loadings = bind_rows(loading_rows),
    reliability = bind_rows(reliability_rows),
    syntax = bind_rows(syntax_rows)
  )
}

############################################################
# 6.12. Correr todos los modelos derivados de AFE
############################################################

# Seguridad adicional: si en el entorno existen estructuras antiguas con más de un factor,
# se excluyen para que esta sección solo procese modelos unidimensionales.
cfa_structures <- cfa_structures[
  vapply(
    cfa_structures,
    function(x) {
      isTRUE(x$collapsed) ||
        (
          !isTRUE(x$collapsed) &&
            !is.null(x$n_factors_efa) &&
            x$n_factors_efa == 1 &&
            length(x$factor_items) == 1
        )
    },
    logical(1)
  )
]

run_structure_ce <- function(structure) {
  if (isTRUE(structure$collapsed)) {
    row <- tibble(
      Domain = get_domain_label_ce(structure$scale_id),
      scale_id = structure$scale_id,
      model_id = structure$model_id,
      cfa_model_id = paste0(structure$model_id, "__collapsed"),
      iteration = 0,
      `Domain and factor structure` = NA_character_,
      model_type = NA_character_,
      n_factors = structure$n_factors_retained,
      `Retained items` = structure$n_items_retained,
      `Items eliminados` = NA_character_,
      chisq = NA_real_,
      df = NA_real_,
      pvalue = NA_real_,
      CFI = NA_real_,
      TLI = NA_real_,
      RMSEA = NA_real_,
      RMSEA_lower = NA_real_,
      RMSEA_upper = NA_real_,
      `RMSEA (90% CI)` = NA_character_,
      SRMR = NA_real_,
      cfa_converged = FALSE,
      post_check = FALSE,
      heywood_flag = NA,
      fit_adequate = FALSE,
      factor_items = paste_items_by_factor_ce(structure$factor_items),
      Model = NA_character_,
      item_deleted_after_fit = NA_character_,
      correlated_with_item = NA_character_,
      correlated_error_metric = NA_character_,
      correlated_error_value = NA_real_,
      correlated_error_mi = NA_real_,
      correlated_error_epc = NA_real_,
      correlated_error_sepc_all = NA_real_,
      deleted_item_loading = NA_real_,
      step_status = "stopped_collapsed_from_efa",
      cfa_status = "not_converged_less_than_3_items_per_factor",
      cfa_note = structure$collapse_reason
    )
    
    return(list(
      steps = row,
      eliminations = tibble(),
      loadings = tibble(),
      reliability = tibble(),
      syntax = tibble()
    ))
  }
  
  model_types <- get_model_types_to_try_ce(structure$factor_items)
  
  runs <- lapply(
    model_types,
    function(mt) {
      cat("\n====================================================\n")
      cat("CFA iterativo por errores correlacionados\n")
      cat("Modelo AFE:", structure$model_id, "\n")
      cat("Tipo CFA:", mt, "\n")
      cat("Dominio:", get_domain_label_ce(structure$scale_id), "\n")
      cat("====================================================\n")
      
      run_one_iterative_cfa_ce(
        structure = structure,
        model_type = mt
      )
    }
  )
  
  list(
    steps = bind_rows(lapply(runs, `[[`, "steps")),
    eliminations = bind_rows(lapply(runs, `[[`, "eliminations")),
    loadings = bind_rows(lapply(runs, `[[`, "loadings")),
    reliability = bind_rows(lapply(runs, `[[`, "reliability")),
    syntax = bind_rows(lapply(runs, `[[`, "syntax"))
  )
}

cfa_ce_runs <- lapply(cfa_structures, run_structure_ce)

cfa_ce_steps_final <- bind_rows(lapply(cfa_ce_runs, `[[`, "steps")) %>%
  round_numeric_df_ce(4)

cfa_ce_elimination_log_final <- bind_rows(lapply(cfa_ce_runs, `[[`, "eliminations")) %>%
  round_numeric_df_ce(4)

cfa_ce_loadings_final <- bind_rows(lapply(cfa_ce_runs, `[[`, "loadings")) %>%
  round_numeric_df_ce(4)

cfa_ce_reliability_final <- bind_rows(lapply(cfa_ce_runs, `[[`, "reliability")) %>%
  round_numeric_df_ce(4)

cfa_ce_syntax_final <- bind_rows(lapply(cfa_ce_runs, `[[`, "syntax"))

############################################################
# 6.13. Resumen final por modelo
############################################################

cfa_ce_final_models <- cfa_ce_steps_final %>%
  group_by(cfa_model_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(
    scale_id,
    model_id,
    model_type
  )

# Mejor modelo por dominio:
# Prioriza ajuste adecuado; luego más ítems retenidos; luego CFI/TLI más altos;
# luego RMSEA/SRMR más bajos.
cfa_ce_best_by_domain <- cfa_ce_final_models %>%
  mutate(
    adequate_rank = ifelse(fit_adequate, 1, 0)
  ) %>%
  group_by(Domain) %>%
  arrange(
    desc(adequate_rank),
    desc(`Retained items`),
    desc(CFI),
    desc(TLI),
    RMSEA,
    SRMR,
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(-adequate_rank)

############################################################
# 6.14. Hoja de configuración
############################################################

cfa_ce_settings <- tibble(
  parameter = c(
    "Database",
    "Estimator",
    "Parameterization",
    "Ordered variables",
    "Minimum items per factor",
    "CFI criterion",
    "TLI criterion",
    "RMSEA criterion",
    "SRMR criterion",
    "Correlated error selection",
    "Item deletion rule",
    "Maximum item deletions"
  ),
  value = c(
    "Database_CFA",
    cfa_estimator_ce,
    cfa_parameterization_ce,
    "All retained observed items in each model",
    min_items_per_factor,
    paste0("> ", fit_cfi_cutoff_ce),
    paste0("> ", fit_tli_cutoff_ce),
    paste0("< ", fit_rmsea_cutoff_ce),
    paste0("< ", fit_srmr_cutoff_ce),
    correlated_error_selection_ce,
    delete_item_rule_ce,
    max_item_deletions_ce
  )
)

############################################################
# 6.15. Exportar a Excel
############################################################

wb_ce <- createWorkbook()

addWorksheet(wb_ce, "CE_CFA_steps")
writeData(wb_ce, "CE_CFA_steps", cfa_ce_steps_final)

addWorksheet(wb_ce, "CE_elimination_log")
writeData(wb_ce, "CE_elimination_log", cfa_ce_elimination_log_final)

addWorksheet(wb_ce, "CE_final_models")
writeData(wb_ce, "CE_final_models", cfa_ce_final_models)

addWorksheet(wb_ce, "CE_best_by_domain")
writeData(wb_ce, "CE_best_by_domain", cfa_ce_best_by_domain)

addWorksheet(wb_ce, "CE_loadings")
writeData(wb_ce, "CE_loadings", cfa_ce_loadings_final)

addWorksheet(wb_ce, "CE_reliability_long")
writeData(wb_ce, "CE_reliability_long", cfa_ce_reliability_final)

addWorksheet(wb_ce, "CE_syntax")
writeData(wb_ce, "CE_syntax", cfa_ce_syntax_final)

addWorksheet(wb_ce, "CE_settings")
writeData(wb_ce, "CE_settings", cfa_ce_settings)

# Formato básico
wrap_style_ce <- createStyle(wrapText = TRUE, valign = "top")

for (sh in names(wb_ce)) {
  freezePane(wb_ce, sh, firstRow = TRUE)
  setColWidths(wb_ce, sh, cols = 1:80, widths = "auto")
  addStyle(
    wb_ce,
    sh,
    style = wrap_style_ce,
    rows = 1:2000,
    cols = 1:80,
    gridExpand = TRUE,
    stack = TRUE
  )
}

saveWorkbook(
  wb_ce,
  output_excel_ce,
  overwrite = TRUE
)

############################################################
# 6.16. Mensaje final
############################################################

cat("\n====================================================\n")
cat("CFA ITERATIVO POR ERRORES CORRELACIONADOS COMPLETADO\n")
cat("====================================================\n")
cat("Archivo exportado:", output_excel_ce, "\n\n")

cat("Hojas exportadas:\n")
cat("- CE_CFA_steps: ajuste por iteración, ítems retenidos/eliminados, alfa y omega.\n")
cat("- CE_elimination_log: par con mayor error correlacionado, ítem eliminado y carga.\n")
cat("- CE_final_models: última iteración de cada modelo.\n")
cat("- CE_best_by_domain: mejor modelo por dominio según criterios de ajuste.\n")
cat("- CE_loadings: cargas estandarizadas por ítem en cada iteración.\n")
cat("- CE_reliability_long: alfa y omega por factor y total.\n")
cat("- CE_syntax: sintaxis lavaan evaluada.\n")
cat("- CE_settings: parámetros usados.\n\n")

cat("Criterios aplicados:\n")
cat("- CFI >", fit_cfi_cutoff_ce, "\n")
cat("- TLI >", fit_tli_cutoff_ce, "\n")
cat("- RMSEA <", fit_rmsea_cutoff_ce, "\n")
cat("- SRMR <", fit_srmr_cutoff_ce, "\n")
cat("- Mínimo de ítems por factor:", min_items_per_factor, "\n")
cat("- Regla de eliminación:", delete_item_rule_ce, "\n")



































############################################################
# 7. INVARIANZA MULTIGRUPO PARA LOS MODELOS CFA FINALES
#
# Modelos:
# 1) Organizational Readiness: dim1_F1
# 2) Processes: dim2_F1
# 3) Digital Environment: dim3_F1
# 4) Human Resources: dim4_F1
# 5) Regulatory Issues: dim5_F1
#
# Grupos:
# 1) telemedicine_role
# 2) time_at_ipress_group
# 3) cat_estab_2
#
# Base: Database_salud_completo
# Estimador: WLSMV
# Ítems ordinales
# Índices reportados: scaled
############################################################

# ==========================================================
# 7.0. Verificaciones iniciales
# ==========================================================

if (!exists("Database_salud_completo")) {
  stop("No existe Database_salud_completo en el entorno.")
}

if (!requireNamespace("semTools", quietly = TRUE)) {
  stop("Instala semTools antes de correr esta sección: install.packages('semTools')")
}

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Instala openxlsx antes de correr esta sección: install.packages('openxlsx')")
}

# ==========================================================
# 7.1. Parámetros generales
# ==========================================================

# Se usa la base completa para invarianza, como en tu código previo.
# Si quieres usar solo la mitad CFA, cambia esta línea por:
# invariance_base <- Database_CFA
invariance_base <- Database_salud_completo

invariance_estimator <- "WLSMV"
invariance_parameterization <- "theta"

delta_cfi_cutoff_inv <- -0.010
delta_rmsea_cutoff_inv <- 0.015

output_excel_invariance <- "Measurement_invariance_best_CFA_models.xlsx"

# ==========================================================
# 7.2. Funciones auxiliares
# ==========================================================

to_chr_inv <- function(x) {
  if (inherits(x, "haven_labelled")) {
    return(as.character(haven::as_factor(x)))
  }
  
  if (is.factor(x)) {
    return(as.character(x))
  }
  
  as.character(x)
}

to_numeric_inv <- function(x) {
  if (exists("to_numeric_safe")) {
    return(to_numeric_safe(x))
  }
  
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  
  if (is.factor(x)) {
    out <- suppressWarnings(as.numeric(as.character(x)))
    if (all(is.na(out)) && !all(is.na(x))) out <- as.numeric(x)
    return(out)
  }
  
  if (is.character(x)) {
    return(suppressWarnings(as.numeric(x)))
  }
  
  suppressWarnings(as.numeric(x))
}

clean_category_code_inv <- function(x) {
  x <- trimws(to_chr_inv(x))
  x <- gsub("[[:space:]]+", "", x)
  x <- gsub("[–—−]", "-", x)
  x
}

safe_fit_measure_inv <- function(fit, primary, fallback = NULL) {
  out <- tryCatch(
    lavaan::fitMeasures(fit, primary),
    error = function(e) NA_real_
  )
  
  out <- suppressWarnings(as.numeric(out[1]))
  
  if (is.na(out) && !is.null(fallback)) {
    out <- tryCatch(
      lavaan::fitMeasures(fit, fallback),
      error = function(e) NA_real_
    )
    
    out <- suppressWarnings(as.numeric(out[1]))
  }
  
  out
}

get_scaled_fit_inv <- function(fit) {
  if (is.null(fit)) {
    return(
      tibble(
        cfa_converged = FALSE,
        post_check = FALSE,
        chisq = NA_real_,
        df = NA_real_,
        pvalue = NA_real_,
        CFI = NA_real_,
        TLI = NA_real_,
        RMSEA = NA_real_,
        RMSEA_90LI = NA_real_,
        RMSEA_90LS = NA_real_,
        SRMR = NA_real_
      )
    )
  }
  
  cfa_converged <- tryCatch(
    isTRUE(lavaan::lavInspect(fit, "converged")),
    error = function(e) FALSE
  )
  
  post_check <- tryCatch(
    isTRUE(lavaan::lavInspect(fit, "post.check")),
    error = function(e) NA
  )
  
  tibble(
    cfa_converged = cfa_converged,
    post_check = post_check,
    chisq = safe_fit_measure_inv(fit, "chisq.scaled", "chisq"),
    df = safe_fit_measure_inv(fit, "df.scaled", "df"),
    pvalue = safe_fit_measure_inv(fit, "pvalue.scaled", "pvalue"),
    CFI = safe_fit_measure_inv(fit, "cfi.scaled", "cfi"),
    TLI = safe_fit_measure_inv(fit, "tli.scaled", "tli"),
    RMSEA = safe_fit_measure_inv(fit, "rmsea.scaled", "rmsea"),
    RMSEA_90LI = safe_fit_measure_inv(
      fit,
      "rmsea.ci.lower.scaled",
      "rmsea.ci.lower"
    ),
    RMSEA_90LS = safe_fit_measure_inv(
      fit,
      "rmsea.ci.upper.scaled",
      "rmsea.ci.upper"
    ),
    SRMR = safe_fit_measure_inv(fit, "srmr")
  )
}

make_1factor_syntax_inv <- function(factor_name, items) {
  paste0(
    factor_name,
    " =~ ",
    paste(items, collapse = " + ")
  )
}

prepare_ordered_data_inv <- function(data, items, group_var) {
  items_present <- intersect(items, names(data))
  missing_items <- setdiff(items, names(data))
  
  if (length(items_present) < 3) {
    stop(
      "El modelo tiene menos de 3 ítems presentes en la base. Ítems faltantes: ",
      paste(missing_items, collapse = ", ")
    )
  }
  
  dat <- data %>%
    dplyr::select(dplyr::all_of(c(group_var, items_present))) %>%
    dplyr::filter(!is.na(.data[[group_var]]))
  
  dat[[group_var]] <- droplevels(as.factor(dat[[group_var]]))
  
  if (nlevels(dat[[group_var]]) < 2) {
    stop("La variable de grupo ", group_var, " tiene menos de 2 niveles.")
  }
  
  for (v in items_present) {
    dat[[v]] <- ordered(to_numeric_inv(dat[[v]]))
  }
  
  # Diagnóstico de variabilidad global y por grupo.
  item_variability <- purrr::map_dfr(
    items_present,
    function(v) {
      global_categories <- length(unique(stats::na.omit(dat[[v]])))
      
      by_group <- dat %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::summarise(
          n_categories = length(unique(stats::na.omit(.data[[v]]))),
          .groups = "drop"
        )
      
      tibble(
        item = v,
        global_categories = global_categories,
        min_categories_by_group = min(by_group$n_categories, na.rm = TRUE),
        adequate_variability = global_categories >= 2 &&
          min(by_group$n_categories, na.rm = TRUE) >= 2
      )
    }
  )
  
  problematic_items <- item_variability %>%
    dplyr::filter(!adequate_variability) %>%
    dplyr::pull(item)
  
  if (length(problematic_items) > 0) {
    stop(
      "Hay ítems sin variabilidad suficiente global o por grupo: ",
      paste(problematic_items, collapse = ", "),
      ". Revisa la hoja Item_variability."
    )
  }
  
  list(
    data = dat,
    ordered_items = items_present,
    missing_items = missing_items,
    item_variability = item_variability
  )
}

# ==========================================================
# 7.3. Modelos finales con mejor ajuste
# ==========================================================

invariance_models <- list(
  dim1 = list(
    Domain = "Organizational Readiness",
    model_id = "dim1_F1",
    factor_name = "dim1_F1",
    items = c(
      "p1", "p14", "p16", "p2", "p20",
      "p21", "p22", "p23", "p26", "p27",
      "p28", "p31", "p32", "p34", "p5"
    )
  ),
  
  dim2 = list(
    Domain = "Processes",
    model_id = "dim2_F1",
    factor_name = "dim2_F1",
    items = c(
      "p35", "p39", "p40", "p41", "p43"
    )
  ),
  
  dim3 = list(
    Domain = "Digital Environment",
    model_id = "dim3_F1",
    factor_name = "dim3_F1",
    items = c(
      "p47", "p49", "p52", "p55",
      "p57", "p58", "p59", "p60"
    )
  ),
  
  dim4 = list(
    Domain = "Human Resources",
    model_id = "dim4_F1",
    factor_name = "dim4_F1",
    items = c(
      "p63", "p66", "p67", "p68", "p69"
    )
  ),
  
  dim5 = list(
    Domain = "Regulatory Issues",
    model_id = "dim5_F1",
    factor_name = "dim5_F1",
    items = c(
      "p70", "p72", "p73", "p74", "p75", "p76"
    )
  )
)

model_definitions_inv <- purrr::imap_dfr(
  invariance_models,
  function(x, scale_id) {
    items_present <- intersect(x$items, names(invariance_base))
    
    tibble(
      scale_id = scale_id,
      Domain = x$Domain,
      CFA_model = x$model_id,
      factor_name = x$factor_name,
      n_items_defined = length(x$items),
      n_items_present = length(items_present),
      items_defined = paste(x$items, collapse = ", "),
      items_present = paste(items_present, collapse = ", "),
      missing_items = paste(setdiff(x$items, names(invariance_base)), collapse = ", "),
      syntax = make_1factor_syntax_inv(x$factor_name, items_present)
    )
  }
)

cat("\nModelos definidos para invarianza:\n")
print(model_definitions_inv)

# ==========================================================
# 7.4. Crear variables de agrupación
# ==========================================================

Database_invariance <- invariance_base %>%
  dplyr::mutate(
    # --------------------------
    # Categoría de establecimiento
    # I-1/I-2 vs I-3/I-4
    # --------------------------
    cat_estab_chr = clean_category_code_inv(categoria),
    cat_estab_2 = dplyr::case_when(
      cat_estab_chr %in% c("I-1", "I-2", "1", "2") ~ "I1_I2",
      cat_estab_chr %in% c("I-3", "I-4", "3", "4") ~ "I3_I4",
      TRUE ~ NA_character_
    ),
    
    # --------------------------
    # Rol en telemedicina
    # cargo: 1-2 = responsable/coordinador
    #        3-4 = no responsable/coordinador
    # --------------------------
    cargo_code = to_numeric_inv(cargo),
    telemedicine_role = dplyr::case_when(
      cargo_code %in% c(1, 2) ~ "Responsible/coordinator of telemedicine",
      cargo_code %in% c(3, 4) ~ "Not responsible/coordinator of telemedicine",
      TRUE ~ NA_character_
    ),
    
    # --------------------------
    # Tiempo en IPRESS
    # tiempo_ipress: 1-2 = < 6 meses
    #                3-4 = >= 6 meses
    # --------------------------
    tiempo_ipress_code = to_numeric_inv(tiempo_ipress),
    time_at_ipress_group = dplyr::case_when(
      tiempo_ipress_code %in% c(1, 2) ~ "Less than 6 months",
      tiempo_ipress_code %in% c(3, 4) ~ "6 months or more",
      TRUE ~ NA_character_
    ),
    
    cat_estab_2 = factor(
      cat_estab_2,
      levels = c("I1_I2", "I3_I4")
    ),
    
    telemedicine_role = factor(
      telemedicine_role,
      levels = c(
        "Responsible/coordinator of telemedicine",
        "Not responsible/coordinator of telemedicine"
      )
    ),
    
    time_at_ipress_group = factor(
      time_at_ipress_group,
      levels = c(
        "Less than 6 months",
        "6 months or more"
      )
    )
  ) %>%
  dplyr::select(-cargo_code, -tiempo_ipress_code, -cat_estab_chr)

group_vars_inv <- tibble(
  group_var = c(
    "telemedicine_role",
    "time_at_ipress_group",
    "cat_estab_2"
  ),
  group_label = c(
    "Telemedicine responsibility role",
    "Time working at the IPRESS",
    "Health facility category: I-1/I-2 vs I-3/I-4"
  )
)

group_distribution_inv <- purrr::map_dfr(
  group_vars_inv$group_var,
  function(gv) {
    tb <- as.data.frame(
      table(Database_invariance[[gv]], useNA = "ifany"),
      stringsAsFactors = FALSE
    )
    
    names(tb) <- c("group_level", "n")
    
    tibble(
      group_var = gv,
      group_label = group_vars_inv$group_label[group_vars_inv$group_var == gv],
      group_level = as.character(tb$group_level),
      n = tb$n
    )
  }
)

cat("\nDistribución de grupos:\n")
print(group_distribution_inv)

# ==========================================================
# 7.5. Función principal de invarianza
# ==========================================================

fit_one_invariance_inv <- function(model_info,
                                   data,
                                   group_var,
                                   group_label) {
  
  cat("\n\n############################################################\n")
  cat("ANÁLISIS DE INVARIANZA\n")
  cat("Dominio: ", model_info$Domain, "\n", sep = "")
  cat("Modelo: ", model_info$model_id, "\n", sep = "")
  cat("Grupo: ", group_var, "\n", sep = "")
  cat("############################################################\n")
  
  items <- model_info$items
  items_present <- intersect(items, names(data))
  model_syntax <- make_1factor_syntax_inv(
    factor_name = model_info$factor_name,
    items = items_present
  )
  
  prepared <- tryCatch(
    prepare_ordered_data_inv(
      data = data,
      items = items,
      group_var = group_var
    ),
    error = function(e) e
  )
  
  if (inherits(prepared, "error")) {
    
    failed_results <- tibble(
      Domain = model_info$Domain,
      scale_id = NA_character_,
      Group_var = group_var,
      Group_label = group_label,
      CFA_model = model_info$model_id,
      Factor = model_info$factor_name,
      Invariance_level = c("Configural", "Thresholds", "Metric", "Strict"),
      n_items = length(items_present),
      n_total = NA_integer_,
      chisq = NA_real_,
      df = NA_real_,
      pvalue = NA_real_,
      CFI = NA_real_,
      TLI = NA_real_,
      RMSEA = NA_real_,
      RMSEA_90LI = NA_real_,
      RMSEA_90LS = NA_real_,
      SRMR = NA_real_,
      Delta_CFI = NA_real_,
      Delta_RMSEA = NA_real_,
      Delta_SRMR = NA_real_,
      cfa_converged = FALSE,
      post_check = FALSE,
      cfa_status = "not_estimated",
      Invariance_decision = paste0("Not estimated: ", prepared$message),
      syntax = model_syntax
    )
    
    return(
      list(
        results = failed_results,
        overall = tibble(
          Domain = model_info$Domain,
          Group_var = group_var,
          Group_label = group_label,
          CFA_model = model_info$model_id,
          Factor = model_info$factor_name,
          final_decision = paste0("Not estimated: ", prepared$message)
        ),
        syntax = tibble(
          Domain = model_info$Domain,
          Group_var = group_var,
          Group_label = group_label,
          CFA_model = model_info$model_id,
          Factor = model_info$factor_name,
          syntax = model_syntax
        ),
        item_variability = tibble()
      )
    )
  }
  
  dat <- prepared$data
  ordered_items <- prepared$ordered_items
  
  cat("\nN por grupo:\n")
  print(table(dat[[group_var]], useNA = "ifany"))
  
  cat("\nÍtems usados:\n")
  print(ordered_items)
  
  cat("\nSintaxis CFA:\n")
  cat(model_syntax, "\n")
  
  # --------------------------------------------------------
  # Niveles de invarianza para ítems ordinales
  # --------------------------------------------------------
  # Configural: sin restricciones entre grupos
  # Thresholds: umbrales iguales
  # Metric: umbrales + cargas iguales
  # Strict: umbrales + cargas + residuales iguales
  #
  # Nota: en modelos ordinales con theta, residuals corresponde a
  # una prueba estricta. Por eso no se etiqueta como scalar.
  # --------------------------------------------------------
  
  steps_inv <- list(
    Configural = NULL,
    Thresholds = "thresholds",
    Metric = c("thresholds", "loadings"),
    Strict = c("thresholds", "loadings", "residuals")
  )
  
  safe_fit_inv <- function(step_name, group_equal) {
    
    cat(
      "\nAjustando ",
      model_info$model_id,
      " | ",
      group_var,
      " | ",
      step_name,
      "...\n",
      sep = ""
    )
    
    out <- tryCatch({
      
      syn <- semTools::measEq.syntax(
        configural.model = model_syntax,
        data = dat,
        group = group_var,
        estimator = invariance_estimator,
        parameterization = invariance_parameterization,
        ID.fac = "std.lv",
        ID.cat = "Wu.Estabrook.2016",
        ordered = ordered_items,
        group.equal = group_equal
      )
      
      fit <- lavaan::cfa(
        model = as.character(syn),
        data = dat,
        group = group_var,
        estimator = invariance_estimator,
        parameterization = invariance_parameterization,
        ordered = ordered_items,
        std.lv = TRUE,
        mimic = "Mplus",
        missing = "listwise",
        warn = TRUE
      )
      
      list(
        fit = fit,
        syntax = as.character(syn),
        error = NA_character_
      )
      
    }, error = function(e) {
      
      message(
        "Falló el fit: ",
        model_info$model_id,
        " | grupo = ",
        group_var,
        " | nivel = ",
        step_name,
        " | ",
        e$message
      )
      
      list(
        fit = NULL,
        syntax = NA_character_,
        error = e$message
      )
    })
    
    out
  }
  
  fits <- purrr::imap(
    steps_inv,
    ~ safe_fit_inv(
      step_name = .y,
      group_equal = .x
    )
  )
  
  results <- purrr::imap_dfr(
    fits,
    function(x, step_name) {
      get_scaled_fit_inv(x$fit) %>%
        dplyr::mutate(
          Domain = model_info$Domain,
          Group_var = group_var,
          Group_label = group_label,
          CFA_model = model_info$model_id,
          Factor = model_info$factor_name,
          Invariance_level = step_name,
          n_items = length(ordered_items),
          n_total = nrow(dat),
          n_group_1 = as.integer(table(dat[[group_var]])[1]),
          n_group_2 = as.integer(table(dat[[group_var]])[2]),
          cfa_error = x$error,
          syntax = model_syntax
        )
    }
  ) %>%
    dplyr::mutate(
      Invariance_level = factor(
        Invariance_level,
        levels = c("Configural", "Thresholds", "Metric", "Strict")
      )
    ) %>%
    dplyr::arrange(Invariance_level) %>%
    dplyr::mutate(
      Delta_CFI = CFI - dplyr::lag(CFI),
      Delta_RMSEA = RMSEA - dplyr::lag(RMSEA),
      Delta_SRMR = SRMR - dplyr::lag(SRMR),
      cfa_status = dplyr::case_when(
        !is.na(cfa_error) ~ "not_estimated_lavaan_error",
        cfa_converged ~ "converged",
        TRUE ~ "not_converged"
      ),
      Invariance_decision = dplyr::case_when(
        Invariance_level == "Configural" & cfa_status == "converged" ~
          "Reference model",
        Invariance_level == "Configural" & cfa_status != "converged" ~
          "Reference model did not converge",
        cfa_status != "converged" ~
          "Not evaluated: model did not converge",
        is.na(Delta_CFI) | is.na(Delta_RMSEA) ~
          "Not evaluated",
        Delta_CFI >= delta_cfi_cutoff_inv &
          Delta_RMSEA <= delta_rmsea_cutoff_inv ~
          "Invariant",
        TRUE ~
          "Not invariant"
      )
    ) %>%
    dplyr::select(
      Domain,
      Group_var,
      Group_label,
      CFA_model,
      Factor,
      Invariance_level,
      n_items,
      n_total,
      n_group_1,
      n_group_2,
      chisq,
      df,
      pvalue,
      CFI,
      TLI,
      RMSEA,
      RMSEA_90LI,
      RMSEA_90LS,
      SRMR,
      Delta_CFI,
      Delta_RMSEA,
      Delta_SRMR,
      cfa_converged,
      post_check,
      cfa_status,
      cfa_error,
      Invariance_decision,
      syntax
    )
  
  # Decisión global secuencial
  configural_ok <- results %>%
    dplyr::filter(Invariance_level == "Configural") %>%
    dplyr::pull(cfa_status) == "converged"
  
  nonref <- results %>%
    dplyr::filter(Invariance_level != "Configural")
  
  if (!isTRUE(configural_ok)) {
    
    final_decision <- "Not supported: configural model did not converge"
    
  } else {
    
    bad_position <- which(nonref$Invariance_decision != "Invariant")
    
    if (length(bad_position) == 0) {
      final_decision <- "Measurement invariance supported up to Strict"
    } else if (bad_position[1] == 1) {
      final_decision <- "Only configural invariance supported"
    } else {
      supported_level <- as.character(nonref$Invariance_level[bad_position[1] - 1])
      final_decision <- paste0(
        "Measurement invariance supported up to ",
        supported_level
      )
    }
  }
  
  overall <- tibble(
    Domain = model_info$Domain,
    Group_var = group_var,
    Group_label = group_label,
    CFA_model = model_info$model_id,
    Factor = model_info$factor_name,
    n_items = length(ordered_items),
    n_total = nrow(dat),
    final_decision = final_decision
  )
  
  syntax_tbl <- tibble(
    Domain = model_info$Domain,
    Group_var = group_var,
    Group_label = group_label,
    CFA_model = model_info$model_id,
    Factor = model_info$factor_name,
    syntax = model_syntax
  )
  
  item_variability <- prepared$item_variability %>%
    dplyr::mutate(
      Domain = model_info$Domain,
      Group_var = group_var,
      Group_label = group_label,
      CFA_model = model_info$model_id,
      Factor = model_info$factor_name,
      .before = 1
    )
  
  list(
    results = results,
    overall = overall,
    syntax = syntax_tbl,
    item_variability = item_variability
  )
}

# ==========================================================
# 7.6. Ejecutar 5 modelos x 3 variables de grupo
# ==========================================================

invariance_outputs <- list()

for (m in names(invariance_models)) {
  
  model_info <- invariance_models[[m]]
  
  for (i in seq_len(nrow(group_vars_inv))) {
    
    gv <- group_vars_inv$group_var[i]
    gl <- group_vars_inv$group_label[i]
    
    key <- paste(model_info$model_id, gv, sep = "__")
    
    invariance_outputs[[key]] <- fit_one_invariance_inv(
      model_info = model_info,
      data = Database_invariance,
      group_var = gv,
      group_label = gl
    )
  }
}

# ==========================================================
# 7.7. Consolidar resultados
# ==========================================================

tabla_invariance_full <- purrr::map_dfr(
  invariance_outputs,
  "results"
) %>%
  dplyr::mutate(
    Invariance_level = factor(
      Invariance_level,
      levels = c("Configural", "Thresholds", "Metric", "Strict")
    ),
    dplyr::across(
      c(
        chisq,
        pvalue,
        CFI,
        TLI,
        RMSEA,
        RMSEA_90LI,
        RMSEA_90LS,
        SRMR,
        Delta_CFI,
        Delta_RMSEA,
        Delta_SRMR
      ),
      ~ round(.x, 4)
    )
  ) %>%
  dplyr::arrange(Group_var, Domain, Invariance_level)

tabla_invariance_main <- tabla_invariance_full %>%
  dplyr::select(
    Domain,
    Group_var,
    Group_label,
    CFA_model,
    Factor,
    Invariance_level,
    n_items,
    n_total,
    CFI,
    TLI,
    RMSEA,
    SRMR,
    Delta_CFI,
    Delta_RMSEA,
    Delta_SRMR,
    Invariance_decision
  )

tabla_invariance_overall <- purrr::map_dfr(
  invariance_outputs,
  "overall"
) %>%
  dplyr::arrange(Group_var, Domain)

tabla_invariance_syntax <- purrr::map_dfr(
  invariance_outputs,
  "syntax"
) %>%
  dplyr::distinct()

tabla_item_variability <- purrr::map_dfr(
  invariance_outputs,
  "item_variability"
) %>%
  dplyr::arrange(Group_var, Domain, item)

tabla_invariance_wide <- tabla_invariance_main %>%
  tidyr::pivot_wider(
    id_cols = c(
      Domain,
      Group_var,
      Group_label,
      CFA_model,
      Factor,
      n_items,
      n_total
    ),
    names_from = Invariance_level,
    values_from = c(
      CFI,
      TLI,
      RMSEA,
      SRMR,
      Delta_CFI,
      Delta_RMSEA,
      Delta_SRMR,
      Invariance_decision
    ),
    names_sep = "_"
  )

tabla_invariance_settings <- tibble(
  parameter = c(
    "Base",
    "Estimator",
    "Parameterization",
    "Items treated as",
    "Grouping variables",
    "Delta CFI criterion",
    "Delta RMSEA criterion",
    "Configural model",
    "Thresholds model",
    "Metric model",
    "Strict model"
  ),
  value = c(
    "Database_salud_completo",
    invariance_estimator,
    invariance_parameterization,
    "Ordered categorical",
    paste(group_vars_inv$group_var, collapse = ", "),
    paste0("Delta CFI >= ", delta_cfi_cutoff_inv),
    paste0("Delta RMSEA <= ", delta_rmsea_cutoff_inv),
    "No equality constraints across groups",
    "Equal thresholds across groups",
    "Equal thresholds and factor loadings across groups",
    "Equal thresholds, factor loadings, and residual variances across groups"
  )
)

# ==========================================================
# 7.8. Imprimir resultados principales
# ==========================================================

cat("\n\n############################################################\n")
cat("TABLA PRINCIPAL DE INVARIANZA\n")
cat("############################################################\n")

print(tabla_invariance_main, n = Inf)

cat("\n\n############################################################\n")
cat("DECISIÓN GLOBAL POR MODELO Y GRUPO\n")
cat("############################################################\n")

print(tabla_invariance_overall, n = Inf)

# ==========================================================
# 7.9. Exportar a Excel
# ==========================================================

wb_inv <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb_inv, "Invariance_full")
openxlsx::writeData(wb_inv, "Invariance_full", tabla_invariance_full)

openxlsx::addWorksheet(wb_inv, "Invariance_main")
openxlsx::writeData(wb_inv, "Invariance_main", tabla_invariance_main)

openxlsx::addWorksheet(wb_inv, "Invariance_wide")
openxlsx::writeData(wb_inv, "Invariance_wide", tabla_invariance_wide)

openxlsx::addWorksheet(wb_inv, "Overall_decision")
openxlsx::writeData(wb_inv, "Overall_decision", tabla_invariance_overall)

openxlsx::addWorksheet(wb_inv, "Group_distribution")
openxlsx::writeData(wb_inv, "Group_distribution", group_distribution_inv)

openxlsx::addWorksheet(wb_inv, "Model_definitions")
openxlsx::writeData(wb_inv, "Model_definitions", model_definitions_inv)

openxlsx::addWorksheet(wb_inv, "Item_variability")
openxlsx::writeData(wb_inv, "Item_variability", tabla_item_variability)

openxlsx::addWorksheet(wb_inv, "Model_syntax")
openxlsx::writeData(wb_inv, "Model_syntax", tabla_invariance_syntax)

openxlsx::addWorksheet(wb_inv, "Settings")
openxlsx::writeData(wb_inv, "Settings", tabla_invariance_settings)

wrap_style_inv <- openxlsx::createStyle(wrapText = TRUE, valign = "top")

for (sh in names(wb_inv)) {
  openxlsx::freezePane(wb_inv, sh, firstRow = TRUE)
  openxlsx::setColWidths(wb_inv, sh, cols = 1:80, widths = "auto")
  openxlsx::addStyle(
    wb_inv,
    sh,
    style = wrap_style_inv,
    rows = 1:5000,
    cols = 1:80,
    gridExpand = TRUE,
    stack = TRUE
  )
}

openxlsx::saveWorkbook(
  wb_inv,
  output_excel_invariance,
  overwrite = TRUE
)

cat("\n====================================================\n")
cat("ANÁLISIS DE INVARIANZA COMPLETADO\n")
cat("====================================================\n")
cat("Archivo exportado:", output_excel_invariance, "\n\n")

cat("Hojas exportadas:\n")
cat("- Invariance_full: resultados completos con χ², df, CFI, TLI, RMSEA, SRMR.\n")
cat("- Invariance_main: tabla principal para reporte.\n")
cat("- Invariance_wide: formato ancho por dominio y grupo.\n")
cat("- Overall_decision: decisión global por dominio y grupo.\n")
cat("- Group_distribution: distribución de los tres grupos.\n")
cat("- Model_definitions: modelos finales evaluados.\n")
cat("- Item_variability: diagnóstico de variabilidad ordinal por ítem.\n")
cat("- Model_syntax: sintaxis lavaan de cada modelo.\n")
cat("- Settings: criterios y parámetros usados.\n")






























############################################################
# 8. BAREMOS GENERALES POR DOMINIO
#
# Base: Database_salud_completo
# No se divide por ninguna variable.
# Puntaje por dominio: suma de ítems retenidos en los modelos finales.
# Niveles:
# - Low: minimum observed score to p25
# - Medium: p26 to p74
# - High: p75 to maximum observed score
############################################################

# ==========================================================
# 8.0. Verificaciones iniciales
# ==========================================================

if (!exists("Database_salud_completo")) {
  stop("No existe Database_salud_completo en el entorno.")
}

# Base para baremos
baremo_base <- Database_salud_completo

# Archivo de salida
output_excel_baremo <- "Baremo_percentiles_5domains_overall.xlsx"

# Método de puntaje
score_method_baremo <- "sum"

# Percentiles a reportar
probs_baremo <- c(.01, .05, .10, .15, .25, .50, .75, .85, .90, .95, .99)
prob_names_baremo <- c("p1", "p5", "p10", "p15", "p25", "p50", "p75", "p85", "p90", "p95", "p99")

# Orden de dominios para la tabla final
domain_order_baremo <- c(
  "Digital Environment",
  "Human Resources",
  "Organizational Readiness",
  "Processes",
  "Regulatory Issues"
)

# ==========================================================
# 8.1. Definir ítems finales por dominio
# ==========================================================

items_by_domain_baremo <- list(
  ORG = list(
    Domain = "Organizational Readiness",
    Factor = "dim1_F1",
    items = c(
      "p1", "p14", "p16", "p2", "p20",
      "p21", "p22", "p23", "p26", "p27",
      "p28", "p31", "p32", "p34", "p5"
    )
  ),
  
  PROC = list(
    Domain = "Processes",
    Factor = "dim2_F1",
    items = c(
      "p35", "p39", "p40", "p41", "p43"
    )
  ),
  
  DIG = list(
    Domain = "Digital Environment",
    Factor = "dim3_F1",
    items = c(
      "p47", "p49", "p52", "p55",
      "p57", "p58", "p59", "p60"
    )
  ),
  
  HR = list(
    Domain = "Human Resources",
    Factor = "dim4_F1",
    items = c(
      "p63", "p66", "p67", "p68", "p69"
    )
  ),
  
  REG = list(
    Domain = "Regulatory Issues",
    Factor = "dim5_F1",
    items = c(
      "p70", "p72", "p73", "p74", "p75", "p76"
    )
  )
)

# ==========================================================
# 8.2. Funciones auxiliares
# ==========================================================

to_numeric_baremo <- function(x) {
  if (exists("to_numeric_safe")) {
    return(to_numeric_safe(x))
  }
  
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  
  if (is.factor(x)) {
    out <- suppressWarnings(as.numeric(as.character(x)))
    if (all(is.na(out)) && !all(is.na(x))) out <- as.numeric(x)
    return(out)
  }
  
  if (is.character(x)) {
    return(suppressWarnings(as.numeric(x)))
  }
  
  suppressWarnings(as.numeric(x))
}

score_dimension_baremo <- function(data, items, method = c("sum", "mean")) {
  method <- match.arg(method)
  
  present <- intersect(items, names(data))
  missing_items <- setdiff(items, names(data))
  
  if (length(missing_items) > 0) {
    stop(
      "Faltan ítems en la base: ",
      paste(missing_items, collapse = ", ")
    )
  }
  
  if (length(present) < 2) {
    stop("Quedan menos de 2 ítems presentes para una dimensión. Revisa los nombres.")
  }
  
  x <- data[, present, drop = FALSE]
  x <- as.data.frame(lapply(x, to_numeric_baremo))
  names(x) <- present
  
  n_missing <- rowSums(is.na(x))
  
  if (method == "sum") {
    score <- rowSums(x, na.rm = FALSE)
  } else {
    score <- rowMeans(x, na.rm = FALSE)
  }
  
  score[n_missing > 0] <- NA_real_
  
  score
}

get_percentiles_baremo <- function(x) {
  q <- stats::quantile(
    x,
    probs = probs_baremo,
    na.rm = TRUE,
    type = 7
  )
  
  setNames(as.numeric(q), prob_names_baremo)
}

make_range_baremo <- function(min_score, p25, p75, max_score) {
  min_score <- floor(min_score)
  max_score <- ceiling(max_score)
  
  low_min <- min_score
  low_max <- floor(p25)
  
  high_min <- ceiling(p75)
  high_max <- max_score
  
  medium_min <- low_max + 1
  medium_max <- high_min - 1
  
  if (medium_min > medium_max) {
    medium_min <- low_max
    medium_max <- high_min
  }
  
  tibble::tibble(
    Low = paste0(low_min, "-", low_max),
    Medium = paste0(medium_min, "-", medium_max),
    High = paste0(high_min, "-", high_max),
    low_min = low_min,
    low_max = low_max,
    medium_min = medium_min,
    medium_max = medium_max,
    high_min = high_min,
    high_max = high_max
  )
}

format_num_baremo <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    NA_character_,
    formatC(x, format = "f", digits = digits)
  )
}

format_int_baremo <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    as.character(round(x, 0))
  )
}

# ==========================================================
# 8.3. Crear puntajes por dominio
# ==========================================================

Database_baremo_scores <- baremo_base

for (domain_id in names(items_by_domain_baremo)) {
  item_info <- items_by_domain_baremo[[domain_id]]
  
  score_name <- paste0("score_", domain_id)
  
  Database_baremo_scores[[score_name]] <- score_dimension_baremo(
    data = Database_baremo_scores,
    items = item_info$items,
    method = score_method_baremo
  )
}

# ==========================================================
# 8.4. Definición de dominios
# ==========================================================

baremo_model_definitions <- purrr::imap_dfr(
  items_by_domain_baremo,
  function(x, domain_id) {
    items_present <- intersect(x$items, names(baremo_base))
    
    tibble::tibble(
      Domain_ID = domain_id,
      Domain = x$Domain,
      Factor = x$Factor,
      n_items = length(x$items),
      n_items_present = length(items_present),
      items = paste(x$items, collapse = ", "),
      missing_items = paste(setdiff(x$items, names(baremo_base)), collapse = ", ")
    )
  }
)

print(baremo_model_definitions)

# ==========================================================
# 8.5. Tabla larga de baremos
# ==========================================================

baremo_long_raw <- Database_baremo_scores %>%
  dplyr::select(dplyr::starts_with("score_")) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("score_"),
    names_to = "Domain_ID",
    values_to = "Score"
  ) %>%
  dplyr::mutate(
    Domain_ID = gsub("^score_", "", Domain_ID)
  ) %>%
  dplyr::left_join(
    baremo_model_definitions %>%
      dplyr::select(Domain_ID, Domain, Factor, n_items),
    by = "Domain_ID"
  ) %>%
  dplyr::group_by(Domain_ID, Domain, Factor, n_items) %>%
  dplyr::summarise(
    N = sum(!is.na(Score)),
    Min = min(Score, na.rm = TRUE),
    Max = max(Score, na.rm = TRUE),
    M = mean(Score, na.rm = TRUE),
    SD = ifelse(N > 1, stats::sd(Score, na.rm = TRUE), NA_real_),
    Median = stats::median(Score, na.rm = TRUE),
    q = list(get_percentiles_baremo(Score)),
    .groups = "drop"
  ) %>%
  tidyr::unnest_wider(q) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    ranges = list(make_range_baremo(Min, p25, p75, Max))
  ) %>%
  tidyr::unnest_wider(ranges) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Domain = factor(Domain, levels = domain_order_baremo)
  ) %>%
  dplyr::arrange(Domain)

# Versión larga para reporte con redondeo
baremo_long_report <- baremo_long_raw %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(prob_names_baremo),
      ~ round(.x, 0)
    ),
    M = round(M, 2),
    SD = round(SD, 2),
    Median = round(Median, 0),
    Min = round(Min, 0),
    Max = round(Max, 0)
  ) %>%
  dplyr::select(
    Domain_ID,
    Domain,
    Factor,
    n_items,
    N,
    Min,
    Max,
    dplyr::all_of(prob_names_baremo),
    M,
    SD,
    Median,
    Low,
    Medium,
    High,
    low_min,
    low_max,
    medium_min,
    medium_max,
    high_min,
    high_max
  )

print(baremo_long_report)

# ==========================================================
# 8.6. Clasificar cada observación en Low / Medium / High
# ==========================================================

baremo_cutoffs <- baremo_long_raw %>%
  dplyr::select(
    Domain_ID,
    Domain,
    Factor,
    Low,
    Medium,
    High,
    low_min,
    low_max,
    medium_min,
    medium_max,
    high_min,
    high_max
  )

for (domain_id in names(items_by_domain_baremo)) {
  score_var <- paste0("score_", domain_id)
  level_var <- paste0("level_", domain_id)
  
  cut_row <- baremo_cutoffs %>%
    dplyr::filter(Domain_ID == domain_id)
  
  Database_baremo_scores[[level_var]] <- dplyr::case_when(
    is.na(Database_baremo_scores[[score_var]]) ~ NA_character_,
    Database_baremo_scores[[score_var]] <= cut_row$low_max ~ "Low",
    Database_baremo_scores[[score_var]] >= cut_row$high_min ~ "High",
    TRUE ~ "Medium"
  )
  
  Database_baremo_scores[[level_var]] <- factor(
    Database_baremo_scores[[level_var]],
    levels = c("Low", "Medium", "High")
  )
}

# Distribución de niveles por dominio
baremo_level_distribution <- purrr::imap_dfr(
  items_by_domain_baremo,
  function(x, domain_id) {
    level_var <- paste0("level_", domain_id)
    
    tb <- as.data.frame(
      table(Database_baremo_scores[[level_var]], useNA = "ifany"),
      stringsAsFactors = FALSE
    )
    
    names(tb) <- c("Level", "n")
    
    total_valid <- sum(tb$n[!is.na(tb$Level) & tb$Level != "<NA>"])
    
    tb %>%
      dplyr::mutate(
        Domain_ID = domain_id,
        Domain = x$Domain,
        Factor = x$Factor,
        percent = ifelse(total_valid > 0, n / total_valid * 100, NA_real_),
        percent = round(percent, 2),
        .before = 1
      )
  }
)

print(baremo_level_distribution)

# ==========================================================
# 8.7. Tabla tipo manuscrito / Word
# ==========================================================

table5_metric_order <- c(
  "p1", "p5", "p10", "p15", "p25",
  "p50", "p75", "p85", "p90", "p95", "p99",
  "M", "SD", "Low", "Medium", "High"
)

table5_metric_labels <- c(
  p1 = "p1",
  p5 = "p5",
  p10 = "p10",
  p15 = "p15",
  p25 = "p25",
  p50 = "p50 (Median)",
  p75 = "p75",
  p85 = "p85",
  p90 = "p90",
  p95 = "p95",
  p99 = "p99",
  M = "M",
  SD = "SD",
  Low = "Low",
  Medium = "Medium",
  High = "High"
)

table5_ready <- baremo_long_raw %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(prob_names_baremo),
      format_int_baremo
    ),
    M = format_num_baremo(M, 2),
    SD = format_num_baremo(SD, 2)
  ) %>%
  dplyr::select(
    Domain,
    dplyr::all_of(table5_metric_order)
  ) %>%
  tidyr::pivot_longer(
    cols = -Domain,
    names_to = "Statistic",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Statistic = dplyr::recode(
      Statistic,
      !!!table5_metric_labels
    ),
    Statistic = factor(
      Statistic,
      levels = unname(table5_metric_labels[table5_metric_order])
    ),
    Domain = factor(Domain, levels = domain_order_baremo)
  ) %>%
  tidyr::pivot_wider(
    names_from = Domain,
    values_from = Value
  ) %>%
  dplyr::arrange(Statistic)

print(table5_ready, n = Inf)

# ==========================================================
# 8.8. Título y nota para el manuscrito
# ==========================================================

n_baremo <- nrow(baremo_base)

table5_title <- paste0(
  "Table 5. Normative Values Based on Percentiles for the Five Dimensions (n = ",
  n_baremo,
  ")."
)

table5_note <- paste0(
  "Note. Scores were calculated as the sum of the retained items for each dimension. ",
  "Low, medium, and high levels were constructed using percentile-based cutoffs: ",
  "low = minimum observed score to the 25th percentile; medium = 26th to 74th percentile; ",
  "and high = 75th percentile to the maximum observed score. ",
  "M = mean; SD = standard deviation."
)

cat("\n", table5_title, "\n\n", sep = "")
print(table5_ready, n = Inf)
cat("\n", table5_note, "\n", sep = "")

# ==========================================================
# 8.9. Exportar a Excel
# ==========================================================

wb_baremo <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb_baremo, "Table5_ready")
openxlsx::writeData(wb_baremo, "Table5_ready", table5_ready)

openxlsx::addWorksheet(wb_baremo, "Baremo_long")
openxlsx::writeData(wb_baremo, "Baremo_long", baremo_long_report)

openxlsx::addWorksheet(wb_baremo, "Cutoffs")
openxlsx::writeData(wb_baremo, "Cutoffs", baremo_cutoffs)

openxlsx::addWorksheet(wb_baremo, "Level_distribution")
openxlsx::writeData(wb_baremo, "Level_distribution", baremo_level_distribution)

openxlsx::addWorksheet(wb_baremo, "Scores_by_case")
openxlsx::writeData(wb_baremo, "Scores_by_case", Database_baremo_scores)

openxlsx::addWorksheet(wb_baremo, "Model_definitions")
openxlsx::writeData(wb_baremo, "Model_definitions", baremo_model_definitions)

openxlsx::addWorksheet(wb_baremo, "Title_note")
openxlsx::writeData(
  wb_baremo,
  "Title_note",
  tibble::tibble(
    Element = c("Title", "Note"),
    Text = c(table5_title, table5_note)
  )
)

wrap_style_baremo <- openxlsx::createStyle(
  wrapText = TRUE,
  valign = "top"
)

for (sh in names(wb_baremo)) {
  openxlsx::freezePane(wb_baremo, sh, firstRow = TRUE)
  openxlsx::setColWidths(wb_baremo, sh, cols = 1:80, widths = "auto")
  openxlsx::addStyle(
    wb_baremo,
    sh,
    style = wrap_style_baremo,
    rows = 1:5000,
    cols = 1:80,
    gridExpand = TRUE,
    stack = TRUE
  )
}

openxlsx::saveWorkbook(
  wb_baremo,
  output_excel_baremo,
  overwrite = TRUE
)

cat("\n====================================================\n")
cat("BAREMOS GENERALES COMPLETADOS\n")
cat("====================================================\n")
cat("Archivo exportado:", output_excel_baremo, "\n\n")

cat("Hojas exportadas:\n")
cat("- Table5_ready: tabla final lista para Word/manuscrito.\n")
cat("- Baremo_long: percentiles, M, SD, Min, Max y rangos por dominio.\n")
cat("- Cutoffs: puntos de corte para Low, Medium y High.\n")
cat("- Level_distribution: distribución de participantes por nivel.\n")
cat("- Scores_by_case: base con puntajes y clasificación por dominio.\n")
cat("- Model_definitions: ítems usados en cada dominio.\n")
cat("- Title_note: título y nota de tabla.\n")





































############################################################
# 9. RELACIÓN ENTRE p_v2 Y LOS CINCO DOMINIOS
#
# Base: Database_salud_completo
# Outcome continuo/proporcional:
# p_v2 = proporción de uso de telemedicina en el establecimiento
#
# Análisis:
# 1) Percentiles descriptivos de p_v2
# 2) Descriptivos de los cinco dominios
# 3) Correlaciones Pearson y Spearman entre p_v2 y cada dominio
# 4) Regresión lineal múltiple con puntajes continuos
# 5) Modelo lineal de sensibilidad con log1p(100*p_v2)
# 6) Modelos logístico y Poisson robusto usando:
#    - outcome: p_v2_high = cuartil superior de p_v2
#    - predictores: dominios High vs Low/Medium
# 7) Plots de correlación
#
# IMPORTANTE:
# Este bloque reemplaza TODA la sección 9 anterior.
# No pegues patches previos ni subsecciones duplicadas.
############################################################

# ==========================================================
# 9.0. Paquetes y verificaciones
# ==========================================================

required_packages_pv2 <- c(
  "dplyr",
  "tidyr",
  "tibble",
  "purrr",
  "openxlsx",
  "haven",
  "ggplot2",
  "sandwich"
)

missing_packages_pv2 <- required_packages_pv2[
  !sapply(required_packages_pv2, requireNamespace, quietly = TRUE)
]

if (length(missing_packages_pv2) > 0) {
  stop(
    "Instala los paquetes faltantes antes de correr esta sección: ",
    paste0(
      "install.packages(c('",
      paste(missing_packages_pv2, collapse = "', '"),
      "'))"
    )
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(openxlsx)
  library(haven)
  library(ggplot2)
  library(sandwich)
})

if (!exists("Database_salud_completo")) {
  stop("No existe Database_salud_completo en el entorno.")
}

if (!"p_v2" %in% names(Database_salud_completo)) {
  stop("No encuentro la variable p_v2 en Database_salud_completo.")
}

output_excel_pv2 <- "PV2_correlations_linear_logistic_poisson_models.xlsx"

# ==========================================================
# 9.1. Funciones auxiliares generales
# ==========================================================

to_numeric_pv2 <- function(x) {
  if (exists("to_numeric_safe")) {
    return(to_numeric_safe(x))
  }
  
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  
  if (is.factor(x)) {
    out <- suppressWarnings(as.numeric(as.character(x)))
    if (all(is.na(out)) && !all(is.na(x))) {
      out <- as.numeric(x)
    }
    return(out)
  }
  
  if (is.character(x)) {
    return(suppressWarnings(as.numeric(x)))
  }
  
  suppressWarnings(as.numeric(x))
}

format_p_value_pv2 <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

score_dimension_pv2 <- function(data, items, method = c("sum", "mean")) {
  method <- match.arg(method)
  
  present <- intersect(items, names(data))
  missing_items <- setdiff(items, names(data))
  
  if (length(missing_items) > 0) {
    stop(
      "Faltan ítems en la base: ",
      paste(missing_items, collapse = ", ")
    )
  }
  
  if (length(present) < 2) {
    stop("Quedan menos de 2 ítems presentes para una dimensión.")
  }
  
  x <- data[, present, drop = FALSE]
  x <- as.data.frame(lapply(x, to_numeric_pv2))
  names(x) <- present
  
  if (method == "sum") {
    return(rowSums(x, na.rm = FALSE))
  }
  
  rowMeans(x, na.rm = FALSE)
}

cor_test_row_pv2 <- function(data, x_var, y_var, method) {
  x <- data[[x_var]]
  y <- data[[y_var]]
  
  ok <- complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]
  
  if (length(x) < 3 || length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(tibble(
      Score_variable = x_var,
      Outcome = y_var,
      Method = tools::toTitleCase(method),
      N = length(x),
      Correlation = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      Statistic = NA_real_,
      p_value = NA_real_,
      p_value_formatted = NA_character_,
      Status = "not_estimated",
      Error = "Insufficient variability or insufficient complete observations."
    ))
  }
  
  ct <- tryCatch(
    suppressWarnings(
      stats::cor.test(
        x = x,
        y = y,
        method = method,
        exact = FALSE
      )
    ),
    error = function(e) e
  )
  
  if (inherits(ct, "error")) {
    return(tibble(
      Score_variable = x_var,
      Outcome = y_var,
      Method = tools::toTitleCase(method),
      N = length(x),
      Correlation = NA_real_,
      CI_low = NA_real_,
      CI_high = NA_real_,
      Statistic = NA_real_,
      p_value = NA_real_,
      p_value_formatted = NA_character_,
      Status = "not_estimated",
      Error = ct$message
    ))
  }
  
  ci_low <- NA_real_
  ci_high <- NA_real_
  
  if (!is.null(ct$conf.int) && method == "pearson") {
    ci_low <- as.numeric(ct$conf.int[1])
    ci_high <- as.numeric(ct$conf.int[2])
  }
  
  tibble(
    Score_variable = x_var,
    Outcome = y_var,
    Method = tools::toTitleCase(method),
    N = length(x),
    Correlation = as.numeric(ct$estimate),
    CI_low = ci_low,
    CI_high = ci_high,
    Statistic = as.numeric(ct$statistic),
    p_value = as.numeric(ct$p.value),
    p_value_formatted = format_p_value_pv2(as.numeric(ct$p.value)),
    Status = "estimated",
    Error = NA_character_
  )
}

# ==========================================================
# 9.2. Ítems finales por dominio
# ==========================================================

items_by_domain_pv2 <- list(
  ORG = list(
    Domain = "Organizational Readiness",
    Score = "score_ORG",
    Items = c(
      "p1", "p14", "p16", "p2", "p20",
      "p21", "p22", "p23", "p26", "p27",
      "p28", "p31", "p32", "p34", "p5"
    )
  ),
  PROC = list(
    Domain = "Processes",
    Score = "score_PROC",
    Items = c("p35", "p39", "p40", "p41", "p43")
  ),
  DIG = list(
    Domain = "Digital Environment",
    Score = "score_DIG",
    Items = c("p47", "p49", "p52", "p55", "p57", "p58", "p59", "p60")
  ),
  HR = list(
    Domain = "Human Resources",
    Score = "score_HR",
    Items = c("p63", "p66", "p67", "p68", "p69")
  ),
  REG = list(
    Domain = "Regulatory Issues",
    Score = "score_REG",
    Items = c("p70", "p72", "p73", "p74", "p75", "p76")
  )
)

domain_lookup_pv2 <- purrr::imap_dfr(
  items_by_domain_pv2,
  function(x, id) {
    tibble(
      Domain_ID = id,
      Domain = x$Domain,
      Score_variable = x$Score,
      n_items = length(x$Items),
      Items = paste(x$Items, collapse = ", ")
    )
  }
)

score_vars_pv2 <- domain_lookup_pv2$Score_variable

# ==========================================================
# 9.3. Crear puntajes por dominio y p_v2 proporcional
# ==========================================================

Database_pv2_analysis <- Database_salud_completo

for (id in names(items_by_domain_pv2)) {
  info <- items_by_domain_pv2[[id]]
  
  Database_pv2_analysis[[info$Score]] <- score_dimension_pv2(
    data = Database_pv2_analysis,
    items = info$Items,
    method = "sum"
  )
}

Database_pv2_analysis <- Database_pv2_analysis %>%
  mutate(
    p_v2_raw = to_numeric_pv2(p_v2)
  )

max_pv2_observed <- max(Database_pv2_analysis$p_v2_raw, na.rm = TRUE)

Database_pv2_analysis <- Database_pv2_analysis %>%
  mutate(
    p_v2_prop = case_when(
      is.na(p_v2_raw) ~ NA_real_,
      max_pv2_observed > 1 ~ p_v2_raw / 100,
      TRUE ~ p_v2_raw
    ),
    p_v2_prop = pmin(pmax(p_v2_prop, 0), 1)
  )

Database_pv2_model <- Database_pv2_analysis %>%
  filter(
    !is.na(p_v2_prop),
    if_all(all_of(score_vars_pv2), ~ !is.na(.x))
  )

cat("\nN total en Database_salud_completo:", nrow(Database_salud_completo), "\n")
cat("N analítico con p_v2 y dominios completos:", nrow(Database_pv2_model), "\n")

# ==========================================================
# 9.4. Percentiles de p_v2
# ==========================================================

probs_pv2 <- c(0.01, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.99)

q_pv2 <- stats::quantile(
  Database_pv2_model$p_v2_prop,
  probs = probs_pv2,
  na.rm = TRUE,
  type = 7
)

tabla_percentiles_pv2 <- tibble(
  Indicator = c(
    "p1", "p5", "p10", "p15", "p25",
    "p50 (Median)", "p75", "p85", "p90", "p95", "p99",
    "M", "SD", "Minimum", "Maximum", "Low", "Medium", "High"
  ),
  Value = c(
    round(q_pv2["1%"], 4),
    round(q_pv2["5%"], 4),
    round(q_pv2["10%"], 4),
    round(q_pv2["15%"], 4),
    round(q_pv2["25%"], 4),
    round(q_pv2["50%"], 4),
    round(q_pv2["75%"], 4),
    round(q_pv2["85%"], 4),
    round(q_pv2["90%"], 4),
    round(q_pv2["95%"], 4),
    round(q_pv2["99%"], 4),
    round(mean(Database_pv2_model$p_v2_prop, na.rm = TRUE), 4),
    round(sd(Database_pv2_model$p_v2_prop, na.rm = TRUE), 4),
    round(min(Database_pv2_model$p_v2_prop, na.rm = TRUE), 4),
    round(max(Database_pv2_model$p_v2_prop, na.rm = TRUE), 4),
    paste0(round(min(Database_pv2_model$p_v2_prop, na.rm = TRUE), 4), " - ", round(q_pv2["25%"], 4)),
    paste0("> ", round(q_pv2["25%"], 4), " and < ", round(q_pv2["75%"], 4)),
    paste0(round(q_pv2["75%"], 4), " - ", round(max(Database_pv2_model$p_v2_prop, na.rm = TRUE), 4))
  )
)

print(tabla_percentiles_pv2, n = Inf)

# ==========================================================
# 9.5. Estadísticos descriptivos de dimensiones
# ==========================================================

domain_descriptives_pv2 <- Database_pv2_model %>%
  select(all_of(score_vars_pv2)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Score_variable",
    values_to = "Score"
  ) %>%
  group_by(Score_variable) %>%
  summarise(
    N = sum(!is.na(Score)),
    M = mean(Score, na.rm = TRUE),
    SD = sd(Score, na.rm = TRUE),
    Median = median(Score, na.rm = TRUE),
    Minimum = min(Score, na.rm = TRUE),
    Maximum = max(Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(domain_lookup_pv2, by = "Score_variable") %>%
  select(Domain_ID, Domain, Score_variable, n_items, N, M, SD, Median, Minimum, Maximum) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

print(domain_descriptives_pv2, n = Inf)

# ==========================================================
# 9.6. Correlaciones Pearson y Spearman
# ==========================================================

correlations_pv2 <- purrr::map_dfr(
  score_vars_pv2,
  function(v) {
    bind_rows(
      cor_test_row_pv2(Database_pv2_model, v, "p_v2_prop", "pearson"),
      cor_test_row_pv2(Database_pv2_model, v, "p_v2_prop", "spearman")
    )
  }
) %>%
  left_join(domain_lookup_pv2, by = "Score_variable") %>%
  select(
    Domain_ID, Domain, Score_variable, Method, N,
    Correlation, CI_low, CI_high, Statistic,
    p_value, p_value_formatted, Status, Error
  ) %>%
  mutate(across(c(Correlation, CI_low, CI_high, Statistic, p_value), ~ round(.x, 4)))

print(correlations_pv2, n = Inf)

correlations_pv2_wide <- correlations_pv2 %>%
  mutate(
    Correlation_p = ifelse(
      is.na(Correlation),
      NA_character_,
      paste0(sprintf("%.3f", Correlation), " (p = ", p_value_formatted, ")")
    )
  ) %>%
  select(Domain, Method, Correlation_p) %>%
  pivot_wider(names_from = Method, values_from = Correlation_p)

print(correlations_pv2_wide, n = Inf)

# ==========================================================
# 9.7. Funciones seguras para modelos
# ==========================================================

term_labels_pv2 <- c(
  "(Intercept)" = "Intercept",
  "z_score_ORG" = "Organizational Readiness",
  "z_score_PROC" = "Processes",
  "z_score_DIG" = "Digital Environment",
  "z_score_HR" = "Human Resources",
  "z_score_REG" = "Regulatory Issues",
  "score_ORG_highHigh" = "Organizational Readiness: High vs Low/Medium",
  "score_PROC_highHigh" = "Processes: High vs Low/Medium",
  "score_DIG_highHigh" = "Digital Environment: High vs Low/Medium",
  "score_HR_highHigh" = "Human Resources: High vs Low/Medium",
  "score_REG_highHigh" = "Regulatory Issues: High vs Low/Medium"
)

label_terms_pv2 <- function(x) {
  out <- term_labels_pv2[x]
  out <- ifelse(is.na(out), x, out)
  unname(out)
}

make_lm_error_table_pv2 <- function(model_name, error_message) {
  tibble(
    Model = model_name, Term = NA_character_, Term_label = NA_character_,
    Beta = NA_real_, Robust_SE = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
    t = NA_real_, p_value = NA_real_, p_value_formatted = NA_character_,
    Status = "not_estimated", Error = error_message
  )
}

make_lm_fit_error_pv2 <- function(model_name, error_message) {
  tibble(
    Model = model_name, N = NA_integer_, R2 = NA_real_, Adjusted_R2 = NA_real_,
    RMSE = NA_real_, MAE = NA_real_, Residual_SE = NA_real_, F = NA_real_,
    df1 = NA_real_, df2 = NA_real_, Model_p_value = NA_real_,
    Model_p_value_formatted = NA_character_, AIC = NA_real_, BIC = NA_real_,
    Status = "not_estimated", Error = error_message
  )
}

make_glm_error_table_pv2 <- function(model_name, effect_label, error_message) {
  tibble(
    Model = model_name, Effect = effect_label, Term = NA_character_,
    Term_label = NA_character_, Beta_log = NA_real_, Robust_SE = NA_real_,
    Estimate = NA_real_, CI_low = NA_real_, CI_high = NA_real_, z = NA_real_,
    p_value = NA_real_, p_value_formatted = NA_character_,
    Status = "not_estimated", Error = error_message
  )
}

make_glm_fit_error_pv2 <- function(model_name, error_message) {
  tibble(
    Model = model_name, N = NA_integer_, AIC = NA_real_, BIC = NA_real_,
    McFadden_R2 = NA_real_, Brier = NA_real_, AUC_ROC = NA_real_,
    Status = "not_estimated", Error = error_message
  )
}

make_poisson_fit_error_pv2 <- function(model_name, error_message) {
  tibble(
    Model = model_name, N = NA_integer_, AIC = NA_real_, BIC = NA_real_,
    Deviance = NA_real_, df_residual = NA_real_,
    Status = "not_estimated", Error = error_message
  )
}

tidy_lm_robust_pv2 <- function(model, model_name) {
  beta <- tryCatch(stats::coef(model), error = function(e) NULL)
  
  if (is.null(beta)) {
    return(make_lm_error_table_pv2(model_name, "Could not extract coefficients."))
  }
  
  vc <- tryCatch(
    sandwich::vcovHC(model, type = "HC3"),
    error = function(e) stats::vcov(model)
  )
  
  se <- sqrt(diag(vc))
  se <- se[names(beta)]
  
  t_value <- beta / se
  p_value <- 2 * stats::pt(abs(t_value), df = stats::df.residual(model), lower.tail = FALSE)
  
  tibble(
    Model = model_name,
    Term = names(beta),
    Term_label = label_terms_pv2(names(beta)),
    Beta = as.numeric(beta),
    Robust_SE = as.numeric(se),
    CI_low = as.numeric(beta - 1.96 * se),
    CI_high = as.numeric(beta + 1.96 * se),
    t = as.numeric(t_value),
    p_value = as.numeric(p_value),
    p_value_formatted = format_p_value_pv2(as.numeric(p_value)),
    Status = "estimated",
    Error = NA_character_
  ) %>%
    mutate(across(c(Beta, Robust_SE, CI_low, CI_high, t, p_value), ~ round(.x, 4)))
}

tidy_glm_exp_robust_pv2 <- function(model, model_name, effect_label = "OR") {
  beta <- tryCatch(stats::coef(model), error = function(e) NULL)
  
  if (is.null(beta)) {
    return(make_glm_error_table_pv2(model_name, effect_label, "Could not extract coefficients."))
  }
  
  vc <- tryCatch(
    sandwich::vcovHC(model, type = "HC0"),
    error = function(e) stats::vcov(model)
  )
  
  se <- sqrt(diag(vc))
  se <- se[names(beta)]
  
  z_value <- beta / se
  p_value <- 2 * stats::pnorm(abs(z_value), lower.tail = FALSE)
  
  tibble(
    Model = model_name,
    Effect = effect_label,
    Term = names(beta),
    Term_label = label_terms_pv2(names(beta)),
    Beta_log = as.numeric(beta),
    Robust_SE = as.numeric(se),
    Estimate = as.numeric(exp(beta)),
    CI_low = as.numeric(exp(beta - 1.96 * se)),
    CI_high = as.numeric(exp(beta + 1.96 * se)),
    z = as.numeric(z_value),
    p_value = as.numeric(p_value),
    p_value_formatted = format_p_value_pv2(as.numeric(p_value)),
    Status = "estimated",
    Error = NA_character_
  ) %>%
    mutate(across(c(Beta_log, Robust_SE, Estimate, CI_low, CI_high, z, p_value), ~ round(.x, 4)))
}

extract_lm_fit_pv2_safe <- function(model, data_model, outcome_var, model_name) {
  s <- summary(model)
  observed <- data_model[[outcome_var]]
  fitted_values <- stats::fitted(model)
  
  rmse <- sqrt(mean((observed - fitted_values)^2, na.rm = TRUE))
  mae <- mean(abs(observed - fitted_values), na.rm = TRUE)
  
  fstat <- s$fstatistic
  f_value <- f_df1 <- f_df2 <- f_p <- NA_real_
  
  if (!is.null(fstat)) {
    f_value <- as.numeric(fstat[1])
    f_df1 <- as.numeric(fstat[2])
    f_df2 <- as.numeric(fstat[3])
    f_p <- stats::pf(f_value, f_df1, f_df2, lower.tail = FALSE)
  }
  
  tibble(
    Model = model_name,
    N = stats::nobs(model),
    R2 = s$r.squared,
    Adjusted_R2 = s$adj.r.squared,
    RMSE = rmse,
    MAE = mae,
    Residual_SE = s$sigma,
    F = f_value,
    df1 = f_df1,
    df2 = f_df2,
    Model_p_value = f_p,
    Model_p_value_formatted = format_p_value_pv2(f_p),
    AIC = stats::AIC(model),
    BIC = stats::BIC(model),
    Status = "estimated",
    Error = NA_character_
  ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
}

extract_glm_fit_pv2_safe <- function(model, data_model, outcome_var, model_name) {
  y <- data_model[[outcome_var]]
  p <- stats::predict(model, type = "response")
  
  brier <- mean((y - p)^2, na.rm = TRUE)
  
  null_model <- tryCatch(
    stats::glm(stats::as.formula(paste0(outcome_var, " ~ 1")), data = data_model, family = stats::binomial()),
    error = function(e) NULL
  )
  
  mcfadden_r2 <- NA_real_
  
  if (!is.null(null_model)) {
    mcfadden_r2 <- tryCatch(
      1 - as.numeric(stats::logLik(model) / stats::logLik(null_model)),
      error = function(e) NA_real_
    )
  }
  
  roc_auc <- NA_real_
  
  if (requireNamespace("pROC", quietly = TRUE) && length(unique(y)) == 2) {
    roc_auc <- tryCatch(
      as.numeric(pROC::auc(pROC::roc(response = y, predictor = p, quiet = TRUE))),
      error = function(e) NA_real_
    )
  }
  
  tibble(
    Model = model_name,
    N = stats::nobs(model),
    AIC = stats::AIC(model),
    BIC = stats::BIC(model),
    McFadden_R2 = mcfadden_r2,
    Brier = brier,
    AUC_ROC = roc_auc,
    Status = "estimated",
    Error = NA_character_
  ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
}

extract_poisson_fit_pv2_safe <- function(model, model_name) {
  tibble(
    Model = model_name,
    N = stats::nobs(model),
    AIC = stats::AIC(model),
    BIC = stats::BIC(model),
    Deviance = stats::deviance(model),
    df_residual = stats::df.residual(model),
    Status = "estimated",
    Error = NA_character_
  ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
}

# ==========================================================
# 9.8. Crear z-scores, dominios altos y outcome binario
# ==========================================================

Database_pv2_model <- Database_pv2_model %>%
  mutate(
    z_score_ORG = as.numeric(scale(score_ORG)),
    z_score_PROC = as.numeric(scale(score_PROC)),
    z_score_DIG = as.numeric(scale(score_DIG)),
    z_score_HR = as.numeric(scale(score_HR)),
    z_score_REG = as.numeric(scale(score_REG))
  )

cat("\nVerificación de variables z creadas:\n")
print(
  names(Database_pv2_model)[
    names(Database_pv2_model) %in%
      c("z_score_ORG", "z_score_PROC", "z_score_DIG", "z_score_HR", "z_score_REG")
  ]
)

domain_high_cutoffs_pv2 <- Database_pv2_model %>%
  summarise(
    across(
      all_of(score_vars_pv2),
      ~ ceiling(as.numeric(stats::quantile(.x, probs = 0.75, na.rm = TRUE, type = 7)))
    )
  ) %>%
  pivot_longer(cols = everything(), names_to = "Score_variable", values_to = "High_cutoff") %>%
  left_join(domain_lookup_pv2, by = "Score_variable") %>%
  select(Domain_ID, Domain, Score_variable, High_cutoff)

print(domain_high_cutoffs_pv2, n = Inf)

for (i in seq_len(nrow(domain_high_cutoffs_pv2))) {
  score_var <- domain_high_cutoffs_pv2$Score_variable[i]
  cutoff <- domain_high_cutoffs_pv2$High_cutoff[i]
  high_var <- paste0(score_var, "_high")
  
  Database_pv2_model[[high_var]] <- ifelse(
    Database_pv2_model[[score_var]] >= cutoff,
    "High",
    "Low_Medium"
  )
  
  Database_pv2_model[[high_var]] <- factor(
    Database_pv2_model[[high_var]],
    levels = c("Low_Medium", "High")
  )
}

domain_high_vars_pv2 <- paste0(score_vars_pv2, "_high")

# Outcome binario:
# p_v2_high = cuartil superior de uso de telemedicina.
# Si quieres usar 10% como punto de corte, cambia la siguiente línea por:
# p_v2_high_cutoff <- 0.10

p_v2_high_cutoff <- as.numeric(
  stats::quantile(Database_pv2_model$p_v2_prop, probs = 0.75, na.rm = TRUE, type = 7)
)

Database_pv2_model <- Database_pv2_model %>%
  mutate(
    p_v2_high = as.integer(p_v2_prop >= p_v2_high_cutoff),
    p_v2_high_label = factor(
      ifelse(p_v2_high == 1, "High_p_v2", "Low_Medium_p_v2"),
      levels = c("Low_Medium_p_v2", "High_p_v2")
    ),
    p_v2_log1p = log1p(100 * p_v2_prop)
  )

p_v2_binary_distribution <- tibble(
  Outcome = "p_v2_high",
  Definition = paste0("p_v2_prop >= p75 = ", round(p_v2_high_cutoff, 4)),
  N = nrow(Database_pv2_model),
  Cases_high = sum(Database_pv2_model$p_v2_high == 1, na.rm = TRUE),
  Non_cases_low_medium = sum(Database_pv2_model$p_v2_high == 0, na.rm = TRUE),
  Prevalence_high = mean(Database_pv2_model$p_v2_high == 1, na.rm = TRUE)
) %>%
  mutate(Prevalence_high = round(Prevalence_high, 4))

print(p_v2_binary_distribution)

# ==========================================================
# 9.9. Modelos lineales
# ==========================================================

# Modelo lineal 1: p_v2 continuo ~ scores continuos estandarizados
lm_formula_pv2_cont <- p_v2_prop ~ z_score_ORG + z_score_PROC + z_score_DIG + z_score_HR + z_score_REG

lm_pv2_cont <- tryCatch(
  stats::lm(formula = lm_formula_pv2_cont, data = Database_pv2_model),
  error = function(e) e
)

if (inherits(lm_pv2_cont, "error")) {
  lm_coefficients_pv2_cont <- make_lm_error_table_pv2("Linear model: continuous domain scores", lm_pv2_cont$message)
  lm_fit_pv2_cont <- make_lm_fit_error_pv2("Linear model: continuous domain scores", lm_pv2_cont$message)
} else {
  lm_coefficients_pv2_cont <- tidy_lm_robust_pv2(lm_pv2_cont, "Linear model: continuous domain scores")
  lm_fit_pv2_cont <- extract_lm_fit_pv2_safe(lm_pv2_cont, Database_pv2_model, "p_v2_prop", "Linear model: continuous domain scores")
}

print(lm_coefficients_pv2_cont, n = Inf)
print(lm_fit_pv2_cont, n = Inf)

# Modelo lineal 2: p_v2 continuo ~ dominios altos
lm_formula_pv2_high <- stats::as.formula(
  paste("p_v2_prop ~", paste(domain_high_vars_pv2, collapse = " + "))
)

lm_pv2_high <- tryCatch(
  stats::lm(formula = lm_formula_pv2_high, data = Database_pv2_model),
  error = function(e) e
)

if (inherits(lm_pv2_high, "error")) {
  lm_coefficients_pv2_high <- make_lm_error_table_pv2("Linear model: high vs low/medium domains", lm_pv2_high$message)
  lm_fit_pv2_high <- make_lm_fit_error_pv2("Linear model: high vs low/medium domains", lm_pv2_high$message)
} else {
  lm_coefficients_pv2_high <- tidy_lm_robust_pv2(lm_pv2_high, "Linear model: high vs low/medium domains")
  lm_fit_pv2_high <- extract_lm_fit_pv2_safe(lm_pv2_high, Database_pv2_model, "p_v2_prop", "Linear model: high vs low/medium domains")
}

print(lm_coefficients_pv2_high, n = Inf)
print(lm_fit_pv2_high, n = Inf)

# Modelo lineal 3: sensibilidad con transformación log1p(100*p_v2)
lm_formula_pv2_log <- p_v2_log1p ~ z_score_ORG + z_score_PROC + z_score_DIG + z_score_HR + z_score_REG

lm_pv2_log <- tryCatch(
  stats::lm(formula = lm_formula_pv2_log, data = Database_pv2_model),
  error = function(e) e
)

if (inherits(lm_pv2_log, "error")) {
  lm_coefficients_pv2_log <- make_lm_error_table_pv2("Linear sensitivity model: log1p(100*p_v2)", lm_pv2_log$message)
  lm_fit_pv2_log <- make_lm_fit_error_pv2("Linear sensitivity model: log1p(100*p_v2)", lm_pv2_log$message)
} else {
  lm_coefficients_pv2_log <- tidy_lm_robust_pv2(lm_pv2_log, "Linear sensitivity model: log1p(100*p_v2)")
  lm_fit_pv2_log <- extract_lm_fit_pv2_safe(lm_pv2_log, Database_pv2_model, "p_v2_log1p", "Linear sensitivity model: log1p(100*p_v2)")
}

print(lm_coefficients_pv2_log, n = Inf)
print(lm_fit_pv2_log, n = Inf)

# ==========================================================
# 9.10. Modelos logístico y Poisson robusto
# ==========================================================

glm_formula_pv2 <- stats::as.formula(
  paste("p_v2_high ~", paste(domain_high_vars_pv2, collapse = " + "))
)

# Logístico: OR
glm_logistic_pv2 <- tryCatch(
  stats::glm(
    formula = glm_formula_pv2,
    data = Database_pv2_model,
    family = stats::binomial(),
    control = stats::glm.control(maxit = 100)
  ),
  error = function(e) e
)

if (inherits(glm_logistic_pv2, "error")) {
  logistic_or_pv2 <- make_glm_error_table_pv2("Logistic model: p_v2 high predicted by high domains", "OR", glm_logistic_pv2$message)
  logistic_fit_pv2 <- make_glm_fit_error_pv2("Logistic model: p_v2 high predicted by high domains", glm_logistic_pv2$message)
} else {
  logistic_or_pv2 <- tidy_glm_exp_robust_pv2(glm_logistic_pv2, "Logistic model: p_v2 high predicted by high domains", "OR")
  logistic_fit_pv2 <- extract_glm_fit_pv2_safe(glm_logistic_pv2, Database_pv2_model, "p_v2_high", "Logistic model: p_v2 high predicted by high domains")
}

print(logistic_or_pv2, n = Inf)
print(logistic_fit_pv2, n = Inf)

# Poisson robusto: RP/PR
glm_poisson_pv2 <- tryCatch(
  stats::glm(
    formula = glm_formula_pv2,
    data = Database_pv2_model,
    family = stats::poisson(link = "log"),
    control = stats::glm.control(maxit = 100)
  ),
  error = function(e) e
)

if (inherits(glm_poisson_pv2, "error")) {
  poisson_pr_pv2 <- make_glm_error_table_pv2("Robust Poisson model: p_v2 high predicted by high domains", "PR", glm_poisson_pv2$message)
  poisson_fit_pv2 <- make_poisson_fit_error_pv2("Robust Poisson model: p_v2 high predicted by high domains", glm_poisson_pv2$message)
} else {
  poisson_pr_pv2 <- tidy_glm_exp_robust_pv2(glm_poisson_pv2, "Robust Poisson model: p_v2 high predicted by high domains", "PR")
  poisson_fit_pv2 <- extract_poisson_fit_pv2_safe(glm_poisson_pv2, "Robust Poisson model: p_v2 high predicted by high domains")
}

print(poisson_pr_pv2, n = Inf)
print(poisson_fit_pv2, n = Inf)

# Modelos univariados OR y PR
fit_univariable_models_pv2 <- function(var) {
  f <- stats::as.formula(paste("p_v2_high ~", var))
  
  logit_fit <- tryCatch(
    stats::glm(formula = f, data = Database_pv2_model, family = stats::binomial(), control = stats::glm.control(maxit = 100)),
    error = function(e) e
  )
  
  poisson_fit <- tryCatch(
    stats::glm(formula = f, data = Database_pv2_model, family = stats::poisson(link = "log"), control = stats::glm.control(maxit = 100)),
    error = function(e) e
  )
  
  out_logit <- if (inherits(logit_fit, "error")) {
    make_glm_error_table_pv2(paste0("Univariable logistic: ", var), "OR", logit_fit$message)
  } else {
    tidy_glm_exp_robust_pv2(logit_fit, paste0("Univariable logistic: ", var), "OR") %>%
      filter(Term != "(Intercept)")
  }
  
  out_poisson <- if (inherits(poisson_fit, "error")) {
    make_glm_error_table_pv2(paste0("Univariable robust Poisson: ", var), "PR", poisson_fit$message)
  } else {
    tidy_glm_exp_robust_pv2(poisson_fit, paste0("Univariable robust Poisson: ", var), "PR") %>%
      filter(Term != "(Intercept)")
  }
  
  bind_rows(out_logit, out_poisson)
}

univariable_or_pr_pv2 <- purrr::map_dfr(domain_high_vars_pv2, fit_univariable_models_pv2)

print(univariable_or_pr_pv2, n = Inf)

# ==========================================================
# 9.11. Comparación de ajuste
# ==========================================================

linear_fit_comparison_pv2 <- bind_rows(
  lm_fit_pv2_cont,
  lm_fit_pv2_high,
  lm_fit_pv2_log
)

binary_fit_comparison_pv2 <- bind_rows(
  logistic_fit_pv2,
  poisson_fit_pv2 %>%
    mutate(
      McFadden_R2 = NA_real_,
      Brier = NA_real_,
      AUC_ROC = NA_real_
    )
)

print(linear_fit_comparison_pv2, n = Inf)
print(binary_fit_comparison_pv2, n = Inf)

# ==========================================================
# 9.12. Plots de correlación
# ==========================================================

plot_data_pv2 <- Database_pv2_model %>%
  select(p_v2_prop, all_of(score_vars_pv2)) %>%
  pivot_longer(
    cols = all_of(score_vars_pv2),
    names_to = "Score_variable",
    values_to = "Domain_score"
  ) %>%
  left_join(domain_lookup_pv2, by = "Score_variable")

plot_cor_labels_pv2 <- correlations_pv2 %>%
  select(Domain, Method, Correlation, p_value_formatted) %>%
  mutate(
    label_part = paste0(
      ifelse(Method == "Pearson", "Pearson r", "Spearman rho"),
      " = ",
      ifelse(is.na(Correlation), "NA", sprintf("%.2f", Correlation)),
      ", p ",
      ifelse(
        is.na(p_value_formatted),
        "NA",
        ifelse(p_value_formatted == "<0.001", "<0.001", paste0("= ", p_value_formatted))
      )
    )
  ) %>%
  group_by(Domain) %>%
  summarise(label = paste(label_part, collapse = "\n"), .groups = "drop")

p_corr_pv2 <- ggplot(plot_data_pv2, aes(x = Domain_score, y = p_v2_prop)) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text(
    data = plot_cor_labels_pv2,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.15,
    size = 3.1
  ) +
  facet_wrap(~ Domain, scales = "free_x") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 1), "%")) +
  labs(
    title = "Association between domain scores and telemedicine use proportion",
    x = "Domain score",
    y = "p_v2: proportion of telemedicine use"
  ) +
  theme_minimal(base_size = 12)

print(p_corr_pv2)

ggplot2::ggsave(
  filename = "PV2_domain_correlation_scatter.png",
  plot = p_corr_pv2,
  width = 12,
  height = 7,
  dpi = 300
)

p_corr_log_pv2 <- ggplot(plot_data_pv2, aes(x = Domain_score, y = log1p(100 * p_v2_prop))) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text(
    data = plot_cor_labels_pv2,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.15,
    size = 3.1
  ) +
  facet_wrap(~ Domain, scales = "free_x") +
  labs(
    title = "Association between domain scores and transformed telemedicine use",
    x = "Domain score",
    y = "log1p(100*p_v2)"
  ) +
  theme_minimal(base_size = 12)

print(p_corr_log_pv2)

ggplot2::ggsave(
  filename = "PV2_domain_correlation_scatter_log1p.png",
  plot = p_corr_log_pv2,
  width = 12,
  height = 7,
  dpi = 300
)

# ==========================================================
# 9.13. Exportar resultados a Excel
# ==========================================================

wb_pv2 <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb_pv2, "PV2_percentiles")
openxlsx::writeData(wb_pv2, "PV2_percentiles", tabla_percentiles_pv2)

openxlsx::addWorksheet(wb_pv2, "Domain_descriptives")
openxlsx::writeData(wb_pv2, "Domain_descriptives", domain_descriptives_pv2)

openxlsx::addWorksheet(wb_pv2, "Correlations_long")
openxlsx::writeData(wb_pv2, "Correlations_long", correlations_pv2)

openxlsx::addWorksheet(wb_pv2, "Correlations_wide")
openxlsx::writeData(wb_pv2, "Correlations_wide", correlations_pv2_wide)

openxlsx::addWorksheet(wb_pv2, "Domain_high_cutoffs")
openxlsx::writeData(wb_pv2, "Domain_high_cutoffs", domain_high_cutoffs_pv2)

openxlsx::addWorksheet(wb_pv2, "PV2_binary_definition")
openxlsx::writeData(wb_pv2, "PV2_binary_definition", p_v2_binary_distribution)

openxlsx::addWorksheet(wb_pv2, "LM_continuous_coef")
openxlsx::writeData(wb_pv2, "LM_continuous_coef", lm_coefficients_pv2_cont)

openxlsx::addWorksheet(wb_pv2, "LM_high_coef")
openxlsx::writeData(wb_pv2, "LM_high_coef", lm_coefficients_pv2_high)

openxlsx::addWorksheet(wb_pv2, "LM_log1p_coef")
openxlsx::writeData(wb_pv2, "LM_log1p_coef", lm_coefficients_pv2_log)

openxlsx::addWorksheet(wb_pv2, "Linear_fit_comparison")
openxlsx::writeData(wb_pv2, "Linear_fit_comparison", linear_fit_comparison_pv2)

openxlsx::addWorksheet(wb_pv2, "Logistic_OR_multivariable")
openxlsx::writeData(wb_pv2, "Logistic_OR_multivariable", logistic_or_pv2)

openxlsx::addWorksheet(wb_pv2, "Poisson_PR_multivariable")
openxlsx::writeData(wb_pv2, "Poisson_PR_multivariable", poisson_pr_pv2)

openxlsx::addWorksheet(wb_pv2, "Univariable_OR_PR")
openxlsx::writeData(wb_pv2, "Univariable_OR_PR", univariable_or_pr_pv2)

openxlsx::addWorksheet(wb_pv2, "Binary_fit_comparison")
openxlsx::writeData(wb_pv2, "Binary_fit_comparison", binary_fit_comparison_pv2)

openxlsx::addWorksheet(wb_pv2, "Analysis_dataset")
openxlsx::writeData(
  wb_pv2,
  "Analysis_dataset",
  Database_pv2_model %>%
    select(
      any_of(c("codigo")),
      p_v2,
      p_v2_raw,
      p_v2_prop,
      p_v2_high,
      p_v2_high_label,
      all_of(score_vars_pv2),
      all_of(domain_high_vars_pv2),
      starts_with("z_score_"),
      p_v2_log1p
    )
)

openxlsx::addWorksheet(wb_pv2, "Domain_items")
openxlsx::writeData(wb_pv2, "Domain_items", domain_lookup_pv2)

openxlsx::addWorksheet(wb_pv2, "Plots")
openxlsx::writeData(
  wb_pv2,
  "Plots",
  tibble(
    Plot = c(
      "PV2_domain_correlation_scatter.png",
      "PV2_domain_correlation_scatter_log1p.png"
    ),
    Description = c(
      "Scatterplots of p_v2_prop against each domain score with linear trend.",
      "Scatterplots of log1p(100*p_v2_prop) against each domain score with linear trend."
    )
  )
)

if (file.exists("PV2_domain_correlation_scatter.png")) {
  openxlsx::insertImage(
    wb_pv2,
    sheet = "Plots",
    file = "PV2_domain_correlation_scatter.png",
    startRow = 5,
    startCol = 1,
    width = 12,
    height = 7
  )
}

if (file.exists("PV2_domain_correlation_scatter_log1p.png")) {
  openxlsx::insertImage(
    wb_pv2,
    sheet = "Plots",
    file = "PV2_domain_correlation_scatter_log1p.png",
    startRow = 40,
    startCol = 1,
    width = 12,
    height = 7
  )
}

wrap_style_pv2 <- openxlsx::createStyle(wrapText = TRUE, valign = "top")

for (sh in names(wb_pv2)) {
  openxlsx::freezePane(wb_pv2, sh, firstRow = TRUE)
  openxlsx::setColWidths(wb_pv2, sh, cols = 1:120, widths = "auto")
  openxlsx::addStyle(
    wb_pv2,
    sh,
    style = wrap_style_pv2,
    rows = 1:10000,
    cols = 1:120,
    gridExpand = TRUE,
    stack = TRUE
  )
}

openxlsx::saveWorkbook(
  wb_pv2,
  output_excel_pv2,
  overwrite = TRUE
)

cat("\n====================================================\n")
cat("ANÁLISIS p_v2 + DOMINIOS COMPLETADO\n")
cat("====================================================\n")
cat("Archivo Excel exportado:", output_excel_pv2, "\n")
cat("Figuras exportadas:\n")
cat("- PV2_domain_correlation_scatter.png\n")
cat("- PV2_domain_correlation_scatter_log1p.png\n\n")

cat("Objetos principales creados:\n")
cat("- tabla_percentiles_pv2\n")
cat("- domain_descriptives_pv2\n")
cat("- correlations_pv2\n")
cat("- correlations_pv2_wide\n")
cat("- lm_coefficients_pv2_cont\n")
cat("- lm_coefficients_pv2_high\n")
cat("- lm_coefficients_pv2_log\n")
cat("- logistic_or_pv2\n")
cat("- poisson_pr_pv2\n")
cat("- univariable_or_pr_pv2\n")
cat("- linear_fit_comparison_pv2\n")
cat("- binary_fit_comparison_pv2\n")
cat("- p_corr_pv2\n")
cat("- p_corr_log_pv2\n")



names(Database_salud_completo)

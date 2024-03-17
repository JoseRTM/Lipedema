############
# LIPEDEMA #
############

pkg_names <- c("skimr","patchwork", "gt","readxl", "dplyr", "ggplot2", "epiDisplay", "tidyverse", "httr", "nortest")

# Packages
for (package in pkg_names) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# LOADING THE DATA
# Set the URL of the Excel file in the GitHub repository
file_url <- "https://github.com/JoseRTM/Lipedema/raw/master/lipedema%20excell%20feb%202024.xlsx"

# Download the file
temp_file <- tempfile(fileext = ".xlsx")
GET(file_url, write_disk(temp_file, overwrite = TRUE))

# Load the data
lipedema <- read_excel(temp_file)

# QUICK CHECK OF DATA
skim(lipedema) # some variables should not be character
table(lipedema$edad) # there are strings inside the column

# DATA CLEANING
data <- lipedema %>% 
  mutate(imc = as.numeric(imc),
         altura = as.numeric(altura),
         peso_pre = as.numeric(peso_pre),
        imc = na_if(imc, 0),
         altura = na_if(altura,0),
         peso_pre = na_if(peso_pre,0),
        volumen = na_if(volumen, 0),
        edad = as.numeric(gsub("\\D", "", edad)),
        peso_post = as.numeric(gsub("\\D", "", peso_post)),
        exp_sintomas_pre = case_when(exp_sintomas_pre == 1 ~ 1,
                                     exp_sintomas_pre == 0 ~ 2,
                                     exp_sintomas_pre == 2 ~ 3),
        exp_sintomas_post = case_when(exp_sintomas_post == 1 ~ 1,
                                      exp_sintomas_post == 0 ~ 2,
                                      exp_sintomas_post == 2 ~ 3),
        exp_estetica_pre = case_when(exp_estetica_pre == 1 ~ 1,
                                     exp_estetica_pre == 0 ~ 2,
                                     exp_estetica_pre == 2 ~ 3),
        exp_estetica_post = case_when(exp_estetica_post == 1 ~ 1,
                                      exp_estetica_post == 0 ~ 2,
                                      exp_estetica_post == 2 ~ 3),
        imc_post = round(peso_post/altura**2,2)) %>%
  rename(imc_pre = imc) %>% 
  rowid_to_column("id") 



# WIDE TO LONG

data_long <- data %>%
  pivot_longer(
    cols = ends_with("_pre") | ends_with("_post"),
    names_to = c(".value", "time"),
    names_pattern = "(.*)_(pre|post)$"
  ) 


# TABLES

format_p_value <- function(p) {
  if (p < 0.001) {
    "<0.001"
  } else if (p < 0.01) {
    "<0.01"
  } else if (p < 0.05) {
    "<0.05"
  } else {
    sprintf("%.3f", p)
  }
}

# BASELINE CHARACTERISTICS
variables_of_interest <- c("edad", "volumen", "imc_pre",  
                           "freq_act_fisica", "reinicia_act_laboral", 
                           "tiempo_comprension")

# Calculate median and IQR
summary_table <- data %>%
  dplyr::select(all_of(variables_of_interest)) %>%
  summarise(across(everything(), 
                   ~ paste0(median(., na.rm = TRUE), " (", IQR(., na.rm = TRUE), ")"),
                   .names = "{.col}"))%>%
  pivot_longer(everything(), names_to = "variable", values_to = "statistics")

p_values <- sapply(data[variables_of_interest], function(x) format_p_value(ad.test(x)$p.value))
p_values_df <- data.frame(variable = names(p_values), p_value = p_values)

# Combine summary statistics and p-values
summary_table <- merge(summary_table, p_values_df, by = "variable")

# Create the table using gt
summary_table %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics and Normality Test for Selected Variables"
  ) %>%
  cols_label(
    variable = "Variable",
    statistics = "Median (IQR)",
    p_value = "Anderson-Darling P-value"
  )

table(data$enf_cronica_tipo)
mean(data$imc_pre, na.rm =T)
sd(data$imc_pre, na.rm = T)
tab1(data$enf_cronica)
tab1(data$anticonceptivo)

# PRE-POST DESCRIPTIVES
variables <- c("eva", "pesadez", "edema", "imc", "mov_limitada", "apariencia", "exp_sintomas", "exp_estetica", "prenda")

# Perform normality test and then t-test or Wilcoxon test based on normality
test_results <- map_df(variables, ~{
  pre_column <- paste(.x, "pre", sep = "_")
  post_column <- paste(.x, "post", sep = "_")
  
  # Calculate differences
  differences <- data[[post_column]] - data[[pre_column]]
  
  # Test normality of differences
  normality_test <- shapiro.test(differences)
  
  # Choose test based on normality
  if (normality_test$p.value >= 0.05) {
    # Use paired t-test if differences are normally distributed
    test_result <- t.test(data[[pre_column]], data[[post_column]], paired = TRUE)
    test_type <- "Paired t-test"
  } else {
    # Use Wilcoxon test if differences are not normally distributed
    test_result <- wilcox.test(data[[pre_column]], data[[post_column]], paired = TRUE, exact = FALSE, correct = FALSE)
    test_type <- "Wilcoxon test"
  }
  
  tibble(
    variable = .x,
    median_pre = round(median(data[[pre_column]], na.rm = TRUE), 2),
    median_post = round(median(data[[post_column]], na.rm = TRUE), 2),
    p_value = format_p_value(test_result$p.value),
    test_type = test_type
  )
})

# Create the summary table
test_results %>%
  gt() %>%
  tab_header(
    title = "Comparison of Pre and Post Intervention Variables"
  ) %>%
  cols_label(
    variable = "Variable",
    median_pre = "median (Pre)",
    median_post = "median (Post)",
    p_value = "P-value",
    test_type = "Test Used"
  )

data <- data %>%
  mutate(exp_sin_pre_dummy = ifelse(exp_sintomas_pre == 1, 0, 1),
         exp_sin_pos_dummy = ifelse(exp_sintomas_post == 1, 0, 1),
         exp_est_pre_dummy = ifelse(exp_estetica_pre == 1, 0, 1),
         exp_est_post_dummy = ifelse(exp_estetica_post == 1, 0, 1))

# Perform McNemar's test
tab1(data$exp_sin_pre_dummy)
tab1(data$exp_sin_pos_dummy)
tab1(data$exp_est_pre_dummy)
tab1(data$exp_est_post_dummy)

mcnemar_symptoms <- mcnemar.test(data$exp_sin_pre_dummy, data$exp_sin_pos_dummy)
print(mcnemar_symptoms)
mcnemar_esthetic <- mcnemar.test(data$exp_est_pre_dummy, data$exp_est_post_dummy) 
print(mcnemar_esthetic)

mean(data$prenda_pre)
tab1(data$prenda_pre)
tab1(data$prenda_post)
mean(data$prenda_post)
mcnemar_clothing <- mcnemar.test(data$prenda_pre, data$prenda_post)
print(mcnemar_clothing)


# EXPLORING RELATIONS BETWEEN BASELINE CHARACTERISTICS AND DIFFERENCE BETWEEN PRE
# AND POST 
data$satisfaccion <- factor(data$satisfaccion, ordered = TRUE)

data$dif_exp <- data$exp_estetica_post-data$exp_estetica_pre

modelo_ordinal <- polr(satisfaccion ~ edad + imc_pre + volumen + enf_cronica + act_fisica + anticonceptivo + data$dif_exp, data = data, Hess = TRUE)
summary(modelo_ordinal)

# Ver los resultados del modelo
summary(modelo_ordinal)


#############
### PLOTS ###
#############
data_long$time <- factor(data_long$time, levels = c("pre", "post"))
pain_plot <- ggplot(data_long, aes(x = time, y = eva, group = time)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5, color = "black") +
  labs(x = "Treatment time", y = "Pain VAS Score") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 10)

pesadez_plot <- ggplot(data_long, aes(x = time, y = pesadez, group = time)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5, color = "black") +
  labs(x = "Treatment time", y = "Feeling of tension") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 10)

edema_plot <- ggplot(data_long, aes(x = time, y = edema, group = time)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5, color = "black") +
  labs(x = "Treatment time", y = "Edema") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 10)

movility_plot <- ggplot(data_long, aes(x = time, y = mov_limitada, group = time)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5, color = "black") +
  labs(x = "Treatment time", y = "Limited Mobility") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 10)

combined_plot <- (pain_plot | pesadez_plot) / (edema_plot | movility_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# Add a rectangular box around the combined plot
combined_plot + plot_layout(guides = "collect") + 
  theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5),
        plot.background = element_rect(color = "black", size = 1.5))

# Print the combined plot
print(combined_plot)

# Esthetic evaluation plot
ggplot(data_long, aes(x = time, y = apariencia, group = time)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA,fill = "white", color = "black") +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("white", "gray"))  +
  labs(
    x = "Treatment time", 
    y = "Esthetic evaluation") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  ylim(1, 5)  



# Contar ocurrencias y calcular proporciones
data_agrupada <- data_long_2 %>%
  mutate(prenda = ifelse(tiempo == "post", 1 - prenda, prenda)) %>% 
  group_by(tiempo, prenda) %>%
  count() %>%
  group_by(tiempo) %>% # Aquí agrupamos solo por tiempo para calcular la proporción dentro de cada grupo de tiempo
  mutate(prop = n / sum(n)) %>%
  ungroup() 

# Gráfico de barras agrupadas
ggplot(data_agrupada, aes(x = tiempo, y = prop, fill = as.factor(prenda))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label=sprintf("%.1f%%", prop*100), y=prop + 0.02), position=position_dodge(width=0.6), size=3.5) +
  scale_fill_brewer(palette = "Pastel1", 
                    name = "Dificultad",
                    labels = c("No", "Sí")) +
  labs(title = "Dificultad para usar una Prenda Antes y Después del Tratamiento",
       x = "Tiempo de tratamiento", 
       y = "Proporción",
       fill = "Uso de Prenda") +
  theme_minimal() +
  theme(legend.position = "top", 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  ylim(0, 1.0)

# GRAFICO SATISFACCION GENERAL
data <- data %>% 
  mutate(satisfaccion = satisfaccion+1,
         satisfaccion = factor(satisfaccion, levels = c(1:5), labels = c("Muy Bajo", "Bajo", "Intermedio", "Alto", "Muy alto")))

data_percent <- data %>%
  count(satisfaccion) %>%
  mutate(percent = n / sum(n) * 100)

ggplot(data_percent, aes(x = satisfaccion, y = percent)) +
  geom_bar(stat = "identity", fill = "#AEDFF7") +  # Color azul apagado y neutro
  labs(
    title = "Satisfacción general con la cirugía",
    x = "Nivel de Satisfacción",
    y = "Porcentaje (%)"
  ) +
  theme_light() +  # Tema limpio y claro
  theme(
    plot.title = element_text(hjust = 0.5, face="bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# GRAFICO EXPECTATIVA ESTETICA
# Crear una nueva columna para las etiquetas correctas
plot_data_sym <- data %>%
  summarise(
    exp_sin_pre = sum(exp_sin_pre_dummy),
    exp_sin_post = sum(exp_sin_pos_dummy),
    
  ) %>%
  pivot_longer(everything(), names_to = "group", values_to = "count") %>% 
  mutate(group = factor(group, levels = c("exp_sin_pre", "exp_sin_post"), 
                        labels = c("Symptoms Pre", "Symptoms Post")))

plot_data_est <- data %>%
  summarise(
    exp_est_pre = sum(exp_est_pre_dummy),
    exp_est_post = sum(exp_est_post_dummy),
    
  ) %>%
  pivot_longer(everything(), names_to = "group", values_to = "count") %>% 
  mutate(group = factor(group, levels = c("exp_est_pre", "exp_est_post"), 
                        labels = c("Esthetic Pre", "Esthetic Post")))

symptoms_plot <- ggplot(plot_data_sym, aes(x = group, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Group", y = "Count of Patients") +
  scale_fill_manual(values = c("gray30", "gray70")) + # Specifying shades of grey
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0,120)

esthetic_plot <- ggplot(plot_data_est, aes(x = group, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Group", y = "Count of Patients") +
  scale_fill_manual(values = c("gray30", "gray70")) + # Specifying shades of grey
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0,120)

combined_bar <- symptoms_plot | esthetic_plot
# Print the combined plot
print(combined_bar)


data_long <- data_long %>%
  mutate(estetico_label = case_when(
    tiempo == "pre" & estetico == 1 ~ "Exp. Baja",
    tiempo == "pre" & estetico == 2 ~ "Exp. Moderada",
    tiempo == "pre" & estetico == 3 ~ "Exp. Alta",
    tiempo == "post" & estetico == 1 ~ "No se cumplió",
    tiempo == "post" & estetico == 2 ~ "Se cumplió",
    tiempo == "post" & estetico == 3 ~ "Superó expectativa"
  ))
total <- nrow(data_long_2)
data_long_2 <- data_long_2 %>%
  group_by(tiempo, estetico_label) %>%
  mutate(percentage = n() / total * 100)

# Paso 1: Calculamos el número total para 'pre' y 'post'.
total_pre <- nrow(data_long_2[data_long_2$tiempo == "pre",])
total_post <- nrow(data_long_2[data_long_2$tiempo == "post",])

# Paso 2: Calculamos el porcentaje.
data_summary <- data_long_2 %>%
  group_by(tiempo, estetico_label) %>%
  tally() %>%
  mutate(percentage = ifelse(tiempo == "pre", n/total_pre*100, n/total_post*100))

# Paso 3: Graficar.
max_y_limit <- 0.35 * nrow(data_long_2)

ggplot(data = data_long_2, aes(x=tiempo, fill=estetico_label)) +
  geom_bar(position=position_dodge()) +
  geom_text(aes(label=sprintf("%.1f%%", ..count../nrow(data_long_2)*100), y=..count..), 
            stat="count", vjust=-0.5, size=3.5, position=position_dodge(0.9)) +
  scale_y_continuous(
    labels = function(x) { paste0(round(x/nrow(data_long_2)*100), "%") },
    limits = c(0, max_y_limit)
  ) +
  labs(x="Tiempo", y="Porcentaje", fill="Estetico", title = "Expectativas estéticas de la cirugía") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    text = element_text(size=14),
    axis.title.y = element_blank()
  ) +
  scale_fill_brewer(palette="Pastel1")

                    

# GRAFICO EXPECTATIVA DE SINTOMAS
data_long_2 <- data_long_2 %>%
  mutate(sintoma_label = case_when(
    tiempo == "pre" & sintoma == 1 ~ "Exp. Baja",
    tiempo == "pre" & sintoma == 2 ~ "Exp. Moderada",
    tiempo == "pre" & sintoma == 3 ~ "Exp. Alta",
    tiempo == "post" & sintoma == 1 ~ "No se cumplió",
    tiempo == "post" & sintoma == 2 ~ "Se cumplió",
    tiempo == "post" & sintoma == 3 ~ "Superó expectativa"
  ))

max_y_limit <- 0.4 * nrow(data_long_2)
ggplot(data = data_long_2, aes(x=tiempo, fill=sintoma_label)) +
  geom_bar(position=position_dodge()) +
  geom_text(aes(label=sprintf("%.1f%%", ..count../nrow(data_long_2)*100), y=..count..), 
            stat="count", vjust=-0.5, size=3.5, position=position_dodge(0.9)) +
  scale_y_continuous(
    labels = function(x) { paste0(round(x/nrow(data_long_2)*100), "%") },
    limits = c(0, max_y_limit)
  ) +
  labs(x="Tiempo", y="Porcentaje", fill="Estetico", title = "Expectativas del manejo de síntomas") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    text = element_text(size=14),
    axis.title.y = element_blank()
  ) +
  scale_fill_brewer(palette="Pastel1")








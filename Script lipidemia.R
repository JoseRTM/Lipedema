############
# LIPEDEMA #
############

# librerias
library(readxl)
library(dplyr)
library(ggplot2)
library(epiDisplay)
library(tidyverse)
setwd("C:/Users/Jose/Desktop/Cirugía plástica")

# CARGAR DATOS
lipedema <- read_excel("C:/Users/Jose/Desktop/Cirugía plástica/lipedema.xls")

# LIMPIEZA Y MANIPULACION
data <- lipedema %>% 
  mutate(imc = as.numeric(imc),
         altura = as.numeric(altura),
         peso_pre = as.numeric(peso_pre),
        imc = na_if(imc, 0),
         altura = na_if(altura,0),
         peso_pre = na_if(peso_pre,0),
        volumen = na_if(volumen, 0),
        sintoma_pre = case_when(sintoma_pre == 1 ~ 1,
                                sintoma_pre == 0 ~ 2,
                                sintoma_pre == 2 ~ 3),
        sintoma_post = case_when(sintoma_post == 1 ~ 1,
                                 sintoma_post == 0 ~ 2,
                                 sintoma_post == 2 ~ 3),
        estetico_pre = case_when(estetico_pre == 1 ~ 1,
                                 estetico_pre == 0 ~ 2,
                                 estetico_pre == 2 ~ 3),
        estetico_post = case_when(estetico_post == 1 ~ 1,
                                  estetico_post == 0 ~ 2,
                                  estetico_post == 2 ~ 3)) %>% 
  rename(movilidad_pre = mov_limitada_pre, movilidad_post = mov_limitada_post) %>% 
  rowid_to_column("id") 


# WIDE TO LONG

data_long <- data %>%
  pivot_longer(cols = ends_with("_pre") | ends_with("_post"), 
               names_to = c("variable", "tiempo"), 
               names_sep = "_", 
               values_to = "valor")

data_long_2 <- data_long %>%
  pivot_wider(names_from = variable, values_from = valor)

# GRAFICO EVA
data_long_2$tiempo <- factor(data_long_2$tiempo, levels = c("pre", "post"))
ggplot(data_long_2, aes(x = tiempo, y = eva, fill = tiempo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Comparación de la Escala Visual Analógica de Dolor Antes y Después del Tratamiento",,
       x = "Tiempo de tratamiento", 
       y = "Escala Visual Analógica de Dolor") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  ylim(0, 10)  

# GRAFICO PESADEZ
ggplot(data_long_2, aes(x = tiempo, y = pesadez, fill = tiempo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Comparación de la pesadez de extremidad Antes y Después del Tratamiento",,
       x = "Tiempo de tratamiento", 
       y = "Escala Visual Analógica de Dolor") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  ylim(0, 10)  

# GRAFICO HINCHAZON
ggplot(data_long_2, aes(x = tiempo, y = hinchazon, fill = tiempo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Comparación de la hinchazón de extremidad Antes y Después del Tratamiento",,
       x = "Tiempo de tratamiento", 
       y = "Escala Visual Analógica de Dolor") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  ylim(0, 10)  

# GRAFICO MOVILIDAD
ggplot(data_long_2, aes(x = tiempo, y = movilidad, fill = tiempo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Comparación de la Movilidad Antes y Después del Tratamiento",,
       x = "Tiempo de tratamiento", 
       y = "Escala Visual Analógica de Dolor") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  ylim(0, 10)  

# GRÁFICO APARIENCIA
ggplot(data_long_2, aes(x = tiempo, y = apariencia, fill = tiempo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Comparación de la Apariencia Antes y Después del Tratamiento",,
       x = "Tiempo de tratamiento", 
       y = "Escala Visual Analógica de Dolor") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  ylim(0, 10)  



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

data_long_2 <- data_long_2 %>%
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








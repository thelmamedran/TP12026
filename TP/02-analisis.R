library(tidyverse)
library(ggplot2)

attach(datos)

# descripción gráfica de una variable categórica medida en escala nominal

ggplot(datos_torta, aes(x = "", y = n, fill = Dimensión_mejor_puntuada)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white"
  ) +
  labs(
    title = "Proporción de países según dimensión con mayor desarrollo\nFuente: GCG, 2023-2024",
    fill = "Dimensión"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )




# descripción gráfica de una variable categórica medida en escala ordinal

datos %>%
  filter(!is.na(MNG_Fuentes_Sec)) %>%
  mutate(
    sec_mng_ordenado = factor(
      sec_mng, 
      levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto") # Del peor al mejor
    )
  ) %>%
  ggplot() + 
  aes(x = sec_mng_ordenado) + 
  geom_bar(fill = "#d35400", width = 0.6) + 
  
  labs(
  title = "Distribución de países según nivel de desarrollo en marcos normativos gubernamentales\nSegún fuentes secundarias, 2023-2024",
  x = "Nivel de desarrollo",
  y = "Cantidad de países"
) +
  
  theme_minimal()





# descripción gráfica de una variable categórica de respuesta múltiple 

tabla_multiple_girai <- datos %>%
  summarize(
    Sesgo = sum(P70_Sesgo, na.rm = TRUE),
    Infancia = sum(P70_Infancia, na.rm = TRUE),
    Diversidad = sum(P70_Diversidad, na.rm = TRUE),
    Datos_Personales = sum(P70_Protección, na.rm = TRUE),
    Genero = sum(P70_Género, na.rm = TRUE),
    Supervision_Humana = sum(P70_Supervisión, na.rm = TRUE),
    Laboral = sum(P70_Laboral, na.rm = TRUE),
    Seguridad = sum(P70_Seguridad, na.rm = TRUE),
    Transparencia = sum(P70_Transparencia, na.rm = TRUE)
  ) %>%
  
  pivot_longer(
    cols = everything(), 
    names_to = "Principio", 
    values_to = "Cantidad"
  )

ggplot(tabla_multiple_girai) +
  
  aes(x = reorder(Principio, Cantidad), y = Cantidad) +
  
  geom_col(fill = "#ff0080", width = 0.7) +
  
  coord_flip() + 
  
  labs(
    title = "Frecuencia de puntaje mayor a 70 en áreas temáticas destacadas\nFuente: GCG, 2023-2024",
    x = "Áreas temáticas",
    y = "Cantidad de países que tienen puntaje mayor a 70"
  ) +
  
  theme_minimal()


# descripción gráfica de una variable cuantitativa discreta

# Africa
datos %>%
  filter(NU_region == "África") %>%
  
  ggplot(aes(x = Areas_AG)) +
  geom_bar(
    fill = "#8A2BE2",
    width = 0.08
  ) +
  
  labs(
    title = "Distribución de áreas con Acciones Gubernamentales en países de África\nFuente: GCG, 2023-2024",
    x = "Cantidad de áreas cubiertas",
    y = "Frecuencia de países"
  ) +
  
  scale_x_continuous(
    breaks = seq(
      min(datos$Areas_AG, na.rm = TRUE),
      max(datos$Areas_AG, na.rm = TRUE),
      by = 1
    )
  ) +
  
  theme_minimal()

# Europa
datos %>%
  filter(NU_region == "Europa") %>%
  
  ggplot(aes(x = Areas_AG)) +
  geom_bar(
    fill = "#2980b9",
    width = 0.08
  ) +
  
  labs(
    title = "Distribución de áreas con Acciones Gubernamentales en países de Europa\nFuente: GCG, 2023-2024",
    x = "Cantidad de áreas cubiertas",
    y = "Frecuencia de países"
  ) +
  
  scale_x_continuous(
    breaks = seq(
      min(datos$Areas_AG, na.rm = TRUE),
      max(datos$Areas_AG, na.rm = TRUE),
      by = 1
    )
  ) +
  
  theme_minimal()

# descripción gráfica de una variable cuantitativa continua 

# Histograma para Marcos Normativos Gubernamentales
ggplot(datos, aes(x = Marcos_nor_gub)) +
  geom_histogram(
    binwidth = int_mng,
    boundary = min(datos$Marcos_nor_gub),
    fill = "#2980b9",
    color = "white"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(datos$Marcos_nor_gub),
      max(datos$Marcos_nor_gub) + int_mng,
      by = int_mng
    )
  ) +
  labs(
    title = "Distribución de puntaje de Marcos Normativos Gubernamentales\nFuente: GCG, 2023-2024",
    x = "Intervalos de puntaje",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Histograma para Acciones Gubernamentales
ggplot(datos, aes(x = Acciones_gub)) +
  geom_histogram(
    binwidth = int_ag,
    boundary = 0,
    fill = "#27ae60",
    color = "white"
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      ceiling(max(datos$Acciones_gub) + int_ag),
      by = int_ag
    )
  ) +
  labs(
    title = "Distribución de puntaje de Acciones Gubernamentales\nFuente: GCG, 2023-2024",
    x = "Intervalos de puntaje",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Histograma para Actores No Estatales
ggplot(datos, aes(x = Actores_NE)) +
  geom_histogram(
    binwidth = int_ane,
    boundary = min(datos$Actores_NE),
    fill = "#e67e22",
    color = "white"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(datos$Actores_NE),
      max(datos$Actores_NE) + int_ane,
      by = int_ane
    )
  ) +
  labs(
    title = "Distribución de puntaje de Actores No Estatales\nFuente: GCG, 2023-2024",
    x = "Intervalos de puntaje",
    y = "Frecuencia"
  ) +
  theme_minimal()

# descripción gráfica de la relación entre dos variables categóricas
# privado
ggplot(datos) +
  aes(x = NU_region, fill = privado) + 
  
  geom_bar(position = "fill") + 
  
  labs(
    title = "Proporción de países con iniciativas del sector privado por Continente\nFuente: GCG, 2023-2024",
    x = "Continente",
    y = "Proporción",
    fill = "¿Hay privado?"
  ) +
  theme_minimal()

# academia
ggplot(datos) +
  aes(x = NU_region, fill = academia) + 
  
  geom_bar(position = "fill") + 
  
  labs(
    title = "Proporción de países con iniciativas académicas por Continente\nFuente: GCG, 2023-2024",
    x = "Continente",
    y = "Proporción",
    fill = "¿Hay academia?"
  ) +
  theme_minimal()

# descripción gráfica de la relación entre una variable categórica y una variable cuantitativa
datos %>%
  filter(!is.na(NU_region), !is.na(GIRAI)) %>%
  
  ggplot(aes(x = NU_region, y = GIRAI, fill = NU_region)) +
  
  geom_boxplot(alpha = 0.7, width = 0.6) +
  
  labs(
    title = "Distribución del índice GIRAI según continente\nFuente: GCG, 2023-2024",
    x = "Continente",
    y = "Puntaje GIRAI"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#  descripción gráfica de la relación entre dos variables cuantitativas
datos_bivariado <- datos %>%
  mutate(
    suma_principios = rowSums(across(starts_with("p70_")), na.rm = TRUE)
  )

ggplot(datos_bivariado) + 
  
  aes(x = suma_principios, y = GIRAI) + 
  
  geom_jitter(width = 0.2, height = 0, color = "#008B8B", alpha = 0.6, size = 2) + 
  
  geom_smooth(method = "lm", color = "#FF69B4", se = FALSE) +
  
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  
  labs(
    title = "Relación entre el GIRAI y la cantidad de dimensiones con puntaje mayor a 70\nFuente: GCG, 2023-2024",
    x = "Cantidad de dimensiones con puntaje mayor a 70",
    y = "Puntaje GIRAI"
  ) +
  
  theme_minimal()

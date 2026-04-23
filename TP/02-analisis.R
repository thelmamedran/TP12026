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

# Moda
datos %>%
  filter(!is.na(Dimensión_mejor_puntuada)) %>%
  count(Dimensión_mejor_puntuada, sort = TRUE) %>%
  slice(1)


# descripción gráfica de una variable categórica medida en escala ordinal

datos %>%
  filter(!is.na(MNG_Fuentes_Sec)) %>%
  mutate(
    sec_mng_ordenado = factor(
      MNG_Fuentes_Sec, 
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

# Moda 

datos %>%
  filter(!is.na(MNG_Fuentes_Sec)) %>%
  count(MNG_Fuentes_Sec, sort = TRUE) %>%
  slice(1)

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
  filter(Continente == "África") %>%
  
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

# Medidas resumen
datos %>%
  filter(Continente == "África", !is.na(Areas_AG)) %>%
  summarise(
    promedio = mean(Areas_AG),
    mediana = median(Areas_AG),
    desvio_estandar = sd(Areas_AG),
    varianza = var(Areas_AG)
  )

# Europa
datos %>%
  filter(Continente == "Europa") %>%
  
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

# Medidas resumen
datos %>%
  filter(Continente == "Europa", !is.na(Areas_AG)) %>%
  summarise(
    promedio = mean(Areas_AG),
    mediana = median(Areas_AG),
    desvio_estandar = sd(Areas_AG),
    variancia = var(Areas_AG)
  )

# descripción gráfica de una variable cuantitativa continua 

# Histograma para Marcos Normativos Gubernamentales
ggplot(datos, aes(x = Marcos_nor_gub)) +
  geom_histogram(
    binwidth = int_mng,
    boundary = 0,
    fill = "#2980b9",
    color = "white"
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
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
    boundary = 0,
    fill = "#e67e22",
    color = "white"
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
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

# Histograma para GIRAI

ggplot(datos, aes(x = GIRAI)) +
  geom_histogram(
    binwidth = int_girai,
    boundary = 0,
    fill = "#FF69B4",
    color = "white"
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      max(datos$GIRAI) + int_girai,
      by = int_girai
    )
  ) +
  labs(
    title = "Distribución de puntaje GIRAI\nFuente: GCG, 2023-2024",
    x = "Intervalos de puntaje",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Medidas resumen de GIRAI

datos %>%
  filter(!is.na(GIRAI)) %>%
  summarise(
    minimo = min(GIRAI),
    q1 = quantile(GIRAI, 0.25),
    mediana = median(GIRAI),
    promedio = mean(GIRAI),
    q3 = quantile(GIRAI, 0.75),
    maximo = max(GIRAI),
    rango = max(GIRAI) - min(GIRAI),
    desvio_estandar = sd(GIRAI),
    variancia = var(GIRAI),
  )

# Tabla comparativa del GIRAI por continente

tabla_continentes <- datos %>%
  filter(!is.na(Continente), !is.na(GIRAI)) %>%
  group_by(Continente) %>%
  summarise(
    Cantidad_paises = n(),
    Promedio = round(mean(GIRAI), 2),
    Mediana = round(median(GIRAI), 2),
    Q1 = round(quantile(GIRAI, 0.25), 2),
    Q3 = round(quantile(GIRAI, 0.75), 2),
    Minimo = round(min(GIRAI), 2),
    Maximo = round(max(GIRAI), 2),
    Desvio_estandar = round(sd(GIRAI), 2),
    Rango = round(max(GIRAI) - min(GIRAI), 2)
  ) %>%
  arrange(desc(Promedio))

tabla_continentes



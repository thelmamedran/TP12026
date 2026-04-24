library(tidyverse)
library(ggplot2)

paleta_colores <- c("#0F2A44", "#2E5E8A", "#5B8DB8", "#A7C0D9", "#DCE3EA")

# -----------------------------------------------------------------------
# descripción gráfica de una variable categórica medida en escala nominal
# -----------------------------------------------------------------------

grafico_torta <- datos_torta %>%
  mutate(Dimensión_mejor_puntuada = recode(Dimensión_mejor_puntuada,
                                           "cap" = "Capacidades en IA",
                                           "ddhh" = "Derechos Humanos",
                                           "gob" = "Gobernanza")) %>%
  ggplot(aes(x = "", y = cantidad, fill = Dimensión_mejor_puntuada)) +
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
  scale_fill_manual(values = paleta_colores) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
print(grafico_torta)

# moda

print(datos %>%
        filter(!is.na(Dimensión_mejor_puntuada)) %>%
        count(Dimensión_mejor_puntuada, sort = TRUE) %>%
        slice(1))

# -----------------------------------------------------------------------
# descripción gráfica de una variable categórica medida en escala ordinal
# -----------------------------------------------------------------------

grafico_ordinal <- datos %>%
  filter(!is.na(MNG_Fuentes_Sec)) %>%
  mutate(
    sec_mng_ordenado = factor(
      MNG_Fuentes_Sec, 
      levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")
    )
  ) %>%
  ggplot() + 
  aes(x = sec_mng_ordenado, fill = sec_mng_ordenado) + 
  geom_bar(width = 0.6) + 
  scale_fill_manual(values = paleta_colores) +
  labs(
    title = "Distribución de países según nivel de desarrollo en marcos normativos gubernamentales\nSegún fuentes secundarias, 2023-2024",
    x = "Nivel de desarrollo",
    y = "Cantidad de países",
    fill = "Nivel de desarrollo normativo"
  ) +
  theme_minimal()
print(grafico_ordinal)

# moda

print(datos %>%
        filter(!is.na(MNG_Fuentes_Sec)) %>%
        count(MNG_Fuentes_Sec, sort = TRUE) %>%
        slice(1))

# -----------------------------------------------------------------------
# descripción gráfica de una variable categórica de respuesta múltiple 
# -----------------------------------------------------------------------

tabla_multiple_girai <- datos %>%
  summarize(
    Sesgo = sum(P70_Sesgo, na.rm = TRUE),
    Infancia = sum(P70_Infancia, na.rm = TRUE),
    Diversidad = sum(P70_Diversidad, na.rm = TRUE),
    `Datos Personales` = sum(P70_Protección, na.rm = TRUE),
    Genero = sum(P70_Género, na.rm = TRUE),
    `Supervisión Humana` = sum(P70_Supervisión, na.rm = TRUE),
    Laboral = sum(P70_Laboral, na.rm = TRUE),
    Seguridad = sum(P70_Seguridad, na.rm = TRUE),
    Transparencia = sum(P70_Transparencia, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "Principio", 
    values_to = "Cantidad"
  )

grafico_multiple <- ggplot(tabla_multiple_girai) +
  aes(x = reorder(Principio, Cantidad), y = Cantidad) +
  geom_col(fill = "#0F2A44", width = 0.7) +
  coord_flip() + 
  labs(
    title = "Frecuencia de puntaje mayor a 70 en áreas temáticas destacadas\nFuente: GCG, 2023-2024",
    x = "Áreas temáticas",
    y = "Cantidad de países que tienen puntaje mayor a 70"
  ) +
  theme_minimal()
print(grafico_multiple)

# -----------------------------------------------------------------------
# descripción gráfica de una variable cuantitativa discreta
# -----------------------------------------------------------------------

# Todos los países (Gráfico de bastones)

grafico_bastones_global <- datos %>%
  filter(!is.na(Areas_AG)) %>%
  ggplot(aes(x = Areas_AG)) +
  geom_bar(
    fill = "#0F2A44",
    width = 0.1 
  ) +
  labs(
    title = "Distribución de áreas con Acciones Gubernamentales a nivel global\nFuente: GCG, 2023-2024",
    x = "Cantidad de áreas cubiertas",
    y = "Frecuencia de países"
  ) +
  scale_x_continuous(
    breaks = seq(0, 19, by = 1),
    limits = c(-0.5, 19.5) # asi se ve en el eje x de 0 a 19
  ) +
  theme_minimal()
print(grafico_bastones_global)

# medidas resumen globales

print(datos %>%
        filter(!is.na(Areas_AG)) %>%
        summarise(
          promedio = mean(Areas_AG),
          mediana = median(Areas_AG),
          desvio_estandar = sd(Areas_AG),
          varianza = var(Areas_AG)
        ))

# -----------------------------------------------------------------------
# descripción gráfica de una variable cuantitativa continua
# -----------------------------------------------------------------------

# histograma para Marcos Normativos Gubernamentales

grafico_hist_mng <- ggplot(datos, aes(x = Marcos_nor_gub)) +
  geom_histogram(
    binwidth = int_mng,
    boundary = 0,
    fill = "#2E5E8A",
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
    title = "Distribución de países según puntaje de Marcos Normativos Gubernamentales\nFuente: GCG, 2023-2024",
    x = "Intervalos de puntaje",
    y = "Frecuencia"
  ) +
  theme_minimal()
print(grafico_hist_mng)

# histograma para Acciones Gubernamentales

grafico_hist_ag <- ggplot(datos, aes(x = Acciones_gub)) +
  geom_histogram(
    binwidth = int_ag,
    boundary = 0,
    fill = "#2E5E8A",
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
    title = "Distribución de países según puntaje de Acciones Gubernamentales\nFuente: GCG, 2023-2024",
    x = "Intervalos de puntaje",
    y = "Frecuencia"
  ) +
  theme_minimal()
print(grafico_hist_ag)

# histograma para Actores no Estatales

grafico_hist_ane <- ggplot(datos, aes(x = Actores_NE)) +
  geom_histogram(
    binwidth = int_ane,
    boundary = 0,
    fill = "#5B8DB8",
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
    title = "Distribución de países según puntaje de Actores No Estatales\nFuente: GCG, 2023-2024",
    x = "Intervalos de puntaje",
    y = "Frecuencia"
  ) +
  theme_minimal()
print(grafico_hist_ane)

# histograma para GIRAI

grafico_hist_girai <- ggplot(datos, aes(x = GIRAI)) +
  geom_histogram(
    binwidth = int_girai,
    boundary = 0,
    fill = "#A7C0D9",
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
    title = "Distribución de países según puntaje de GIRAI\nFuente: GCG, 2023-2024",
    x = "Intervalos de puntaje",
    y = "Frecuencia"
  ) +
  theme_minimal()
print(grafico_hist_girai)

# medidas resumen GIRAI

print(datos %>%
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
        ))

# tabla comparativa del GIRAI por continente

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

print(tabla_continentes)

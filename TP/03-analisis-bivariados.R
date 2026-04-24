library(tidyverse)
library(ggplot2)

paleta_colores <- c("#0F2A44", "#2E5E8A", "#5B8DB8", "#A7C0D9", "#DCE3EA")

# -----------------------------------------------------------------------
# descripción gráfica de la relación entre dos variables categóricas
# -----------------------------------------------------------------------

# privado
grafico_privado <- ggplot(datos) +
  aes(x = Continente, fill = Privado) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = paleta_colores) +
  labs(
    title = "Proporción de países con iniciativas del sector privado por Continente\nFuente: GCG, 2023-2024",
    x = "Continente",
    y = "Proporción",
    fill = "¿Hay privado?"
  ) +
  theme_minimal()
print(grafico_privado)

# academia
grafico_academia <- ggplot(datos) +
  aes(x = Continente, fill = Academia) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = paleta_colores) +
  labs(
    title = "Proporción de países con iniciativas académicas por Continente\nFuente: GCG, 2023-2024",
    x = "Continente",
    y = "Proporción",
    fill = "¿Hay academia?"
  ) +
  theme_minimal()
print(grafico_academia)

# -----------------------------------------------------------------------
# descripción gráfica de la relación entre una variable categórica y una variable cuantitativa
# -----------------------------------------------------------------------

grafico_boxplot <- datos %>%
  filter(!is.na(Continente), !is.na(GIRAI)) %>%
  ggplot(aes(x = Continente, y = GIRAI, fill = Continente)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  scale_fill_manual(values = paleta_colores) +
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
print(grafico_boxplot)

# Análisis Númerico
dispersion_continentes <- datos %>%
  group_by(Continente) %>%
  summarize(
    Desvio_Estandar = sd(GIRAI, na.rm = TRUE),
    Rango_Intercuartilico = IQR(GIRAI, na.rm = TRUE),
    Rango_Total = max(GIRAI, na.rm = TRUE) - min(GIRAI, na.rm = TRUE)
  )
print(dispersion_continentes)

# -----------------------------------------------------------------------
#  descripción gráfica de la relación entre dos variables cuantitativas
# -----------------------------------------------------------------------

datos_bivariado <- datos %>%
  mutate(
    suma_principios = rowSums(across(starts_with("P70_")), na.rm = TRUE)
  )

grafico_dispersion <- ggplot(datos_bivariado) + 
  aes(x = suma_principios, y = GIRAI) + 
  geom_jitter(width = 0.2, height = 0, color = "#2E5E8A", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#0F2A44", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  labs(
    title = "Relación entre el GIRAI y la cantidad de áreas con puntaje mayor a 70\nFuente: GCG, 2023-2024",
    x = "Cantidad de áreas con puntaje mayor a 70",
    y = "Puntaje GIRAI"
  ) +
  theme_minimal()
print(grafico_dispersion)

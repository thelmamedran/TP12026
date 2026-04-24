library(tidyverse)
library(ggplot2)

paleta_colores <- c("#0F2A44", "#2E5E8A", "#5B8DB8", "#A7C0D9", "#DCE3EA")

# -----------------------------------------------------------------------
# descripción gráfica de la relación entre dos variables categóricas
# -----------------------------------------------------------------------

datos_agrupados <- datos %>%
  filter(!is.na(Continente)) %>%
  select(Continente, Academia, Privado) %>%
  pivot_longer(cols = c(Academia, Privado), names_to = "Sector", values_to = "Estado") %>%
  group_by(Continente, Sector) %>%
  summarise(
    # solo casos afirmativos
    Porcentaje = sum(Estado %in% c("Sí", "Si", "1", 1, "Yes", "Tiene"), na.rm = TRUE) / n() * 100,
    .groups = "drop"
  )

grafico_sectores <- ggplot(datos_agrupados, aes(x = Continente, y = Porcentaje, fill = Sector)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  scale_fill_manual(values = c(paleta_colores[1], paleta_colores[3])) +
  labs(
    title = "Porcentaje de países con iniciativas del sector privado y académico\nSegún continente, Fuente: GCG, 2023-2024",
    x = "Continente",
    y = "Porcentaje (%)",
    fill = "Sector"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(grafico_sectores)

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
    title = "Relación entre el GIRAI y la cantidad de dimensiones con puntaje mayor a 70\nFuente: GCG, 2023-2024",
    x = "Cantidad de dimensiones con puntaje mayor a 70",
    y = "Puntaje GIRAI"
  ) +
  theme_minimal()
print(grafico_dispersion)

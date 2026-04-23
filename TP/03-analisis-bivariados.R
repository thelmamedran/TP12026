library(tidyverse)
library(ggplot2)

attach(datos)

# descripción gráfica de la relación entre dos variables categóricas
# privado
ggplot(datos) +
  aes(x = Continente, fill = Privado) + 
  
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
  aes(x = Continente, fill = Academia) + 
  
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
  filter(!is.na(Continente), !is.na(GIRAI)) %>%
  
  ggplot(aes(x = Continente, y = GIRAI, fill = Continente)) +
  
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

# Análisis Númerico
dispersion_continentes <- datos %>%
  group_by(Continente) %>%
  
  summarize(
    Desvio_Estandar = sd(GIRAI, na.rm = TRUE),
    
    Rango_Intercuartilico = IQR(GIRAI, na.rm = TRUE),
    
    Rango_Total = max(GIRAI, na.rm = TRUE) - min(GIRAI, na.rm = TRUE)
  )
print(dispersion_continentes)


#  descripción gráfica de la relación entre dos variables cuantitativas
datos_bivariado <- datos %>%
  mutate(
    suma_principios = rowSums(across(starts_with("P70_")), na.rm = TRUE)
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
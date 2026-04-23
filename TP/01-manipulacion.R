library(tidyverse)

# Borramos las columnas que no necesitamos
datos <- datos %>% 
  select(-Ranking, -ISO3, -Country, -GIRAI_region, -UN_subregion, -tipo_academia_en, -tipo_privado_en)


# Renombramos columnas
colnames(datos) <- c("Pais","Continente","GIRAI","Marcos_nor_gub","Acciones_gub","Actores_NE",
                     "Dim_DDHH","Dim_Gobobernanza","DIM_Capacidades","MNG_Fuentes_Sec","AG_Fuentes_Sec",
                     "ANE_Fuentes_Sec","Dimensión_mejor_puntuada","P70_Sesgo","P70_Infancia",
                     "P70_Diversidad","P70_Protección","P70_Género","P70_Supervisión",
                     "P70_Laboral","P70_Seguridad","P70_Transparencia","Areas_MNG","Areas_AG","Areas_Parlamentarias",
                     "Areas_Concienticacion","Areas_ANE","Academia","Tipo_Academia","Privado","Tipo_Privado")

str(datos)

int_mng <- floor((max(datos$Marcos_nor_gub) - min(datos$Marcos_nor_gub)) / sqrt(nrow(datos)))
int_ag <- floor((max(datos$Acciones_gub) - min(datos$Acciones_gub)) / sqrt(nrow(datos)))
int_ane <- floor((max(datos$Actores_NE) - min(datos$Actores_NE)) / sqrt(nrow(datos)))
int_girai <- floor((max(datos$GIRAI) - min(datos$GIRAI)) / sqrt(nrow(datos)))

datos <- datos %>%
  
  mutate(
    # Creamos variables nuevas para los intervalos de mng, ag y ane.
    Marcos_nor_gub_int = cut(Marcos_nor_gub, breaks = seq( from = min(datos$Marcos_nor_gub), to = max(datos$Marcos_nor_gub), by = int_mng),
                             right = FALSE
    ),
    
    Acciones_gub_int = cut( Acciones_gub, breaks = seq( from = min(datos$Acciones_gub), to = max(datos$Acciones_gub), by = int_ag),
                            right = FALSE
    ),
    
    GIRAI_int = cut( Acciones_gub, breaks = seq( from = min(datos$GIRAI), to = max(datos$GIRAI), by = int_girai),
                            right = FALSE
    ),
    
    Actores_NE_int = cut(Actores_NE, breaks = seq(from = min(datos$Actores_NE), to = max(datos$Actores_NE),by = int_ane),
                         right = FALSE
    ))

str(datos)
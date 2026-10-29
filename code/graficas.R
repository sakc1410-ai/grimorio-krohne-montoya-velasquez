# Proyecto 1 - Grupo 5
# Gráficas de informalidad laboral y derecho a pensión, Colombia y Chile (2019-2023)
# Fuente: LABLAC (CEDLAS y Banco Mundial). Elaboración propia.
#
# INSTRUCCIONES:
# 1. Guarda este archivo graficas.R.
# 2. Ejecuta el script con el botón Source.
# 3. Cuando se abra la ventana, selecciona el archivo Excel de la base unificada.
# 4. Las cuatro gráficas se guardarán en una carpeta llamada "graficas_generadas"
#    junto al archivo Excel que selecciones.

# 1. Instalar paquetes (solo si hacen falta)
# Si R indica que falta un paquete, quita el # de la línea correspondiente y ejecútala:
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyr")

# 2. Cargar paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# 3. Seleccionar manualmente el archivo Excel
# Esto evita errores por diferencias en el nombre del archivo.
archivo_datos <- file.choose()

# 4. Leer la hoja "Base unificada"
# Los encabezados están en la fila 4, por eso se omiten las primeras 3 filas.
datos <- read_excel(
  path = archivo_datos,
  sheet = "Base unificada",
  skip = 3
)

# 5. Comprobar que estén las columnas necesarias
columnas_necesarias <- c(
  "País", "Año", "Trimestre", "Periodo",
  "Informalidad laboral (%)",
  "Asalariados con derecho a pensión (%)"
)

faltantes <- setdiff(columnas_necesarias, names(datos))
if (length(faltantes) > 0) {
  stop(
    paste(
      "Faltan estas columnas en la hoja 'Base unificada':",
      paste(faltantes, collapse = ", ")
    )
  )
}

# 6. Preparar y ordenar los datos
datos <- datos %>%
  filter(País %in% c("Colombia", "Chile")) %>%
  mutate(
    Trimestre_num = match(Trimestre, c("Q1", "Q2", "Q3", "Q4")),
    Periodo = as.character(Periodo)
  ) %>%
  arrange(Año, Trimestre_num)

# Verificar que existan 40 observaciones: 20 por país.
conteo <- datos %>% count(País)
print(conteo)

if (nrow(datos) != 40 || any(conteo$n != 20)) {
  stop("La base no contiene 40 observaciones (20 por país). Revisa el archivo seleccionado.")
}

# Mantener los periodos en orden cronológico en los gráficos
niveles_periodo <- unique(datos$Periodo)
datos$Periodo <- factor(datos$Periodo, levels = niveles_periodo)

# 7. Textos comunes para las gráficas
fuente <- paste(
  "Fuente: LABLAC (CEDLAS y Banco Mundial), hojas",
  "'Informality (LP)' y 'Pensions', consultadas en septiembre de 2026.",
  "Elaboración propia."
)

# 8. Crear carpeta de salida junto al archivo Excel
carpeta_salida <- file.path(dirname(archivo_datos), "graficas_generadas")
if (!dir.exists(carpeta_salida)) {
  dir.create(carpeta_salida)
}

# 9. Gráfica 1: informalidad laboral, Colombia y Chile
g1 <- ggplot(
  datos,
  aes(x = Periodo, y = `Informalidad laboral (%)`, color = País, group = País)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  labs(
    title = "Informalidad laboral en Colombia y Chile (2019-2023)",
    x = "Trimestre",
    y = "Población en empleo informal (%)",
    caption = stringr::str_wrap(fuente, width = 110),
    color = "País"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    plot.caption = element_text(
      hjust = 0,
      size = 7,
      lineheight = 1.15,
      margin = margin(t = 10)
    ),
    legend.position = "bottom",
    plot.margin = margin(t = 10, r = 15, b = 20, l = 15)
  )

ggsave(
  file.path(carpeta_salida, "grafica_1_informalidad_colombia_chile.png"),
  g1, width = 10, height = 6, dpi = 300
)

# 10. Gráfica 2: asalariados con derecho a pensión, Colombia y Chile
g2 <- ggplot(
  datos,
  aes(
    x = Periodo,
    y = `Asalariados con derecho a pensión (%)`,
    color = País,
    group = País
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  labs(
    title = "Asalariados con derecho a pensión en Colombia y Chile (2019-2023)",
    x = "Trimestre",
    y = "Asalariados con derecho a pensión al retirarse (%)",
    caption = stringr::str_wrap(paste(fuente, "Nota: este indicador corresponde a trabajadores asalariados, no a toda la población ocupada."), width = 110),
    color = "País"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    plot.caption = element_text(hjust = 0, size = 7),
    legend.position = "bottom"
  )

ggsave(
  file.path(carpeta_salida, "grafica_2_derecho_pension_colombia_chile.png"),
  g2, width = 10, height = 6, dpi = 300
)

# 11. Preparar los indicadores para las gráficas dentro de cada país
datos_largos <- datos %>%
  select(
    País, Periodo,
    `Informalidad laboral (%)`,
    `Asalariados con derecho a pensión (%)`
  ) %>%
  pivot_longer(
    cols = c(
      `Informalidad laboral (%)`,
      `Asalariados con derecho a pensión (%)`
    ),
    names_to = "Indicador",
    values_to = "Porcentaje"
  ) %>%
  mutate(
    Indicador = recode(
      Indicador,
      `Informalidad laboral (%)` = "Informalidad laboral",
      `Asalariados con derecho a pensión (%)` = "Derecho a pensión entre asalariados"
    )
  )

# 12. Gráfica 3: ambos indicadores en Chile
g3 <- ggplot(
  filter(datos_largos, País == "Chile"),
  aes(x = Periodo, y = Porcentaje, color = Indicador, group = Indicador)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  labs(
    title = "Informalidad y derecho a pensión en Chile (2019-2023)",
    x = "Trimestre",
    y = "Porcentaje (%)",
    caption = stringr::str_wrap(
      paste(
        fuente,
        "Nota: los indicadores tienen universos distintos;",
        "la informalidad se mide entre personas ocupadas y el derecho a pensión entre trabajadores asalariados."
      ),
      width = 110
    ),
    color = "Indicador"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    plot.caption = element_text(hjust = 0, size = 7),
    legend.position = "bottom"
  )

ggsave(
  file.path(carpeta_salida, "grafica_3_indicadores_chile.png"),
  g3, width = 10, height = 6, dpi = 300
)

# 13. Gráfica 4: ambos indicadores en Colombia
g4 <- ggplot(
  filter(datos_largos, País == "Colombia"),
  aes(x = Periodo, y = Porcentaje, color = Indicador, group = Indicador)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  labs(
    title = "Informalidad y derecho a pensión en Colombia (2019-2023)",
    x = "Trimestre",
    y = "Porcentaje (%)",
    caption = stringr::str_wrap(
      paste(
        fuente,
        "Nota: los indicadores tienen universos distintos;",
        "la informalidad se mide entre personas ocupadas y el derecho a pensión entre trabajadores asalariados."
      ),
      width = 110
    ),
    color = "Indicador"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    plot.caption = element_text(hjust = 0, size = 7),
    legend.position = "bottom"
  )

ggsave(
  file.path(carpeta_salida, "grafica_4_indicadores_colombia.png"),
  g4, width = 10, height = 6, dpi = 300
)

# 14. Mostrar las gráficas en RStudio
print(g1)
print(g2)
print(g3)
print(g4)

# 15. Resumen descriptivo: promedio, primer trimestre y último trimestre
resumen <- datos %>%
  group_by(País) %>%
  arrange(Año, Trimestre_num, .by_group = TRUE) %>%
  summarise(
    Informalidad_inicio = first(`Informalidad laboral (%)`),
    Informalidad_final = last(`Informalidad laboral (%)`),
    Informalidad_promedio = mean(`Informalidad laboral (%)`, na.rm = TRUE),
    Pension_inicio = first(`Asalariados con derecho a pensión (%)`),
    Pension_final = last(`Asalariados con derecho a pensión (%)`),
    Pension_promedio = mean(`Asalariados con derecho a pensión (%)`, na.rm = TRUE),
    .groups = "drop"
  )

cat("\\nResumen descriptivo (porcentajes):\\n")
print(resumen)

cat(
  "\\nProceso terminado. Las cuatro gráficas están guardadas en:\\n",
  carpeta_salida, "\\n"
)

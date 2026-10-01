# =====================================================================
#  PRIMER TRABAJO ESTADISTICA II - GEIH 2025 (mayo, junio y julio)
#  EJECUCION COMPLETA
#
#  Corre los siete scripts en orden y reproduce todos los resultados del
#  informe desde los CSV originales del DANE: la base, los descriptivos,
#  los 20 graficos, los estimadores, los intervalos y la simulacion.
#
#  COMO USARLO
#    1. Poner el directorio de trabajo en la carpeta de este archivo,
#       la que contiene "Mayo 2025", "Junio 2025", "Julio 2025" y "salidas".
#       En RStudio: Session > Set Working Directory > To Source File Location.
#       O bien:  setwd("C:/Estadistica 2")
#    2. source("00_ejecutar_todo.R", encoding = "UTF-8")
#
#  Los scripts tambien funcionan por separado, en el mismo orden. Este
#  archivo es solo una comodidad: no contiene ningun calculo propio.
#
#  REQUISITOS
#    - R >= 4.0
#    - Paquetes readr y dplyr, solo para el script 01b. Los demas usan R base.
#      install.packages(c("readr", "dplyr"))
#
#  TIEMPO APROXIMADO: entre 10 y 20 minutos. La mayor parte se la llevan
#  08 (simulacion con B = 10.000) y 09 (graficos de consistencia).
# =====================================================================

SCRIPTS <- c(
  "01b_construccion_estilo_docente.R",   # base, diccionario y validaciones
  "03_descriptivos_fase3.R",             # descriptivos y frecuencias de V4
  "04_graficos_log_arriendo.R",          # graficos 1 a 5   (V3 y su logaritmo)
  "05_graficos_educacion_superior.R",    # graficos 6 a 9   (V1)
  "06_graficos_v2_mujeres.R",            # graficos 10 a 13 (V2)
  "07_graficos_v4_analfabetismo.R",      # graficos 14 a 16 (V4)
  "08_estimadores_simulacion.R",         # estimadores, IC y simulacion
  "09_graficos_consistencia.R"           # graficos 17 a 20 (consistencia)
)

# El directorio de trabajo debe contener los scripts y las carpetas de mes.
faltan <- SCRIPTS[!file.exists(SCRIPTS)]
if (length(faltan))
  stop("No encuentro estos scripts desde el directorio de trabajo actual (",
       getwd(), "):\n  ", paste(faltan, collapse = "\n  "),
       "\nPon el directorio de trabajo en la carpeta del trabajo y vuelve a intentar.")

t0 <- Sys.time()
for (s in SCRIPTS) {
  cat("\n\n", strrep("=", 70), "\n", sep = "")
  cat("  EJECUTANDO: ", s, "\n", sep = "")
  cat(strrep("=", 70), "\n\n", sep = "")
  source(s, encoding = "UTF-8", local = new.env())
  cat(sprintf("\n  -> %s terminado (%.1f min acumulados)\n", s,
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("  TODO LISTO en ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
    " minutos.\n", sep = "")
cat(strrep("=", 70), "\n", sep = "")
cat("\nSalidas en 'salidas/':\n")
print(list.files("salidas", recursive = TRUE))

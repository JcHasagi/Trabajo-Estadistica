# =====================================================================
#  PRIMER TRABAJO ESTADISTICA II - GEIH 2025 (mayo, junio y julio)
#  EJECUCION COMPLETA
#
#  Corre en orden los diez scripts del vector SCRIPTS (de 01b a 10) y
#  genera todos los resultados del informe desde los CSV originales del
#  DANE: la base, los descriptivos, las cifras citadas en los comentarios,
#  los 20 graficos, los estimadores, los intervalos de la media y de
#  sigma^2, la simulacion y la comparacion entre ejecuciones.
#
#  SIN SEMILLA: la simulacion (08 y 10) no fija semilla, asi que cada
#  ejecucion produce replicas nuevas y sus cifras cambian en los ultimos
#  decimales, en una magnitud del orden del error de Monte Carlo que
#  documenta el 10. El 09 no simula: dibuja las replicas que guardo el 08
#  en salidas/fase3_distribuciones.rds. Todo lo demas es determinista y se
#  repite identico en cada corrida.
#  OJO: correr este archivo REESCRIBE los archivos de salidas/. Las cifras de
#  Monte Carlo del informe son las de la ejecucion entregada; si se quiere
#  conservarla, copiar antes la carpeta salidas/ a otro lugar.
#
#  COMO USARLO
#    1. Poner el directorio de trabajo en la carpeta de este archivo,
#       la que contiene "Mayo 2025", "Junio 2025" y "Julio 2025".
#       En RStudio: Session > Set Working Directory > To Source File Location.
#       O bien:  setwd("C:/Estadistica 2")
#    2. Abrir R sin restaurar un espacio de trabajo guardado (.RData), para
#       que el generador aleatorio no arranque siempre en el mismo estado.
#    3. source("00_ejecutar_todo.R", encoding = "UTF-8")
#
#  Los scripts tambien funcionan por separado, en el mismo orden. Este
#  archivo es solo una comodidad: no contiene ningun calculo propio.
#
#  REQUISITOS
#    - R >= 4.0
#    - Paquetes readr y dplyr, solo para el script 01b. Los demas usan R base.
#      install.packages(c("readr", "dplyr"))
#    - En Linux o macOS, una sesion con configuracion regional UTF-8: con la
#      configuracion "C", R no puede leer las tildes de los encabezados del
#      01b. En Windows con R >= 4.2 no hace falta nada.
#
#  TIEMPO: no se escribe a mano. Al terminar cada script este archivo
#  imprime los minutos acumulados, y al final el total. Como referencia
#  aproximada, en la prueba con datos sinteticos del mismo tamano que los
#  reales (72.737 viviendas) la corrida completa tardo unos 12 minutos: unos
#  4 el 08 (simulacion con n = N y B = 100, 1.000 y 10.000) y unos 7,5 el 10
#  (dos ejecuciones mas del mismo ciclo). El resto tarda menos de un minuto.
# =====================================================================

SCRIPTS <- c(
  "01b_construccion_estilo_docente.R",   # base, diccionario y validaciones
  "03_descriptivos_fase3.R",             # descriptivos y frecuencias de V4
  "03b_cifras_del_texto.R",              # cifras citadas en el informe
  "04_graficos_log_arriendo.R",          # graficos 1 a 5   (V3 y su logaritmo)
  "05_graficos_educacion_superior.R",    # graficos 6 a 9   (V1)
  "06_graficos_v2_mujeres.R",            # graficos 10 a 13 (V2)
  "07_graficos_v4_analfabetismo.R",      # graficos 14 a 16 (V4)
  "08_estimadores_simulacion.R",         # estimadores, IC, sigma^2 y simulacion
  "09_graficos_consistencia.R",          # graficos 17 a 20, con las replicas del 08
  "10_variacion_entre_ejecuciones.R"     # tres ejecuciones sin semilla vs. error de MC
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

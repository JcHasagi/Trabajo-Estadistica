# =====================================================================
#  PRIMER TRABAJO ESTADISTICA II - GEIH 2025 (mayo, junio y julio)
#  Construccion de las cuatro variables
#  VERSION ESCRITA CON LA ESTRUCTURA DE LA SINTAXIS DE EJEMPLO DEL DOCENTE
#
#  Unidad de analisis: VIVIENDA = PERIODO + MES + DIRECTORIO
#  Si ya existe una base en salidas/, al final la compara columna por columna
#  con la nueva antes de exportar.
#
#  Cada comprobacion se imprime en consola y, si alguna falla, stopifnot()
#  detiene la ejecucion con un mensaje que dice cual fue.
#
#  RUTAS RELATIVAS: el script asume que el directorio de trabajo contiene las
#  carpetas "Mayo 2025", "Junio 2025" y "Julio 2025". En RStudio se consigue
#  abriendo el proyecto en esa carpeta, o con setwd("C:/Estadistica 2") una vez
#  antes de correr esto. La comprobacion previa lo verifica y se detiene si no
#  es asi.
#
#  Antes de correrlo, una sola vez:
#    install.packages(c("readr", "dplyr"))
#
#  Nota de codificacion: los comentarios van sin tildes a proposito, para no
#  depender de la codificacion con que R abra el archivo en Windows. Los
#  nombres de los CSV del DANE si llevan tildes, por eso se escriben con
#  secuencias \u00xx, que R interpreta igual en cualquier codificacion.
# =====================================================================


#### Comprobación previa: rutas y codificación

options(scipen = 999)   # sin notacion cientifica: 400000, nunca 4e+05.
                        # Se fija aqui para que la salida no dependa del
                        # estado de la sesion ni del orden de ejecucion.

# comprobar(): recibe comprobaciones con nombre (cada una TRUE o FALSE), las
# imprime y, si alguna no da TRUE (un NA tambien cuenta como falla),
# stopifnot() detiene la ejecucion con un mensaje que nombra la primera que
# fallo. Se usa en los saltos de patron, en HOGAR = SECUENCIA_P y en los rangos.
comprobar <- function(ch) {
  for (k in names(ch)) cat(sprintf("  %-52s: %s\n", k, ch[[k]]))
  do.call(stopifnot, setNames(as.list(ch), paste("No se cumple:", names(ch))))
}

RUTA <- "."                                   # ruta relativa al directorio de trabajo
MESES <- c("Mayo 2025", "Junio 2025", "Julio 2025")

ARCH_CG <- "Caracter\u00edsticas generales, seguridad social en salud y educaci\u00f3n.CSV"
ARCH_HV <- "Datos del hogar y la vivienda.CSV"

ruta_de <- function(mes, arch) file.path(RUTA, mes, "CSV", arch)

faltan <- character(0)
for (m in MESES) for (a in c(ARCH_CG, ARCH_HV))
  if (!file.exists(ruta_de(m, a))) faltan <- c(faltan, ruta_de(m, a))
if (length(faltan))
  stop("No encuentro estos archivos desde el directorio de trabajo actual (",
       getwd(), "):\n  ", paste(faltan, collapse = "\n  "))
cat("Directorio de trabajo:", getwd(), "\n")

# --- Un solo formato: solo CSV -----------------------------------------------
# Los microdatos del DANE se publican tambien en .DTA (Stata) y .SAV (SPSS).
# Mezclar formatos duplicaria registros. Se comprueba que en las carpetas de mes
# no exista ninguno de esos archivos, y se listan los que este script lee.
otros_formatos <- list.files(file.path(RUTA, MESES),
                             pattern = "\\.(dta|sav|DTA|SAV)$",
                             recursive = TRUE, full.names = TRUE)
cat("\n=== UN SOLO FORMATO ===\n")
cat("  archivos .DTA o .SAV en las carpetas de mes:", length(otros_formatos), "\n")
cat("  archivos leidos por este script (los 6, todos .CSV):\n")
cat(paste0("    ", c(file.path(RUTA, MESES, "CSV", ARCH_CG),
                     file.path(RUTA, MESES, "CSV", ARCH_HV))), sep = "\n")
cat("\n")
stopifnot(length(otros_formatos) == 0)

# --- Codificacion: se COMPRUEBA archivo por archivo, no se asume -------------
# El enunciado pide que el codigo compruebe la codificacion de cada mes y no
# suponga que todos los archivos se guardaron igual. Esta funcion lee los bytes
# crudos y decide:
#   - si no hay ningun byte por encima de 0x7F, el archivo es ASCII puro y
#     cualquier codificacion de un byte da exactamente el mismo resultado;
#   - si los hay y la secuencia completa es UTF-8 valida, es UTF-8;
#   - si los hay y no es UTF-8 valida, es una codificacion de un byte (latin-1).
codificacion_de <- function(ruta) {
  n   <- file.info(ruta)$size
  con <- file(ruta, "rb"); b <- readBin(con, "raw", n = n); close(con)
  bom   <- n >= 3 && identical(as.integer(b[1:3]), c(239L, 187L, 191L))
  altos <- sum(as.integer(b) > 127L)
  txt <- rawToChar(b); Encoding(txt) <- "UTF-8"
  utf8_ok <- !is.na(iconv(txt, "UTF-8", "UTF-8"))
  enc <- if (altos == 0L) "ASCII" else if (utf8_ok) "UTF-8" else "latin1"
  data.frame(archivo = ruta,
             mes   = basename(dirname(dirname(ruta))),
             tabla = if (grepl("hogar", basename(ruta))) "hogares" else "personas",
             bytes = n, BOM_UTF8 = bom,
             bytes_no_ascii = altos, utf8_valido = utf8_ok,
             codificacion = enc, stringsAsFactors = FALSE)
}

cat("\n=== COMPROBACION DE CODIFICACION (uno por uno) ===\n")
cod <- do.call(rbind, lapply(MESES, function(m)
  rbind(codificacion_de(ruta_de(m, ARCH_CG)),
        codificacion_de(ruta_de(m, ARCH_HV)))))
print(cod[, c("mes", "tabla", "bytes", "BOM_UTF8", "bytes_no_ascii",
              "utf8_valido", "codificacion")], row.names = FALSE)

if (length(unique(cod$codificacion)) > 1)
  cat("\n  AVISO: los meses NO tienen la misma codificacion. Cada archivo se lee\n",
      "        con la suya, que es justamente por lo que no se asume una sola.\n")
if (all(cod$codificacion == "ASCII"))
  cat(sprintf(paste0("\n  Los %d archivos son ASCII puro: no hay un solo byte por encima\n",
                     "  de 0x7F en su contenido, de modo que la codificacion declarada no\n",
                     "  altera ningun valor. Las tildes estan en los NOMBRES de archivo,\n",
                     "  no adentro.\n"), nrow(cod)))

# Para readr, ASCII se lee sin problema declarando latin1 (es un superconjunto).
enc_lectura <- function(e) if (e == "UTF-8") "UTF-8" else "latin1"


#### Importación de Datos

library(readr)

# Todas las columnas se leen como TEXTO.
# Motivo: si readr adivina el tipo, DPTO "05" (Antioquia) se convierte en 5 y
# se pierde el cero a la izquierda; ademas las conversiones automaticas pueden
# volver NA en silencio a los codigos especiales (98, 99). La conversion a
# numero se hace mas adelante, solo donde la decidimos.
TIPOS <- cols(.default = col_character())

leer <- function(mes, arch) {
  e <- cod$codificacion[cod$archivo == ruta_de(mes, arch)]
  read_delim(ruta_de(mes, arch), delim = ";", escape_double = FALSE,
             trim_ws = TRUE, col_types = TIPOS,
             locale = locale(encoding = enc_lectura(e)))
}

# Tabla 10 (una fila por PERSONA)
general_mayo  <- leer("Mayo 2025",  ARCH_CG)
general_junio <- leer("Junio 2025", ARCH_CG)
general_julio <- leer("Julio 2025", ARCH_CG)

# Tabla 01 (una fila por HOGAR)
vivienda_mayo  <- leer("Mayo 2025",  ARCH_HV)
vivienda_junio <- leer("Junio 2025", ARCH_HV)
vivienda_julio <- leer("Julio 2025", ARCH_HV)


# --- Los tres meses asignados, identificados DENTRO de los datos -------------
# No basta con que la carpeta se llame "Mayo 2025": se comprueba que la columna
# MES del propio archivo traiga el codigo correspondiente y solo ese.
CODIGOS <- c("Mayo 2025" = "05", "Junio 2025" = "06", "Julio 2025" = "07")
CG <- list("Mayo 2025" = general_mayo,  "Junio 2025" = general_junio,  "Julio 2025" = general_julio)
HV <- list("Mayo 2025" = vivienda_mayo, "Junio 2025" = vivienda_junio, "Julio 2025" = vivienda_julio)

cat("=== LOS TRES MESES, IDENTIFICADOS DENTRO DE LOS DATOS ===\n")
for (m in MESES) {
  mp <- sort(unique(CG[[m]]$MES)); mh <- sort(unique(HV[[m]]$MES))
  ok <- identical(mp, unname(CODIGOS[m])) && identical(mh, unname(CODIGOS[m]))
  cat(sprintf("  %-11s MES en los datos: personas = %-4s hogares = %-4s (esperado %s)  %s\n",
              m, paste(mp, collapse = ","), paste(mh, collapse = ","),
              CODIGOS[m], if (ok) "OK" else "AVISO"))
  stopifnot(ok)
}
per_todos <- unlist(lapply(CG, function(d) d$PERIODO))
cat(sprintf("  PERIODO (semana de recoleccion) va de %s a %s\n",
            min(per_todos), max(per_todos)))
cat(sprintf("  Viviendas distintas por mes: %s\n",
            paste(sprintf("%s=%d", CODIGOS[MESES],
                          sapply(MESES, function(m) length(unique(CG[[m]]$DIRECTORIO)))),
                  collapse = " | ")))
# Ningun DIRECTORIO se repite entre meses: son tres muestras independientes,
# no un panel de viviendas.
dirs <- lapply(MESES, function(m) unique(CG[[m]]$DIRECTORIO))
cat("  DIRECTORIO en comun entre los tres meses:", length(Reduce(intersect, dirs)), "\n\n")


#### Procesamiento

library(dplyr)

# --- Los encabezados deben ser identicos en los tres meses -------------------
# Si no lo fueran, apilarlos seria invalido. Se comprueba antes de seleccionar.
stopifnot(identical(names(general_mayo),  names(general_junio)))
stopifnot(identical(names(general_mayo),  names(general_julio)))
stopifnot(identical(names(vivienda_mayo), names(vivienda_junio)))
stopifnot(identical(names(vivienda_mayo), names(vivienda_julio)))
cat("\nEncabezados identicos en los 3 meses:", ncol(general_mayo), "columnas en la\n",
    "tabla de personas y", ncol(vivienda_mayo), "en la de hogares.\n")

# --- Se seleccionan UNICAMENTE las columnas necesarias -----------------------
# Se verifica primero que existan; si falta alguna el script se detiene en vez
# de suponerla.
# SECUENCIA_P, HOGAR y ORDEN se conservan para las validaciones intermedias,
# aunque ninguna llegue a la base final: son las llaves que permiten comprobar
# que no hay duplicados a nivel de persona ni de hogar.
CAMPOS_CG <- c("PERIODO", "MES", "DIRECTORIO", "SECUENCIA_P", "HOGAR", "ORDEN",
               "DPTO", "CLASE", "FEX_C18", "P6040", "POB_MAY18",
               "P3042", "P3039", "P6160", "P6050")
CAMPOS_HV <- c("PERIODO", "MES", "DIRECTORIO", "SECUENCIA_P", "HOGAR",
               "DPTO", "P5090", "P5140")
falt <- c(setdiff(CAMPOS_CG, names(general_mayo)),
          setdiff(CAMPOS_HV, names(vivienda_mayo)))
if (length(falt)) stop("Campos ausentes en los archivos: ", paste(falt, collapse = ", "))

general <- bind_rows(general_mayo, general_junio, general_julio) %>%
  select(all_of(CAMPOS_CG))
vivienda <- bind_rows(vivienda_mayo, vivienda_junio, vivienda_julio) %>%
  select(all_of(CAMPOS_HV))

cat("Columnas conservadas:", ncol(general), "de personas y", ncol(vivienda),
    "de hogares.\n")
cat("Filas: personas", nrow(general), "| hogares", nrow(vivienda), "\n")

# --- Dominios: se verifican ANTES de recodificar -----------------------------
# Dominios tal como los documenta el diccionario. Cualquier valor fuera de esta
# lista se reporta: no se recodifica ni se convierte a cero.
DOMINIOS <- list(
  P3042     = as.character(c(1:13, 99)),   # nivel educativo; 99 = No sabe/No informa
  P3039     = as.character(1:4),           # identidad de genero (solo 18+)
  P6160     = as.character(1:2),           # sabe leer y escribir; 1 = Si, 2 = No
  POB_MAY18 = "1"
)
cat("\n=== DOMINIOS OBSERVADOS FRENTE AL DICCIONARIO ===\n")
n_fuera <- 0L
for (v in names(DOMINIOS)) {
  fuera <- setdiff(setdiff(unique(general[[v]]), NA), DOMINIOS[[v]])
  n_fuera <- n_fuera + length(fuera)
  cat(sprintf("  %-10s %s\n", v,
      if (length(fuera)) paste("AVISO, valores fuera del dominio:",
                               paste(fuera, collapse = ",")) else "conforme"))
}
fuera_p5090 <- setdiff(setdiff(unique(vivienda$P5090), NA), as.character(1:7))
cat(sprintf("  %-10s %s\n", "P5090",
    if (length(fuera_p5090)) paste("AVISO:", paste(fuera_p5090, collapse = ",")) else "conforme"))
# Un valor fuera del dominio se corrige en el diccionario (DOMINIOS) si es un
# codigo legitimo, nunca forzando el dato.
stopifnot(
  "Valores fuera del dominio en P3042, P3039, P6160 o POB_MAY18 (ver AVISO arriba)" =
    n_fuera == 0L,
  "Valores de P5090 fuera de 1 a 7 (ver AVISO arriba)" = length(fuera_p5090) == 0L)

# Saltos de patron declarados en el diccionario: se verifican, no se asumen.
# Tambien se imprimen los vacios que cita el informe (menores de 18 sin P3039
# y menores de 3 sin P6160).
cat("\n--- Saltos de patron ---\n")
edad <- as.numeric(general$P6040)
cat("  personas con P3039 vacio (menores de 18):", sum(is.na(general$P3039)), "\n")
cat("  personas con P6160 vacio (menores de 3) :", sum(is.na(general$P6160)), "\n")
saltos <- c(
  "P6040 (edad) sin vacios y numerica"         = !anyNA(edad),
  "P3039 vacio exactamente en menores de 18"   = all(is.na(general$P3039) == (edad < 18)),
  "P6160 vacio exactamente en menores de 3"    = all(is.na(general$P6160) == (edad < 3)),
  "POB_MAY18 = 1 equivale a edad >= 18"        =
    all((!is.na(general$POB_MAY18) & general$POB_MAY18 == "1") == (edad >= 18)))
comprobar(saltos)

# --- Llaves intermedias y atributos constantes -------------------------------
# El diccionario documenta SECUENCIA_P como "Identificador del hogar" y HOGAR
# como "Numero del hogar en la vivienda"; ambas estan marcadas como PK. Se
# conservan las dos y se comprueba su coherencia.
cat("\n=== LLAVES INTERMEDIAS Y ATRIBUTOS CONSTANTES ===\n")
llaves_hogar <- c(
  "HOGAR coincide con SECUENCIA_P (personas)" = all(general$HOGAR  == general$SECUENCIA_P),
  "HOGAR coincide con SECUENCIA_P (hogares)"  = all(vivienda$HOGAR == vivienda$SECUENCIA_P))
comprobar(llaves_hogar)

# DPTO debe tener UN SOLO valor dentro de cada PERIODO + MES + DIRECTORIO;
# de lo contrario no podria arrastrarse a la base final con first().
const_viv <- general %>%
  group_by(PERIODO, MES, DIRECTORIO) %>%
  summarise(n_dpto = n_distinct(DPTO), n_clase = n_distinct(CLASE), .groups = 'drop')
cat("  DPTO  con un solo valor por PERIODO+MES+DIRECTORIO:",
    all(const_viv$n_dpto == 1),
    sprintf("(maximo observado: %d)\n", max(const_viv$n_dpto)))
cat("  CLASE con un solo valor por PERIODO+MES+DIRECTORIO:",
    all(const_viv$n_clase == 1),
    sprintf("(maximo observado: %d)\n", max(const_viv$n_clase)))
stopifnot(all(const_viv$n_dpto == 1), all(const_viv$n_clase == 1))

# --- Unicidad de las llaves ANTES de unir ------------------------------------
k_per <- paste(general$PERIODO, general$MES, general$DIRECTORIO,
               general$SECUENCIA_P, general$ORDEN, sep = "|")
k_hog <- paste(vivienda$PERIODO, vivienda$MES, vivienda$DIRECTORIO,
               vivienda$SECUENCIA_P, sep = "|")
cat("\n=== UNICIDAD DE LLAVES ANTES DE UNIR ===\n")
cat("  llave de persona unica :", !any(duplicated(k_per)),
    sprintf("(%d duplicados en %d filas)\n", sum(duplicated(k_per)), length(k_per)))
cat("  llave de hogar unica   :", !any(duplicated(k_hog)),
    sprintf("(%d duplicados en %d filas)\n", sum(duplicated(k_hog)), length(k_hog)))
stopifnot(!any(duplicated(k_per)), !any(duplicated(k_hog)))

# --- Marcas a nivel persona, antes de resumir --------------------------------
# adulto: 18 anios o mas.
# den_educ excluye el codigo 99 ("no sabe / no informa") del numerador Y del
#   denominador: es un faltante, no un cero.
# num_educ cuenta los niveles 8 a 13 (tecnica profesional en adelante).
#   El 7 (normalista) NO cuenta.
# num_muj cuenta solo el codigo 2 de P3039 ("se reconoce como mujer").
# P6160 solo se pregunta a personas de 3 anios o mas: ese es el denominador
#   de V4, no toda la poblacion de la vivienda.
general <- general %>%
  mutate(
    adulto       = !is.na(POB_MAY18) & POB_MAY18 == "1",
    es_den_educ  = adulto & !is.na(P3042) & P3042 != "99",
    es_num_educ  = adulto & !is.na(P3042) & P3042 %in% as.character(8:13),
    es_den_gen   = adulto & !is.na(P3039) & P3039 %in% as.character(1:4),
    es_num_muj   = adulto & !is.na(P3039) & P3039 == "2",
    es_den_alfab = !is.na(P6160) & P6160 %in% c("1", "2"),
    es_no_alfab  = !is.na(P6160) & P6160 == "2"
  )

# Los codigos 3 y 4 de P3039 entran en el denominador de V2 pero no en el
# numerador; el informe cita cuantas personas son.
cat("\n  adultos con P3039 = 3 o 4 (en den_gen, no en num_muj):",
    sum(general$adulto & general$P3039 %in% c("3", "4")), "\n")

# --- Marcas a nivel hogar ----------------------------------------------------
# Descripciones literales del diccionario:
#   P5090 = "La vivienda ocupada por este hogar es:"; codigo 3 = "En arriendo o
#           subarriendo", que define el universo.
#   P5140 = "Cuanto pagan mensualmente por arriendo?"; regla documentada, literal:
#           "Rango > 1000 o 98 o 99" — estrictamente MAYOR que 1.000.
# Verificado sobre los 29.816 hogares: ningun valor es exactamente 1.000, asi que
# la regla estricta y la no estricta dan el mismo resultado; se usa la estricta
# por fidelidad al diccionario. Los codigos 98 ("No sabe") y 99 ("No informa")
# van a NA y nunca se tratan como pesos. Los decimales del DANE usan coma, por
# eso el gsub.
vivienda <- vivienda %>%
  mutate(
    arr   = as.numeric(gsub(",", ".", P5140, fixed = TRUE)),
    monto = if_else(!is.na(arr) & arr > 1000 & !(arr %in% c(98, 99)), arr, NA_real_)
  )

# El universo de P5140 son EXACTAMENTE los hogares en arriendo. Se verifica.
stopifnot(all(!is.na(vivienda$arr) == (vivienda$P5090 == "3")))

# Cifras que cita el informe sobre ese universo: hogares en arriendo y cuantos
# responden 98 o 99. Tambien cuantos reportan exactamente 1.000 u otro monto de
# 1.000 o menos, que irian a NA (el informe dice que ninguno).
cat("\n=== HOGARES EN ARRIENDO (P5090 = 3) ===\n")
cat("  hogares en arriendo                         :", sum(vivienda$P5090 == "3"), "\n")
cat("  con P5140 = 98 o 99 (van a NA)              :", sum(vivienda$arr %in% c(98, 99)), "\n")
cat("  con P5140 exactamente igual a 1.000         :", sum(vivienda$arr %in% 1000), "\n")
cat("  con P5140 <= 1.000 distinto de 98 y 99      :",
    sum(!is.na(vivienda$arr) & vivienda$arr <= 1000 & !(vivienda$arr %in% c(98, 99))), "\n")

# --- Resumen de cada modulo POR SEPARADO, al nivel de VIVIENDA ---------------
# Se resume ANTES de unir: nunca se unen personas con hogares fila a fila.
# La llave es PERIODO + MES + DIRECTORIO, no DIRECTORIO solo: son tres meses
# apilados y agrupar unicamente por DIRECTORIO fundiria viviendas distintas.

res_personas <- general %>%
  group_by(PERIODO, MES, DIRECTORIO) %>%
  summarise(
    DPTO       = first(DPTO),
    CLASE      = first(CLASE),
    FEX_C18    = first(FEX_C18),
    n_hogares  = n_distinct(SECUENCIA_P),
    n_personas = n(),
    n_adultos  = sum(adulto),
    den_educ   = sum(es_den_educ),
    num_educ   = sum(es_num_educ),
    den_gen    = sum(es_den_gen),
    num_muj    = sum(es_num_muj),
    den_alfab  = sum(es_den_alfab),
    n_no_alfab = sum(es_no_alfab),
    .groups = 'drop'
  )

res_hogares <- vivienda %>%
  group_by(PERIODO, MES, DIRECTORIO) %>%
  summarise(
    n_hog_arriendo     = sum(!is.na(monto)),
    n_hog_arr_sin_dato = sum(P5090 == "3" & is.na(monto)),
    # OJO: na.rm = TRUE aqui NO convierte faltantes en cero. Es una suma parcial
    # intermedia; el mutate de mas abajo pone la variable en NA si algun hogar
    # arrendatario quedo sin monto, de modo que ese cero nunca sobrevive.
    suma_arriendo      = sum(monto, na.rm = TRUE),
    .groups = 'drop'
  )

# Cada resumen debe tener una sola fila por vivienda ANTES de unir
kp <- paste(res_personas$PERIODO, res_personas$MES, res_personas$DIRECTORIO, sep = "|")
kh <- paste(res_hogares$PERIODO,  res_hogares$MES,  res_hogares$DIRECTORIO,  sep = "|")
cat("\n=== UNICIDAD DE LOS RESUMENES ANTES DE UNIR ===\n")
cat("  res_personas: una fila por vivienda :", !any(duplicated(kp)),
    sprintf("(%d viviendas)\n", nrow(res_personas)))
cat("  res_hogares : una fila por vivienda :", !any(duplicated(kh)),
    sprintf("(%d viviendas)\n", nrow(res_hogares)))
stopifnot(!any(duplicated(kp)), !any(duplicated(kh)))

# --- Union CON VALIDACION DE CARDINALIDAD ------------------------------------
# La sintaxis de ejemplo del docente usa plyr::join_all, que no valida nada.
# Otra de sus reglas pide uniones con validacion de cardinalidad cuando la
# version de dplyr lo permita, y desde dplyr 1.1.0 existe el argumento
# 'relationship'. Se usa cuando esta disponible, y si no, se valida a mano.
n_antes <- nrow(res_personas)
if (utils::packageVersion("dplyr") >= "1.1.0") {
  base <- left_join(res_personas, res_hogares,
                    by = c("PERIODO", "MES", "DIRECTORIO"),
                    relationship = "one-to-one")
  cat("\nUnion validada por dplyr con relationship = \"one-to-one\".\n")
} else {
  base <- left_join(res_personas, res_hogares, by = c("PERIODO", "MES", "DIRECTORIO"))
  cat("\ndplyr anterior a 1.1.0: la cardinalidad se valida a mano.\n")
}

# --- Unicidad de la llave DESPUES de unir ------------------------------------
kb <- paste(base$PERIODO, base$MES, base$DIRECTORIO, sep = "|")
cat("=== UNICIDAD DE LA LLAVE DESPUES DE UNIR ===\n")
cat("  llave unica            :", !any(duplicated(kb)),
    sprintf("(%d duplicados)\n", sum(duplicated(kb))))
cat("  la union no multiplico filas:", nrow(base) == n_antes,
    sprintf("(%d -> %d)\n", n_antes, nrow(base)))
stopifnot(!any(duplicated(kb)), nrow(base) == n_antes)

# --- Construccion de las cuatro variables ------------------------------------
# OJO con la diferencia frente a la sintaxis de ejemplo: alli los faltantes se
# convierten en cero con if_else(is.na(x), 0, x). Aqui NO se puede hacer eso en
# V1, V2 ni V3. Una vivienda sin adultos no tiene proporcion 0 de adultos con
# educacion superior: no tiene proporcion, y debe quedar en NA. El cero solo se
# usa en los CONTADORES de hogares, donde si es el valor correcto.
base <- base %>%
  mutate(
    n_hog_arriendo     = if_else(is.na(n_hog_arriendo),     0L, as.integer(n_hog_arriendo)),
    n_hog_arr_sin_dato = if_else(is.na(n_hog_arr_sin_dato), 0L, as.integer(n_hog_arr_sin_dato)),
    suma_arriendo      = if_else(is.na(suma_arriendo),       0,  suma_arriendo),

    # V1 - proporcion de adultos con educacion tecnica o superior
    prop_educ_superior_18mas = if_else(den_educ > 0, num_educ / den_educ, NA_real_),

    # V2 - proporcion de adultos que se reconocen como mujeres
    prop_mujeres_adultas = if_else(den_gen > 0, num_muj / den_gen, NA_real_),

    # V3 - arriendo mensual de la vivienda. Se SUMA entre los hogares
    # arrendatarios (175 viviendas tienen dos o mas). NA si ningun hogar
    # arrienda, o si a alguno que si arrienda le falta el monto: una suma
    # parcial seria un dato falso, no un faltante.
    arriendo_mensual_vivienda = if_else(n_hog_arriendo > 0 & n_hog_arr_sin_dato == 0,
                                        suma_arriendo, NA_real_),

    # Columna auxiliar en logaritmo (sugerencia del docente): comprime la cola
    # derecha y deja ver la forma del grueso de los datos. NO reemplaza a V3.
    # Es segura: el minimo observado es $10.000, no hay ceros ni negativos.
    log_arriendo_mensual = if_else(is.na(arriendo_mensual_vivienda), NA_real_,
                                   log(arriendo_mensual_vivienda)),

    # V4 - presencia de analfabetismo en la vivienda (regla de PRESENCIA, no de
    # mayoria: la moda dejaria la variable en 97,6% / 2,4% y exigiria una regla
    # de desempate arbitraria).
    presencia_analfabetismo = factor(
      if_else(den_alfab == 0, NA_character_,
              if_else(n_no_alfab > 0, "Con al menos una persona analfabeta",
                                      "Sin personas analfabetas")),
      levels = c("Sin personas analfabetas", "Con al menos una persona analfabeta")),

    MES_NOMBRE = factor(MES, levels = c("05", "06", "07"),
                        labels = c("Mayo", "Junio", "Julio"))
  )

base_directorio_mes <- base %>%
  arrange(MES, DPTO, DIRECTORIO) %>%
  select(PERIODO, MES, MES_NOMBRE, DIRECTORIO, DPTO, CLASE,
         n_hogares, n_personas, n_adultos,
         den_educ, num_educ, prop_educ_superior_18mas,
         den_gen, num_muj, prop_mujeres_adultas,
         n_hog_arriendo, n_hog_arr_sin_dato, arriendo_mensual_vivienda,
         log_arriendo_mensual,
         den_alfab, n_no_alfab, presencia_analfabetismo,
         FEX_C18) %>%
  as.data.frame()


#### Verificación

base_directorio_mes %>% tibble() %>% summary()

# Viviendas sin adultos: son las que dejan V1 y V2 en NA. Deben ser 88.
base_directorio_mes %>% filter(n_adultos == 0) %>% count()

# Una vivienda con dos hogares arrendatarios, para ver la suma a mano
base_directorio_mes %>%
  filter(n_hog_arriendo > 1) %>%
  select(DIRECTORIO, MES, n_hogares, n_hog_arriendo, arriendo_mensual_vivienda) %>%
  head()

# --- Comprobaciones de rango y coherencia ------------------------------------
# Son las que el informe declara en "Comprobaciones de calidad" de cada
# variable. Si alguna falla, comprobar() detiene la ejecucion y dice cual.
# La de exp(log) usa error RELATIVO: con montos de cientos de millones, el
# redondeo de exp(log(x)) ya pasa de 1e-6 pesos sin que haya ningun error.
bd          <- base_directorio_mes
sin_adultos <- bd$n_adultos == 0
con_v3      <- !is.na(bd$arriendo_mensual_vivienda)
cat("\n--- Comprobaciones de rango y coherencia ---\n")
rangos <- c(
  "V1 dentro de [0, 1]" =
    all(is.na(bd$prop_educ_superior_18mas) |
        (bd$prop_educ_superior_18mas >= 0 & bd$prop_educ_superior_18mas <= 1)),
  "V2 dentro de [0, 1]" =
    all(is.na(bd$prop_mujeres_adultas) |
        (bd$prop_mujeres_adultas >= 0 & bd$prop_mujeres_adultas <= 1)),
  "num_educ <= den_educ (V1)"    = all(bd$num_educ   <= bd$den_educ),
  "num_muj <= den_gen (V2)"      = all(bd$num_muj    <= bd$den_gen),
  "n_no_alfab <= den_alfab (V4)" = all(bd$n_no_alfab <= bd$den_alfab),
  "V1 es NA exactamente en las viviendas sin adultos" =
    all(is.na(bd$prop_educ_superior_18mas) == sin_adultos),
  "V2 es NA exactamente en las viviendas sin adultos" =
    all(is.na(bd$prop_mujeres_adultas) == sin_adultos),
  "V3 siempre por encima de 1.000" =
    all(!con_v3 | bd$arriendo_mensual_vivienda > 1000),
  "V3 es NA si nadie arrienda o falta algun monto" =
    all(is.na(bd$arriendo_mensual_vivienda) ==
        (bd$n_hog_arriendo == 0 | bd$n_hog_arr_sin_dato > 0)),
  "n_hog_arriendo <= n_hogares (V3)" = all(bd$n_hog_arriendo <= bd$n_hogares),
  "log de V3 es NA exactamente donde V3 es NA" =
    all(is.na(bd$log_arriendo_mensual) == !con_v3),
  "exp(log) reproduce V3 (error relativo < 1e-12)" =
    all(abs(exp(bd$log_arriendo_mensual[con_v3]) /
            bd$arriendo_mensual_vivienda[con_v3] - 1) < 1e-12),
  "toda vivienda tiene alguien de 3 anios o mas" = all(bd$den_alfab > 0),
  "V4 sin faltantes" = !any(is.na(bd$presencia_analfabetismo)),
  "V4 = Con... exactamente cuando n_no_alfab > 0" =
    all((bd$presencia_analfabetismo == "Con al menos una persona analfabeta") ==
        (bd$n_no_alfab > 0)))
comprobar(rangos)

# Cifras de la base que cita el informe
rango_v3  <- range(bd$arriendo_mensual_vivienda, na.rm = TRUE)
rango_log <- range(bd$log_arriendo_mensual,      na.rm = TRUE)
cat("\n  viviendas sin adultos (V1 y V2 en NA)          :", sum(sin_adultos), "\n")
cat("  viviendas con V3 (arriendo con monto valido)   :", sum(con_v3), "\n")
cat("  viviendas con dos o mas hogares arrendatarios  :", sum(bd$n_hog_arriendo >= 2), "\n")
cat("    (contando tambien los hogares sin monto)     :",
    sum(bd$n_hog_arriendo + bd$n_hog_arr_sin_dato >= 2), "\n")
cat("  V3 minimo y maximo (pesos)                     :", rango_v3[1], "y", rango_v3[2], "\n")
cat("  log_arriendo_mensual minimo y maximo           :",
    sprintf("%.2f y %.2f", rango_log[1], rango_log[2]), "\n")

# --- De donde sale el pico de 0,50 en V2: se verifica con P6050 --------------
# La composicion por sexo NO dice que relacion hay entre las dos personas. El
# diccionario documenta P6050 = "Cual es el parentesco de ... con el jefe o jefa
# del hogar?", con codigo 2 = "Pareja, esposo(a), conyuge, companero(a)".
dos_adultos <- general %>%
  filter(adulto) %>%
  group_by(PERIODO, MES, DIRECTORIO) %>%
  filter(n() == 2) %>%
  summarise(una_mujer   = sum(P3039 == "2") == 1,
            hombre_mujer = identical(sort(P3039), c("1", "2")),
            jefe_pareja  = identical(sort(P6050), c("1", "2")),
            jefe_hijo    = identical(sort(P6050), c("1", "3")),   # 3 = hijo(a)
            .groups = 'drop')
n_hm <- sum(dos_adultos$hombre_mujer)
n_jp <- sum(dos_adultos$hombre_mujer & dos_adultos$jefe_pareja)
n_jh <- sum(dos_adultos$hombre_mujer & dos_adultos$jefe_hijo)
pct  <- function(a, b) sub(".", ",", sprintf("%.1f %%", 100 * a / b), fixed = TRUE)
cat("\n--- V2: composicion de las viviendas de dos adultos ---\n")
cat("  viviendas con dos adultos          :", nrow(dos_adultos), "\n")
cat("  con exactamente una mujer (V2=0,50):", sum(dos_adultos$una_mujer), "\n")
cat("    de esas, el otro adulto responde 3 o 4 en P3039:",
    sum(dos_adultos$una_mujer & !dos_adultos$hombre_mujer), "\n")
cat("  con un hombre y una mujer          :", n_hm, "\n")
cat("  de esas, jefe(a) + pareja (P6050=2):", n_jp, sprintf("(%s)", pct(n_jp, n_hm)), "\n")
cat("  de esas, jefe(a) + hijo(a) (P6050=3):", n_jh, sprintf("(%s)", pct(n_jh, n_hm)), "\n")


#### Diccionario de las variables derivadas

diccionario_variables_derivadas <- data.frame(rbind(
 c("PERIODO","A\u00f1o, mes y semana de recolecci\u00f3n","Llave","texto","20250518-20250731","Tomado de la tabla 10","tabla 10: PERIODO","No"),
 c("MES","Mes de la encuesta","Llave","texto","05, 06, 07","Tomado de la tabla 10","tabla 10: MES","No"),
 c("MES_NOMBRE","Etiqueta legible del mes","Cualitativa ordinal","texto","Mayo, Junio, Julio","factor(MES) etiquetado como Mayo, Junio y Julio","derivada de tabla 10: MES","No"),
 c("DIRECTORIO","Identificador de la vivienda","Llave","texto","-","Tomado de la tabla 10","tabla 10: DIRECTORIO","No"),
 c("DPTO","Departamento","Cualitativa nominal","texto","33 c\u00f3digos observados","Constante dentro de la vivienda","tabla 10: DPTO","No"),
 c("CLASE","Cabecera (1) o resto (2)","Cualitativa nominal","texto","1, 2","Constante dentro de la vivienda","tabla 10: CLASE","No"),
 c("n_hogares","Hogares en la vivienda","Cuantitativa discreta","conteo",">=1","N\u00ba de SECUENCIA_P distintos","tabla 10","No"),
 c("n_personas","Personas en la vivienda","Cuantitativa discreta","conteo",">=1","Filas por vivienda","tabla 10","No"),
 c("n_adultos","Personas de 18 a\u00f1os o m\u00e1s","Cuantitativa discreta","conteo",">=0","Suma de POB_MAY18 = 1","tabla 10: POB_MAY18","No"),
 c("den_educ","Denominador de V1","Cuantitativa discreta","conteo",">=0","Adultos con P3042 entre 1 y 13","tabla 10: P3042, POB_MAY18","No"),
 c("num_educ","Numerador de V1","Cuantitativa discreta","conteo",">=0","Adultos con P3042 entre 8 y 13","tabla 10: P3042, POB_MAY18","No"),
 c("prop_educ_superior_18mas","V1: proporci\u00f3n de adultos con educaci\u00f3n t\u00e9cnica o superior","Cuantitativa continua","proporci\u00f3n","[0,1]","num_educ / den_educ; NA si den_educ = 0","tabla 10: P3042, POB_MAY18","S\u00ed: viviendas sin adultos"),
 c("den_gen","Denominador de V2","Cuantitativa discreta","conteo",">=0","Adultos con P3039 entre 1 y 4","tabla 10: P3039","No"),
 c("num_muj","Numerador de V2","Cuantitativa discreta","conteo",">=0","Adultos con P3039 = 2","tabla 10: P3039","No"),
 c("prop_mujeres_adultas","V2: proporci\u00f3n de adultos que se reconocen como mujeres","Cuantitativa continua","proporci\u00f3n","[0,1]","num_muj / den_gen; NA si den_gen = 0","tabla 10: P3039","S\u00ed: viviendas sin adultos"),
 c("n_hog_arriendo","Hogares en arriendo con monto v\u00e1lido","Cuantitativa discreta","conteo",">=0","Hogares con P5090 = 3 y P5140 > 1.000","tabla 01: P5090, P5140","No"),
 c("n_hog_arr_sin_dato","Hogares en arriendo sin monto utilizable","Cuantitativa discreta","conteo",">=0","P5090 = 3 pero P5140 en 98 o 99","tabla 01: P5090, P5140","No"),
 c("arriendo_mensual_vivienda","V3: arriendo mensual que paga la vivienda","Cuantitativa continua","pesos por mes","> 1.000","Suma de P5140 entre los hogares arrendatarios; NA si no hay arriendo o falta el monto","tabla 01: P5090, P5140","S\u00ed: 43.172 viviendas"),
 c("log_arriendo_mensual","Logaritmo natural de V3 (columna auxiliar)","Cuantitativa continua","log de pesos por mes","[9.21, 20.21]","log(arriendo_mensual_vivienda); NA donde V3 es NA. NO reemplaza a V3","derivada de tabla 01: P5090, P5140","S\u00ed: las mismas 43.172 viviendas"),
 c("den_alfab","Personas de 3 a\u00f1os o m\u00e1s (denominador de V4)","Cuantitativa discreta","conteo",">=1","Personas con P6160 en 1 o 2","tabla 10: P6160","No"),
 c("n_no_alfab","Personas que no saben leer ni escribir","Cuantitativa discreta","conteo",">=0","Personas con P6160 = 2","tabla 10: P6160","No"),
 c("presencia_analfabetismo","V4: presencia de analfabetismo en la vivienda","Cualitativa nominal","categor\u00eda","Sin personas analfabetas / Con al menos una persona analfabeta","Con... si n_no_alfab > 0; Sin... si n_no_alfab = 0","tabla 10: P6160","No: cobertura del 100%"),
 c("FEX_C18","Factor de expansi\u00f3n de personas y hogares","Cuantitativa continua","factor","[9.26, 17668.83]","Constante dentro de la vivienda. NO SE APLICA en este trabajo","tabla 10: FEX_C18","No")
), stringsAsFactors = FALSE)
names(diccionario_variables_derivadas) <- c("variable","etiqueta","tipo","unidad",
  "dominio_rango","regla_de_construccion","campos_fuente","admite_faltantes")

cat("\nDiccionario de derivadas:", nrow(diccionario_variables_derivadas), "variables documentadas.\n")
cat("Cubre TODAS las columnas de la base:",
    setequal(diccionario_variables_derivadas$variable,
             names(base_directorio_mes)), "\n")


#### Agregación departamental  (base_departamento_mes)

# Producto auxiliar: una fila por DPTO + MES. No se usa en la Fase 3, pero se
# reproduce para que esta sintaxis pueda reemplazar por completo a la anterior.
#
# NOTA METODOLOGICA: los resumenes departamentales son la MEDIA (o la mediana)
# de los valores de vivienda dentro del departamento; es decir, una media NO
# ponderada de razones, no la razon de los totales.

# Helpers: descartan los NA y devuelven NA_real_ cuando no queda ningun dato,
# en vez del Inf que devolveria min() sobre un vector vacio.
f_n   <- function(x) sum(!is.na(x))
f_na  <- function(x) sum(is.na(x))
f_med <- function(x) { x <- x[!is.na(x)]; if (!length(x))     NA_real_ else mean(x) }
f_mdn <- function(x) { x <- x[!is.na(x)]; if (!length(x))     NA_real_ else stats::median(x) }
f_de  <- function(x) { x <- x[!is.na(x)]; if (length(x) <= 1) NA_real_ else stats::sd(x) }
f_min <- function(x) { x <- x[!is.na(x)]; if (!length(x))     NA_real_ else min(x) }
f_max <- function(x) { x <- x[!is.na(x)]; if (!length(x))     NA_real_ else max(x) }

base_departamento_mes <- base_directorio_mes %>%
  group_by(DPTO, MES) %>%
  summarise(
    n_viviendas   = n(),

    educ_n        = f_n(prop_educ_superior_18mas),
    educ_na       = f_na(prop_educ_superior_18mas),
    educ_media    = f_med(prop_educ_superior_18mas),
    educ_mediana  = f_mdn(prop_educ_superior_18mas),
    educ_de       = f_de(prop_educ_superior_18mas),
    educ_min      = f_min(prop_educ_superior_18mas),
    educ_max      = f_max(prop_educ_superior_18mas),

    muj_n         = f_n(prop_mujeres_adultas),
    muj_na        = f_na(prop_mujeres_adultas),
    muj_media     = f_med(prop_mujeres_adultas),
    muj_mediana   = f_mdn(prop_mujeres_adultas),
    muj_de        = f_de(prop_mujeres_adultas),
    muj_min       = f_min(prop_mujeres_adultas),
    muj_max       = f_max(prop_mujeres_adultas),

    arr_n         = f_n(arriendo_mensual_vivienda),
    arr_na        = f_na(arriendo_mensual_vivienda),
    arr_media     = f_med(arriendo_mensual_vivienda),
    arr_mediana   = f_mdn(arriendo_mensual_vivienda),
    arr_de        = f_de(arriendo_mensual_vivienda),
    arr_min       = f_min(arriendo_mensual_vivienda),
    arr_max       = f_max(arriendo_mensual_vivienda),

    alfab_n_validas = sum(!is.na(presencia_analfabetismo)),
    prop_sin_analf  = if_else(alfab_n_validas > 0,
                        sum(presencia_analfabetismo == "Sin personas analfabetas",
                            na.rm = TRUE) / alfab_n_validas, NA_real_),
    prop_con_analf  = if_else(alfab_n_validas > 0,
                        sum(presencia_analfabetismo == "Con al menos una persona analfabeta",
                            na.rm = TRUE) / alfab_n_validas, NA_real_),
    .groups = 'drop'
  ) %>%
  mutate(MES_NOMBRE = factor(MES, levels = c("05", "06", "07"),
                             labels = c("Mayo", "Junio", "Julio"))) %>%
  arrange(MES, DPTO) %>%
  as.data.frame()

# Departamentos presentes en los tres meses (para comparaciones pareadas)
comunes <- Reduce(intersect, split(base_departamento_mes$DPTO, base_departamento_mes$MES))
base_departamento_mes$en_conjunto_comun <- base_departamento_mes$DPTO %in% comunes

cat(sprintf("\n  Departamentos en el conjunto comun: %d\n", length(comunes)))
cat("  Las proporciones de V4 suman 1 en cada DPTO+MES: ",
    all(abs(base_departamento_mes$prop_sin_analf +
            base_departamento_mes$prop_con_analf - 1) < 1e-9), "\n")


#### Comparación contra la base ya verificada

# Compara columna por columna contra salidas/base_directorio_mes.rds ANTES de
# exportar, de modo que la comparacion sea contra la version anterior.
RUTA_SALIDA <- file.path(RUTA, "salidas")
f_viejo <- file.path(RUTA_SALIDA, "base_directorio_mes.rds")

if (file.exists(f_viejo)) {
  viejo <- readRDS(f_viejo)
  nuevo <- base_directorio_mes
  viejo <- viejo[order(paste(viejo$PERIODO, viejo$MES, viejo$DIRECTORIO, sep = "|")), , drop = FALSE]
  nuevo <- nuevo[order(paste(nuevo$PERIODO, nuevo$MES, nuevo$DIRECTORIO, sep = "|")), , drop = FALSE]

  cat("\n=== COMPARACION CONTRA LA BASE ANTERIOR ===\n")
  cat(sprintf("  filas    : nuevo %d | anterior %d\n", nrow(nuevo), nrow(viejo)))
  cat(sprintf("  columnas : nuevo %d | anterior %d\n", ncol(nuevo), ncol(viejo)))
  n_dif <- 0
  for (v in intersect(names(viejo), names(nuevo))) {
    a <- nuevo[[v]]; b <- viejo[[v]]
    if (is.factor(a) || is.factor(b)) { a <- as.character(a); b <- as.character(b) }
    if (is.numeric(a) && is.numeric(b)) {
      dif <- abs(a - b)
      ok  <- all(is.na(a) == is.na(b)) &&
             (all(is.na(dif)) || max(dif, na.rm = TRUE) < 1e-9)
    } else ok <- identical(as.character(a), as.character(b))
    if (!isTRUE(ok)) n_dif <- n_dif + 1
    cat(sprintf("  [%s] %s\n", if (isTRUE(ok)) "IGUAL  " else "DIFIERE", v))
  }
  cat(sprintf("\n  RESULTADO: %d columnas con diferencias.\n", n_dif))
} else {
  cat("\nNo hay base anterior con que comparar; se exporta directamente.\n")
}


#### Comparación de la base departamental

f_viejo_d <- file.path(RUTA_SALIDA, "base_departamento_mes.rds")
if (file.exists(f_viejo_d)) {
viejo_d <- readRDS(f_viejo_d)
nuevo_d <- base_departamento_mes
viejo_d <- viejo_d[order(paste(viejo_d$DPTO, viejo_d$MES, sep = "|")), , drop = FALSE]
nuevo_d <- nuevo_d[order(paste(nuevo_d$DPTO, nuevo_d$MES, sep = "|")), , drop = FALSE]

cat("\n=== COMPARACION DE base_departamento_mes ===\n")
cat(sprintf("  filas    : nuevo %d | anterior %d\n", nrow(nuevo_d), nrow(viejo_d)))
cat(sprintf("  columnas : nuevo %d | anterior %d\n", ncol(nuevo_d), ncol(viejo_d)))
faltd <- setdiff(names(viejo_d), names(nuevo_d))
sobrd <- setdiff(names(nuevo_d), names(viejo_d))
if (length(faltd)) cat("  COLUMNAS QUE FALTAN :", paste(faltd, collapse = ", "), "\n")
if (length(sobrd)) cat("  COLUMNAS DE MAS     :", paste(sobrd, collapse = ", "), "\n")

n_dif_d <- 0
for (v in intersect(names(viejo_d), names(nuevo_d))) {
  a <- nuevo_d[[v]]; b <- viejo_d[[v]]
  if (is.factor(a) || is.factor(b)) { a <- as.character(a); b <- as.character(b) }
  if (is.numeric(a) && is.numeric(b)) {
    dif <- abs(a - b)
    ok  <- all(is.na(a) == is.na(b)) &&
           (all(is.na(dif)) || max(dif, na.rm = TRUE) < 1e-9)
  } else {
    ok <- identical(as.character(a), as.character(b))
  }
  if (!isTRUE(ok)) n_dif_d <- n_dif_d + 1
  cat(sprintf("  [%s] %s\n", if (isTRUE(ok)) "IGUAL  " else "DIFIERE", v))
}
cat(sprintf("\n  RESULTADO: %d columnas con diferencias.\n", n_dif_d))
} else cat("\nNo hay base departamental anterior con que comparar.\n")


#### Exportación

# CSV con separador ";" y decimal "," (convencion del DANE, abre bien en Excel
# en espanol). RDS conserva tipos y factores.
if (!dir.exists(RUTA_SALIDA)) dir.create(RUTA_SALIDA, recursive = TRUE)
exportar <- function(df, nombre) {
  write.table(df, file.path(RUTA_SALIDA, paste0(nombre, ".csv")),
              sep = ";", dec = ",", row.names = FALSE, na = "",
              qmethod = "double", fileEncoding = "UTF-8")
  saveRDS(df, file.path(RUTA_SALIDA, paste0(nombre, ".rds")))
  cat(sprintf("    %-32s %6d filas x %2d columnas -> .csv y .rds\n",
              nombre, nrow(df), ncol(df)))
}
cat("\n=== EXPORTACION ===\n")
exportar(base_directorio_mes,             "base_directorio_mes")
exportar(base_departamento_mes,           "base_departamento_mes")
exportar(diccionario_variables_derivadas, "diccionario_variables_derivadas")
cat("\nListo. Archivos en:", normalizePath(RUTA_SALIDA), "\n")

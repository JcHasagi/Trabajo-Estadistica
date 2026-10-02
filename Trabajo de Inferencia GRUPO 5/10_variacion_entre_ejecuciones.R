# ============================================================================
# FASE 3 - VARIACION ENTRE EJECUCIONES (SIN SEMILLA)
# Primer Trabajo Estadistica II - GEIH 2025
#
# Requisitos: haber corrido 08_estimadores_simulacion.R, que deja las
#             replicas en salidas/fase3_distribuciones.rds (y antes el 01b,
#             que deja la base). R base, sin paquetes.
# Modo de uso: source("10_variacion_entre_ejecuciones.R")
# Salidas en salidas/ (ademas de las tablas impresas en consola):
#   fase3_variacion_ejecuciones.csv  una fila por variable y valor de B: las
#                                    tres ejecuciones y su comparacion frente
#                                    al error de Monte Carlo
#   fase3_valores_informe_10.csv     las cifras de esa tabla, listas para el
#                                    informe (mismo formato que
#                                    fase3_valores_informe.csv del 08)
#
# TIEMPO: unos 8 minutos (dos ejecuciones mas del ciclo del 08). Imprime avance.
#
# QUE HACE Y POR QUE
# La simulacion del 08 no fija semilla, asi que cada ejecucion da cifras algo
# distintas. Este script hace visible esa variacion:
#   - Ejecucion 1: la que guardo el 08, que es la que reporta el informe. No
#     se vuelve a simular: sus replicas se leen del .rds.
#   - Ejecuciones 2 y 3: el MISMO ciclo del 08 (mismo N, mismos valores de B,
#     mismo cuerpo de la replica, con reemplazo), sin semilla.
# Para cada variable y cada B compara entre las tres ejecuciones el promedio
# de las replicas de la media (y su diferencia con mu) y la varianza de las
# replicas, cada una frente a su error de Monte Carlo.
#
# CRITERIO DE COMPARACION
# Si la unica fuente de diferencia es el azar del remuestreo, el rango de R
# ejecuciones independientes dividido por el error estandar de Monte Carlo
# (EE) se comporta como el rango de R normales estandar. Con R = 3:
#   - en promedio vale 3/raiz(pi) = 1,69;
#   - supera qtukey(0,95; 3; Inf) = 3,31 solo en el 5 % de los casos.
# Por eso se dice que las ejecuciones difieren en una magnitud DEL ORDEN del
# error de Monte Carlo, y no "dentro" de el: con 12 filas por cifra es
# esperable que alguna supere el limite por azar. Como EE = s/raiz(B), la
# variacion se reduce al crecer B: de B = 100 a B = 10.000 se divide por diez.
# ============================================================================


# ---------------------------------------------------------------------------
# 0. PREPARACION
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# RUTAS RELATIVAS: el directorio de trabajo debe ser la carpeta del trabajo,
# la que contiene "salidas/" y las carpetas de mes. En RStudio, abrir el
# proyecto ahi, o hacer setwd() una vez antes de correr esto.
# ---------------------------------------------------------------------------
options(scipen = 999)   # sin notacion cientifica: 400000, nunca 4e+05.
                        # Se fija aqui para que la salida no dependa del
                        # estado de la sesion ni del orden de ejecucion.

RUTA        <- "."
RUTA_SALIDA <- file.path(RUTA, "salidas")
ARCH_SIM    <- file.path(RUTA_SALIDA, "fase3_distribuciones.rds")
ARCH_BASE   <- file.path(RUTA_SALIDA, "base_directorio_mes.rds")
if (!file.exists(ARCH_BASE))
  stop("No encuentro ", ARCH_BASE, " desde el directorio de trabajo actual (",
       getwd(), "). Corre antes 01b_construccion_estilo_docente.R, que crea esa base.")
if (!file.exists(ARCH_SIM))
  stop("No encuentro ", ARCH_SIM, " desde el directorio de trabajo actual (",
       getwd(), "). Corre antes 08_estimadores_simulacion.R, que guarda las replicas.")

SIM   <- readRDS(ARCH_SIM)
PARAM <- SIM$parametros
BS    <- SIM$BS          # mismos valores de B que el 08: se leen, no se reescriben

d <- readRDS(ARCH_BASE)

# Las cuatro poblaciones, igual que en el 08. V4 se pasa a 0/1.
POB <- list(
  V1 = d$prop_educ_superior_18mas[!is.na(d$prop_educ_superior_18mas)],
  V2 = d$prop_mujeres_adultas[!is.na(d$prop_mujeres_adultas)],
  V3 = d$log_arriendo_mensual[!is.na(d$log_arriendo_mensual)],
  V4 = as.numeric(d$presencia_analfabetismo == "Con al menos una persona analfabeta")
)

# La base tiene que ser la misma con que se corrio el 08.
N_08  <- setNames(PARAM$N, PARAM$variable)[names(POB)]
MU_08 <- setNames(PARAM$media, PARAM$variable)[names(POB)]
if (!all(sapply(POB, length) == N_08) ||
    !isTRUE(all.equal(as.numeric(sapply(POB, mean)), as.numeric(MU_08))))
  stop("La base de salidas/ no es la misma con que se corrio el 08 (cambio N o la media). ",
       "Vuelve a correr 08_estimadores_simulacion.R antes de este script.")

R_EJEC <- 3                                          # la del 08 + 2 nuevas
LIM95  <- qtukey(0.95, nmeans = R_EJEC, df = Inf)    # 3,31 con R = 3
# Promedio del rango de R normales estandar (3/raiz(pi) = 1,69 con R = 3).
ESPERADO <- integrate(function(z) 1 - pnorm(z)^R_EJEC - (1 - pnorm(z))^R_EJEC,
                      -Inf, Inf)$value

# Error de Monte Carlo de la varianza de B replicas y (m4: cuarto momento
# central de las replicas). Con replicas normales vale ~ var(y) raiz(2/(B-1)).
ee_var <- function(y) {
  B  <- length(y)
  m4 <- mean((y - mean(y))^4)
  sqrt(max(m4 - var(y)^2 * (B - 3) / (B - 1), 0) / B)
}


# ---------------------------------------------------------------------------
# 1. TRES EJECUCIONES DEL MISMO CICLO, ALMACENADAS
#    EJ[[1]] = las replicas guardadas por el 08 (no se vuelve a simular).
#    EJ[[2]] y EJ[[3]] = el ciclo del 08 repetido, sin semilla. Misma
#    estructura: EJ[[k]]$V1$B_100$media, EJ[[k]]$V1$B_100$mediana, ...
# ---------------------------------------------------------------------------
EJ <- list()
EJ[[1]] <- SIM$distribuciones
t0 <- Sys.time()

for (ej in 2:R_EJEC) {
  DIST <- list()
  for (v in names(POB)) {
    x <- POB[[v]]
    N <- length(x)                   # n = N: el conjunto analitico completo
    DIST[[v]] <- list()
    for (B in BS) {                  # EL MISMO CICLO SOBRE B DEL 08
      ms <- numeric(B); md <- numeric(B)
      for (b in seq_len(B)) {
        s <- sample(x, N, replace = TRUE)
        ms[b] <- mean(s); md[b] <- median(s)
      }
      DIST[[v]][[paste0("B_", B)]] <- list(media = ms, mediana = md)   # se almacena
      cat(sprintf("  ejecucion %d  %s  N=%5d  B=%5d  listo  (%.0f s acumulados)\n",
                  ej, v, N, B, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      flush.console()
    }
  }
  EJ[[ej]] <- DIST
}
t_sim <- as.numeric(difftime(Sys.time(), t0, units = "mins"))


# ---------------------------------------------------------------------------
# 2. RESUMEN DE CADA EJECUCION (las mismas formulas del 08)
# ---------------------------------------------------------------------------
EJEC <- data.frame()
for (ej in seq_len(R_EJEC)) for (v in names(POB)) for (B in BS) {
  r  <- EJ[[ej]][[v]][[paste0("B_", B)]]
  pv <- PARAM[PARAM$variable == v, ]
  EJEC <- rbind(EJEC, data.frame(
    ejecucion          = ej,
    variable           = v,
    N                  = pv$N,
    B                  = B,
    prom_media         = mean(r$media),
    sesgo_media        = mean(r$media) - pv$media,
    err_MC_media       = sd(r$media) / sqrt(B),
    var_media          = var(r$media),
    err_MC_var_media   = ee_var(r$media),
    prop_med_igual_pob = mean(r$mediana == pv$mediana),
    var_mediana        = var(r$mediana),
    razon_var          = if (var(r$media) > 0) var(r$mediana) / var(r$media) else NA,
    n_med_distinta     = sum(r$mediana != pv$mediana),
    row.names = NULL))
}

# Control: la ejecucion 1 tiene que dar exactamente las cifras del 08.
e1  <- EJEC[EJEC$ejecucion == 1, ]
r08 <- SIM$resumen
r08 <- r08[match(paste(e1$variable, e1$B), paste(r08$variable, r08$B)), ]
if (!isTRUE(all.equal(e1$prom_media, r08$prom_media)) ||
    !isTRUE(all.equal(e1$var_media, r08$var_media)))
  stop("Las replicas del .rds no coinciden con su propio resumen. Vuelve a correr el 08.")


# ---------------------------------------------------------------------------
# 3. TABLA: DIFERENCIAS ENTRE EJECUCIONES FRENTE AL ERROR DE MONTE CARLO
#    rango = max - min de las tres ejecuciones (el rango del promedio es el
#    mismo que el del sesgo, porque mu no cambia).
#    err_MC = promedio de los tres errores de Monte Carlo.
# ---------------------------------------------------------------------------
# Primera cifra decimal en que difieren los promedios de las ejecuciones,
# comparando sus cifras escritas (asi cuenta tambien el acarreo: 0,33249 y
# 0,33251 difieren desde la cuarta, aunque el rango sea del orden de 10^-5).
decimal_dif <- function(p) {
  s   <- formatC(p, format = "f", digits = 12)
  ent <- sub("[.].*$", "", s)
  if (length(unique(ent)) > 1) return(0)
  m <- do.call(rbind, strsplit(sub("^[^.]*[.]", "", s), ""))
  k <- which(apply(m, 2, function(z) length(unique(z)) > 1))
  if (length(k)) k[1] else NA
}

TAB <- data.frame()
for (v in names(POB)) for (B in BS) {
  z <- EJEC[EJEC$variable == v & EJEC$B == B, ]
  z <- z[order(z$ejecucion), ]
  fila <- data.frame(variable = v, N = z$N[1], B = B)
  for (ej in seq_len(R_EJEC)) fila[[paste0("prom_ej", ej)]]  <- z$prom_media[ej]
  for (ej in seq_len(R_EJEC)) fila[[paste0("sesgo_ej", ej)]] <- z$sesgo_media[ej]
  fila$rango               <- diff(range(z$prom_media))
  fila$err_MC              <- mean(z$err_MC_media)
  fila$rango_en_err_MC     <- fila$rango / fila$err_MC
  fila$supera_95           <- fila$rango_en_err_MC > LIM95
  for (ej in seq_len(R_EJEC)) fila[[paste0("var_ej", ej)]]   <- z$var_media[ej]
  fila$rango_var           <- diff(range(z$var_media))
  fila$err_MC_var          <- mean(z$err_MC_var_media)
  fila$rango_var_en_err_MC <- fila$rango_var / fila$err_MC_var
  fila$supera_95_var       <- fila$rango_var_en_err_MC > LIM95
  for (ej in seq_len(R_EJEC)) fila[[paste0("med_igual_ej", ej)]] <- z$prop_med_igual_pob[ej]
  # Razon var_mediana / var_media y replicas con la mediana distinta de la
  # poblacional en cada ejecucion. En V3 la varianza de la mediana la producen
  # solo esas pocas replicas: la razon cambia mucho de una ejecucion a otra.
  for (ej in seq_len(R_EJEC)) fila[[paste0("razon_var_ej", ej)]] <- z$razon_var[ej]
  for (ej in seq_len(R_EJEC)) fila[[paste0("n_med_dist_ej", ej)]] <- z$n_med_distinta[ej]
  fila$decimal_dif         <- decimal_dif(z$prom_media)
  TAB <- rbind(TAB, fila)
}

write.csv2(TAB, file.path(RUTA_SALIDA, "fase3_variacion_ejecuciones.csv"),
           row.names = FALSE)

# Impresion con coma decimal. Los decimales del sesgo se ajustan al error de
# Monte Carlo: tres cifras significativas del EE mas chico.
coma <- function(z, dig) formatC(z, format = "f", digits = dig,
                                 big.mark = ".", decimal.mark = ",")
miles <- function(z) formatC(z, format = "d", big.mark = ".", decimal.mark = ",")
dig_s <- max(6, 2 - floor(log10(min(TAB$err_MC))))

cat("\n=== VARIACION ENTRE EJECUCIONES: SESGO DE LA MEDIA (sin semilla) ===\n")
cat("Ejecucion 1 = la guardada por el 08 (la del informe); 2 y 3 = repeticiones.\n")
cat(sprintf("Rango de %d ejecuciones medido en errores estandar de Monte Carlo (EE):\n",
            R_EJEC))
cat(sprintf("por azar vale en promedio %s EE y supera %s EE en el 5 %% de los casos.\n\n",
            coma(ESPERADO, 2), coma(LIM95, 2)))
imp <- data.frame(
  Variable   = TAB$variable,
  B          = miles(TAB$B),
  `Ejec. 1`  = coma(TAB$sesgo_ej1, dig_s),
  `Ejec. 2`  = coma(TAB$sesgo_ej2, dig_s),
  `Ejec. 3`  = coma(TAB$sesgo_ej3, dig_s),
  `Rango`    = coma(TAB$rango, dig_s),
  `Error MC` = coma(TAB$err_MC, dig_s),
  `Rango/EE` = coma(TAB$rango_en_err_MC, 2),
  check.names = FALSE)
print(imp, row.names = FALSE, right = TRUE)
cat(sprintf("\nRango/EE promedio: %s (esperado por azar: %s). Filas que superan %s EE: %d de %d.\n",
            coma(mean(TAB$rango_en_err_MC), 2), coma(ESPERADO, 2), coma(LIM95, 2),
            sum(TAB$supera_95), nrow(TAB)))

cat("\n=== VARIACION ENTRE EJECUCIONES: VARIANZA DE LAS REPLICAS DE LA MEDIA ===\n")
cat("(varianzas multiplicadas por 1.000.000)\n\n")
imp2 <- data.frame(
  Variable   = TAB$variable,
  B          = miles(TAB$B),
  `Ejec. 1`  = coma(TAB$var_ej1 * 1e6, 4),
  `Ejec. 2`  = coma(TAB$var_ej2 * 1e6, 4),
  `Ejec. 3`  = coma(TAB$var_ej3 * 1e6, 4),
  `Rango`    = coma(TAB$rango_var * 1e6, 4),
  `Error MC` = coma(TAB$err_MC_var * 1e6, 4),
  `Rango/EE` = coma(TAB$rango_var_en_err_MC, 2),
  check.names = FALSE)
print(imp2, row.names = FALSE, right = TRUE)
cat(sprintf("\nRango/EE promedio: %s (esperado por azar: %s). Filas que superan %s EE: %d de %d.\n",
            coma(mean(TAB$rango_var_en_err_MC), 2), coma(ESPERADO, 2), coma(LIM95, 2),
            sum(TAB$supera_95_var), nrow(TAB)))


# ---------------------------------------------------------------------------
# 4. VALORES PARA EL INFORME: salidas/fase3_valores_informe_10.csv
#    Mismo formato que salidas/fase3_valores_informe.csv del 08:
#    clave ; valor ; valor_num ; descripcion, UTF-8, separador ";" y coma
#    decimal. Claves:
#      <V>_B<B>_ej_<col>   una por variable, valor de B y columna de la tabla
#      ej_<col>            resumen de las 12 filas y constantes del criterio
# ---------------------------------------------------------------------------

# --- Formato de las cifras: la misma funcion fmt_informe del 08 -------------
#   tipo "num"   : |x| >= 0,001 -> 6 decimales; 0 < |x| < 0,001 -> 3 cifras
#                  significativas como potencia de 10 (2,27 x 10^-6, con el
#                  signo de multiplicar y superindices); 0 exacto -> "0"
#   tipo "pct"   : proporcion escrita como porcentaje con 2 decimales y " %"
#   tipo "razon" : 1 decimal y punto de miles ("razon2": 2 decimales)
#   tipo "entero": sin decimales y con punto de miles
#   Negativos con el signo menos tipografico; "no definida" si no existe.
utf8  <- function(...) rawToChar(as.raw(c(...)))
POR   <- utf8(0xc3, 0x97)            # signo de multiplicar
MENOS <- utf8(0xe2, 0x88, 0x92)      # signo menos tipografico
SUP   <- c("0" = utf8(0xe2, 0x81, 0xb0), "1" = utf8(0xc2, 0xb9),
           "2" = utf8(0xc2, 0xb2),       "3" = utf8(0xc2, 0xb3),
           "4" = utf8(0xe2, 0x81, 0xb4), "5" = utf8(0xe2, 0x81, 0xb5),
           "6" = utf8(0xe2, 0x81, 0xb6), "7" = utf8(0xe2, 0x81, 0xb7),
           "8" = utf8(0xe2, 0x81, 0xb8), "9" = utf8(0xe2, 0x81, 0xb9),
           "-" = utf8(0xe2, 0x81, 0xbb))     # superindices 0-9 y menos

pot10 <- function(l10) {
  e <- floor(l10); m <- round(10^(l10 - e), 2)
  if (m >= 10) { m <- m / 10; e <- e + 1 }
  paste0(coma(m, 2), " ", POR, " 10",
         paste(SUP[strsplit(as.character(e), "")[[1]]], collapse = ""))
}

fmt_informe <- function(x, tipo = "num") {
  if (is.na(x) || !is.finite(x)) return("no definida")
  signo <- if (x < 0) MENOS else ""
  a <- abs(x)
  txt <- switch(tipo,
    entero = formatC(round(a), format = "d", big.mark = ".", decimal.mark = ","),
    pct    = paste0(coma(100 * a, 2), " %"),
    razon  = coma(a, 1),
    razon2 = coma(a, 2),
    num    = if (a == 0) "0" else if (signif(a, 3) >= 0.001) coma(a, 6) else
               pot10(log10(signif(a, 3))))
  if (grepl("^0[,0]*( %)?$", txt)) signo <- ""   # sin "-0,0"
  paste0(signo, txt)
}

fila_val <- function(clave, x, tipo, descripcion, valor = fmt_informe(x, tipo)) {
  data.frame(clave = clave, valor = valor,
             valor_num = if (is.finite(x)) x else NA,
             descripcion = descripcion, stringsAsFactors = FALSE)
}

VAL <- data.frame()
for (i in seq_len(nrow(TAB))) {
  z <- TAB[i, ]
  pref <- paste0(z$variable, "_B", z$B, "_ej_")
  txtB <- paste0(" (", z$variable, ", n = N, B = ", miles(z$B), ")")
  for (ej in seq_len(R_EJEC)) {
    quien <- if (ej == 1) "ejecucion 1, la del 08 y del informe" else paste("ejecucion", ej)
    VAL <- rbind(VAL,
      fila_val(paste0(pref, "prom_", ej), z[[paste0("prom_ej", ej)]], "num",
               paste0("promedio de las replicas de la media, ", quien, txtB)),
      fila_val(paste0(pref, "sesgo_", ej), z[[paste0("sesgo_ej", ej)]], "num",
               paste0("sesgo de la media (promedio - mu), ", quien, txtB)),
      fila_val(paste0(pref, "var_", ej), z[[paste0("var_ej", ej)]], "num",
               paste0("varianza de las replicas de la media, ", quien, txtB)))
  }
  VAL <- rbind(VAL,
    fila_val(paste0(pref, "rango"), z$rango, "num",
             paste0("rango de los tres promedios (= rango de los tres sesgos)", txtB)),
    fila_val(paste0(pref, "err_MC"), z$err_MC, "num",
             paste0("error de Monte Carlo del promedio, s/raiz(B), media de las tres ejecuciones", txtB)),
    fila_val(paste0(pref, "rango_en_err_MC"), z$rango_en_err_MC, "razon",
             paste0("rango / err_MC (por azar: 1,7 en promedio; 3,3 o mas en el 5 % de los casos)", txtB)),
    fila_val(paste0(pref, "decimal_dif"), z$decimal_dif, "entero",
             paste0("primera cifra decimal en que difieren los promedios escritos",
                    " de las tres ejecuciones", txtB)),
    fila_val(paste0(pref, "rango_var"), z$rango_var, "num",
             paste0("rango de las tres varianzas de las replicas", txtB)),
    fila_val(paste0(pref, "err_MC_var"), z$err_MC_var, "num",
             paste0("error de Monte Carlo de la varianza de las replicas, media de las tres", txtB)),
    fila_val(paste0(pref, "rango_var_en_err_MC"), z$rango_var_en_err_MC, "razon",
             paste0("rango_var / err_MC_var", txtB)),
    fila_val(paste0(pref, "med_igual_min"),
             min(z$med_igual_ej1, z$med_igual_ej2, z$med_igual_ej3), "pct",
             paste0("minimo en las tres ejecuciones de las replicas con mediana muestral",
                    " = mediana poblacional", txtB)))
  for (ej in seq_len(R_EJEC)) {
    quien <- if (ej == 1) "ejecucion 1, la del 08 y del informe" else paste("ejecucion", ej)
    VAL <- rbind(VAL,
      fila_val(paste0(pref, "razon_var_", ej), z[[paste0("razon_var_ej", ej)]], "razon2",
               paste0("var_mediana / var_media, ", quien, txtB)),
      fila_val(paste0(pref, "n_med_distinta_", ej), z[[paste0("n_med_dist_ej", ej)]], "entero",
               paste0("replicas con la mediana muestral distinta de la poblacional, ",
                      quien, txtB)))
  }
  rv <- c(z$razon_var_ej1, z$razon_var_ej2, z$razon_var_ej3)
  VAL <- rbind(VAL,
    fila_val(paste0(pref, "razon_var_min"), if (all(is.na(rv))) NA else min(rv, na.rm = TRUE),
             "razon2", paste0("minimo de var_mediana / var_media en las tres ejecuciones", txtB)),
    fila_val(paste0(pref, "razon_var_max"), if (all(is.na(rv))) NA else max(rv, na.rm = TRUE),
             "razon2", paste0("maximo de var_mediana / var_media en las tres ejecuciones", txtB)))
}
VAL <- rbind(VAL,
  fila_val("ej_n_ejecuciones", R_EJEC, "entero",
           "numero de ejecuciones comparadas (la del 08 y dos repeticiones)"),
  fila_val("ej_total_filas", nrow(TAB), "entero",
           "filas comparadas (4 variables x 3 valores de B)"),
  fila_val("ej_esperado", ESPERADO, "razon",
           "promedio del rango de 3 ejecuciones en EE si solo actua el azar (3/raiz(pi))"),
  fila_val("ej_limite_95", LIM95, "razon",
           "limite que el rango en EE supera solo en el 5 % de los casos, qtukey(0,95; 3; Inf)"),
  fila_val("ej_prom_rango_en_err_MC", mean(TAB$rango_en_err_MC), "razon",
           "promedio de rango_en_err_MC en las 12 filas (promedio de las replicas)"),
  fila_val("ej_filas_supera_95", sum(TAB$supera_95), "entero",
           "filas en que rango_en_err_MC supera ej_limite_95 (promedio de las replicas)"),
  fila_val("ej_prom_rango_var_en_err_MC", mean(TAB$rango_var_en_err_MC), "razon",
           "promedio de rango_var_en_err_MC en las 12 filas (varianza de las replicas)"),
  fila_val("ej_filas_supera_95_var", sum(TAB$supera_95_var), "entero",
           "filas en que rango_var_en_err_MC supera ej_limite_95 (varianza de las replicas)"),
  fila_val("ej_factor_err_MC", sqrt(max(BS) / min(BS)), "razon",
           "factor en que baja el error de Monte Carlo del menor al mayor B, raiz(Bmax/Bmin)"),
  # Debe coincidir con la clave fecha_ejecucion_08 de fase3_valores_informe.csv:
  # si no coincide, el 08 se volvio a correr despues del 10.
  fila_val("ej_fecha_08", NA, "num",
           "fecha de la ejecucion del 08 que se leyo (la de salidas/fase3_distribuciones.rds)",
           valor = format(SIM$fecha, "%Y-%m-%d %H:%M:%OS3")))

comillas <- function(z) paste0('"', gsub('"', '""', z, fixed = TRUE), '"')
num_csv  <- ifelse(is.na(VAL$valor_num), "NA",
                   sub(".", ",", sprintf("%.15g", VAL$valor_num), fixed = TRUE))
lineas <- c('"clave";"valor";"valor_num";"descripcion"',
            paste(comillas(VAL$clave), comillas(VAL$valor), num_csv,
                  comillas(VAL$descripcion), sep = ";"))
con <- file(file.path(RUTA_SALIDA, "fase3_valores_informe_10.csv"), open = "wb")
writeLines(lineas, con, useBytes = TRUE)
close(con)


# ---------------------------------------------------------------------------
# 5. RESUMEN Y COMO LEER LOS RESULTADOS (cifras calculadas, no escritas)
# ---------------------------------------------------------------------------
cat("\nTiempo de las dos ejecuciones nuevas:", round(t_sim, 1), "min\n")
cat("\nArchivos escritos:\n")
cat("  salidas/fase3_variacion_ejecuciones.csv\n")
cat(sprintf("  salidas/fase3_valores_informe_10.csv  (%d claves)\n", nrow(VAL)))
cat("\n--------------------------------------------------------------------\n")
cat("COMO LEER ESTO ANTES DE REDACTAR:\n\n")
cat("1. Ninguna ejecucion repite a otra: sin semilla, las cifras cambian en los\n")
cat("   ultimos decimales. El cambio es del orden del error de Monte Carlo y se\n")
cat(sprintf("   reduce al crecer B (el error baja %s veces de B = %s a B = %s).\n",
            coma(sqrt(max(BS) / min(BS)), 0), miles(min(BS)), miles(max(BS))))
cat(sprintf("2. Rango/EE promedio del sesgo: %s (azar: %s). Filas sobre %s: %d de %d.\n",
            coma(mean(TAB$rango_en_err_MC), 2), coma(ESPERADO, 2), coma(LIM95, 2),
            sum(TAB$supera_95), nrow(TAB)))
cat(sprintf("   Con %d comparaciones (sesgo y varianza), que alguna supere ese limite\n",
            2 * nrow(TAB)))
cat("   es lo esperable por azar (en promedio 1 de cada 20).\n")
cat("3. Con el mayor B, las ejecuciones difieren a partir del decimal:\n")
for (v in names(POB)) {
  z <- TAB[TAB$variable == v & TAB$B == max(BS), ]
  cat(sprintf("     %s: %s  (rango %s; error de Monte Carlo %s)\n", v,
              if (is.na(z$decimal_dif)) "-" else z$decimal_dif,
              coma(z$rango, dig_s), coma(z$err_MC, dig_s)))
}
cat("4. Mediana muestral igual a la poblacional (minimo de las tres ejecuciones,\n")
cat("   mayor B):", paste(sprintf("%s %s %%", TAB$variable[TAB$B == max(BS)],
                                 coma(100 * pmin(TAB$med_igual_ej1, TAB$med_igual_ej2,
                                                 TAB$med_igual_ej3)[TAB$B == max(BS)], 2)),
                         collapse = " - "), "\n")
z3 <- TAB[TAB$variable == "V3" & TAB$B == max(BS), ]
cat(sprintf("5. V3, mayor B: var_mediana / var_media = %s, %s y %s en las tres ejecuciones\n",
            coma(z3$razon_var_ej1, 2), coma(z3$razon_var_ej2, 2), coma(z3$razon_var_ej3, 2)))
cat(sprintf("   (replicas con la mediana distinta: %d, %d y %d). Esa razon depende de\n",
            z3$n_med_dist_ej1, z3$n_med_dist_ej2, z3$n_med_dist_ej3))
cat("   pocas replicas y no permite ordenar media y mediana por su varianza; el\n")
cat("   ECM frente a mu, dominado por el sesgo de la mediana, si es estable.\n")

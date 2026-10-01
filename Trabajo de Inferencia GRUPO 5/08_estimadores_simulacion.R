# ============================================================================
# FASE 3 - CALCULO DE LOS ESTIMADORES Y EVALUACION DEL ESTIMADOR
# Primer Trabajo Estadistica II - GEIH 2025
#
# Requisitos: ninguno. R base, sin paquetes.
# Modo de uso: source("08_estimadores_simulacion.R")
# Salidas: dos CSV en salidas/ y las tablas impresas en consola.
#
# TIEMPO: la simulacion tarda entre 2 y 5 minutos. Imprime avance.
#
# ---------------------------------------------------------------------------
# DECISIONES DE METODO (declararlas en el informe)
#
# 1. ESCALA DE V3: los estimadores van sobre el LOGARITMO, confirmado por el
#    docente. El parametro es mu = E[log(arriendo)], NO la media del arriendo.
#    exp() sobre el intervalo de mu devuelve un intervalo para la MEDIA
#    GEOMETRICA (leible como la mediana bajo lognormalidad), no para la media
#    aritmetica. Ese es el error clasico y hay que decirlo.
#
# 2. NIVEL DE CONFIANZA: 95 % en todo el trabajo.
#
# 3. SIMULACION CON REEMPLAZO. Se toma la base como poblacion y se extraen
#    muestras de tamano n CON reemplazo, de modo que cada extraccion es i.i.d.
#    de la distribucion poblacional. Asi Var(media) = sigma^2/n exactamente y
#    la simulacion se puede contrastar contra la teoria. Sin reemplazo habria
#    que introducir el factor de correccion por poblacion finita (1 - n/N),
#    que con n = 5.000 sobre N = 29.565 vale 0,83 y NO es despreciable.
#
# 4. NUMERO DE REPETICIONES B = 10.000, documentado con el criterio del
#    docente a partir del IC 95 %:
#
#       B >= z^2 * p(1-p) / e^2 ,  z = 1,96 ,  p = 0,95 ,  e = error tolerado
#
#       e = 1,00 pp -> B >=  1.825
#       e = 0,50 pp -> B >=  7.299     <- B = 10.000 lo cumple con margen
#       e = 0,20 pp -> B >= 45.617
#
#    Con B = 10.000 el error estandar de Monte Carlo es 0,00218 y el
#    semiancho del IC 95 % de la proporcion simulada es +-0,427 pp.
#
# 5. SEMILLA FIJA: set.seed(2026). Sin esto el experimento no es reproducible
#    y el requisito de "documentado" queda a medias.
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
if (!dir.exists(RUTA_SALIDA))
  stop("No encuentro la carpeta 'salidas/' desde el directorio de trabajo actual (",
       getwd(), "). Corre antes 01b_construccion_estilo_docente.R.")

base_directorio_mes <- readRDS(file.path(RUTA_SALIDA, "base_directorio_mes.rds"))
d <- base_directorio_mes

RUTA_S <- RUTA_SALIDA

Z  <- qnorm(0.975)          # 1,959964
B  <- 10000                 # repeticiones, justificadas arriba
NS <- c(100, 500, 1000, 5000)
SEMILLA <- 2026

# Las cuatro poblaciones. V4 se pasa a 0/1 para poder promediarla.
POB <- list(
  V1 = d$prop_educ_superior_18mas[!is.na(d$prop_educ_superior_18mas)],
  V2 = d$prop_mujeres_adultas[!is.na(d$prop_mujeres_adultas)],
  V3 = d$log_arriendo_mensual[!is.na(d$log_arriendo_mensual)],
  V4 = as.numeric(d$presencia_analfabetismo == "Con al menos una persona analfabeta")
)
ETIQ <- c(V1 = "V1 prop_educ_superior_18mas",
          V2 = "V2 prop_mujeres_adultas",
          V3 = "V3 log_arriendo_mensual (escala log)",
          V4 = "V4 presencia_analfabetismo (proporcion)")

cat("Tamanos de poblacion:\n"); print(sapply(POB, length))
cat("\nDeben decir: V1 72649 - V2 72649 - V3 29565 - V4 72737\n\n")


# ---------------------------------------------------------------------------
# 1. CALCULO DE LOS ESTIMADORES
#    Tabla de la plantilla: Analogia | Maxima Verosimilitud | LI | LS
#
#    Por que las dos primeras columnas coinciden en las cuatro, y por que la
#    razon es distinta en cada caso, va en las celdas de Comentario. Resumen:
#      V4 - se DERIVA: Bernoulli, se maximiza la verosimilitud, p = x/n.
#      V3 - bajo el modelo lognormal, mu de MV es la media de los logaritmos.
#           Pero sigma^2 SI difiere: n-1 frente a n (ver abajo).
#      V1 y V2 - coinciden porque bajo un modelo normal el estimador de MV de
#           la media es la media muestral. La coincidencia es FORMAL: V1 es
#           bimodal y V2 trimodal, el modelo normal las describe mal. El
#           estimador se justifica por analogia y el intervalo por el TLC.
# ---------------------------------------------------------------------------
est <- data.frame()
for (v in names(POB)) {
  x  <- POB[[v]]; n <- length(x)
  m  <- mean(x)
  if (v == "V4") {
    # Proporcion: error estandar de Bernoulli.
    se <- sqrt(m * (1 - m) / n)
  } else {
    se <- sd(x) / sqrt(n)
  }
  est <- rbind(est, data.frame(
    variable   = v,
    etiqueta   = ETIQ[[v]],
    n          = n,
    analogia   = m,
    max_verosi = m,          # coinciden; el porque va en el comentario
    LI_95      = m - Z * se,
    LS_95      = m + Z * se,
    error_est  = se,
    row.names  = NULL))
}
cat("=== CALCULO DE LOS ESTIMADORES (IC 95 %) ===\n")
print(format(est[, c("variable","n","analogia","max_verosi","LI_95","LS_95")],
             digits = 8), row.names = FALSE)

# V3: la unica donde analogia y MV DIFIEREN en la varianza. Es el mejor
# ejemplo del trabajo para comentar que el sesgo de MV se desvanece con n.
x3 <- POB[["V3"]]; n3 <- length(x3)
s2_insesgado <- var(x3)                    # denominador n-1
s2_mv        <- var(x3) * (n3 - 1) / n3    # denominador n
cat("\n--- V3: varianza por los dos metodos ---\n")
cat(sprintf("sigma^2 insesgado (n-1) = %.6f\n", s2_insesgado))
cat(sprintf("sigma^2 max. verosim (n) = %.6f\n", s2_mv))
cat(sprintf("diferencia = %.8f  (aparece en el quinto decimal: es conceptual, no numerica)\n",
            s2_insesgado - s2_mv))

# V3 devuelto a pesos. OJO con como se rotula.
cat("\n--- V3 devuelto a pesos con exp() ---\n")
e3 <- est[est$variable == "V3", ]
cat(sprintf("media geometrica = %s   IC 95 %% = [%s ; %s]\n",
            format(round(exp(e3$analogia)), big.mark = ".", decimal.mark = ","),
            format(round(exp(e3$LI_95)),   big.mark = ".", decimal.mark = ","),
            format(round(exp(e3$LS_95)),   big.mark = ".", decimal.mark = ",")))
cat("ROTULARLO COMO INTERVALO PARA LA MEDIA GEOMETRICA, NO PARA LA MEDIA.\n")

write.csv2(est, file.path(RUTA_S, "fase3_estimadores_puntuales.csv"),
           row.names = FALSE)


# ---------------------------------------------------------------------------
# 2. EVALUACION DEL ESTIMADOR - SIMULACION
#    Un solo experimento llena las tres tablas de la plantilla:
#      Insesgamiento -> sesgo de la media y de la mediana
#      Consistencia  -> las varianzas caen al crecer n
#      Eficiencia    -> se comparan las dos varianzas
#
#    Se registra ademas 'prop_med_igual_pob': la fraccion de replicas en que
#    la mediana muestral cae exactamente en la mediana poblacional. Sirve para
#    detectar cuando la mediana es practicamente una constante, cosa que pasa
#    en las variables con masa puntual en su mediana.
# ---------------------------------------------------------------------------
set.seed(SEMILLA)
sim <- data.frame()
t0 <- Sys.time()

for (v in names(POB)) {
  x <- POB[[v]]; MU <- mean(x); MED <- median(x); SIG2 <- var(x)
  for (n in NS) {
    ms <- numeric(B); md <- numeric(B)
    for (b in seq_len(B)) {
      s <- sample(x, n, replace = TRUE)
      ms[b] <- mean(s); md[b] <- median(s)
    }
    sim <- rbind(sim, data.frame(
      variable            = v,
      n                   = n,
      B                   = B,
      param_media         = MU,
      param_mediana       = MED,
      sesgo_media         = mean(ms) - MU,
      sesgo_mediana       = mean(md) - MED,
      var_media           = var(ms),
      var_mediana         = var(md),
      var_teorica_media   = SIG2 / n,           # control: debe cuadrar
      eficiencia_rel      = var(ms) / var(md),  # < 1 -> la media es mejor
      # --- Comparacion contra un MISMO objetivo (la media poblacional) ---
      # Sin esto la comparacion enga~na: en las variables con masa puntual la
      # mediana muestral es casi una constante, su varianza tiende a cero y
      # parece "mas eficiente" cuando en realidad esta estimando OTRA cosa.
      sesgo_mediana_vs_mu = mean(md) - MU,
      mse_media_vs_mu     = var(ms) + (mean(ms) - MU)^2,
      mse_mediana_vs_mu   = var(md) + (mean(md) - MU)^2,
      err_MC_sesgo_media  = sd(ms) / sqrt(B),
      err_MC_sesgo_median = sd(md) / sqrt(B),
      prop_med_igual_pob  = mean(md == MED),
      row.names = NULL))
    cat(sprintf("  %s  n=%5d  listo  (%.0f s acumulados)\n",
                v, n, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    flush.console()
  }
}

cat("\n=== EVALUACION DEL ESTIMADOR (B =", B, ", semilla =", SEMILLA, ") ===\n\n")
for (v in names(POB)) {
  cat("---", ETIQ[[v]], "---\n")
  z <- sim[sim$variable == v, ]
  cat("  a) cada estimador frente a SU PROPIO parametro (lo que pide la plantilla)\n")
  print(format(z[, c("n","sesgo_media","sesgo_mediana","var_media",
                     "var_mediana","var_teorica_media","prop_med_igual_pob")],
               digits = 4, scientific = FALSE), row.names = FALSE)
  cat("  b) los dos como estimadores del MISMO objetivo (la media poblacional)\n")
  print(format(z[, c("n","sesgo_media","sesgo_mediana_vs_mu",
                     "mse_media_vs_mu","mse_mediana_vs_mu")],
               digits = 4, scientific = FALSE), row.names = FALSE)
  cat("\n")
}

write.csv2(sim, file.path(RUTA_S, "fase3_simulacion.csv"), row.names = FALSE)

cat("Tiempo total:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
cat("\nArchivos escritos:\n")
cat("  salidas/fase3_estimadores_puntuales.csv\n")
cat("  salidas/fase3_simulacion.csv\n")
cat("\n--------------------------------------------------------------------\n")
cat("COMO LEER ESTO ANTES DE REDACTAR:\n\n")
cat("1. 'prop_med_igual_pob' cerca de 1 significa que la mediana muestral es\n")
cat("   practicamente una CONSTANTE. Pasa en las variables con masa puntual en\n")
cat("   su mediana (V1 53,61 %, V2 39,15 %, V4 81,18 %). Su varianza tiende a\n")
cat("   cero y la tabla (a) la hace ver 'mas eficiente' que la media. ES UN\n")
cat("   ARTEFACTO: una constante no es un buen estimador, es una constante.\n\n")
cat("2. Por eso esta la tabla (b). Comparados como estimadores del MISMO\n")
cat("   objetivo, la mediana carga un sesgo grande y su ECM es peor. Esa es la\n")
cat("   comparacion honesta y la que hay que comentar.\n\n")
cat("3. V3 en escala log es la unica de las cuatro sin masa puntual dominante\n")
cat("   en su mediana, y la unica donde la comparacion clasica media-contra-\n")
cat("   mediana se comporta como dice el libro.\n")

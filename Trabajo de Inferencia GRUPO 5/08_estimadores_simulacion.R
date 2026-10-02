# ============================================================================
# FASE 3 - CALCULO DE LOS ESTIMADORES Y EVALUACION DEL ESTIMADOR
# Primer Trabajo Estadistica II - GEIH 2025
#
# Requisitos: haber corrido 01b_construccion_estilo_docente.R, que deja la
#             base en salidas/base_directorio_mes.rds. R base, sin paquetes.
# Modo de uso: source("08_estimadores_simulacion.R")
# Salidas en salidas/ (ademas de las tablas impresas en consola):
#   fase3_estimadores_puntuales.csv  media o proporcion: estimadores e IC 95 %
#   fase3_estimadores_varianza.csv   sigma^2 de V1, V2 y V3: estimadores e IC 95 %
#   fase3_simulacion.csv             resumen de la simulacion, una fila por
#                                    variable y valor de B
#   fase3_distribuciones.rds         TODAS las replicas de la media y la mediana,
#                                    por variable y valor de B (las leen el 09
#                                    y el 10)
#   fase3_valores_informe.csv        las cifras que cita el informe, ya escritas
#                                    con coma decimal, cada una con su clave
#
# TIEMPO: la simulacion tarda unos 4 minutos. Imprime avance.
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
# 3. SIMULACION CON REEMPLAZO Y n = N. Se toma la base como poblacion y en
#    cada replica se extrae, CON reemplazo, una muestra del MISMO tamano que
#    el conjunto analitico de la variable (N = numero de viviendas con dato).
#    Con reemplazo cada extraccion es i.i.d. de la distribucion poblacional y
#    Var(media) = sigma2_pob/N, con sigma2_pob = var() * (N-1)/N la varianza
#    de la base con divisor N. El trabajo usa var() (divisor N-1) como sigma^2:
#    la diferencia es un factor 1/N (1,4 x 10^-5 con N = 72.649), despreciable
#    frente al error de Monte Carlo, de modo que la simulacion se contrasta
#    contra la teoria y contra el error estandar del intervalo. El reemplazo
#    es obligatorio: sin reemplazo y con n = N cada replica seria la base
#    completa reordenada, y la media y la mediana saldrian identicas en todas.
#
# 4. NUMERO DE REPETICIONES: B = 100, 1.000 y 10.000 EN EL MISMO CICLO, con
#    N fijo, guardando la distribucion completa de cada B. El criterio del
#    docente a partir del IC 95 % dice que error de Monte Carlo admite cada B:
#
#       e = z * raiz(p(1-p)/B) ,  z = 1,96 ,  p = 0,95
#
#       B =    100 -> e = 4,27 pp
#       B =  1.000 -> e = 1,35 pp
#       B = 10.000 -> e = 0,43 pp   <- cumple el umbral de 0,5 pp (B >= 7.299)
#
#    (la seccion 4 calcula estas cifras y las exporta). Por eso el
#    insesgamiento y la eficiencia se leen con B = 10.000, y la consistencia
#    compara los tres valores de B: al crecer B, con N fijo, la simulacion
#    aproxima cada vez mejor la distribucion de la media.
#
# 5. SIN SEMILLA. El bloque de simulacion no fija semilla: cada ejecucion
#    produce replicas distintas, y la diferencia entre ejecuciones es parte de
#    lo que se observa (con B = 100 las cifras cambian mas que con B = 10.000;
#    el script 10 lo muestra). Las cifras del informe son las de la ejecucion
#    guardada en salidas/fase3_distribuciones.rds, que el 09 dibuja sin volver
#    a simular: por eso tablas y graficos coinciden sin semilla.
#    Para que cada sesion de R arranque con un estado distinto del generador,
#    abrir R sin restaurar un espacio de trabajo guardado (.RData).
#
# 6. VARIANZA sigma^2 (V1, V2 y V3), seccion 1b. Analogia = var() (divisor
#    n-1); maxima verosimilitud = divisor n (modelo normal, formal en V1 y
#    V2; lognormal en V3). Intervalo de la tabla: el clasico chi-cuadrado con
#    n-1 grados de libertad. Es exacto solo con datos normales: supone
#    curtosis 3 y el TLC no lo corrige al crecer n. Por eso se calcula
#    tambien el intervalo asintotico robusto a la curtosis k,
#    S^2 +- z * raiz((m4 - S^4)/n), y el NIVEL REAL aproximado del
#    chi-cuadrado, 2 * pnorm(z / raiz((k-1)/2)) - 1, que se declara en el
#    comentario. V3 en escala log: a la varianza NO se le aplica exp().
#    V4 no lleva este intervalo: en una Bernoulli sigma^2 = p(1-p) queda
#    determinada por p.
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
ARCH_BASE   <- file.path(RUTA_SALIDA, "base_directorio_mes.rds")
if (!file.exists(ARCH_BASE))
  stop("No encuentro ", ARCH_BASE, " desde el directorio de trabajo actual (",
       getwd(), "). Corre antes 01b_construccion_estilo_docente.R, que crea esa base.")

base_directorio_mes <- readRDS(ARCH_BASE)
d <- base_directorio_mes

RUTA_S <- RUTA_SALIDA

Z  <- qnorm(0.975)          # 1,959964
BS <- c(100, 1000, 10000)   # valores de B que recorre el ciclo, ver punto 4
B_EVAL <- max(BS)           # B con que se leen insesgamiento y eficiencia

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
#           Pero sigma^2 SI difiere: n-1 frente a n (ver seccion 1b).
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
# 1b. ESTIMADORES E INTERVALO DE CONFIANZA PARA LA VARIANZA sigma^2
#     (V1, V2 y V3). Es la tabla de la plantilla repetida para el estimador
#     de la varianza: Analogia | Maxima Verosimilitud | LI | LS.
#
#     Convencion del trabajo (la misma que ya se usaba para V3):
#       Analogia  -> varianza muestral S^2, divisor n-1 (var() de R).
#       Max. ver. -> divisor n. Bajo el modelo normal para V1 y V2 (formal,
#                    igual que en la media) y bajo el lognormal para V3
#                    (normal sobre el logaritmo). Difieren en el factor
#                    (n-1)/n: dif_metodos = S^2/n.
#
#     Intervalo de la tabla: el clasico para la varianza (chi-cuadrado),
#       [ (n-1) S^2 / qchisq(0,975; n-1) ; (n-1) S^2 / qchisq(0,025; n-1) ]
#     Es exacto SOLO si la variable es normal. La formula supone
#     Var(S^2) = 2 sigma^4 / (n-1), que es el caso curtosis = 3. En general
#     Var(S^2) ~ sigma^4 (k - 1) / n, con k la curtosis.
#
#     Intervalo complementario (va en el comentario), asintotico y robusto a
#     la curtosis:
#       S^2 +- z * raiz( (m4 - S^4) / n ) ,  m4 = cuarto momento central
#     (m4 - S^4)/n = S^4 (k - 1)/n con la MISMA curtosis de la tabla de
#     descriptivos (curt() del script 03), asi que se verifica a mano.
#     La razon de anchos robusto / chi-cuadrado es ~ raiz((k - 1)/2):
#       k < 3 -> el chi-cuadrado sale mas ANCHO de lo necesario y su nivel
#                real supera el 95 % (conservador).
#       k > 3 -> el chi-cuadrado sale mas ANGOSTO y su nivel real queda por
#                debajo del 95 %; ahi la inferencia se apoya en el robusto.
#     Nivel real aproximado del chi-cuadrado: 2 * pnorm(z / raiz((k-1)/2)) - 1.
#
#     ESCALA DE V3: logaritmo, igual que mu. sigma^2 = Var[log(arriendo)].
#     NO se le aplica exp() a los limites: exp() de una varianza no es una
#     cantidad interpretable. Para volver a pesos se usa exp(sigma), la
#     desviacion estandar geometrica (factor multiplicativo de dispersion),
#     con los limites del intervalo robusto.
#
#     No hay nada aleatorio en este bloque.
# ---------------------------------------------------------------------------
est_var <- data.frame()
for (v in c("V1", "V2", "V3")) {
  x  <- POB[[v]]; n <- length(x)
  s2    <- var(x)                         # analogia, divisor n-1
  s2_mv <- s2 * (n - 1) / n               # maxima verosimilitud, divisor n
  # Intervalo clasico: chi-cuadrado con n-1 grados de libertad
  q975   <- qchisq(0.975, df = n - 1)
  q025   <- qchisq(0.025, df = n - 1)
  LI_chi <- (n - 1) * s2 / q975
  LS_chi <- (n - 1) * s2 / q025
  # Complemento: asintotico con el cuarto momento
  m4    <- mean((x - mean(x))^4)          # divisor n, como en curt() del 03
  k     <- m4 / s2^2                      # curtosis (3 = normal)
  ee_s2 <- sqrt((m4 - s2^2) / n)          # error estandar de S^2
  est_var <- rbind(est_var, data.frame(
    variable        = v,
    etiqueta        = ETIQ[[v]],
    n               = n,
    analogia        = s2,
    max_verosi      = s2_mv,
    dif_metodos     = s2 - s2_mv,
    LI_chi2         = LI_chi,
    LS_chi2         = LS_chi,
    chi2_q975       = q975,
    chi2_q025       = q025,
    curtosis        = k,
    ee_robusto      = ee_s2,
    LI_robusto      = s2 - Z * ee_s2,
    LS_robusto      = s2 + Z * ee_s2,
    razon_anchos    = (2 * Z * ee_s2) / (LS_chi - LI_chi),   # robusto / chi2
    nivel_real_chi2 = 2 * pnorm(Z / sqrt((k - 1) / 2)) - 1,
    row.names       = NULL))
}
cat("\n=== CALCULO DE LOS ESTIMADORES DE sigma^2 (IC 95 % chi-cuadrado) ===\n")
print(format(est_var[, c("variable","n","analogia","max_verosi",
                         "LI_chi2","LS_chi2")], digits = 8),
      row.names = FALSE)
cat("\n--- Complemento: intervalo robusto a la curtosis y nivel real del chi-cuadrado ---\n")
print(format(est_var[, c("variable","curtosis","ee_robusto","LI_robusto",
                         "LS_robusto","razon_anchos","nivel_real_chi2")],
             digits = 6), row.names = FALSE)

# Analogia frente a MV en las tres cuantitativas: la diferencia (n-1)/n
# aparece en las tres, no solo en V3.
cat("\n")
for (v in c("V1", "V2", "V3")) {
  e <- est_var[est_var$variable == v, ]
  cat(sprintf("%s  S^2 (n-1) = %.6f   MV (n) = %.6f   diferencia = %.8f\n",
              v, e$analogia, e$max_verosi, e$dif_metodos))
}

# V3: lectura en pesos SIN aplicar exp() a la varianza. exp(sigma) es la
# desviacion estandar geometrica; su intervalo sale de la raiz de los limites.
ev3 <- est_var[est_var$variable == "V3", ]
cat(sprintf("\nV3  sigma (log) = %.6f   IC 95 %% chi2 = [%.6f ; %.6f]   robusto = [%.6f ; %.6f]\n",
            sqrt(ev3$analogia), sqrt(ev3$LI_chi2), sqrt(ev3$LS_chi2),
            sqrt(ev3$LI_robusto), sqrt(ev3$LS_robusto)))
cat(sprintf("V3  desv. est. geometrica exp(sigma) = %.4f   robusto = [%.4f ; %.4f]\n",
            exp(sqrt(ev3$analogia)), exp(sqrt(ev3$LI_robusto)),
            exp(sqrt(ev3$LS_robusto))))

write.csv2(est_var, file.path(RUTA_S, "fase3_estimadores_varianza.csv"),
           row.names = FALSE)


# ---------------------------------------------------------------------------
# 2. EVALUACION DEL ESTIMADOR - SIMULACION
#    Un solo experimento llena las tres tablas de la plantilla. En cada
#    replica: muestra CON reemplazo de tamano N (el conjunto analitico
#    completo), media y mediana de la MISMA muestra. B toma los valores de BS
#    dentro del mismo ciclo y se guarda la distribucion completa de cada B.
#      Insesgamiento -> promedio de las replicas menos el parametro (B = 10.000)
#      Consistencia  -> al crecer B, con N fijo, el promedio de las replicas se
#                       acerca al parametro (su error de Monte Carlo baja como
#                       1/raiz(B)) y su varianza se estabiliza en sigma^2/N
#      Eficiencia    -> varianza y ECM de media y mediana (B = 10.000)
#
#    Se registra ademas 'prop_med_igual_pob': la fraccion de replicas en que
#    la mediana muestral cae exactamente en la mediana poblacional. Sirve para
#    detectar cuando la mediana es practicamente una constante, cosa que pasa
#    en las variables con masa puntual en su mediana.
#
#    Sin semilla: ver punto 5 del encabezado.
# ---------------------------------------------------------------------------
DIST  <- list()       # DIST$V1$B_100$media, DIST$V1$B_100$mediana, ...
PARAM <- data.frame()
sim   <- data.frame()
t0 <- Sys.time()

for (v in names(POB)) {
  x    <- POB[[v]]
  N    <- length(x)                  # n = N: el conjunto analitico completo
  MU   <- mean(x)
  MED  <- median(x)
  SIG2 <- var(x)                     # divisor N-1, el de la seccion 1
  # Error estandar teorico de la media con n = N: el MISMO del intervalo de
  # la seccion 1 (sigma/raiz(N) en V1-V3; raiz(p(1-p)/N) en V4).
  EE   <- est$error_est[est$variable == v]

  # Mediana muestral distinta de la poblacional: probabilidad teorica
  # (binomial) con muestras de tamano N. La mediana muestral queda por debajo
  # de MED si al menos k1 datos caen bajo MED, y por encima si a lo sumo
  # k2 - 1 datos caen en MED o por debajo. Con N impar (las cuatro variables)
  # es exacta; con N par seria una cota superior. Se calcula en logaritmos
  # porque puede ser menor que el numero mas chico que R representa.
  pa <- mean(x < MED); pb <- mean(x <= MED)
  k1 <- (N + 1) %/% 2; k2 <- N + 1 - k1
  lp_baja <- pbinom(k1 - 1, N, pa, lower.tail = FALSE, log.p = TRUE)
  lp_sube <- pbinom(k2 - 1, N, pb, log.p = TRUE)
  lp_max  <- max(lp_baja, lp_sube)
  log10_p <- if (lp_max == -Inf) -Inf else
    (lp_max + log(exp(lp_baja - lp_max) + exp(lp_sube - lp_max))) / log(10)

  PARAM <- rbind(PARAM, data.frame(
    variable             = v,
    N                    = N,
    media                = MU,
    mediana              = MED,
    sigma2               = SIG2,
    ee_teorico           = EE,
    var_teorica_media    = EE^2,
    prop_bajo_mediana    = pa,        # P(X <  mediana)
    prop_hasta_mediana   = pb,        # P(X <= mediana)
    p_teor_med_distinta  = 10^log10_p,
    log10_p_med_distinta = log10_p,   # por si la anterior da 0 por redondeo
    row.names = NULL))
  DIST[[v]] <- list()

  for (B in BS) {                    # EL CICLO SOBRE B, con N fijo
    ms <- numeric(B); md <- numeric(B)
    for (b in seq_len(B)) {
      s <- sample(x, N, replace = TRUE)
      ms[b] <- mean(s); md[b] <- median(s)
    }
    DIST[[v]][[paste0("B_", B)]] <- list(media = ms, mediana = md)   # se almacena

    mse_ms <- var(ms) + (mean(ms) - MU)^2
    mse_md <- var(md) + (mean(md) - MU)^2
    sim <- rbind(sim, data.frame(
      variable            = v,
      N                   = N,
      B                   = B,
      param_media         = MU,
      param_mediana       = MED,
      prom_media          = mean(ms),           # promedio de las B replicas
      sesgo_media         = mean(ms) - MU,
      err_MC_media        = sd(ms) / sqrt(B),   # cuanto puede moverse prom_media
                                                # de una ejecucion a otra
      de_media            = sd(ms),
      ee_teorico          = EE,                 # error estandar del IC
      var_media           = var(ms),
      var_teorica_media   = EE^2,               # control: debe cuadrar
      var_x_N             = var(ms) * N,        # debe acercarse a sigma^2
      prom_mediana        = mean(md),
      sesgo_mediana       = mean(md) - MED,
      var_mediana         = var(md),
      prop_med_igual_pob  = mean(md == MED),
      # --- Comparacion contra un MISMO objetivo (la media poblacional) ---
      # Sin esto la comparacion enga~na: en las variables con masa puntual la
      # mediana muestral es casi una constante, su varianza es casi cero y
      # parece "mas eficiente" cuando en realidad esta estimando OTRA cosa.
      sesgo_mediana_vs_mu = mean(md) - MU,
      mse_media_vs_mu     = mse_ms,
      mse_mediana_vs_mu   = mse_md,
      razon_var           = if (var(ms) > 0) var(md) / var(ms) else NA,
      razon_mse           = if (mse_ms > 0) mse_md / mse_ms else NA,
      # Replicas en que la mediana muestral NO fue la poblacional. Cuando son
      # pocas (V3), var_mediana depende solo de ellas: ver punto 4 de la
      # seccion 5.
      n_med_distinta      = sum(md != MED),
      row.names = NULL))
    cat(sprintf("  %s  N=%5d  B=%5d  listo  (%.0f s acumulados)\n",
                v, N, B, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    flush.console()
  }
}
t_sim <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

# Se guardan las distribuciones: de aqui leen el 09, el 10 y cualquier
# revision. Asi los graficos muestran exactamente las replicas de las tablas.
FECHA_SIM <- Sys.time()
saveRDS(list(distribuciones = DIST, parametros = PARAM, resumen = sim,
             BS = BS, fecha = FECHA_SIM, version_R = R.version.string),
        file.path(RUTA_S, "fase3_distribuciones.rds"))
write.csv2(sim, file.path(RUTA_S, "fase3_simulacion.csv"), row.names = FALSE)


# ---------------------------------------------------------------------------
# 3. TABLAS PARA EL INFORME
# ---------------------------------------------------------------------------
B_txt <- format(B_EVAL, big.mark = ".", decimal.mark = ",")
cat(sprintf("\n=== EVALUACION DEL ESTIMADOR (n = N; B = %s; sin semilla) ===\n\n",
            paste(format(BS, big.mark = ".", decimal.mark = ",", trim = TRUE), collapse = " / ")))
for (v in names(POB)) {
  pv <- PARAM[PARAM$variable == v, ]
  cat("---", ETIQ[[v]], "  N =", pv$N, "---\n")
  z <- sim[sim$variable == v, ]
  cat("  a) CONSISTENCIA: la aproximacion al crecer B, con N fijo\n")
  print(format(z[, c("B","prom_media","sesgo_media","err_MC_media",
                     "de_media","ee_teorico","var_x_N")],
               digits = 6, scientific = FALSE), row.names = FALSE)
  cat(sprintf("     mu = %.6f   sigma^2 = var() de la base = %.6f  (var_x_N debe acercarse a este valor;\n",
              pv$media, pv$sigma2))
  cat("     con reemplazo su valor esperado exacto es sigma^2 (N-1)/N, que difiere en 1/N)\n")
  z10 <- z[z$B == B_EVAL, ]
  cat(sprintf("  b) INSESGAMIENTO Y EFICIENCIA, cada estimador frente a SU PROPIO parametro, B = %s\n",
              B_txt))
  print(format(z10[, c("prom_media","sesgo_media","prom_mediana","sesgo_mediana",
                       "var_media","var_mediana","prop_med_igual_pob","razon_var")],
               digits = 6, scientific = FALSE), row.names = FALSE)
  cat(sprintf("  c) los dos como estimadores del MISMO objetivo (la media poblacional), B = %s\n",
              B_txt))
  print(format(z10[, c("sesgo_media","sesgo_mediana_vs_mu",
                       "mse_media_vs_mu","mse_mediana_vs_mu","razon_mse")],
               digits = 6, scientific = FALSE), row.names = FALSE)
  cat(sprintf("     P(X < mediana) = %.4f   P(X <= mediana) = %.4f\n",
              pv$prop_bajo_mediana, pv$prop_hasta_mediana))
  cat(sprintf("     prob. teorica de que la mediana muestral NO sea la poblacional: 10^(%.1f)\n\n",
              pv$log10_p_med_distinta))
}


# ---------------------------------------------------------------------------
# 4. VALORES PARA EL INFORME: salidas/fase3_valores_informe.csv
#    Cada cifra del informe que depende de esta ejecucion sale de aqui, con
#    una clave fija. Columnas: clave ; valor ; valor_num ; descripcion.
#      valor     -> el texto listo para pegar en el informe
#      valor_num -> el numero sin redondear (las proporciones van como
#                   proporcion, no como porcentaje)
#    Archivo UTF-8, separador ";" y coma decimal (como write.csv2).
#
#    Claves:
#      <V>_<col>        parametros de cada variable (V1 a V4)
#      <V>_B<B>_<col>   una por variable, valor de B y columna de la simulacion
#      <V>_s2_<col>     estimadores e intervalos de sigma^2 (V1 a V3)
#      B<B>_<col>       criterio del docente para el numero de replicas
# ---------------------------------------------------------------------------

# --- Formato de las cifras (funcion fmt_informe) ---------------------------
#   tipo "num"   : |x| >= 0,001      -> 6 decimales, coma decimal y punto de
#                                       miles (0,332529 ; 73.396,994720)
#                  0 < |x| < 0,001   -> 3 cifras significativas como potencia
#                                       de 10: "2,27 x 10^-6", escrito con el
#                                       signo de multiplicar y superindices
#                  x = 0 exacto      -> "0"
#   tipo "pct"   : x es una proporcion: 100*x con 2 decimales y " %"
#   tipo "razon" : 1 decimal y punto de miles (48.676,3)
#   tipo "entero": sin decimales y con punto de miles (72.649)
#   Los negativos llevan el signo menos tipografico, como el informe.
#   Un valor no definido (division por 0) se escribe "no definida".
#
#   Los simbolos se escriben byte a byte en UTF-8 para que el archivo salga
#   bien en cualquier configuracion regional de R (Windows, Mac o Linux).
utf8  <- function(...) rawToChar(as.raw(c(...)))
POR   <- utf8(0xc3, 0x97)            # signo de multiplicar
MENOS <- utf8(0xe2, 0x88, 0x92)      # signo menos tipografico
SUP   <- c("0" = utf8(0xe2, 0x81, 0xb0), "1" = utf8(0xc2, 0xb9),
           "2" = utf8(0xc2, 0xb2),       "3" = utf8(0xc2, 0xb3),
           "4" = utf8(0xe2, 0x81, 0xb4), "5" = utf8(0xe2, 0x81, 0xb5),
           "6" = utf8(0xe2, 0x81, 0xb6), "7" = utf8(0xe2, 0x81, 0xb7),
           "8" = utf8(0xe2, 0x81, 0xb8), "9" = utf8(0xe2, 0x81, 0xb9),
           "-" = utf8(0xe2, 0x81, 0xbb))     # superindices 0-9 y menos

coma <- function(z, dig) formatC(z, format = "f", digits = dig,
                                 big.mark = ".", decimal.mark = ",")

# Potencia de 10 a partir de log10(|x|): sirve tambien cuando |x| es tan
# chico que R lo redondea a 0 (la probabilidad de la mediana en V4).
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
    num    = if (a == 0) "0" else if (signif(a, 3) >= 0.001) coma(a, 6) else
               pot10(log10(signif(a, 3))))
  if (grepl("^0[,0]*( %)?$", txt)) signo <- ""   # sin "-0,0"
  paste0(signo, txt)
}

# Una fila del archivo de valores.
fila_val <- function(clave, x, tipo, descripcion, valor = fmt_informe(x, tipo)) {
  data.frame(clave = clave, valor = valor,
             valor_num = if (is.finite(x)) x else NA,
             descripcion = descripcion, stringsAsFactors = FALSE)
}

VAL <- data.frame()

# --- Parametros de cada variable -------------------------------------------
for (v in names(POB)) {
  p <- PARAM[PARAM$variable == v, ]
  VAL <- rbind(VAL,
    fila_val(paste0(v, "_N"), p$N, "entero",
             "N: viviendas del conjunto analitico (= n de cada replica)"),
    fila_val(paste0(v, "_media"), p$media, "num",
             "mu: media poblacional (V3: del logaritmo; V4: proporcion p)"),
    fila_val(paste0(v, "_mediana"), p$mediana, "num", "mediana poblacional"),
    fila_val(paste0(v, "_sigma2"), p$sigma2, "num",
             "sigma^2 de la base, var() con divisor N-1"),
    fila_val(paste0(v, "_sigma"), sqrt(p$sigma2), "num",
             "sigma = raiz(sigma2)"),
    fila_val(paste0(v, "_ee_teorico"), p$ee_teorico, "num",
             "error estandar teorico de la media con n = N (el del IC 95 %)"),
    fila_val(paste0(v, "_var_teorica_media"), p$var_teorica_media, "num",
             "varianza teorica de la media con n = N (ee_teorico^2)"),
    fila_val(paste0(v, "_prop_bajo_mediana"), p$prop_bajo_mediana, "pct",
             "P(X < mediana) en la base"),
    fila_val(paste0(v, "_prop_hasta_mediana"), p$prop_hasta_mediana, "pct",
             "P(X <= mediana) en la base"),
    fila_val(paste0(v, "_prop_sobre_mediana"), 1 - p$prop_hasta_mediana, "pct",
             "P(X > mediana) en la base"),
    fila_val(paste0(v, "_masa_mediana"),
             p$prop_hasta_mediana - p$prop_bajo_mediana, "pct",
             "masa puntual de la base en su mediana, P(X = mediana)"),
    fila_val(paste0(v, "_p_teor_med_distinta"), p$p_teor_med_distinta, "num",
             paste("probabilidad teorica (binomial) de que la mediana muestral",
                   "con n = N no sea la poblacional; si es menor que el minimo",
                   "que R representa, se escribe a partir de su logaritmo"),
             valor = if (p$log10_p_med_distinta == -Inf) "0" else
                       if (p$p_teor_med_distinta > 0)
                         fmt_informe(p$p_teor_med_distinta, "num") else
                           pot10(p$log10_p_med_distinta)))
}

# --- Simulacion: una clave por variable, B y columna -----------------------
COLS_SIM <- c(
  prom_media          = "num|promedio de las replicas de la media",
  sesgo_media         = "num|sesgo de la media: prom_media - mu",
  err_MC_media        = "num|error de Monte Carlo de prom_media, de_media/raiz(B)",
  de_media            = "num|desviacion estandar de las replicas de la media",
  var_media           = "num|varianza de las replicas de la media",
  var_x_N             = "num|var_media * N (debe acercarse a sigma^2)",
  prom_mediana        = "num|promedio de las replicas de la mediana",
  sesgo_mediana       = "num|sesgo de la mediana: prom_mediana - mediana poblacional",
  var_mediana         = "num|varianza de las replicas de la mediana",
  prop_med_igual_pob  = "pct|replicas en que la mediana muestral = mediana poblacional",
  sesgo_mediana_vs_mu = "num|sesgo de la mediana frente a mu: prom_mediana - mu",
  mse_media_vs_mu     = "num|ECM de la media frente a mu",
  mse_mediana_vs_mu   = "num|ECM de la mediana frente a mu",
  razon_var           = "razon|var_mediana / var_media",
  razon_mse           = "razon|mse_mediana_vs_mu / mse_media_vs_mu",
  n_med_distinta      = "entero|replicas en que la mediana muestral NO fue la mediana poblacional")
for (i in seq_len(nrow(sim))) {
  z <- sim[i, ]
  pref <- paste0(z$variable, "_B", z$B, "_")
  txtB <- paste0(" (", z$variable, ", n = N, B = ",
                 formatC(z$B, format = "d", big.mark = ".", decimal.mark = ","), ")")
  for (cc in names(COLS_SIM)) {
    partes <- strsplit(COLS_SIM[[cc]], "|", fixed = TRUE)[[1]]
    VAL <- rbind(VAL, fila_val(paste0(pref, cc), z[[cc]], partes[1],
                               paste0(partes[2], txtB)))
  }
  VAL <- rbind(VAL,
    fila_val(paste0(pref, "sesgo_en_err_MC"),
             z$sesgo_media / z$err_MC_media, "razon",
             paste0("sesgo_media medido en errores de Monte Carlo (para decidir",
                    " un umbral, usar valor_num, sin redondeo)", txtB)),
    fila_val(paste0(pref, "n_med_distinta_esperado"),
             z$B * PARAM$p_teor_med_distinta[PARAM$variable == z$variable], "razon",
             paste0("numero esperado de replicas con la mediana distinta de la",
                    " poblacional: B * p_teor_med_distinta", txtB)))
}

# --- sigma^2 de V1, V2 y V3 -------------------------------------------------
COLS_S2 <- c(
  analogia        = "num|S^2, varianza muestral con divisor n-1 (analogia)",
  max_verosi      = "num|estimador de maxima verosimilitud de sigma^2, divisor n",
  dif_metodos     = "num|analogia - max_verosi (= S^2/n)",
  LI_chi2         = "num|limite inferior del IC 95 % chi-cuadrado (tabla)",
  LS_chi2         = "num|limite superior del IC 95 % chi-cuadrado (tabla)",
  chi2_q975       = "num|cuantil 0,975 de la chi-cuadrado con n-1 gl",
  chi2_q025       = "num|cuantil 0,025 de la chi-cuadrado con n-1 gl",
  curtosis        = "num|curtosis k = m4/S^4 (3 = normal), la del script 03",
  ee_robusto      = "num|error estandar de S^2 robusto, raiz((m4 - S^4)/n)",
  LI_robusto      = "num|limite inferior del IC 95 % robusto a la curtosis",
  LS_robusto      = "num|limite superior del IC 95 % robusto a la curtosis",
  razon_anchos    = "razon|ancho del IC robusto / ancho del IC chi-cuadrado",
  nivel_real_chi2 = "pct|nivel real aproximado del IC chi-cuadrado, 2*pnorm(z/raiz((k-1)/2))-1")
for (i in seq_len(nrow(est_var))) {
  z <- est_var[i, ]
  pref <- paste0(z$variable, "_s2_")
  for (cc in names(COLS_S2)) {
    partes <- strsplit(COLS_S2[[cc]], "|", fixed = TRUE)[[1]]
    VAL <- rbind(VAL, fila_val(paste0(pref, cc), z[[cc]], partes[1],
                               paste0(partes[2], " (", z$variable, ")")))
  }
  VAL <- rbind(VAL,
    fila_val(paste0(pref, "sigma_LI_robusto"), sqrt(z$LI_robusto), "num",
             paste0("raiz del limite inferior robusto: sigma minima (", z$variable, ")")),
    fila_val(paste0(pref, "sigma_LS_robusto"), sqrt(z$LS_robusto), "num",
             paste0("raiz del limite superior robusto: sigma maxima (", z$variable, ")")))
}
VAL <- rbind(VAL,
  fila_val("V3_s2_sigma", sqrt(ev3$analogia), "num",
           "sigma del logaritmo del arriendo, raiz(S^2)"),
  fila_val("V3_s2_exp_sigma", exp(sqrt(ev3$analogia)), "num",
           "desviacion estandar geometrica exp(sigma) (factor multiplicativo)"),
  fila_val("V3_s2_exp_sigma_LI", exp(sqrt(ev3$LI_robusto)), "num",
           "exp(sigma) en el limite inferior del IC robusto"),
  fila_val("V3_s2_exp_sigma_LS", exp(sqrt(ev3$LS_robusto)), "num",
           "exp(sigma) en el limite superior del IC robusto"))

# --- Criterio del docente para B (punto 4 del encabezado) -------------------
for (B in BS) {
  VAL <- rbind(VAL,
    fila_val(paste0("B", B, "_criterio_e"), Z * sqrt(0.95 * 0.05 / B), "pct",
             paste0("criterio del docente: semiancho del IC 95 % de una proporcion",
                    " de 0,95 con B = ", B, " (en puntos porcentuales)")),
    fila_val(paste0("B", B, "_err_rel_de"), 1 / sqrt(2 * (B - 1)), "pct",
             paste0("error relativo tipico de la desviacion de las replicas con B = ", B,
                    ", 1/raiz(2(B-1))")),
    fila_val(paste0("B", B, "_err_rel_var"), sqrt(2 / (B - 1)), "pct",
             paste0("error relativo tipico de var_media y var_x_N con B = ", B,
                    ", raiz(2/(B-1))")))
}
VAL <- rbind(VAL,
  fila_val("criterio_B_min", ceiling(Z^2 * 0.95 * 0.05 / 0.005^2), "entero",
           "B minimo para un error de 0,5 pp con el criterio del docente"))

# --- Fecha de esta ejecucion ------------------------------------------------
# La misma que queda en salidas/fase3_distribuciones.rds ($fecha). El 10
# escribe la del .rds que leyo con la clave ej_fecha_08: si las dos no
# coinciden, el 08 se volvio a correr despues del 10 y los dos archivos de
# valores ya no son de la misma ejecucion.
FECHA_TXT <- format(FECHA_SIM, "%Y-%m-%d %H:%M:%OS3")
VAL <- rbind(VAL,
  fila_val("fecha_ejecucion_08", NA, "num",
           "fecha y hora de esta ejecucion del 08 (la de salidas/fase3_distribuciones.rds)",
           valor = FECHA_TXT))

# --- Escritura en UTF-8 -----------------------------------------------------
# write.csv2 traduciria los simbolos a la codificacion de la sesion; por eso
# se escriben las lineas tal cual, byte a byte.
comillas <- function(z) paste0('"', gsub('"', '""', z, fixed = TRUE), '"')
num_csv  <- ifelse(is.na(VAL$valor_num), "NA",
                   sub(".", ",", sprintf("%.15g", VAL$valor_num), fixed = TRUE))
lineas <- c('"clave";"valor";"valor_num";"descripcion"',
            paste(comillas(VAL$clave), comillas(VAL$valor), num_csv,
                  comillas(VAL$descripcion), sep = ";"))
con <- file(file.path(RUTA_S, "fase3_valores_informe.csv"), open = "wb")
writeLines(lineas, con, useBytes = TRUE)
close(con)
cat("Valores para el informe:", nrow(VAL), "claves en salidas/fase3_valores_informe.csv\n")


# ---------------------------------------------------------------------------
# 5. RESUMEN Y COMO LEER LOS RESULTADOS
#    Todas las cifras de esta seccion se calculan: ninguna esta escrita a mano.
# ---------------------------------------------------------------------------
cat("\nTiempo de la simulacion:", round(t_sim, 1), "min\n")
cat("\nArchivos escritos:\n")
cat("  salidas/fase3_estimadores_puntuales.csv\n")
cat("  salidas/fase3_estimadores_varianza.csv\n")
cat("  salidas/fase3_simulacion.csv\n")
cat("  salidas/fase3_distribuciones.rds  (todas las replicas, para el 09 y el 10)\n")
cat("  salidas/fase3_valores_informe.csv\n")

pct_txt <- function(z) paste(coma(100 * z, 2), "%")
z10_todas <- sim[sim$B == B_EVAL, ]
masa_med  <- PARAM$prop_hasta_mediana - PARAM$prop_bajo_mediana

cat("\n--------------------------------------------------------------------\n")
cat("COMO LEER ESTO ANTES DE REDACTAR:\n\n")
cat("1. Consistencia (tabla a): N esta fijo y lo que cambia es B. La diferencia\n")
cat("   entre el promedio de las replicas y mu es del orden de err_MC_media, que\n")
cat("   baja como 1/raiz(B). En una ejecucion concreta esa diferencia no tiene\n")
cat("   por que bajar en cada paso: lo que baja de forma sistematica es el error\n")
cat("   de Monte Carlo. de_media NO baja al crecer B: se estabiliza en\n")
cat("   ee_teorico, el error estandar del intervalo de confianza.\n")
cat("   En esta ejecucion, sesgo_media medido en errores de Monte Carlo:\n")
tab_z <- sapply(BS, function(B) sapply(names(POB), function(v) {
  z <- sim[sim$variable == v & sim$B == B, ]
  coma(z$sesgo_media / z$err_MC_media, 2) }))
colnames(tab_z) <- paste0("B=", format(BS, big.mark = ".", decimal.mark = ",", trim = TRUE))
print(noquote(tab_z), right = TRUE)

cat("\n2. 'prop_med_igual_pob' cerca de 1 significa que la mediana muestral es\n")
cat("   practicamente una CONSTANTE. Pasa en las variables con masa puntual en\n")
cat("   su mediana. Masa de cada base en su propia mediana:\n")
cat("     ", paste(sprintf("%s %s", PARAM$variable, pct_txt(masa_med)), collapse = " - "), "\n")
cat(sprintf("   Replicas (B = %s) en que la mediana muestral fue igual a la poblacional:\n",
            B_txt))
cat("     ", paste(sprintf("%s %s", z10_todas$variable,
                         pct_txt(z10_todas$prop_med_igual_pob)), collapse = " - "), "\n")
cat("   Su varianza es casi cero y la tabla (b) la hace ver 'mas eficiente' que\n")
cat("   la media. ES UN ARTEFACTO: una constante no es un buen estimador, es una\n")
cat("   constante.\n\n")
cat("3. Por eso esta la tabla (c). Comparados como estimadores del MISMO\n")
cat("   objetivo, la mediana carga un sesgo que no depende de B. Razon de ECM\n")
cat(sprintf("   (mediana / media) frente a mu, con B = %s:\n", B_txt))
cat("     ", paste(sprintf("%s %s", z10_todas$variable, coma(z10_todas$razon_mse, 1)),
                   collapse = " - "), "\n")
cat("   Esa es la comparacion honesta y la que hay que comentar.\n\n")
z3 <- z10_todas[z10_todas$variable == "V3", ]
p3 <- PARAM[PARAM$variable == "V3", ]
cat(sprintf("4. V3 (escala log), B = %s: la mediana muestral fue distinta de la\n", B_txt))
cat(sprintf("   poblacional en %d replicas (se esperaban %s = B * p_teor). Su varianza\n",
            z3$n_med_distinta, coma(B_EVAL * p3$p_teor_med_distinta, 1)))
cat(sprintf("   (%s) la producen SOLO esas replicas, de modo que la razon\n",
            format(signif(z3$var_mediana, 3), scientific = TRUE, decimal.mark = ",")))
cat(sprintf("   var_mediana / var_media = %s cambia mucho de una ejecucion a otra: con\n",
            coma(z3$razon_var, 2)))
cat("   k replicas su error relativo es del orden de 1/raiz(k), y puede quedar a\n")
cat("   cualquier lado de 1 (el 10 lo muestra con tres ejecuciones). No permite\n")
if (z3$n_med_distinta > 0)
  cat(sprintf("   ordenar los estimadores: aqui, 1/raiz(k) = %s.\n",
              coma(1 / sqrt(z3$n_med_distinta), 2))) else
  cat("   ordenar los estimadores.\n")
cat("   Tampoco es la razon pi/2 de la teoria clasica, que supone densidad continua\n")
cat("   en la mediana. La comparacion estable es el ECM frente a mu (punto 3),\n")
cat("   dominado por el sesgo de la mediana, que no depende de la ejecucion.\n")

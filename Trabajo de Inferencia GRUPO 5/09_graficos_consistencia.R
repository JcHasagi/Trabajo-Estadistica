# ============================================================================
# FASE 3 - GRAFICOS DE CONSISTENCIA
# Primer Trabajo Estadistica II - GEIH 2025
#
# Requisitos: ninguno. R base, sin paquetes.
# Modo de uso: source("09_graficos_consistencia.R")
# Salida: 4 PNG en salidas/graficos/ (numerados 17 a 20)
#
# TIEMPO: entre 2 y 5 minutos. Imprime avance.
#
# QUE HACE Y POR QUE
# La plantilla del docente trae, en la seccion de Consistencia de cada
# variable, una figura de ejemplo con cuatro histogramas de la distribucion
# muestral de la media para tamanos de muestra crecientes. Este script produce
# esa misma figura con NUESTROS datos.
#
# REPRODUCE EXACTAMENTE la simulacion del script 08: misma semilla (2026),
# mismo B (10.000), mismo orden de recorrido (V1, V2, V3, V4 y dentro de cada
# una n = 100, 500, 1.000, 5.000) y las mismas llamadas a sample(). Como
# mean() y median() no consumen numeros aleatorios, la secuencia del generador
# es identica y las replicas son las mismas que produjeron las cifras de las
# tablas. Si se cambia B, NS o el orden, deja de coincidir.
#
# DECISION DE FORMA: los cuatro paneles comparten el MISMO eje horizontal.
# Es lo que hace visible la consistencia: la distribucion se estrecha alrededor
# del parametro a medida que n crece. Con ejes independientes las cuatro se
# verian igual de anchas y el grafico no demostraria nada.
# ============================================================================


# ---------------------------------------------------------------------------
# 0. PREPARACION - identica a la del script 08
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

RUTA_G <- file.path(RUTA_SALIDA, "graficos")
dir.create(RUTA_G, showWarnings = FALSE, recursive = TRUE)

B  <- 10000
NS <- c(100, 500, 1000, 5000)
SEMILLA <- 2026

POB <- list(
  V1 = d$prop_educ_superior_18mas[!is.na(d$prop_educ_superior_18mas)],
  V2 = d$prop_mujeres_adultas[!is.na(d$prop_mujeres_adultas)],
  V3 = d$log_arriendo_mensual[!is.na(d$log_arriendo_mensual)],
  V4 = as.numeric(d$presencia_analfabetismo == "Con al menos una persona analfabeta")
)
TITULO <- c(
  V1 = "V1: distribucion muestral de la media (prop. con educacion superior)",
  V2 = "V2: distribucion muestral de la media (prop. de mujeres adultas)",
  V3 = "V3: distribucion muestral de la media (log del arriendo)",
  V4 = "V4: distribucion muestral de la proporcion (presencia de analfabetismo)")
EJEX <- c(V1 = "Media muestral", V2 = "Media muestral",
          V3 = "Media muestral del logaritmo", V4 = "Proporcion muestral")
ARCH <- c(V1 = "17_consistencia_v1.png", V2 = "18_consistencia_v2.png",
          V3 = "19_consistencia_v3.png", V4 = "20_consistencia_v4.png")

BARRA <- "#8C8C8C"
PARAM <- "#D55E00"   # bermellon: el parametro poblacional
EJE   <- "#4D4D4D"


# ---------------------------------------------------------------------------
# 1. SIMULACION - mismo recorrido que el script 08, guardando las replicas
# ---------------------------------------------------------------------------
set.seed(SEMILLA)
REP <- list()
t0 <- Sys.time()

for (v in names(POB)) {
  x <- POB[[v]]
  for (n in NS) {
    ms <- numeric(B); md <- numeric(B)
    for (b in seq_len(B)) {
      s <- sample(x, n, replace = TRUE)
      ms[b] <- mean(s); md[b] <- median(s)
    }
    REP[[paste(v, n, sep = "_")]] <- ms
    cat(sprintf("  %s  n=%5d  listo  (%.0f s)\n", v, n,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    flush.console()
  }
}


# ---------------------------------------------------------------------------
# 2. UN PANEL 2x2 POR VARIABLE, CON EJE HORIZONTAL COMPARTIDO
# ---------------------------------------------------------------------------
for (v in names(POB)) {
  MU <- mean(POB[[v]])
  todos <- unlist(REP[paste(v, NS, sep = "_")])
  rango <- range(todos)

  png(file.path(RUTA_G, ARCH[[v]]), width = 1800, height = 1250, res = 165)
  op <- par(mfrow = c(2, 2), oma = c(4.5, 3.5, 5, 1),
            mar = c(3.2, 4.2, 3, 1), col.axis = EJE, col.lab = EJE,
            las = 1, mgp = c(2.8, 0.7, 0))

  for (n in NS) {
    ms <- REP[[paste(v, n, sep = "_")]]
    h  <- hist(ms, breaks = 45, plot = FALSE)
    hist(ms, breaks = 45, xlim = rango, col = BARRA, border = "white",
         main = sprintf("n = %s", format(n, big.mark = ".", decimal.mark = ",")),
         font.main = 1, cex.main = 1.15, xlab = "", ylab = "Replicas",
         xaxt = "n", yaxt = "n")
    # Eje X con coma decimal, para no mezclar punto y coma en el mismo panel.
    ax <- axTicks(1)
    axis(1, at = ax, labels = format(ax, decimal.mark = ",", trim = TRUE),
         cex.axis = 0.85)
    ay <- pretty(c(0, max(h$counts)))
    axis(2, at = ay, labels = format(ay, big.mark = ".", decimal.mark = ",", trim = TRUE),
         cex.axis = 0.85)
    abline(v = MU, col = PARAM, lwd = 2)
    # La desviacion de las replicas, escrita en el propio panel.
    mtext(sprintf("desv. est. = %s",
                  format(round(sd(ms), 6), decimal.mark = ",", nsmall = 6)),
          side = 3, line = 0.1,
          cex = 0.72, col = EJE)
  }

  mtext(TITULO[[v]], side = 3, outer = TRUE, line = 2.4, cex = 1.05)
  mtext(sprintf("B = %s replicas por tamano, muestreo con reemplazo, semilla %d. La linea vertical es el parametro poblacional.",
                format(B, big.mark = ".", decimal.mark = ","), SEMILLA),
        side = 3, outer = TRUE, line = 0.6, cex = 0.78, col = EJE)
  mtext(EJEX[[v]], side = 1, outer = TRUE, line = 1.6, cex = 0.95, col = EJE)
  mtext("Los cuatro paneles comparten el eje horizontal: por eso se ve el estrechamiento.",
        side = 1, outer = TRUE, line = 3.1, cex = 0.75, col = EJE)
  par(op)
  dev.off()
  cat("escrito:", ARCH[[v]], "\n")
}

cat("\nTiempo total:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")

# Control: estas desviaciones deben ser la raiz de las varianzas del script 08.
cat("\nDesviacion estandar de las replicas de la media (raiz de var_media):\n")
ctrl <- t(sapply(names(POB), function(v)
  sapply(NS, function(n) sd(REP[[paste(v, n, sep = "_")]]))))
colnames(ctrl) <- paste0("n=", NS)
print(round(ctrl, 6))
